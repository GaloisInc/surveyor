{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType #-}
-- | An emacs-style minibuffer as a Brick widget
module Brick.Widget.Minibuffer (
  Completer(..),
  CompletionType(..),
  Minibuffer,
  minibuffer,
  activeCompletionTarget,
  setCompletions,
  MinibufferStatus(..),
  handleMinibufferEvent,
  renderMinibuffer
  ) where

import qualified Brick as B
import qualified Brick.Widgets.Edit as BWE
import qualified Control.Concurrent.Async as A
import           Control.Monad.IO.Class ( liftIO )
import qualified Data.Foldable as F
import qualified Data.Functor.Const as C
import           Data.Kind ( Type )
import           Data.Maybe ( fromMaybe )
import           Data.Parameterized.Classes
import qualified Data.Parameterized.List as PL
import qualified Data.Text as T
import qualified Data.Text.Zipper.Generic as ZG
import qualified Data.Vector as V
import qualified Graphics.Vty as V
import           Text.Printf ( printf )

import qualified Surveyor.Core as C
import qualified Brick.Match.Subword as SW
import qualified Brick.Widget.FilterList as FL

data MinibufferState b where
  -- | In the process of collecting arguments
  CollectingArguments :: forall b (tps :: [C.ArgumentKind b]) (tps' :: [C.ArgumentKind b]) (tps0 :: [C.ArgumentKind b])
                       . PL.List (C.Const T.Text) tps
                      -> PL.List (C.ArgumentRepr b) tps
                      -> PL.List (C.ArgumentRepr b) tps'
                      -> PL.List (C.ArgumentType b) tps'
                      -> PL.List (C.ArgumentRepr b) tps0
                      -> (C.Chan (C.EventType b) -> C.StateType b -> PL.List (C.ArgumentType b) tps0 -> IO ())
                      -> MinibufferState b
  -- | The input editor has focus
  Editing :: MinibufferState b

-- | The abstract state of a minibuffer
--
-- The @a@ type parameter is the type of arguments for commands
--
-- The @r@ type parameter is the type of the type representative for arguments
--
-- The @t@ parameter is the type of the content (probably 'T.Text', but anything
-- that implements 'GenericTextZipper').
--
-- The @n@ parameter is the type of the names used to identify widgets in your
-- application.
data Minibuffer b t n =
  Minibuffer { prefix :: !T.Text
             , commandList :: FL.FilterList n t (C.SomeCommand b)
             , argumentList :: FL.FilterList n t t
             , allCommands :: !(V.Vector (C.SomeCommand b))
             , state :: MinibufferState b
             , parseArgument :: forall (tp :: C.ArgumentKind b) . t -> C.ArgumentRepr b tp -> Maybe (C.ArgumentType b tp)
             , completeArgument :: CompletionType b t
             , outstandingCompletion :: Maybe (t, A.Async ())
             , showRepr :: forall (tp :: C.ArgumentKind b) . C.ArgumentRepr b tp -> T.Text
             }

data Completer b t = Completer (forall (tp :: C.ArgumentKind b) . t -> C.ArgumentRepr b tp -> IO (V.Vector t))
data CompletionType b t = Synchronous (Completer b t)
                        | Asynchronous (Completer b t) (t -> V.Vector t -> IO ())

activeCompletionTarget :: Minibuffer b t n -> Maybe t
activeCompletionTarget = fmap fst . outstandingCompletion

-- | Create a new 'Minibuffer' state.
--
-- The minibuffer supports the given set of commands
minibuffer :: (ZG.GenericTextZipper t)
           => (forall (tp :: C.ArgumentKind b) . t -> C.ArgumentRepr b tp -> Maybe (C.ArgumentType b tp))
           -- ^ Parse a textual object into an argument
           -> CompletionType b t
           -- ^ Generate completions for an argument of an expected type; note
           -- that this runs in the UI thread and should be quick.
           -> (forall (tp :: C.ArgumentKind b) . C.ArgumentRepr b tp -> T.Text)
           -- ^ Convert a type repr into a friendly name
           -> B.AttrName
           -- ^ The attribute used to highlight the current completion target
           -> n
           -- ^ The name to assign to the editor widget
           -> n
           -- ^ The name to assign to the completion list
           -> T.Text
           -- ^ The prefix to display before the editor widget
           -> [C.SomeCommand b]
           -- ^ The commands supported by the minibuffer
           -> Minibuffer b t n
minibuffer parseArg compArg showRep attr edName compName pfx cmds =
  Minibuffer { prefix = pfx
             , commandList = FL.filterList commandConfig allCmds
             , argumentList = FL.filterList argumentConfig V.empty
             , allCommands = allCmds
             , state = Editing
             , parseArgument = parseArg
             , completeArgument = compArg
             , showRepr = showRep
             , outstandingCompletion = Nothing
             }
  where
    allCmds = V.fromList cmds
    commandConfig = FL.FilterListConfig { FL.flEditorName = edName
                                        , FL.flListName = compName
                                        , FL.flEditorPosition = FL.Top
                                        , FL.flMaxListHeight = Just 5
                                        , FL.flToText = \(C.SomeCommand cmd) ->
                                            mconcat (map ZG.singleton (T.unpack (C.cmdName cmd)))
                                        , FL.flRenderListItem = renderCommandCompletionItem showRep attr
                                        , FL.flRenderEditorContent = drawContent
                                        }
    argumentConfig = FL.FilterListConfig { FL.flEditorName = edName
                                         , FL.flListName = compName
                                         , FL.flEditorPosition = FL.Top
                                         , FL.flMaxListHeight = Just 5
                                         , FL.flToText = id
                                         , FL.flRenderEditorContent = drawContent
                                         , FL.flRenderListItem = renderArgumentCompletionItem attr
                                         }

data MinibufferStatus b t n = Completed (Minibuffer b t n)
                            | Executed (Minibuffer b t n)
                            | Canceled (Minibuffer b t n)

-- | Extract the argument from the argument filter list
--
-- If there is an argument selected in the completion list, return it.
-- Otherwise, return the textual value in the editor
argumentValue :: (Monoid t) => FL.FilterList n t t -> t
argumentValue argList =
  fromMaybe (FL.editorContents argList) (FL.selectedItem argList)

-- | Largely delegate events to the editor widget, but also handles some completion tasks
--
-- The additional features over the default editor widget include:
--
--  * Completion of commands (unique commands can be completed with <tab>, while
--    multiple valid completions can be cycled with tab)
--
--  * After a command is selected (with <enter>), the minibuffer will prompt for
--    the required number of arguments
--
--  * Pressing <C-g> while a command is processing arguments or waiting for a
--    command will deactivate the minibuffer
handleMinibufferEvent :: forall b n t (r :: C.ArgumentKind b -> Type)
                      . ( Ord n, Eq t, Monoid t
                        , ZG.GenericTextZipper t
                        , C.CommandLike b
                        , r ~ C.ArgumentRepr b
                        , TestEquality r
                        , BWE.DecodeUtf8 t
                        )
                      => V.Event
                      -> C.Chan (C.EventType b)
                      -> C.StateType b
                      -> Minibuffer b t n
                      -> B.EventM n (MinibufferStatus b t n)
handleMinibufferEvent evt customEventChan s mb@(Minibuffer { parseArgument = parseArg }) =
  case evt of
    V.EvKey V.KEnter [] ->
      case state mb of
        Editing -> do
          -- If we are waiting for a command, try to accept the command and
          -- start processing arguments.  If there are no arguments, activate
          -- the command immediately
          case FL.selectedItem (commandList mb) of
            Nothing -> return (Completed mb)
            Just (C.SomeCommand (C.Command _ _ argNames argTypes callback _)) ->
              case (argNames, argTypes) of
                (PL.Nil, PL.Nil) -> do
                  liftIO (callback customEventChan s PL.Nil)
                  return (Executed (resetMinibuffer mb))
                _ ->
                  return $ Completed mb { state = CollectingArguments argNames argTypes PL.Nil PL.Nil argTypes callback
                                        , commandList = FL.resetList (commandList mb)
                                        , argumentList = FL.resetList (argumentList mb)
                                        }
        CollectingArguments expectedArgNames expectedArgTypes collectedArgTypes collectedArgValues callbackType callback ->
          case (expectedArgNames, expectedArgTypes) of
            (PL.Nil, PL.Nil) ->
              withReversedF collectedArgTypes collectedArgValues $ \collectedArgTypes' collectedArgValues' -> do
                case () of
                  _ | Just Refl <- testEquality callbackType collectedArgTypes' -> do
                        liftIO (callback customEventChan s collectedArgValues')
                        return (Executed (resetMinibuffer mb))
                    | otherwise -> error "impossible"
            (C.Const _expectedArgName PL.:< restArgs, expectedArgType PL.:< restTypes) -> do
              let completedArg = argumentValue (argumentList mb)
              case parseArg completedArg expectedArgType of
                Nothing -> return (Completed mb)
                Just parsedArg ->
                  case (restArgs, restTypes) of
                    (PL.Nil, PL.Nil) ->
                      withReversedF (expectedArgType PL.:< collectedArgTypes) (parsedArg PL.:< collectedArgValues) $ \collectedArgTypes' collectedArgValues' -> do
                        case () of
                          _ | Just Refl <- testEquality callbackType collectedArgTypes' -> do
                                liftIO (callback customEventChan s collectedArgValues')
                                return (Executed (resetMinibuffer mb))
                            | otherwise -> error "impossible"
                    (_, _) ->
                      return $ Completed mb { state = CollectingArguments restArgs
                                                                          restTypes
                                                                          (expectedArgType PL.:< collectedArgTypes)
                                                                          (parsedArg PL.:< collectedArgValues)
                                                                          callbackType
                                                                          callback
                                            , argumentList = FL.resetList (argumentList mb)
                                            , commandList = FL.resetList (commandList mb)
                                            }
    V.EvKey (V.KChar 'g') [V.MCtrl] ->
      -- Cancel everything and reset to a base state (including an empty editor line)
      return $ Canceled (resetMinibuffer mb)
    _ -> do
      -- All other keys go to the editor widget
      case state mb of
        Editing -> do
          let edContents0 = FL.editorContents (commandList mb)
          cmdList' <- FL.handleFilterListEvent evt (commandList mb)
          let completionTarget = FL.editorContents cmdList'
          if | edContents0 == completionTarget -> return $ Completed mb { commandList = cmdList' }
             | otherwise -> do
               cmdList'' <- case SW.matcher (ZG.toList (FL.editorContents cmdList')) of
                 Nothing -> return cmdList'
                 Just matcher -> return (FL.updateList (V.filter (commandMatches matcher)) cmdList')
               return $ Completed mb { commandList = cmdList'' }
        CollectingArguments _expectedArgNames expectedArgTypes _ _ _ _ -> do
          case expectedArgTypes of
            PL.Nil -> error "impossible"
            nextTy PL.:< _ -> do
              let edContents0 = FL.editorContents (argumentList mb)
              argList' <- FL.handleFilterListEvent evt (argumentList mb)
              let completionTarget = FL.editorContents argList'
              if | edContents0 == completionTarget -> return $ Completed mb { argumentList = argList' }
                 | otherwise ->
                   case completeArgument mb of
                     Synchronous (Completer comp) -> do
                       completions <- liftIO $ comp completionTarget nextTy
                       return $ Completed mb { argumentList = FL.updateList (const completions) argList' }
                     Asynchronous (Completer comp) updateCallback -> do
                       F.forM_ (outstandingCompletion mb) $ \(_, oldCompleter) -> do
                         liftIO $ A.cancel oldCompleter
                       ac <- liftIO $ A.async $ do
                         completions <- comp completionTarget nextTy
                         updateCallback completionTarget completions
                       return $ Completed mb { argumentList = argList'
                                             , outstandingCompletion = Just (completionTarget, ac)
                                             }

setCompletions :: V.Vector t -> Minibuffer b t n -> Minibuffer b t n
setCompletions comps mb =
  mb { argumentList = FL.updateList (const comps) (argumentList mb) }


resetMinibuffer :: (ZG.GenericTextZipper t) => Minibuffer b t n -> Minibuffer b t n
resetMinibuffer mb = Minibuffer { prefix = prefix mb
                                , commandList = FL.resetList (commandList mb)
                                , argumentList = FL.resetList (argumentList mb)
                                , allCommands = allCommands mb
                                , state = Editing
                                , parseArgument = parseArgument mb
                                , completeArgument = completeArgument mb
                                , showRepr = showRepr mb
                                , outstandingCompletion = Nothing
                                }

withReversedF :: forall a b c tps
               . PL.List a tps
              -> PL.List b tps
              -> (forall tps' . PL.List a tps' -> PL.List b tps' -> c)
              -> c
withReversedF l1 l2 k = go PL.Nil PL.Nil l1 l2
  where
    go :: PL.List a tps0 -> PL.List b tps0 -> PL.List a tps1 -> PL.List b tps1 -> c
    go acc1 acc2 lst1 lst2 =
      case (lst1, lst2) of
        (PL.Nil, PL.Nil) -> k acc1 acc2
        (x1 PL.:< xs1, x2 PL.:< xs2) -> go (x1 PL.:< acc1) (x2 PL.:< acc2) xs1 xs2

commandMatches :: SW.Matcher -> C.SomeCommand b -> Bool
commandMatches m (C.SomeCommand cmd) = SW.matches m (C.cmdName cmd)

renderMinibuffer :: (Ord n, Show n, B.TextWidth t, ZG.GenericTextZipper t)
                 => Bool
                 -> Minibuffer b t n
                 -> B.Widget n
renderMinibuffer hasFocus mb =
  case state mb of
    Editing ->
      let renderPrompt n = B.str (printf "%d %s " n (prefix mb))
      in FL.renderFilterList renderPrompt hasFocus (commandList mb)
    CollectingArguments expectedArgNames expectedArgTypes _collectedArgTypes _collectedArgValues _callbackType _callback ->
      case (expectedArgNames, expectedArgTypes) of
        (PL.Nil, PL.Nil) -> error "impossible"
        (C.Const name PL.:< _, ty PL.:< _) ->
          case mb of
            Minibuffer { showRepr = showR } ->
              let renderPrompt _n = B.str (printf "%s (%s): " name (showR ty))
              in FL.renderFilterList renderPrompt hasFocus (argumentList mb)

renderCommandCompletionItem :: forall b n
                             . (forall (tp :: C.ArgumentKind b) . C.ArgumentRepr b tp -> T.Text)
                            -> B.AttrName
                            -> Bool
                            -> C.SomeCommand b
                            -> B.Widget n
renderCommandCompletionItem showRep focAttr isFocused (C.SomeCommand (C.Command name _ argNames argTypes _ _)) =
  let xfrm = if isFocused then B.withAttr focAttr else id
  in xfrm (B.str (printf "%s (%s)" name sig))
  where
    sig = T.intercalate " -> " [ T.pack (printf "%s :: %s" n t)
                               | (n, t) <- args
                               ]
    args = collect argNames argTypes
    collect :: forall tps . PL.List (C.Const T.Text) tps -> PL.List (C.ArgumentRepr b) tps -> [(T.Text, T.Text)]
    collect names types =
      case (names, types) of
        (PL.Nil, PL.Nil) -> []
        (C.Const n PL.:< rn, t PL.:< rt) -> (n, showRep t) : collect rn rt

renderArgumentCompletionItem :: forall t n
                              . (Monoid t, ZG.GenericTextZipper t)
                             => B.AttrName
                             -> Bool
                             -> t
                             -> B.Widget n
renderArgumentCompletionItem focAttr isFocused t =
  let xfrm = if isFocused then B.withAttr focAttr else id
  in xfrm (B.str (ZG.toList t))

drawContent :: (Monoid t, ZG.GenericTextZipper t) => [t] -> B.Widget n
drawContent txts = B.str (ZG.toList (mconcat txts))
