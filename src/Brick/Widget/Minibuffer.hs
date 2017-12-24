{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | An emacs-style minibuffer as a Brick widget
module Brick.Widget.Minibuffer (
  Minibuffer,
  minibuffer,
  MinibufferStatus(..),
  handleMinibufferEvent,
  renderMinibuffer
  ) where

import qualified Brick as B
import qualified Brick.Widgets.Edit as B
import qualified Brick.Widgets.List as B
import qualified Control.Lens as L
import           Control.Monad.IO.Class ( liftIO )
import qualified Data.Foldable as F
import qualified Data.Functor.Const as C
import qualified Data.Map as M
import           Data.Parameterized.Classes
import qualified Data.Parameterized.List as PL
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Text as T
import qualified Data.Text.Zipper as Z
import qualified Data.Text.Zipper.Generic as ZG
import qualified Data.Vector as V
import qualified Graphics.Vty as V
import           Text.Printf ( printf )

import           Brick.Command ( Command(..) )
import qualified Brick.Match.Subword as SW

data MinibufferState a r where
  CollectingArguments :: PL.List (C.Const T.Text) tps
                      -> PL.List r tps
                      -> PL.List r tps'
                      -> PL.List a tps'
                      -> PL.List r tps0
                      -> (PL.List a tps0 -> IO ())
                      -> MinibufferState a r
  -- ^ In the process of collecting arguments
  Editing :: MinibufferState a r
  -- ^ The input editor has focus

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
data Minibuffer a r t n =
  Minibuffer { prefix :: !T.Text
             , editor :: !(B.Editor t n)
             , completionName :: n
             , allCommands :: !(V.Vector (Some (Command a r)))
             , commandIndex :: !(M.Map T.Text (Some (Command a r)))
             , matchedCommands :: !(V.Vector (Some (Command a r)))
             , matchedCommandsList :: !(B.List n (Some (Command a r)))
             , argumentCompletionsList :: !(B.List n t)
             , state :: MinibufferState a r
             , focusedListAttr :: B.AttrName
             , parseArgument :: forall tp . t -> r tp -> Maybe (a tp)
             , completeArgument :: forall tp . t -> r tp -> IO (V.Vector t)
             , showRepr :: forall tp . r tp -> T.Text
             }

-- | Create a new 'Minibuffer' state.
--
-- The minibuffer supports the given set of commands
minibuffer :: (ZG.GenericTextZipper t)
           => (forall tp . t -> r tp -> Maybe (a tp))
           -- ^ Parse a textual object into an argument
           -> (forall tp . t -> r tp -> IO (V.Vector t))
           -- ^ Generate completions for an argument of an expected type; note
           -- that this runs in the UI thread and should be quick.
           -> (forall tp . r tp -> T.Text)
           -- ^ Convert a type repr into a friendly name
           -> B.AttrName
           -- ^ The attribute used to highlight the current completion target
           -> n
           -- ^ The name to assign to the editor widget
           -> n
           -- ^ The name to assign to the completion list
           -> T.Text
           -- ^ The prefix to display before the editor widget
           -> [Some (Command a r)]
           -- ^ The commands supported by the minibuffer
           -> Minibuffer a r t n
minibuffer parseArg compArg showRep attr edName compName pfx cmds =
  Minibuffer { prefix = pfx
             , editor = B.editor edName (Just 1) mempty
             , completionName = compName
             , allCommands = allCmds
             , commandIndex = F.foldl' indexCommand M.empty cmds
             , matchedCommands = allCmds
             , matchedCommandsList = B.list compName allCmds 1
             , argumentCompletionsList = B.list compName V.empty 1
             , state = Editing
             , focusedListAttr = attr
             , parseArgument = parseArg
             , completeArgument = compArg
             , showRepr = showRep
             }
  where
    allCmds = V.fromList cmds
    indexCommand m (Some cmd) = M.insert (cmdName cmd) (Some cmd) m

data MinibufferStatus a r t n = Completed (Minibuffer a r t n)
                              | Canceled (Minibuffer a r t n)

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
handleMinibufferEvent :: (Ord n, Eq t, Monoid t, ZG.GenericTextZipper t, TestEquality r)
                      => V.Event
                      -> Minibuffer a r t n
                      -> B.EventM n (MinibufferStatus a r t n)
handleMinibufferEvent evt mb@(Minibuffer { parseArgument = parseArg }) =
  case evt of
    V.EvKey V.KEnter [] ->
      case state mb of
        Editing -> do
          -- If we are waiting for a command, try to accept the command and start
          -- processing arguments.  If there are no arguments, activate the command immediately
          let val = ZG.toList (mconcat (B.getEditContents (editor mb)))
          case M.lookup (T.pack val) (commandIndex mb) of
            Nothing -> return (Completed mb)
            Just (Some (Command _ _ argNames argTypes callback)) ->
              case (argNames, argTypes) of
                (PL.Nil, PL.Nil) -> do
                  liftIO (callback PL.Nil)
                  return (Completed (resetMinibuffer mb))
                _ -> do
                  return $ Completed mb { state = CollectingArguments argNames argTypes PL.Nil PL.Nil argTypes callback
                                        , editor = clearEditor (editor mb)
                                        }
        CollectingArguments expectedArgNames expectedArgTypes collectedArgTypes collectedArgValues callbackType callback ->
          case (expectedArgNames, expectedArgTypes) of
            (PL.Nil, PL.Nil) ->
              withReversedF collectedArgTypes collectedArgValues $ \collectedArgTypes' collectedArgValues' -> do
                case () of
                  _ | Just Refl <- testEquality callbackType collectedArgTypes' -> do
                        liftIO (callback collectedArgValues')
                        return (Completed (resetMinibuffer mb))
                    | otherwise -> error "impossible"
            (C.Const _expectedArgName PL.:< restArgs, expectedArgType PL.:< restTypes) -> do
              let val = mconcat (B.getEditContents (editor mb))
              case parseArg val expectedArgType of
                Just arg ->
                  case (restArgs, restTypes) of
                    (PL.Nil, PL.Nil) ->
                      withReversedF (expectedArgType PL.:< collectedArgTypes) (arg PL.:< collectedArgValues) $ \collectedArgTypes' collectedArgValues' -> do
                        case () of
                          _ | Just Refl <- testEquality callbackType collectedArgTypes' -> do
                                liftIO (callback collectedArgValues')
                                return (Completed (resetMinibuffer mb))
                            | otherwise -> error "impossible"
                    (_, _) ->
                      return $ Completed mb { state = CollectingArguments restArgs
                                                                          restTypes
                                                                          (expectedArgType PL.:< collectedArgTypes)
                                                                          (arg PL.:< collectedArgValues)
                                                                          callbackType
                                                                          callback
                                            }
                Nothing -> return (Completed mb)
    V.EvKey (V.KChar '\t') [] ->
      case state mb of
        Editing -> do
          -- If there is a single match, replace the editor contents with it.
          -- Otherwise, do nothing.
          case V.length (matchedCommands mb) == 1 of
            False -> return (Completed mb)
            True -> do
              case matchedCommands mb V.! 0 of
                Some cmd -> do
                  let str = T.unpack (cmdName cmd)
                  let chars = map ZG.singleton str
                  let newZipper = Z.gotoEOL (ZG.textZipper [mconcat chars] Nothing)
                  return $ Completed mb { editor = B.applyEdit (const newZipper) (editor mb)
                                        }
        CollectingArguments {} -> do
          let completions = argumentCompletionsList mb
          case V.length (completions L.^. B.listElementsL) == 1 of
            False -> return (Completed mb)
            True -> do
              let cmpTxt = (completions L.^. B.listElementsL) V.! 0
              let newZipper = Z.gotoEOL (ZG.textZipper [cmpTxt] Nothing)
              return $ Completed mb { editor = B.applyEdit (const newZipper) (editor mb)
                                    }
    V.EvKey (V.KChar 'n') [V.MCtrl] ->
      -- Select the next match in the completion list
      return $ Completed mb { matchedCommandsList = B.listMoveDown (matchedCommandsList mb) }
    V.EvKey (V.KChar 'p') [V.MCtrl] ->
      -- Select the previous match in the completion list
      return $ Completed mb { matchedCommandsList = B.listMoveUp (matchedCommandsList mb) }
    V.EvKey (V.KChar 'g') [V.MCtrl] ->
      -- Cancel everything and reset to a base state (including an empty editor line)
      return $ Canceled (resetMinibuffer mb)
    _ -> do
      -- All other keys go to the editor widget
      editor' <- B.handleEditorEvent evt (editor mb)
      let val = mconcat (B.getEditContents editor')
      case state mb of
        Editing -> do
          -- If we are waiting for a command, we update the matched commands
          -- list with possible completions of the command.
          case SW.matcher (ZG.toList val) of
            Nothing -> return $ Completed mb { editor = editor' }
            Just re -> do
              let matches = V.filter (commandMatches re) (allCommands mb)
              let oldSel = matchedCommandsList mb L.^. B.listSelectedL
              let newSel = fmap (\ix -> if ix >= V.length matches then 0 else ix) oldSel
              let newList = B.list (completionName mb) matches 1
              return $ Completed mb { editor = editor'
                                    , matchedCommands = matches
                                    , matchedCommandsList = L.set B.listSelectedL newSel newList
                                    }
        CollectingArguments _expectedArgNames expectedArgTypes _ _ _ _ -> do
          -- If we are collecting arguments, update the possible argument
          -- completions instead.
          case expectedArgTypes of
            PL.Nil -> error "impossible"
            nextTy PL.:< _ -> do
              comps <- liftIO (completeArgument mb val nextTy)
              let newList = B.list (completionName mb) comps 1
              return $ Completed mb { editor = editor'
                                    , argumentCompletionsList = newList
                                    }


clearEditor :: (ZG.GenericTextZipper t) => B.Editor t n -> B.Editor t n
clearEditor = B.applyEdit (const (ZG.textZipper [] Nothing))

resetMinibuffer :: (ZG.GenericTextZipper t) => Minibuffer a r t n -> Minibuffer a r t n
resetMinibuffer mb = Minibuffer { prefix = prefix mb
                                , editor = clearEditor (editor mb)
                                , completionName = completionName mb
                                , allCommands = allCommands mb
                                , commandIndex = commandIndex mb
                                , matchedCommands = allCommands mb
                                , matchedCommandsList = B.list (completionName mb) (allCommands mb) 1
                                , argumentCompletionsList = B.list (completionName mb) V.empty 1
                                , state = Editing
                                , focusedListAttr = focusedListAttr mb
                                , parseArgument = parseArgument mb
                                , completeArgument = completeArgument mb
                                , showRepr = showRepr mb
                                }

withReversedF :: forall a b c tps . PL.List a tps -> PL.List b tps -> (forall tps' . PL.List a tps' -> PL.List b tps' -> c) -> c
withReversedF l1 l2 k = go PL.Nil PL.Nil l1 l2
  where
    go :: PL.List a tps0 -> PL.List b tps0 -> PL.List a tps1 -> PL.List b tps1 -> c
    go acc1 acc2 lst1 lst2 =
      case (lst1, lst2) of
        (PL.Nil, PL.Nil) -> k acc1 acc2
        (x1 PL.:< xs1, x2 PL.:< xs2) -> go (x1 PL.:< acc1) (x2 PL.:< acc2) xs1 xs2

commandMatches :: SW.Matcher -> Some (Command a r) -> Bool
commandMatches m (Some cmd) = SW.matches m (cmdName cmd)

renderMinibuffer :: (Ord n, Show n, B.TextWidth t, ZG.GenericTextZipper t)
                 => Bool
                 -> Minibuffer a r t n
                 -> B.Widget n
renderMinibuffer hasFocus mb =
  case state mb of
    Editing ->
      let matches = V.length (matchedCommands mb)
          editorLine = B.hBox [ B.str (printf "%d %s " matches (prefix mb))
                              , B.renderEditor (drawContent mb) hasFocus (editor mb)
                              ]
          compList = B.renderList (renderCommandCompletionItem mb) False (matchedCommandsList mb)
      in B.vBox [ B.vLimit 1 editorLine
                , B.vLimit (min matches 5) compList
                ]
    CollectingArguments expectedArgNames expectedArgTypes _collectedArgTypes _collectedArgValues _callbackType _callback ->
      case (expectedArgNames, expectedArgTypes) of
        (PL.Nil, PL.Nil) -> error "impossible"
        (C.Const name PL.:< _, ty PL.:< _) ->
          let argComps = argumentCompletionsList mb
              editorLine = B.hBox [ B.str (printf "%s (%s): " name (showRepr mb ty))
                                  , B.renderEditor (drawContent mb) hasFocus (editor mb)
                                  ]
              matches = V.length (argComps L.^. B.listElementsL)
              compList =
                if matches == 0
                then B.emptyWidget
                else B.renderList (renderArgumentCompletionItem mb) False argComps
          in B.vBox [ B.vLimit 1 editorLine
                    , B.vLimit (min matches 5) compList
                    ]

renderCommandCompletionItem :: forall a r t n . Minibuffer a r t n -> Bool -> Some (Command a r) -> B.Widget n
renderCommandCompletionItem mb isFocused (Some (Command name _ argNames argTypes _)) =
  let xfrm = if isFocused then B.withAttr (focusedListAttr mb) else id
  in xfrm (B.str (printf "%s (%s)" name sig))
  where
    sig = T.intercalate " -> " [ T.pack (printf "%s :: %s" n t)
                               | (n, t) <- args
                               ]
    args = collect argNames argTypes
    collect :: forall tps . PL.List (C.Const T.Text) tps -> PL.List r tps -> [(T.Text, T.Text)]
    collect names types =
      case (names, types) of
        (PL.Nil, PL.Nil) -> []
        (C.Const n PL.:< rn, t PL.:< rt) -> (n, showRepr mb t) : collect rn rt

renderArgumentCompletionItem :: forall a r t n
                              . (Monoid t, ZG.GenericTextZipper t)
                             => Minibuffer a r t n
                             -> Bool
                             -> t
                             -> B.Widget n
renderArgumentCompletionItem mb isFocused t =
  let xfrm = if isFocused then B.withAttr (focusedListAttr mb) else id
  in xfrm (B.str (ZG.toList t))

drawContent :: (Monoid t, ZG.GenericTextZipper t) => Minibuffer a r t n -> [t] -> B.Widget n
drawContent _mb txts = B.str (ZG.toList (mconcat txts))
