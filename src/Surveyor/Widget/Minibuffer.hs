{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | An emacs-style minibuffer as a Brick widget
module Surveyor.Widget.Minibuffer (
  Minibuffer,
  minibuffer,
  handleMinibufferEvent,
  renderMinibuffer,
  Command(..)
  ) where

import qualified Brick as B
import qualified Brick.Widgets.Edit as B
import qualified Brick.Widgets.List as B
import           Control.Monad.IO.Class ( liftIO )
import qualified Data.Foldable as F
import qualified Data.Functor.Const as C
import qualified Data.List as L
import qualified Data.Map as M
import           Data.Parameterized.Classes
import qualified Data.Parameterized.List as PL
import qualified Data.Text as T
import qualified Data.Text.Zipper.Generic as Z
import qualified Data.Vector as V
import qualified Graphics.Vty as V
import           Text.Printf ( printf )
import qualified Text.RE.TDFA.Text as RE

data Command a r where
  Command :: T.Text
          -- ^ Command name
          -> PL.List (C.Const T.Text) tps
          -- ^ Argument names
          -> PL.List r tps
          -- ^ Argument types
          -> (PL.List a tps -> IO ())
          -- ^ Function to call on the argument list
          -> Command a r

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
             , allCommands :: !(V.Vector (Command a r))
             , commandIndex :: !(M.Map T.Text (Command a r))
             , matchedCommands :: !(V.Vector (Command a r))
             , matchedCommandsList :: !(B.List n (Command a r))
             , selectedMatch :: !Int
             , state :: MinibufferState a r
             , focusedListAttr :: B.AttrName
             , parseArgument :: forall tp . t -> r tp -> Maybe (a tp)
             , showRepr :: forall tp . r tp -> T.Text
             }

-- | Create a new 'Minibuffer' state.
--
-- The minibuffer supports the given set of commands
minibuffer :: (Z.GenericTextZipper t)
           => (forall tp . t -> r tp -> Maybe (a tp))
           -- ^ Parse a textual object into an argument
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
           -> [Command a r]
           -- ^ The commands supported by the minibuffer
           -> Minibuffer a r t n
minibuffer parseArg showRep attr edName compName pfx cmds =
  Minibuffer { prefix = pfx
             , editor = B.editor edName (Just 1) mempty
             , completionName = compName
             , allCommands = V.fromList cmds
             , commandIndex = F.foldl' indexCommand M.empty cmds
             , matchedCommands = V.empty
             , matchedCommandsList = B.list compName V.empty 1
             , selectedMatch = 0
             , state = Editing
             , focusedListAttr = attr
             , parseArgument = parseArg
             , showRepr = showRep
             }
  where
    indexCommand m cmd@(Command name _ _ _) = M.insert name cmd m

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
handleMinibufferEvent :: (Ord n, Eq t, Monoid t, Z.GenericTextZipper t, TestEquality r)
                      => V.Event
                      -> Minibuffer a r t n
                      -> B.EventM n (Minibuffer a r t n)
handleMinibufferEvent evt mb@(Minibuffer { parseArgument = parseArg }) =
  case evt of
    V.EvKey V.KEnter [] ->
      case state mb of
        Editing -> do
          -- If we are waiting for a command, try to accept the command and start
          -- processing arguments.  If there are no arguments, activate the command immediately
          let val = Z.toList (mconcat (B.getEditContents (editor mb)))
          case M.lookup (T.pack val) (commandIndex mb) of
            Nothing -> return mb
            Just (Command _ argNames argTypes callback) ->
              case (argNames, argTypes) of
                (PL.Nil, PL.Nil) -> do
                  liftIO (callback PL.Nil)
                  return (resetMinibuffer mb)
                _ -> do
                  return mb { state = CollectingArguments argNames argTypes PL.Nil PL.Nil argTypes callback
                            , editor = clearEditor (editor mb)
                            }
        CollectingArguments expectedArgNames expectedArgTypes collectedArgTypes collectedArgValues callbackType callback ->
          case (expectedArgNames, expectedArgTypes) of
            (PL.Nil, PL.Nil) ->
              withReversedF collectedArgTypes collectedArgValues $ \collectedArgTypes' collectedArgValues' -> do
                case () of
                  _ | Just Refl <- testEquality callbackType collectedArgTypes' -> do
                        liftIO (callback collectedArgValues')
                        return (resetMinibuffer mb)
                    | otherwise -> error "impossible"
            (C.Const _expectedArgName PL.:< restArgs, expectedArgType PL.:< restTypes) -> do
              let val = mconcat (B.getEditContents (editor mb))
              case parseArg val expectedArgType of
                Just arg ->
                  return mb { state = CollectingArguments restArgs restTypes (expectedArgType PL.:< collectedArgTypes) (arg PL.:< collectedArgValues) callbackType callback
                            }
                Nothing -> return mb
    V.EvKey (V.KChar '\t') [] ->
      -- If there is a single match, replace the editor contents with it.
      -- Otherwise, do nothing.
      case V.length (matchedCommands mb) == 1 of
        True -> do
          let Command ctxt _ _ _ = matchedCommands mb V.! 0
          let str = T.unpack ctxt
          let chars = map Z.singleton str
          return mb { editor = B.applyEdit (const (Z.textZipper [mconcat chars] Nothing)) (editor mb)
                    }
        _ -> return mb
    V.EvKey (V.KChar 'n') [V.MCtrl] ->
      -- Select the next match in the completion list
      return mb { selectedMatch = min (V.length (matchedCommands mb)) (selectedMatch mb + 1) }
    V.EvKey (V.KChar 'p') [V.MCtrl] ->
      -- Select the previous match in the completion list
      return mb { selectedMatch = max 0 (selectedMatch mb - 1) }
    V.EvKey (V.KChar 'g') [V.MCtrl] ->
      -- Cancel everything and reset to a base state (including an empty editor line)
      return (resetMinibuffer mb)
    _ -> do
      editor' <- B.handleEditorEvent evt (editor mb)
      let val = Z.toList (mconcat (B.getEditContents editor'))
      let reStr = L.intercalate ".*" (map RE.escapeREString (words val))
      case RE.compileRegex reStr of
        Nothing -> return mb { editor = editor' }
        Just re -> do
          let matches = V.filter (commandMatches re) (allCommands mb)
          return mb { editor = editor'
                    , matchedCommands = matches
                    , selectedMatch =
                      if selectedMatch mb >= V.length matches then 0 else selectedMatch mb
                    }

clearEditor :: (Z.GenericTextZipper t) => B.Editor t n -> B.Editor t n
clearEditor = B.applyEdit (const (Z.textZipper [] Nothing))

resetMinibuffer :: (Z.GenericTextZipper t) => Minibuffer a r t n -> Minibuffer a r t n
resetMinibuffer mb = Minibuffer { prefix = prefix mb
                                , editor = clearEditor (editor mb)
                                , completionName = completionName mb
                                , allCommands = allCommands mb
                                , commandIndex = commandIndex mb
                                , matchedCommands = V.empty
                                , matchedCommandsList = B.list (completionName mb) V.empty 1
                                , selectedMatch = 0
                                , state = Editing
                                , focusedListAttr = focusedListAttr mb
                                , parseArgument = parseArgument mb
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

commandMatches :: RE.RE -> Command a r -> Bool
commandMatches rx (Command name _ _ _) = RE.matched (name RE.?=~ rx)

renderMinibuffer :: (Ord n, Show n, B.TextWidth t, Z.GenericTextZipper t)
                 => Bool
                 -> Minibuffer a r t n
                 -> B.Widget n
renderMinibuffer hasFocus mb =
  case state mb of
    Editing ->
      let editorLine = B.hBox [ B.str (printf "%d %s " (V.length (matchedCommands mb)) (prefix mb))
                              , B.renderEditor (drawContent mb) hasFocus (editor mb)
                              ]
          compList = B.vLimit 5 (B.renderList (renderCompletionItem mb) False (matchedCommandsList mb))
      in B.vBox [editorLine, compList]
    CollectingArguments expectedArgNames expectedArgTypes _collectedArgTypes _collectedArgValues _callbackType _callback ->
      case (expectedArgNames, expectedArgTypes) of
        (PL.Nil, PL.Nil) -> error "impossible"
        (C.Const name PL.:< _, ty PL.:< _) ->
          B.hBox [ B.str (printf "%s (%s): " name (showRepr mb ty))
                 , B.renderEditor (drawContent mb) hasFocus (editor mb)
                 ]

renderCompletionItem :: forall a r t n . Minibuffer a r t n -> Bool -> Command a r -> B.Widget n
renderCompletionItem mb isFocused (Command name argNames argTypes _) =
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

drawContent :: (Monoid t, Z.GenericTextZipper t) => Minibuffer a r t n -> [t] -> B.Widget n
drawContent _mb txts = B.str (Z.toList (mconcat txts))
