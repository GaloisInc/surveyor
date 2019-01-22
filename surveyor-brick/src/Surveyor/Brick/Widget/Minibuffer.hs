{-# LANGUAGE GADTs #-}
-- | An instantiation of the generic minibuffer widget with a specific argument type
module Surveyor.Brick.Widget.Minibuffer (
  MB.Minibuffer,
  minibuffer,
  MB.MinibufferStatus(..),
  MB.handleMinibufferEvent,
  MB.renderMinibuffer,
  ) where

import qualified Data.Text as T
import qualified Data.Text.Zipper.Generic as Z
import qualified Data.Vector as V

import           Surveyor.Brick.Attributes
import qualified Surveyor.Core as C
import qualified Brick.Match.Subword as SW
import qualified Brick.Widget.Minibuffer as MB

completeArgument :: (Z.GenericTextZipper t)
                 => [C.SomeCommand (C.SurveyorCommand s st)]
                 -> (t -> C.TypeRepr tp -> IO (V.Vector t))
completeArgument cmds =
  let cmdNames = V.fromList [ C.cmdName cmd | C.SomeCommand cmd <- cmds ]
  in \t r ->
    case r of
      C.StringTypeRepr -> return V.empty
      C.AddressTypeRepr -> return V.empty
      C.IntTypeRepr -> return V.empty
      C.WordTypeRepr -> return V.empty
      -- FIXME: We can actually do file path completion
      C.FilePathTypeRepr -> return V.empty
      C.CommandTypeRepr ->
        case SW.matcher (Z.toList t) of
          Nothing -> return V.empty
          Just matcher -> do
            let matches = V.filter (SW.matches matcher) cmdNames
            let toGeneric txt = mconcat (map Z.singleton (T.unpack txt))
            return (fmap toGeneric matches)

minibuffer :: (Z.GenericTextZipper t)
           => (String -> Maybe (C.SomeAddress s))
           -> (t -> V.Vector t -> IO ())
           -> n
           -- ^ The name of the editor widget
           -> n
           -- ^ The name of the completion list
           -> T.Text
           -> [C.SomeCommand (C.SurveyorCommand s st)]
           -> MB.Minibuffer (C.SurveyorCommand s st) t n
minibuffer parseAddr updateMatches edName compName pfx cmds =
  MB.minibuffer (C.parseArgument parseAddr cmds) ct C.showRepr focusedListAttr edName compName pfx cmds
  where
    comp = MB.Completer (completeArgument cmds)
    ct = MB.Asynchronous comp updateMatches
