{-# LANGUAGE GADTs #-}
-- | An instantiation of the generic minibuffer widget with a specific argument type
module Surveyor.Widget.Minibuffer (
  MB.Minibuffer,
  minibuffer,
  MB.MinibufferStatus(..),
  MB.handleMinibufferEvent,
  MB.renderMinibuffer,
  ) where

import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Text as T
import qualified Data.Text.Zipper.Generic as Z
import qualified Data.Vector as V

import qualified Brick.Command as C
import qualified Brick.Match.Subword as SW
import qualified Brick.Widget.Minibuffer as MB
import qualified Surveyor.Architecture as A
import           Surveyor.Arguments
import           Surveyor.Attributes

completeArgument :: (Z.GenericTextZipper t)
                 => [Some (C.Command st (Argument arch st s) TypeRepr)]
                 -> (t -> TypeRepr tp -> IO (V.Vector t))
completeArgument cmds =
  let cmdNames = V.fromList [ C.cmdName cmd | Some cmd <- cmds ]
  in \t r ->
    case r of
      StringTypeRepr -> return V.empty
      AddressTypeRepr -> return V.empty
      IntTypeRepr -> return V.empty
      WordTypeRepr -> return V.empty
      CommandTypeRepr ->
        case SW.matcher (Z.toList t) of
          Nothing -> return V.empty
          Just matcher -> do
            let matches = V.filter (SW.matches matcher) cmdNames
            let toGeneric txt = mconcat (map Z.singleton (T.unpack txt))
            return (fmap toGeneric matches)

minibuffer :: (Z.GenericTextZipper t, A.Architecture arch s)
           => n
           -- ^ The name of the editor widget
           -> n
           -- ^ The name of the completion list
           -> T.Text
           -> [Some (C.Command st (Argument arch st s) TypeRepr)]
           -> MB.Minibuffer st (Argument arch st s) TypeRepr t n
minibuffer edName compName pfx cmds =
  MB.minibuffer (parseArgument cmds) (completeArgument cmds) showRepr focusedListAttr edName compName pfx cmds
