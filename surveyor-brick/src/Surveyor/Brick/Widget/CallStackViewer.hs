{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- | A viewer for Crucible call stacks (from a suspended symbolic execution)
module Surveyor.Brick.Widget.CallStackViewer (
  CallStackViewer,
  callStackViewer,
  renderCallStackViewer,
  handleCallStackViewerEvent
  ) where

import qualified Brick as B
import qualified Brick.Widgets.List as BL
import           Control.Lens ( (^.) )
import qualified Control.Lens as L
import           Data.Parameterized.Some ( Some(..) )
import           Data.Proxy ( Proxy(..) )
import qualified Data.Vector as DV
import qualified Lang.Crucible.CFG.Core as LCCC
import qualified Lang.Crucible.FunctionHandle as CFH
import qualified Lang.Crucible.Simulator.CallFrame as LCSC
import qualified Lang.Crucible.Simulator.ExecutionTree as LCSET
import qualified What4.FunctionName as WFN

import qualified Surveyor.Brick.Names as SBN
import qualified Surveyor.Core as SC

data CallStackViewer arch s where
  CallStackViewer :: CallStackViewerState sym arch s -> CallStackViewer arch s

data CallStackViewerState sym arch s =
  CallStackViewerState { frameList :: BL.GenericList SBN.Names DV.Vector (LCSET.SomeFrame (LCSC.SimFrame sym (SC.CrucibleExt arch)))
                       , proxy :: Proxy arch
                       }

-- | Construct the viewer widget from a callstack
--
-- Future developments should be able to inspect the registers for each call
-- frame (as if they were the breakpoint registers)
callStackViewer :: forall proxy arch sym s
                 . proxy arch
                -> [LCSET.SomeFrame (LCSC.SimFrame sym (SC.CrucibleExt arch))]
                -> CallStackViewer arch s
callStackViewer _ frames = CallStackViewer vs
  where
    frmList = BL.list SBN.CallStackViewer (DV.fromList frames) 1
    vs = CallStackViewerState { frameList = frmList
                              , proxy = Proxy @arch
                              }

renderCallStackViewer :: Bool -> CallStackViewer arch s -> B.Widget SBN.Names
renderCallStackViewer hasFocus (CallStackViewer vs) =
  B.vBox [ B.txt "Call Stack"
         , BL.renderList (renderSomeFrame (proxy vs)) hasFocus (frameList vs)
         ]

renderSomeFrame :: proxy arch -> Bool -> LCSET.SomeFrame (LCSC.SimFrame sym (SC.CrucibleExt arch)) -> B.Widget SBN.Names
renderSomeFrame _ _hasFocus (LCSET.SomeFrame sf) =
  case sf of
    LCSC.OF oframe -> B.txt (oframe ^. LCSC.override . L.to WFN.functionName) B.<+> B.txt " (Override)"
    LCSC.MF cframe -> renderCallFrame cframe
    LCSC.RF fn _retval -> B.txt "Returning from " B.<+> B.txt (WFN.functionName fn)

renderCallFrame :: LCSC.CallFrame sym ext blocks ret ctx -> B.Widget n
renderCallFrame cf =
  case LCSC.frameHandle cf of
    LCSC.SomeHandle fnHdl ->
      B.hBox [ B.txt (WFN.functionName (CFH.handleName fnHdl))
             , B.txt " (block "
             , renderBlockID (cf ^. LCSC.frameBlockID)
             , B.txt ")"
             ]

renderBlockID :: Some (LCCC.BlockID blocks) -> B.Widget n
renderBlockID (Some bid) = B.str (show bid)

handleCallStackViewerEvent :: B.BrickEvent SBN.Names e
                           -> CallStackViewer arch s
                           -> B.EventM SBN.Names (CallStackViewer arch s)
handleCallStackViewerEvent evt cs0@(CallStackViewer vs) =
  case evt of
    B.VtyEvent ve -> do
      fl' <- BL.handleListEvent ve (frameList vs)
      return (CallStackViewer vs { frameList = fl' })
    _ -> return cs0
