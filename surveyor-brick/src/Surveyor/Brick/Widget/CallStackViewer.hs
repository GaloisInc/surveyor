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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
-- | A viewer for Crucible call stacks (from a suspended symbolic execution)
module Surveyor.Brick.Widget.CallStackViewer (
  CallStackViewer,
  callStackViewer,
  frameList,
  selectedValue,
  renderCallStackViewer,
  handleCallStackViewerEvent
  ) where

import qualified Brick as B
import qualified Brick.Focus as BF
import qualified Brick.Widgets.List as BL
import           Control.Lens ( (^.), (&), (.~), (%~) )
import qualified Control.Lens as L
import           Data.Maybe ( fromMaybe )
import qualified Data.Parameterized.Classes as PC
import qualified Data.Parameterized.Context as Ctx
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Parameterized.TraversableFC as FC
import           Data.Proxy ( Proxy(..) )
import qualified Data.Text as T
import qualified Data.Vector as DV
import qualified Graphics.Vty as GV
import qualified Lang.Crucible.CFG.Core as LCCC
import qualified Lang.Crucible.FunctionHandle as CFH
import qualified Lang.Crucible.Simulator.CallFrame as LCSC
import qualified Lang.Crucible.Simulator.ExecutionTree as LCSET
import qualified Lang.Crucible.Simulator.RegMap as LMCR
import qualified What4.Expr.Builder as WEB
import qualified What4.FunctionName as WFN

import qualified Surveyor.Brick.Names as SBN
import qualified Surveyor.Brick.Widget.ValueSelector as SBV
import qualified Surveyor.Brick.Widget.ValueViewer as WVV
import qualified Surveyor.Core as SC

-- | A type to let us distinguish between breakpoint frames and normal call
-- frames, while treating them relatively uniformly.
data CallStackEntry arch s sym where
  BreakpointFrame :: Maybe T.Text -> CallStackEntry arch s sym
  CallFrame :: LCSC.SimFrame sym (SC.CrucibleExt arch) l args -> CallStackEntry arch s sym

-- | Another type wrapper that adds a phantom type parameter so that we can
-- store 'WVV.ValueViewer's in 'Ctx.Assignment's.  It isn't strictly necessary
-- to do so, but it gives us total indexing where a Map would not.
data WrappedViewer s sym (tp :: LCCC.CrucibleType) where
  WrappedViewer :: WVV.ValueViewer s sym -> WrappedViewer s sym tp

-- | A collection of all of the state we track for a call frame (including breakpoints)
--
--  - The 'CallStackEntry' marks the frame as either the breakpoint frame or one
--    of the special frame types maintained by Crucible
--  - The 'SBV.ValueSelectorForm' is a form that allows the user to select the
--    value they want to view out of the current frame
--  - The 'Ctx.Assignment' caches the 'WVV.ValueViewer' widgets constructed for each viewable value
--
-- The former two are tracked and cached so that they don't need to be rebuilt
-- and to preserve any state that is built up (i.e., so that switching between
-- stack entries doesn't discard user selection states).
data CallStackFrame arch s sym e where
  CallStackFrame :: CallStackEntry arch s sym
                 -> SBV.ValueSelectorForm s sym ctx e
                 -> Ctx.Assignment (WrappedViewer s sym) ctx
                 -> CallStackFrame arch s sym e

data CallStackViewer arch s sym e =
  CallStackViewer { _frameList :: BL.GenericList SBN.Names DV.Vector (CallStackFrame arch s sym e)
                  , _focusRing :: BF.FocusRing SBN.Names
                  }

L.makeLenses ''CallStackViewer

-- | Construct the viewer widget from a callstack
--
-- Future developments should be able to inspect the registers for each call
-- frame (as if they were the breakpoint registers)
callStackViewer :: forall proxy arch sym s ctx e st fs
                 . (sym ~ WEB.ExprBuilder s st fs)
                => proxy arch
                -> Maybe T.Text
                -- ^ Breakpoint name
                -> Ctx.Assignment (LMCR.RegEntry sym) ctx
                -- ^ Breakpoint captured values
                -> [LCSET.SomeFrame (LCSC.SimFrame sym (SC.CrucibleExt arch))]
                -- ^ Frames on the stack of the symbolic execution engine
                -> CallStackViewer arch s sym e
callStackViewer proxy bpName bpVals simFrames = CallStackViewer frmList fr
  where
    bpEntry = BreakpointFrame bpName
    bpViewers = FC.fmapFC regViewer bpVals
    bpFrame = CallStackFrame bpEntry (SBV.valueSelectorForm (Just 0) bpVals) bpViewers
    frames = bpFrame : map (fromSimFrame proxy) simFrames
    frmList = BL.list SBN.CallStackViewer (DV.fromList frames) 1

    fr = BF.focusRing [ SBN.CallStackViewer, SBN.BreakpointValueSelectorForm, SBN.BreakpointValueViewer ]

fromSimFrame :: (sym ~ WEB.ExprBuilder s st fs)
             => proxy arch
             -> LCSET.SomeFrame (LCSC.SimFrame sym (SC.CrucibleExt arch))
             -> CallStackFrame arch s sym e
fromSimFrame proxy (LCSET.SomeFrame sf) =
  withFrameRegs proxy sf $ \nArgs regs ->
    let viewers = FC.fmapFC regViewer regs
    in CallStackFrame entry (SBV.valueSelectorForm nArgs regs) viewers
  where
    entry = CallFrame sf

withFrameRegs :: proxy arch
              -> LCSC.SimFrame sym (SC.CrucibleExt arch) l args
              -> (forall ctx . Maybe Int -> Ctx.Assignment (LMCR.RegEntry sym) ctx -> a)
              -> a
withFrameRegs _ sf k =
  case sf of
    LCSC.OF oframe -> k Nothing (oframe ^. LCSC.overrideRegMap . L.to LMCR.regMap)
    LCSC.MF mframe@(LCSC.CallFrame { LCSC._frameCFG = cfg }) ->
      let nArgs = cfgArgCount cfg
      in k (Just nArgs) (mframe ^. LCSC.frameRegs . L.to LMCR.regMap)
    LCSC.RF _name e -> k Nothing (Ctx.Empty Ctx.:> e)

-- | Return the number of formal parameters to the CFG
cfgArgCount :: LCCC.CFG ext blocks ctx ret -> Int
cfgArgCount cfg =
  Ctx.sizeInt (Ctx.size argReprs)
  where
    hdl = LCCC.cfgHandle cfg
    argReprs = CFH.handleArgTypes hdl

regViewer :: (sym ~ WEB.ExprBuilder s st fs)
          => LMCR.RegEntry sym tp
          -> WrappedViewer s sym tp
regViewer re = WrappedViewer (WVV.valueViewer (LMCR.regType re) (LMCR.regValue re))

renderCallStackViewer :: forall arch s sym e st fs
                       . (sym ~ WEB.ExprBuilder s st fs)
                      => Bool
                      -> SC.ValueNameMap s
                      -> CallStackViewer arch s sym e
                      -> B.Widget SBN.Names
renderCallStackViewer hasFocus valNames cs =
  B.hBox [ B.vBox [ B.txt "Call Stack"
                  , BL.renderList (renderCallStackFrame (Proxy @arch)) (hasFocus && stackSelected) (cs ^. frameList)
                  ]
         , renderValueSelector (hasFocus && valSelSelected) (cs ^. frameList . L.to BL.listSelectedElement)
         , renderSelectedValue (hasFocus && viewerSelected) valNames (cs ^. frameList . L.to BL.listSelectedElement)
         ]
  where
    curSel = cs ^. focusRing . L.to BF.focusGetCurrent
    stackSelected = Just SBN.CallStackViewer == curSel
    valSelSelected = Just SBN.BreakpointValueSelectorForm == curSel
    viewerSelected = Just SBN.BreakpointValueViewer == curSel

-- | This renders the form that lets the user select the value in the current
-- stack frame to view
renderValueSelector :: Bool -> Maybe (Int, CallStackFrame arch s sym e) -> B.Widget SBN.Names
renderValueSelector _hasFocus mf =
  case mf of
    Nothing -> B.emptyWidget
    Just (_, CallStackFrame _ selForm _viewers) ->
      SBV.renderValueSelectorForm selForm

renderSelectedValue :: (sym ~ WEB.ExprBuilder s st fs)
                    => Bool
                    -> SC.ValueNameMap s
                    -> Maybe (Int, CallStackFrame arch s sym e)
                    -> B.Widget SBN.Names
renderSelectedValue hasFocus valNames mf =
  case mf of
    Just (_, CallStackFrame _ selForm viewers)
      | Just (Some idx) <- SBV.selectedIndex selForm
      , WrappedViewer vv <- viewers Ctx.! idx ->
        WVV.renderValueViewer hasFocus valNames vv
    _ -> B.emptyWidget

renderCallStackFrame :: proxy arch -> Bool -> CallStackFrame arch s sym e -> B.Widget SBN.Names
renderCallStackFrame proxy hasFocus (CallStackFrame cse _ _) =
  case cse of
    BreakpointFrame mName -> B.txt (fromMaybe "<Unnamed Breakpoint>" mName)
    CallFrame sf -> renderSomeFrame proxy hasFocus sf

renderSomeFrame :: proxy arch -> Bool -> LCSC.SimFrame sym (SC.CrucibleExt arch) l args -> B.Widget SBN.Names
renderSomeFrame _ _hasFocus sf =
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
                           -> CallStackViewer arch s sym e
                           -> B.EventM SBN.Names (CallStackViewer arch s sym e)
handleCallStackViewerEvent evt cs0 =
  case evt of
    B.VtyEvent (GV.EvKey (GV.KChar '\t') []) ->
      return (cs0 & focusRing %~ BF.focusNext)
    B.VtyEvent ve
      | BF.focusGetCurrent (cs0 ^. focusRing) == Just SBN.CallStackViewer -> do
          fl' <- BL.handleListEvent ve (cs0 ^. frameList)
          return (cs0 & frameList .~ fl')
      | BF.focusGetCurrent (cs0 ^. focusRing) == Just SBN.BreakpointValueViewer
      , Just (listIdx, CallStackFrame cse selForm viewers) <- cs0 ^. frameList . L.to BL.listSelectedElement
      , Just (Some idx) <- SBV.selectedIndex selForm
      , WrappedViewer vv <- viewers Ctx.! idx -> do
          vv' <- WVV.handleValueViewerEvent ve vv
          let frame = CallStackFrame cse selForm (L.set (PC.ixF idx) (WrappedViewer vv') viewers)
          return (cs0 & frameList . BL.listElementsL %~ (DV.// [(listIdx, frame)]))
      | BF.focusGetCurrent (cs0 ^. focusRing) == Just SBN.BreakpointValueSelectorForm
      , Just (listIdx, CallStackFrame cse selForm viewers) <- cs0 ^. frameList . L.to BL.listSelectedElement -> do
          selForm' <- SBV.handleValueSelectorFormEvent evt selForm
          let frame = CallStackFrame cse selForm' viewers
          return (cs0 & frameList . BL.listElementsL %~ (DV.// [(listIdx, frame)]))
    _ -> return cs0

-- | Return the currently-selected value in the 'CallStackViewer'
--
-- If the user has selected a sub-value in the value viewer for the
-- currently-selected top-level value, the value from the ValueViewer is
-- returned.  Otherwise, the top-level value in the value selector is returned
-- (if any).
selectedValue :: (sym ~ WEB.ExprBuilder s st fs) => CallStackViewer arch s sym e -> Maybe (Some (LMCR.RegEntry sym))
selectedValue csv = do
  (_, CallStackFrame _cse selForm viewers) <- csv ^. frameList . L.to BL.listSelectedElement
  Some selValIdx <- SBV.selectedIndex selForm
  case viewers Ctx.! selValIdx of
    WrappedViewer vv ->
      case WVV.selectedValue vv of
        Nothing -> SBV.selectedValue selForm
        Just sre -> return sre
