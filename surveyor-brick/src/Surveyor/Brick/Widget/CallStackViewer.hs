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
import qualified Brick.Widgets.Border as BB
import qualified Brick.Widgets.List as BL
import           Control.Lens ( (^.), (&), (.~), (%~) )
import qualified Control.Lens as L
import           Data.Maybe ( fromMaybe )
import qualified Data.Parameterized.Classes as PC
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.Nonce as PN
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
import qualified Surveyor.Brick.Widget.BlockViewer as SBB
import qualified Surveyor.Brick.Widget.ValueSelector as SBV
import qualified Surveyor.Brick.Widget.ValueViewer as WVV
import qualified Surveyor.Core as SC

-- | A type to let us distinguish between breakpoint frames and normal call
-- frames, while treating them relatively uniformly.
data CallStackEntry arch s sym where
  BreakpointFrame :: Maybe T.Text -> CallStackEntry arch s sym
  AssertionFrame :: CallStackEntry arch s sym
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
--  - The 'SC.Block' is the (translation of) the Crucible block corresponding to
--    the code in this frame; it may be absent for frame types with no
--    associated CFG block (e.g., a Return frame)
--
-- The former two are tracked and cached so that they don't need to be rebuilt
-- and to preserve any state that is built up (i.e., so that switching between
-- stack entries doesn't discard user selection states).
data CallStackFrame arch s sym e where
  CallStackFrame :: CallStackEntry arch s sym
                 -> SBV.ValueSelectorForm s sym ctx e
                 -> Ctx.Assignment (WrappedViewer s sym) ctx
                 -> Maybe (SC.Block (SC.Crucible arch) s)
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
callStackViewer :: forall proxy arch sym s ctx e st fs p rtp ext
                 . ( sym ~ WEB.ExprBuilder s st fs
                   , SC.Architecture arch s
                   )
                => proxy arch
                -> PN.NonceGenerator IO s
                -> SC.SuspendedReason p sym ext rtp
                -- ^ The reason that execution was suspended; this changes how
                -- we render the call stack, as we'll add a phantom entry for
                -- the breakpoint (if this is a breakpoint)
                -> Ctx.Assignment (LMCR.RegEntry sym) ctx
                -- ^ Breakpoint captured values
                -> [LCSET.SomeFrame (LCSC.SimFrame sym (SC.CrucibleExt arch))]
                -- ^ Frames on the stack of the symbolic execution engine
                -> IO (CallStackViewer arch s sym e)
callStackViewer proxy ng reason capturedValues simFrames = do
  frames <- mapM (fromSimFrame proxy ng) simFrames
  let makeFrameList extraFrames = BL.list SBN.CallStackViewer (DV.fromList (extraFrames ++ frames)) 1
  case reason of
    SC.SuspendedBreakpoint bp -> do
      let bpEntry = BreakpointFrame (SC.breakpointName bp)
      let bpViewers = FC.fmapFC regViewer capturedValues
      let bpFrame = CallStackFrame bpEntry (SBV.valueSelectorForm (Just 0) capturedValues) bpViewers Nothing
      return (CallStackViewer (makeFrameList [bpFrame]) fr)
    SC.SuspendedAssertionFailure {} -> do
      -- FIXME: We can add a separate special frame for the assertion to view the model
      let assertionViewers = FC.fmapFC regViewer capturedValues
      let afFrame = CallStackFrame AssertionFrame (SBV.valueSelectorForm Nothing capturedValues) assertionViewers Nothing
      return (CallStackViewer (makeFrameList [afFrame]) fr)
    SC.SuspendedExecutionStep {} ->
      return (CallStackViewer (makeFrameList []) fr)
  where
    fr = BF.focusRing [ SBN.CallStackViewer, SBN.BreakpointValueSelectorForm, SBN.BreakpointValueViewer ]

fromSimFrame :: ( sym ~ WEB.ExprBuilder s st fs
                , SC.Architecture arch s
                )
             => proxy arch
             -> PN.NonceGenerator IO s
             -> LCSET.SomeFrame (LCSC.SimFrame sym (SC.CrucibleExt arch))
             -> IO (CallStackFrame arch s sym e)
fromSimFrame proxy ng (LCSET.SomeFrame sf) =
  withFrameRegs proxy sf $ \nArgs mCFG regs -> do
    let viewers = FC.fmapFC regViewer regs
    mBlock <- case (mCFG, SC.fromCrucibleBlock) of
      (Just (cfg, blockID), Just fromCruc) -> Just <$> buildBlock ng fromCruc cfg blockID
      _ -> return Nothing
    return (CallStackFrame entry (SBV.valueSelectorForm nArgs regs) viewers mBlock)
  where
    entry = CallFrame sf

buildBlock :: PN.NonceGenerator IO s
           -> (PN.NonceGenerator IO s -> LCCC.CFG ext blocks init ret -> LCCC.Block ext blocks ret args -> a)
           -> LCCC.CFG ext blocks init ret
           -> LCCC.BlockID blocks args
           -> a
buildBlock ng fromCruc cfg blockID  =
  fromCruc ng cfg (LCCC.getBlock blockID (LCCC.cfgBlockMap cfg))

withFrameRegs :: proxy arch
              -> LCSC.SimFrame sym (SC.CrucibleExt arch) l args
              -> (forall ctx blocks init ret tp . Maybe Int -> Maybe (LCCC.CFG (SC.CrucibleExt arch) blocks init ret, LCCC.BlockID blocks tp) -> Ctx.Assignment (LMCR.RegEntry sym) ctx -> a)
              -> a
withFrameRegs _ sf k =
  case sf of
    LCSC.OF oframe ->
      -- Pass Nothing to label all override frame values
      k Nothing Nothing (oframe ^. LCSC.overrideRegMap . L.to LMCR.regMap)
    LCSC.MF mframe@(LCSC.CallFrame { LCSC._frameCFG = cfg
                                   , LCSC._frameBlockID = Some blockID
                                   }) ->
      -- Either label all of the arguments if this block is the entry block OR
      -- label no arguments (because the block args are just locals)
      --
      -- NOTE: Investigate if we could figure out names for the arguments to
      -- function frames not in the call stack, if relevant?
      let nArgs = if mframe ^. LCSC.frameBlockID == Some (LCCC.cfgEntryBlockID cfg)
                  then cfgArgCount cfg
                  else 0
      in k (Just nArgs) (Just (cfg, blockID)) (mframe ^. LCSC.frameRegs . L.to LMCR.regMap)
    LCSC.RF _name e -> k Nothing Nothing (Ctx.Empty Ctx.:> e)

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
                       . ( sym ~ WEB.ExprBuilder s st fs
                         , SC.Architecture arch s
                         , SC.CrucibleExtension arch
                         )
                      => Bool
                      -> SC.ValueNameMap s
                      -> CallStackViewer arch s sym e
                      -> B.Widget SBN.Names
renderCallStackViewer hasFocus valNames cs =
  B.hBox [ BB.borderWithLabel (B.txt "Call Stack") stackEntries
         , BB.borderWithLabel (B.txt "Values In Scope") $
              renderValueSelector (hasFocus && valSelSelected) (cs ^. frameList . L.to BL.listSelectedElement)
         , BB.borderWithLabel (B.txt "Selected Value") $
              renderSelectedValue (hasFocus && viewerSelected) valNames (cs ^. frameList . L.to BL.listSelectedElement)
         , cfgWidget
         ]
  where
    stackEntries = BL.renderList (renderCallStackFrame (Proxy @arch)) (hasFocus && stackSelected) (cs ^. frameList)

    blockViewer = SBB.blockViewer SBN.CallStackBlockViewer SC.CrucibleRepr

    curSel = cs ^. focusRing . L.to BF.focusGetCurrent
    stackSelected = Just SBN.CallStackViewer == curSel
    valSelSelected = Just SBN.BreakpointValueSelectorForm == curSel
    viewerSelected = Just SBN.BreakpointValueViewer == curSel

    cfgWidget = fromMaybe B.emptyWidget $ do
      (_, CallStackFrame _ _ _ mBlock) <- cs ^. frameList . L.to BL.listSelectedElement
      block <- mBlock
      let blockState = SC.BlockState { SC.bsBlock = block
                                     , SC.bsSelection = SC.NoSelection
                                     , SC.bsList = SC.toInstructionList block
                                     , SC.bsBlockMapping = Nothing
                                     , SC.bsWithConstraints = \a -> a
                                     , SC.bsRepr = SC.CrucibleRepr
                                     }
      return (BB.borderWithLabel (B.txt "BasicBlock") (SBB.renderBlockViewer blockState blockViewer))

-- | This renders the form that lets the user select the value in the current
-- stack frame to view
renderValueSelector :: Bool -> Maybe (Int, CallStackFrame arch s sym e) -> B.Widget SBN.Names
renderValueSelector _hasFocus mf =
  case mf of
    Nothing -> B.emptyWidget
    Just (_, CallStackFrame _ selForm _viewers _block) ->
      SBV.renderValueSelectorForm selForm

renderSelectedValue :: (sym ~ WEB.ExprBuilder s st fs)
                    => Bool
                    -> SC.ValueNameMap s
                    -> Maybe (Int, CallStackFrame arch s sym e)
                    -> B.Widget SBN.Names
renderSelectedValue hasFocus valNames mf =
  case mf of
    Just (_, CallStackFrame _ selForm viewers _block)
      | Just (Some idx) <- SBV.selectedIndex selForm
      , WrappedViewer vv <- viewers Ctx.! idx ->
        WVV.renderValueViewer hasFocus valNames vv
    _ -> B.emptyWidget

renderCallStackFrame :: proxy arch -> Bool -> CallStackFrame arch s sym e -> B.Widget SBN.Names
renderCallStackFrame proxy hasFocus (CallStackFrame cse _ _ _) =
  case cse of
    BreakpointFrame mName -> B.txt (fromMaybe "<Unnamed Breakpoint>" mName)
    AssertionFrame -> B.txt "<Failed Assertion>"
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
      , Just (listIdx, CallStackFrame cse selForm viewers block) <- cs0 ^. frameList . L.to BL.listSelectedElement
      , Just (Some idx) <- SBV.selectedIndex selForm
      , WrappedViewer vv <- viewers Ctx.! idx -> do
          vv' <- WVV.handleValueViewerEvent ve vv
          let frame = CallStackFrame cse selForm (L.set (PC.ixF idx) (WrappedViewer vv') viewers) block
          return (cs0 & frameList . BL.listElementsL %~ (DV.// [(listIdx, frame)]))
      | BF.focusGetCurrent (cs0 ^. focusRing) == Just SBN.BreakpointValueSelectorForm
      , Just (listIdx, CallStackFrame cse selForm viewers block) <- cs0 ^. frameList . L.to BL.listSelectedElement -> do
          selForm' <- SBV.handleValueSelectorFormEvent evt selForm
          let frame = CallStackFrame cse selForm' viewers block
          return (cs0 & frameList . BL.listElementsL %~ (DV.// [(listIdx, frame)]))
    _ -> return cs0

-- | Return the currently-selected value in the 'CallStackViewer'
--
-- If the user has selected a sub-value in the value viewer for the
-- currently-selected top-level value, the value from the ValueViewer is
-- returned.  Otherwise, the top-level value in the value selector is returned
-- (if any).
--
-- NOTE: This currently only returns the value selected from the stack frame,
-- and does not fully support picking sub-values out of the ValueViewer.  The
-- infrastructure is in place to support that, but it doesn't seem as useful.
-- With some UI work, it might be easier to surface that capability.
selectedValue :: (sym ~ WEB.ExprBuilder s st fs) => CallStackViewer arch s sym e -> Maybe (Some (LMCR.RegEntry sym))
selectedValue csv = do
  (_, CallStackFrame _cse selForm _viewers _) <- csv ^. frameList . L.to BL.listSelectedElement
  SBV.selectedValue selForm
  {-
  Some selValIdx <- SBV.selectedIndex selForm
  case viewers Ctx.! selValIdx of
    WrappedViewer vv ->
      case WVV.selectedValue vv of
        Nothing -> SBV.selectedValue selForm
        Just sre -> SBV.selectedValue selForm -- return sre
-}
