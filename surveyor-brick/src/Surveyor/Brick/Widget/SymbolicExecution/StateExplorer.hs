{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module Surveyor.Brick.Widget.SymbolicExecution.StateExplorer (
  renderSymbolicExecutionStateExplorer,
  handleSymbolicExecutionStateExplorerEvent
  ) where

import qualified Brick as B
import           Brick.Forms ( (@@=) )
import qualified Brick.Forms as B
import           Control.Lens ( (^.) )
import qualified Control.Lens as L
import           Data.Maybe ( fromMaybe )
import qualified Data.Parameterized.Context as Ctx
import           Data.Parameterized.Some ( Some(..) )
import           Data.Proxy ( Proxy(..) )
import qualified Data.Text as T
import qualified Lang.Crucible.CFG.Core as LCCC
import qualified Lang.Crucible.Simulator.CallFrame as LCSC
import qualified Lang.Crucible.Simulator.RegMap as LMCR
import qualified What4.Expr.Builder as WEB

import           Surveyor.Brick.Names ( Names(..) )
import qualified Surveyor.Core as C

import qualified Surveyor.Brick.Widget.ValueViewer as WVV

-- | This is a simple internal-only type to render our brick form.
--
-- The real state type is 'SuspendedState' (unfortunately, it isn't convenient
-- to use with a brick widget because it contains existentials)
data ValueSelector sym ctx =
  ValueSelector { _values :: Ctx.Assignment (LMCR.RegEntry sym) ctx
                , _index :: Some (Ctx.Index ctx)
                }

L.makeLenses ''ValueSelector

form :: forall sym ctx e
      . Ctx.Assignment (LMCR.RegEntry sym) ctx
     -> ValueSelector sym ctx
     -> B.Form (ValueSelector sym ctx) e Names
form vals =
  B.newForm [ (B.str "Breakpoint Value: " B.<+>) @@= B.radioField index idxList
            ]
  where
    idxEntry :: forall tp . Ctx.Index ctx tp -> (Some (Ctx.Index ctx), Names, T.Text)
    idxEntry idx = (Some idx, SelectedBreakpointValue (Ctx.indexVal idx), T.pack (show idx))
    idxList = reverse $ Ctx.forIndex (Ctx.size vals) (\acc idx -> idxEntry idx : acc) []

-- | Render a view of the final state from symbolic execution
--
-- Eventually, this should provide some mechanisms for deeply inspecting the
-- state (including arch-specific inspection of memory).
renderSymbolicExecutionStateExplorer :: forall arch s
                                      . C.SymbolicExecutionState arch s C.Suspend
                                     -> B.Widget Names
renderSymbolicExecutionStateExplorer (C.Suspended suspSt) =
  case C.suspendedCallFrame suspSt of
    cf@LCSC.CallFrame { LCSC._frameCFG = fcfg } ->
      B.vBox [ B.txt "Current Function:" B.<+> B.txt (T.pack (show (LCCC.cfgHandle fcfg)))
           , B.txt "Current Block:" B.<+> B.txt (T.pack (show (cf ^. LCSC.frameBlockID)))
           , B.txt "Breakpoint name:" B.<+> B.txt (fromMaybe "<Unnamed Breakpoint>" (C.breakpointName =<< mbp))
           , renderBreakpointValueSelector regs mIdx
           , renderSelectedValue Proxy regs mIdx
           ]
  where
    mbp = C.suspendedBreakpoint suspSt
    regs = C.suspendedRegVals suspSt
    mIdx = C.suspendedRegSelection suspSt

renderBreakpointValueSelector :: Ctx.Assignment (LMCR.RegEntry sym) ctx
                              -> Maybe (Some (Ctx.Index ctx))
                              -> B.Widget Names
renderBreakpointValueSelector regs mIdx =
  case mIdx of
    Nothing -> B.emptyWidget
    Just idx ->
      let vs = ValueSelector regs idx
      in B.renderForm (form regs vs)

renderSelectedValue :: (sym ~ WEB.ExprBuilder s st fs)
                    => proxy sym
                    -> Ctx.Assignment (LMCR.RegEntry sym) ctx
                    -> Maybe (Some (Ctx.Index ctx))
                    -> B.Widget Names
renderSelectedValue proxy regs mIdx =
  case mIdx of
    Nothing -> B.emptyWidget
    Just (Some idx) ->
      case regs Ctx.! idx of
        LMCR.RegEntry tp val ->
          let vv = WVV.valueViewer proxy tp val
          in WVV.renderValueViewer vv

handleSymbolicExecutionStateExplorerEvent :: B.BrickEvent Names e
                                          -> C.SymbolicExecutionState arch s C.Suspend
                                          -> B.EventM Names (C.SymbolicExecutionState arch s C.Suspend)
handleSymbolicExecutionStateExplorerEvent evt s0@(C.Suspended suspSt) =
  case mIdx of
    Nothing ->
      -- If there is no index, that is only because there are no breakpoint
      -- values (and therefore nothing to select and no events to handle)
      return s0
    Just idx -> do
      let vs = ValueSelector regs idx
      vs' <- B.formState <$> B.handleFormEvent evt (form regs vs)
      let suspSt' = suspSt { C.suspendedRegSelection = Just (vs' ^. index) }
      return (C.Suspended suspSt')
  where
    regs = C.suspendedRegVals suspSt
    mIdx = C.suspendedRegSelection suspSt
