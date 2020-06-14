{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module Surveyor.Brick.Widget.SymbolicExecution.StateExplorer (
  StateExplorer,
  stateExplorer,
  renderSymbolicExecutionStateExplorer,
  handleSymbolicExecutionStateExplorerEvent
  ) where

import qualified Brick as B
import qualified Brick.Focus as BF
import           Brick.Forms ( (@@=) )
import qualified Brick.Forms as B
import           Control.Lens ( (^.) )
import qualified Control.Lens as L
import           Data.Maybe ( fromMaybe )
import qualified Data.Parameterized.Classes as PC
import qualified Data.Parameterized.Context as Ctx
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Parameterized.TraversableFC as FC
import           Data.Proxy ( Proxy(..) )
import qualified Data.Text as T
import qualified Graphics.Vty as GV
import qualified Lang.Crucible.CFG.Core as LCCC
import qualified Lang.Crucible.Simulator.CallFrame as LCSC
import qualified Lang.Crucible.Simulator.RegMap as LMCR
import qualified What4.Expr.Builder as WEB

import           Surveyor.Brick.Names ( Names(..) )
import qualified Surveyor.Brick.Panic as SBP
import qualified Surveyor.Core as C

import qualified Surveyor.Brick.Widget.ValueViewer as WVV

-- | This is a wrapper around a symbolic-execution time value (a RegEntry).  The
-- wrapper is helpful for hiding the @sym@ type parameter while still leaving
-- 'ValueSelector' as a plain data type (so lenses and brick still work).
--
-- While we hide the parameter, we do record the concrete symbolic backend type
-- so that we can traverse terms.
--
-- While this quantification is useful for now, it could become problematic if
-- we need to use any of this information with values taken from the symbolic
-- execution state, which has separately quantified it all out.
data RegWrapper s tp where
  RegWrapper :: LMCR.RegEntry (WEB.ExprBuilder s st fs) tp -> RegWrapper s tp

-- | This is a type capturing the data necessary to render the value selector form
--
-- We have to carefully quantify type parameters so that we can mesh well with brick.
data ValueSelector s ctx =
  ValueSelector { _values :: Ctx.Assignment (RegWrapper s) ctx
                , _index :: Some (Ctx.Index ctx)
                }

L.makeLenses ''ValueSelector

-- | This wrapper quantifies out the ctx parameter from the form so that we can
-- return it
data ValueSelectorForm s ctx e where
  NoValues :: ValueSelectorForm s ctx e
  ValueSelectorForm :: B.Form (ValueSelector s ctx) e Names -> ValueSelectorForm s ctx e

data WrappedViewer s (tp :: LCCC.CrucibleType) where
  WrappedViewer :: WVV.ValueViewer s -> WrappedViewer s tp

-- | Holds the state of the StateExplorer widget
--
-- This includes the selected value state as well as the state for the value
-- viewer widget itself.
data StateExplorer s e where
  StateExplorer :: ValueSelectorForm s ctx e
                -> Ctx.Assignment (WrappedViewer s) ctx
                -> BF.FocusRing Names
                -> StateExplorer s e

stateExplorer :: forall e arch s
               . C.SymbolicExecutionState arch s C.Suspend
              -> StateExplorer s e
stateExplorer (C.Suspended suspSt) =
  case C.suspendedRegVals suspSt of
    Ctx.Empty -> StateExplorer NoValues Ctx.Empty (BF.focusRing [])
    vals@(_ Ctx.:> _) ->
      let idxList = reverse $ Ctx.forIndex (Ctx.size vals) (\acc idx -> idxEntry idx : acc) []
          wrappedVals = FC.fmapFC RegWrapper vals
          vs = ValueSelector { _values = wrappedVals
                             , _index = head idxList ^. L._1
                             }
          formCon = B.newForm [ (B.str "Breakpoint Value: " B.<+>) @@= B.radioField index idxList
                              ]
          vsf = ValueSelectorForm (formCon vs)

          toValueViewer :: forall tp . RegWrapper s tp -> WrappedViewer s tp
          toValueViewer (RegWrapper (re :: LMCR.RegEntry (WEB.ExprBuilder s st fs) tp)) =
            case re of
              LMCR.RegEntry tp rv -> WrappedViewer (WVV.valueViewer (Proxy @(WEB.ExprBuilder s st fs)) tp rv)
          viewers = FC.fmapFC toValueViewer wrappedVals
          fr = BF.focusRing [BreakpointValueSelectorForm, BreakpointValueViewer]
      in StateExplorer vsf viewers fr
  where
    idxEntry :: forall tp (ctx :: Ctx.Ctx LCCC.CrucibleType) . Ctx.Index ctx tp -> (Some (Ctx.Index ctx), Names, T.Text)
    idxEntry idx = (Some idx, SelectedBreakpointValue (Ctx.indexVal idx), T.pack (show idx))

-- | Render a view of the final state from symbolic execution
--
-- Eventually, this should provide some mechanisms for deeply inspecting the
-- state (including arch-specific inspection of memory).
renderSymbolicExecutionStateExplorer :: forall arch s e
                                      . (C.SymbolicExecutionState arch s C.Suspend, StateExplorer s e)
                                     -> B.Widget Names
renderSymbolicExecutionStateExplorer (C.Suspended suspSt, se@(StateExplorer vsf _viewers _focus)) =
  case C.suspendedCallFrame suspSt of
    cf@LCSC.CallFrame { LCSC._frameCFG = fcfg } ->
      B.vBox [ B.txt "Current Function:" B.<+> B.txt (T.pack (show (LCCC.cfgHandle fcfg)))
           , B.txt "Current Block:" B.<+> B.txt (T.pack (show (cf ^. LCSC.frameBlockID)))
           , B.txt "Breakpoint name:" B.<+> B.txt (fromMaybe "<Unnamed Breakpoint>" (C.breakpointName =<< mbp))
           , renderBreakpointValueSelector vsf
           , renderSelectedValue se
           ]
  where
    mbp = C.suspendedBreakpoint suspSt

renderBreakpointValueSelector :: ValueSelectorForm s ctx e
                              -> B.Widget Names
renderBreakpointValueSelector vsf =
  case vsf of
    NoValues -> B.emptyWidget
    ValueSelectorForm f -> B.renderForm f

renderSelectedValue :: forall s e
                     . StateExplorer s e
                    -> B.Widget Names
renderSelectedValue (StateExplorer vsf viewers focus) =
  case vsf of
    NoValues -> B.emptyWidget
    ValueSelectorForm f ->
      case f ^. L.to B.formState . index of
        Some idx ->
          case viewers Ctx.! idx of
            WrappedViewer vv ->
              let isFocused = BF.focusGetCurrent focus == Just BreakpointValueViewer
              in WVV.renderValueViewer isFocused vv

handleSymbolicExecutionStateExplorerEvent :: B.BrickEvent Names e
                                          -> (C.SymbolicExecutionState arch s C.Suspend, StateExplorer s e)
                                          -> B.EventM Names (C.SymbolicExecutionState arch s C.Suspend, StateExplorer s e)
handleSymbolicExecutionStateExplorerEvent evt s0@(C.Suspended suspSt, StateExplorer vsf viewers focus) =
  case evt of
    B.VtyEvent (GV.EvKey (GV.KChar '\t') []) ->
      return (C.Suspended suspSt, StateExplorer vsf viewers (BF.focusNext focus))
    B.VtyEvent ve ->
      case vsf of
        NoValues ->
          -- If there is no index, that is only because there are no breakpoint
          -- values (and therefore nothing to select and no events to handle)
          return s0
        ValueSelectorForm f ->
          case BF.focusGetCurrent focus of
            Just BreakpointValueSelectorForm -> do
              f' <- B.handleFormEvent evt f
              return (C.Suspended suspSt, StateExplorer (ValueSelectorForm f') viewers focus)
            Just BreakpointValueViewer -> do
              case f ^. L.to B.formState . index of
                Some idx | WrappedViewer valView <- viewers Ctx.! idx -> do
                  v' <- WVV.handleValueViewerEvent ve valView
                  let viewers' = L.set (PC.ixF idx) (WrappedViewer v') viewers
                  return (C.Suspended suspSt, StateExplorer vsf viewers' focus)
            n -> SBP.panic "handleSymbolicExecutionExplorerEvent" ["Unexpected component in focus ring: " ++ show n]
    _ -> return s0
