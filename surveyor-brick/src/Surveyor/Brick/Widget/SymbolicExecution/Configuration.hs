{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
-- | This widget provides an interface for choosing the symbolic execution backend to use
--
-- The backend currently consists of a choice of SMT solver and floating point
-- interpretation.  These two parameters are a separate configuration step, as
-- changing them can invalidate the initial state setup (which is the next
-- configuration step after this one)
module Surveyor.Brick.Widget.SymbolicExecution.Configuration (
  form,
  renderSymbolicExecutionConfigurator,
  handleSymbolicExecutionConfiguratorEvent
  ) where

import           Brick ( (<+>) )
import qualified Brick as B
import           Brick.Forms ( (@@=) )
import qualified Brick.Forms as B
import           Control.Lens ( (^.) )
import           Control.Monad.IO.Class ( liftIO )
import qualified Data.Text as T
import qualified What4.Expr.Builder as WEB

import           Surveyor.Brick.Names ( Names(..) )
import qualified Surveyor.Core as C

form :: C.SymbolicExecutionConfig s -> B.Form (C.SymbolicExecutionConfig s) e Names
form = B.newForm [ (B.str "Solver: " <+>) @@= B.radioField C.configSolverL solvers
                 , (B.str "Floating point mode: " <+>) @@= B.radioField C.configFloatReprL floatModes
                 , (B.str "Solver interaction file: " <+>) @@= B.editTextField C.solverInteractionFileL SolverInteractionFileEdit Nothing
                 ]

solvers :: [(C.Solver, Names, T.Text)]
solvers = [ (C.CVC4, SolverRadioSelection C.CVC4, "CVC4")
          , (C.Yices, SolverRadioSelection C.Yices, "Yices")
          , (C.Z3, SolverRadioSelection C.Z3, "Z3")
          ]

floatModes :: [(C.SomeFloatModeRepr s, Names, T.Text)]
floatModes = [ (C.SomeFloatModeRepr WEB.FloatRealRepr, FloatModeRadioSelection "Real", "Real")
             , (C.SomeFloatModeRepr WEB.FloatIEEERepr, FloatModeRadioSelection "IEEE", "IEEE")
             , (C.SomeFloatModeRepr WEB.FloatUninterpretedRepr, FloatModeRadioSelection "Uninterpreted", "Uninterpreted")
             ]

renderSymbolicExecutionConfigurator :: B.Form (C.SymbolicExecutionConfig s) e Names
                                    -> B.Widget Names
renderSymbolicExecutionConfigurator = B.renderForm

handleSymbolicExecutionConfiguratorEvent :: C.S archEvt u arch s
                                         -> B.BrickEvent Names e
                                         -> B.Form (C.SymbolicExecutionConfig s) e Names
                                         -> B.EventM Names (B.Form (C.SymbolicExecutionConfig s) e Names)
handleSymbolicExecutionConfiguratorEvent s0 evt f = do
  f' <- B.handleFormEvent evt f
  -- We update the real symbolic execution state using an event instead of
  -- attempting to manage it directly, so that it is easier to have a single
  -- canonical symbolic execution state that cannot get out of sync.
  liftIO $ C.sEmitEvent s0 (C.UpdateSymbolicExecutionState (s0 ^. C.lNonce) (C.Configuring (B.formState f')))
  return f'
