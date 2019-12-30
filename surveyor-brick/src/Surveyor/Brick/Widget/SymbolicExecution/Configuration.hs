{-# LANGUAGE OverloadedStrings #-}
-- | This widget provides an interface for choosing the symbolic execution backend to use
--
-- The backend currently consists of a choice of SMT solver and floating point
-- interpretation.  These two parameters are a separate configuration step, as
-- changing them can invalidate the initial state setup (which is the next
-- configuration step after this one)
module Surveyor.Brick.Widget.SymbolicExecution.Configuration (
  SymbolicExecutionConfigurator,
  symbolicExecutionConfigurator,
  renderSymbolicExecutionConfigurator,
  handleSymbolicExecutionConfiguratorEvent,
  selectedConfiguration
  ) where

import           Brick ( (<+>) )
import qualified Brick as B
import           Brick.Forms ( (@@=) )
import qualified Brick.Forms as B
import           Data.Parameterized.Some ( Some(..) )
import qualified What4.Expr.Builder as WEB

import           Surveyor.Brick.Names ( Names(..) )
import qualified Surveyor.Core as C

data SymbolicExecutionConfigurator e =
  SymbolicExecutionConfigurator { configuratorForm :: B.Form C.SymbolicExecutionConfig e Names
                                }

selectedConfiguration :: SymbolicExecutionConfigurator e -> C.SymbolicExecutionConfig
selectedConfiguration = B.formState . configuratorForm

symbolicExecutionConfigurator :: C.SymbolicExecutionConfig -> SymbolicExecutionConfigurator e
symbolicExecutionConfigurator c0 =
  SymbolicExecutionConfigurator { configuratorForm = form c0
                                }
  where
    form = B.newForm [ (B.str "Solver: " <+>) @@= B.radioField C.configSolverL solvers
                     , (B.str "Floating point mode: " <+>) @@= B.radioField C.configFloatReprL floatModes
                     ]
    solvers = [ (C.CVC4, SolverRadioSelection C.CVC4, "CVC4")
              , (C.Yices, SolverRadioSelection C.Yices, "Yices")
              , (C.Z3, SolverRadioSelection C.Z3, "Z3")
              ]
    floatModes = [ (Some WEB.FloatRealRepr, FloatModeRadioSelection "Real", "Real")
                 , (Some WEB.FloatIEEERepr, FloatModeRadioSelection "IEEE", "IEEE")
                 , (Some WEB.FloatUninterpretedRepr, FloatModeRadioSelection "Uninterpreted", "Uninterpreted")
                 ]

renderSymbolicExecutionConfigurator :: SymbolicExecutionConfigurator e
                                    -> B.Widget Names
renderSymbolicExecutionConfigurator c =
  B.renderForm (configuratorForm c)

handleSymbolicExecutionConfiguratorEvent :: B.BrickEvent Names e
                                         -> SymbolicExecutionConfigurator e
                                         -> B.EventM Names (SymbolicExecutionConfigurator e)
handleSymbolicExecutionConfiguratorEvent be c =
  SymbolicExecutionConfigurator <$> B.handleFormEvent be (configuratorForm c)
