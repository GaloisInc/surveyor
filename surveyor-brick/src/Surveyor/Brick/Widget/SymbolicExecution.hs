{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
module Surveyor.Brick.Widget.SymbolicExecution (
  SymbolicExecutionManager,
  symbolicExecutionManager,
  renderSymbolicExecutionManager,
  handleSymbolicExecutionManagerEvent,
  symbolicExecutionManagerState
  ) where

import qualified Brick as B
import qualified Brick.Forms as B
import           Data.Parameterized.Some ( Some(..) )

import           Surveyor.Brick.Names ( Names(..) )
import qualified Surveyor.Brick.Widget.SymbolicExecution.Configuration as SEC
import qualified Surveyor.Brick.Widget.SymbolicExecution.Monitor as SEM
import qualified Surveyor.Brick.Widget.SymbolicExecution.Setup as SES
import qualified Surveyor.Brick.Widget.SymbolicExecution.Inspect as SEI
import qualified Surveyor.Brick.Widget.SymbolicExecution.StateExplorer as SEE
import qualified Surveyor.Core as C


data SymbolicExecutionManager e arch s =
  SymbolicExecutionManager { managerState :: Some (C.SymbolicExecutionState arch s)
                           , configForm :: B.Form (C.SymbolicExecutionConfig s) e Names
                           }

symbolicExecutionManager :: Some (C.SymbolicExecutionState arch s)
                         -> SymbolicExecutionManager e arch s
symbolicExecutionManager (Some state) =
  SymbolicExecutionManager { managerState = Some state
                           , configForm = SEC.form (C.symbolicExecutionConfig state)
                           }

renderSymbolicExecutionManager :: SymbolicExecutionManager e arch s
                               -> B.Widget Names
renderSymbolicExecutionManager sem@(managerState -> Some st) =
  case st of
    C.Configuring {} -> SEC.renderSymbolicExecutionConfigurator (configForm sem)
    C.Initializing {} -> SES.renderSymbolicExecutionSetup st
    C.Executing {} -> SEM.renderSymbolicExecutionMonitor st
    C.Inspecting {} -> SEI.renderSymbolicExecutionInspector st
    C.Suspended {} -> SEE.renderSymbolicExecutionStateExplorer st

handleSymbolicExecutionManagerEvent :: B.BrickEvent Names e
                                    -> SymbolicExecutionManager e arch s
                                    -> B.EventM Names (SymbolicExecutionManager e arch s)
handleSymbolicExecutionManagerEvent evt sem@(managerState -> Some st) =
  case st of
    C.Configuring {} -> do
      f' <- SEC.handleSymbolicExecutionConfiguratorEvent evt (configForm sem)
      return SymbolicExecutionManager { managerState = Some (C.Configuring (B.formState f'))
                                      , configForm = f'
                                      }
    C.Initializing {} -> do
      st' <- SES.handleSymbolicExecutionSetupEvent evt st
      return sem { managerState = Some st' }
    C.Executing {} -> do
      st' <- SEM.handleSymbolicExecutionMonitorEvent evt st
      return sem { managerState = Some st' }
    C.Inspecting {} -> do
      st' <- SEI.handleSymbolicExecutionInspectorEvent evt st
      return sem { managerState = Some st' }
    C.Suspended {} -> do
      st' <- SEE.handleSymbolicExecutionStateExplorerEvent evt st
      return sem { managerState = Some st' }


symbolicExecutionManagerState :: SymbolicExecutionManager e arch s
                              -> Some (C.SymbolicExecutionState arch s)
symbolicExecutionManagerState = managerState
