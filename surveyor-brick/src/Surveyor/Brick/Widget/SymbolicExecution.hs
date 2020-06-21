{-# LANGUAGE DataKinds #-}
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

-- | This is a wrapper around the core 'C.SymbolicExecutionState' type because
-- we need to store some additional information with some states, but we can't
-- really sink it into the core because it is brick-specific.
--
-- We could store it in the UI-specific state extension, but it would lead to
-- more partial code, since the state is only valid when the symbolic execution
-- is in the corresponding state.
data SymbolicExecutionManager e arch s where
  Configuring :: C.SymbolicExecutionState arch s C.Config -> B.Form (C.SymbolicExecutionConfig s) e Names -> SymbolicExecutionManager e arch s
  Initializing :: C.SymbolicExecutionState arch s C.SetupArgs -> SymbolicExecutionManager e arch s
  Executing :: C.SymbolicExecutionState arch s C.Execute -> SymbolicExecutionManager e arch s
  Inspecting :: C.SymbolicExecutionState arch s C.Inspect -> SymbolicExecutionManager e arch s
  Suspended :: C.SymbolicExecutionState arch s C.Suspend -> SEE.StateExplorer arch s e -> SymbolicExecutionManager e arch s

symbolicExecutionManagerState :: SymbolicExecutionManager e arch s -> Some (C.SymbolicExecutionState arch s)
symbolicExecutionManagerState sem =
  case sem of
    Configuring st _ -> Some st
    Initializing st -> Some st
    Executing st -> Some st
    Inspecting st -> Some st
    Suspended st _ -> Some st

symbolicExecutionManager :: Some (C.SymbolicExecutionState arch s)
                         -> SymbolicExecutionManager e arch s
symbolicExecutionManager (Some state) =
  case state of
    C.Configuring {} -> Configuring state (SEC.form (C.symbolicExecutionConfig state))
    C.Initializing {} -> Initializing state
    C.Executing {} -> Executing state
    C.Inspecting {} -> Inspecting state
    C.Suspended {} -> Suspended state (SEE.stateExplorer state)

renderSymbolicExecutionManager :: SymbolicExecutionManager e arch s
                               -> C.ValueNameMap s
                               -> B.Widget Names
renderSymbolicExecutionManager sem valNames =
  case sem of
    Configuring _ form -> SEC.renderSymbolicExecutionConfigurator form
    Initializing st -> SES.renderSymbolicExecutionSetup st
    Executing st -> SEM.renderSymbolicExecutionMonitor st
    Inspecting st -> SEI.renderSymbolicExecutionInspector st
    Suspended st form -> SEE.renderSymbolicExecutionStateExplorer (st, form) valNames

handleSymbolicExecutionManagerEvent :: B.BrickEvent Names e
                                    -> SymbolicExecutionManager e arch s
                                    -> B.EventM Names (SymbolicExecutionManager e arch s)
handleSymbolicExecutionManagerEvent evt sem =
  case sem of
    Configuring _ f -> do
      (st', f') <- SEC.handleSymbolicExecutionConfiguratorEvent evt f
      return (Configuring st' f')
    Initializing st -> do
      st' <- SES.handleSymbolicExecutionSetupEvent evt st
      return (Initializing st')
    Executing st -> do
      st' <- SEM.handleSymbolicExecutionMonitorEvent evt st
      return (Executing st')
    Inspecting st -> do
      st' <- SEI.handleSymbolicExecutionInspectorEvent evt st
      return (Inspecting st')
    Suspended st f -> do
      (st', f') <- SEE.handleSymbolicExecutionStateExplorerEvent evt (st, f)
      return (Suspended st' f')
