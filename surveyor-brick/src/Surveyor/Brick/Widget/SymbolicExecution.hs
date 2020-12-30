{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
module Surveyor.Brick.Widget.SymbolicExecution (
  SymbolicExecutionManager,
  symbolicExecutionManager,
  renderSymbolicExecutionManager,
  handleSymbolicExecutionManagerEvent
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
  Configuring :: B.Form (C.SymbolicExecutionConfig s) e Names -> SymbolicExecutionManager e arch s
  Initializing :: SymbolicExecutionManager e arch s
  Executing :: SymbolicExecutionManager e arch s
  Inspecting :: SymbolicExecutionManager e arch s
  -- | A suspended symbolic execution state exploration widget
  --
  -- The nonce is taken from the symbolic execution state used to construct the
  -- widget and cached.  If the current symbolic execution state has a different
  -- nonce, the cached state needs to be discarded and re-built (because the
  -- underlying symbolic execution state has changed).
  Suspended :: SEE.StateExplorer arch s e -> SymbolicExecutionManager e arch s

symbolicExecutionManager :: Some (C.SymbolicExecutionState arch s)
                         -> SymbolicExecutionManager e arch s
symbolicExecutionManager (Some state) =
  case state of
    C.Configuring {} -> Configuring (SEC.form (C.symbolicExecutionConfig state))
    C.Initializing {} -> Initializing
    C.Executing {} -> Executing
    C.Inspecting {} -> Inspecting
    C.Suspended {} -> Suspended (SEE.stateExplorer state)

-- | For each possible symbolic execution state, render the cached state (if
-- any), or construct a fresh UI state for the backend symbolic execution state.
renderSymbolicExecutionManager :: SymbolicExecutionManager e arch s
                               -> C.SymbolicExecutionState arch s k
                               -> C.ValueNameMap s
                               -> B.Widget Names
renderSymbolicExecutionManager sem st valNames =
  case (st, sem) of
    (C.Configuring {}, Configuring form) -> SEC.renderSymbolicExecutionConfigurator form
    (C.Configuring conf, _) -> SEC.renderSymbolicExecutionConfigurator (SEC.form conf)
    (C.Initializing {}, Initializing) -> SES.renderSymbolicExecutionSetup st
    (C.Initializing {}, _) -> SES.renderSymbolicExecutionSetup st
    (C.Executing {}, Executing) -> SEM.renderSymbolicExecutionMonitor st
    (C.Executing {}, _) -> SEM.renderSymbolicExecutionMonitor st
    (C.Inspecting {}, Inspecting) -> SEI.renderSymbolicExecutionInspector st
    (C.Inspecting {}, _) -> SEI.renderSymbolicExecutionInspector st
    (C.Suspended {}, Suspended xp) -> SEE.renderSymbolicExecutionStateExplorer st xp valNames
    (C.Suspended {}, _) -> SEE.renderSymbolicExecutionStateExplorer st (SEE.stateExplorer st) valNames

handleSymbolicExecutionManagerEvent :: C.S evt u arch s
                                    -> B.BrickEvent Names e
                                    -> SymbolicExecutionManager e arch s
                                    -> C.SymbolicExecutionState arch s k
                                    -> B.EventM Names (SymbolicExecutionManager e arch s)
handleSymbolicExecutionManagerEvent s0 evt sem st =
  case (st, sem) of
    (C.Configuring {}, Configuring f) -> do
      f' <- SEC.handleSymbolicExecutionConfiguratorEvent s0 evt f
      return (Configuring f')
    (C.Configuring {}, _) -> do
      -- The execution manager GUI state doesn't match the underlying symbolic
      -- execution state, so make a fresh UI based on the current state
      let f = SEC.form (C.symbolicExecutionConfig st)
      f' <- SEC.handleSymbolicExecutionConfiguratorEvent s0 evt f
      return (Configuring f')
    (C.Initializing {}, Initializing) -> do
      -- Currently there are no handled events and no state changes
      SES.handleSymbolicExecutionSetupEvent evt st
      return Initializing
    (C.Initializing {}, _) -> do
      -- Currently there are no handled events and no state changes
      SES.handleSymbolicExecutionSetupEvent evt st
      return Initializing
    (C.Executing {}, Executing) -> do
      SEM.handleSymbolicExecutionMonitorEvent evt st
      return Executing
    (C.Executing {}, _) -> do
      SEM.handleSymbolicExecutionMonitorEvent evt st
      return Executing
    (C.Inspecting {}, Inspecting) -> do
      SEI.handleSymbolicExecutionInspectorEvent evt st
      return Inspecting
    (C.Inspecting {}, _) -> do
      SEI.handleSymbolicExecutionInspectorEvent evt st
      return Inspecting
    (C.Suspended {}, Suspended f) -> do
      f' <- SEE.handleSymbolicExecutionStateExplorerEvent s0 evt st f
      return (Suspended f')
    (C.Suspended {}, _) -> do
      let f = SEE.stateExplorer st
      f' <- SEE.handleSymbolicExecutionStateExplorerEvent s0 evt st f
      return (Suspended f')
