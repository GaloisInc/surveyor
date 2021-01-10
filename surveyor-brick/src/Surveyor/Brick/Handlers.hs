{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
-- | The main event handler for the Brick UI
--
-- All of the state is hidden in this module.  The system can only modify state
-- by sending messages.  To facilitate this, no mutation functions are exported.
module Surveyor.Brick.Handlers (
  appHandleEvent,
  -- * State types
  SBE.BrickUIExtension,
  SBE.BrickUIState,
  -- * Lenses ('L.Getter's only)
  SBE.blockSelectorG,
  SBE.blockViewerG,
  SBE.functionSelectorG,
  SBE.functionViewerG,
  SBE.minibufferG,
  SBE.symbolicExecutionStateG
  ) where

import qualified Brick as B
import           Control.Lens ( (&), (^.), (.~), (%~), (^?), _Just )
import qualified Control.Lens as L
import           Control.Monad.IO.Class ( liftIO )
import qualified Control.NF as NF
import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map
import           Data.Parameterized.Classes
import qualified Data.Parameterized.List as PL
import           Data.Parameterized.Some ( Some(..) )
import qualified Graphics.Vty as V

import           Prelude

import qualified Surveyor.Brick.Extension as SBE
import           Surveyor.Brick.Names ( Names(..) )
import qualified Surveyor.Brick.Widget.BlockSelector as BS
import qualified Surveyor.Brick.Widget.FunctionSelector as FS
import qualified Surveyor.Brick.Widget.FunctionViewer as FV
import qualified Surveyor.Brick.Widget.Minibuffer as MB
import qualified Surveyor.Brick.Widget.SymbolicExecution as SEM
import qualified Surveyor.Core as C

import           Surveyor.Brick.Handlers.Extension ( handleExtensionEvent )
import           Surveyor.Brick.Handlers.Load ( handleLoadEvent )
import           Surveyor.Brick.Handlers.SymbolicExecution ( handleSymbolicExecutionEvent )

appHandleEvent :: C.State SBE.BrickUIExtension SBE.BrickUIState s -> B.BrickEvent Names (C.Events s (C.S SBE.BrickUIExtension SBE.BrickUIState)) -> B.EventM Names (B.Next (C.State SBE.BrickUIExtension SBE.BrickUIState s))
appHandleEvent (C.State s0) evt =
  case evt of
    B.AppEvent ae -> handleCustomEvent s0 ae
    B.VtyEvent vtyEvt -> handleVtyEvent (C.State s0) vtyEvt
    B.MouseDown {} -> B.continue (C.State s0)
    B.MouseUp {} -> B.continue (C.State s0)

handleVtyEvent :: C.State SBE.BrickUIExtension SBE.BrickUIState s -> V.Event -> B.EventM Names (B.Next (C.State SBE.BrickUIExtension SBE.BrickUIState s))
handleVtyEvent s0@(C.State s) evt
  | V.EvKey k mods <- evt
  , km <- s ^. C.lKeymap
  , Just (C.SomeCommand cmd) <- C.lookupKeyCommand (C.sUIMode s) (C.Key k mods) km
  , Just Refl <- testEquality (C.cmdArgTypes cmd) PL.Nil = do
      -- First, we try to consult the keymap.  For now, we can only handle
      -- commands that take no arguments.  Later, once we develop a notion of
      -- "current context", we can use that to call commands that take an
      -- argument.
      liftIO (C.cmdFunc cmd (C.sEventChannel s) (C.SomeState s) PL.Nil)
      B.continue (C.State s)
  | otherwise =
  case C.sUIMode s of
    C.SomeMiniBuffer (C.MiniBuffer oldMode)
      | mb <- s ^. C.lUIExtension . SBE.minibufferL -> do
          mbs <- MB.handleMinibufferEvent evt (C.sEventChannel s) (C.SomeState s) mb
          case mbs of
            MB.Canceled mb' -> do
              let s' = s & C.lUIMode .~ C.SomeUIMode oldMode
                         & C.lUIExtension . SBE.minibufferL .~ mb'
              B.continue $! C.State s'
            MB.Completed mb' -> do
              let s' = s & C.lUIExtension . SBE.minibufferL .~ mb'
              B.continue $! C.State s'
            MB.Executed mb' -> do
              let s' = s & C.lUIMode .~ C.SomeUIMode oldMode
                         & C.lUIExtension . SBE.minibufferL .~ mb'
              B.continue $! C.State s'
      | otherwise -> B.continue s0
    C.SomeUIMode C.BlockSelector
      | Just bsel <- s ^? C.lArchState . _Just . C.lUIState . SBE.blockSelectorL -> do
          bsel' <- BS.handleBlockSelectorEvent evt bsel
          let s' = s & C.lArchState . _Just . C.lUIState . SBE.blockSelectorL .~ bsel'
          B.continue $! C.State s'
      | otherwise -> B.continue s0
    C.SomeUIMode (C.BlockViewer _archNonce _rep) -> B.continue s0
    C.SomeUIMode C.FunctionSelector
      | Just fsel <- s ^? C.lArchState . _Just . C.lUIState . SBE.functionSelectorL -> do
          fsel' <- FS.handleFunctionSelectorEvent evt fsel
          let s' = s & C.lArchState . _Just . C.lUIState . SBE.functionSelectorL .~ fsel'
          B.continue $! C.State s'
      | otherwise -> B.continue s0
    C.SomeUIMode (C.FunctionViewer fvNonce rep)
      | Just Refl <- testEquality fvNonce (s ^. C.lNonce)
      , Just archState <- s ^. C.lArchState
      , Just fview <- archState ^. SBE.functionViewerG rep
      , Just cstk <- s ^? C.lArchState . _Just . C.contextL -> do
          cstk' <- FV.handleFunctionViewerEvent evt fview cstk
          let s' = s & C.lArchState . _Just . C.contextL .~ cstk'
          B.continue $! C.State s'
    C.SomeUIMode C.SymbolicExecutionManager
      | Just archState <- s ^. C.lArchState
      , Just sessionID <- archState ^? C.contextL . C.currentContext . C.symExecSessionIDL
      , Just sessions <- archState ^? C.symExStateL
      , Just (Some symExState) <- C.lookupSessionState sessions sessionID
      , Just manager0 <- archState ^. C.lUIState . SBE.symbolicExecutionStateL . L.at sessionID -> do
          manager1 <- SEM.handleSymbolicExecutionManagerEvent s (B.VtyEvent evt) manager0 symExState
          let s' = s & C.lArchState . _Just . C.lUIState . SBE.symbolicExecutionStateL %~ Map.insert sessionID manager1
          B.continue $! C.State s'
    C.SomeUIMode _m -> B.continue s0


handleCustomEvent :: ( C.Architecture arch s
                     , C.CrucibleExtension arch
                     )
                  => C.S SBE.BrickUIExtension SBE.BrickUIState arch s
                  -> C.Events s (C.S SBE.BrickUIExtension SBE.BrickUIState)
                  -> B.EventM Names (B.Next (C.State SBE.BrickUIExtension SBE.BrickUIState s))
handleCustomEvent s0 evt =
  case evt of
    C.LoadEvent le -> handleLoadEvent s0 le
    C.SymbolicExecutionEvent se -> do
      -- Delegate to the core handler, then add a hook to do any special
      -- handling required to keep the UI in sync (using the brick UI specific
      -- handler)
      C.State s1 <- C.handleSymbolicExecutionEvent s0 se
      handleSymbolicExecutionEvent s1 se
    C.LoggingEvent le -> do
      s1 <- C.handleLoggingEvent s0 le
      B.continue s1
    C.InfoEvent ie -> do
      s1 <- C.handleInfoEvent s0 ie
      B.continue s1
    C.ContextEvent ce -> do
      s1 <- C.handleContextEvent s0 ce
      B.continue s1
    C.DebuggingEvent de -> do
      s1 <- C.runHandlerT (C.State s0) (C.handleDebuggingEvent s0 de)
      B.continue s1
    C.ExtensionEvent ee -> handleExtensionEvent s0 ee

    -- We discard async state updates if the type of the state has changed in
    -- the interim (i.e., if another binary has been loaded)
    C.AsyncStateUpdate archNonce nfVal upd
      | oldNonce <- C.sArchNonce s0
      , Just Refl <- testEquality oldNonce archNonce ->
          B.continue (C.State (upd (NF.getNF nfVal) s0))
      | otherwise -> B.continue (C.State s0)

    C.Exit -> do
      liftIO (F.traverse_ C.cancelLoader (C.sLoader s0))
      case s0 ^. C.lArchState of
        Nothing -> return ()
        Just archState -> liftIO (C.cleanupActiveSessions (archState ^. C.symExStateL))
      B.halt (C.State s0)
