{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Surveyor.Brick.Handlers.Load ( handleLoadEvent ) where

import qualified Brick as B
import           Control.Lens ( (&), (^.), (.~), (%~), _Just )
import           Control.Monad.IO.Class ( liftIO )
import qualified Data.Foldable as F
import qualified Data.Parameterized.Classes as PC
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Some ( Some(..) )
import           Data.Proxy ( Proxy(..) )
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Text.Prettyprint.Doc as PP
import qualified Data.Text.Prettyprint.Doc.Render.Text as PPT
import           Surveyor.Brick.Names ( Names(..) )
import qualified Surveyor.Core as C
import           Text.Printf ( printf )


import           Surveyor.Brick.Attributes ( focusedListAttr )
import qualified Surveyor.Brick.Command as BC
import qualified Surveyor.Brick.Extension as SBE
import qualified Surveyor.Brick.Keymap as SBK
import qualified Surveyor.Brick.Widget.BlockSelector as BS
import qualified Surveyor.Brick.Widget.BlockViewer as BV
import qualified Surveyor.Brick.Widget.FunctionSelector as FS
import qualified Surveyor.Brick.Widget.FunctionViewer as FV
import qualified Surveyor.Brick.Widget.Minibuffer as MB
import qualified Surveyor.Brick.Widget.SymbolicExecution as SEM

handleLoadEvent :: (C.Architecture arch s)
                => C.S SBE.BrickUIExtension SBE.BrickUIState arch s
                -> C.LoadEvent s (C.S SBE.BrickUIExtension SBE.BrickUIState)
                -> B.EventM Names (B.Next (C.State SBE.BrickUIExtension SBE.BrickUIState s))
handleLoadEvent s0 evt =
  case evt of
    C.AnalysisFinished (C.SomeResult bar) diags -> do
      let newDiags = map (\d -> T.pack ("Analysis: " ++ show d)) diags
          notification = "Finished loading file"
      s1 <- liftIO $ stateFromAnalysisResult s0 bar (Seq.fromList newDiags <> Seq.singleton notification) C.Ready (C.SomeUIMode C.Diags)
      B.continue (C.State s1)
    C.AnalysisProgress (C.SomeResult bar) -> do
      s1 <- liftIO $ stateFromAnalysisResult s0 bar Seq.empty C.Loading (C.sUIMode s0)
      B.continue (C.State s1)
    C.AnalysisFailure exn -> do
      liftIO $ C.logMessage s0 (C.msgWith { C.logLevel = C.Error
                                          , C.logSource = C.EventHandler "Analysis Failure"
                                          , C.logText = [PPT.renderStrict (PP.layoutCompact ("Analysis failure:" PP.<+> PP.viaShow exn))]
                                          })
      B.continue $! C.State s0
    C.ErrorLoadingELFHeader off msg -> do
      liftIO $ C.logMessage s0 (C.msgWith { C.logLevel = C.Error
                                          , C.logSource = C.EventHandler "ELF Loader"
                                          , C.logText = [T.pack (printf "ELF Loading error at offset 0x%x: %s" off msg)]
                                          })
      B.continue $! C.State s0
    C.ErrorLoadingELF errs -> do
      liftIO $ C.logMessage s0 (C.msgWith { C.logLevel = C.Error
                                          , C.logSource = C.EventHandler "ELF Loader"
                                          , C.logText = map (\d -> PPT.renderStrict (PP.layoutCompact ("ELF Loading error:" PP.<+> PP.viaShow d))) errs
                                          })
      B.continue $! C.State s0
    C.ErrorLoadingLLVM s -> do
      liftIO $ C.logMessage s0 (C.msgWith { C.logLevel = C.Error
                                          , C.logSource = C.EventHandler "LLVM Loader"
                                          , C.logText = [PPT.renderStrict (PP.layoutCompact ("Error loading LLVM bitcode:" PP.<+> PP.pretty s))]
                                          })
      B.continue $! C.State s0
    C.LoadFile filename -> do
      liftIO (F.traverse_ C.cancelLoader (C.sLoader s0))
      loader <- liftIO $ C.asynchronouslyLoad (C.sNonceGenerator s0) (C.sEventChannel s0) filename
      let s1 = s0 & C.lLoader .~ Just loader
                  & C.lInputFile .~ Just filename
      B.continue $! C.State s1
    C.LoadELF filename -> do
      liftIO (F.traverse_ C.cancelLoader (C.sLoader s0))
      loader <- liftIO $ C.asynchronouslyLoadElf (C.sNonceGenerator s0) (C.sEventChannel s0) filename
      let s1 = s0 & C.lLoader .~ Just loader
                  & C.lInputFile .~ Just filename
      B.continue $! C.State s1
    C.LoadJAR filename -> do
      liftIO (F.traverse_ C.cancelLoader (C.sLoader s0))
      loader <- liftIO $ C.asynchronouslyLoadJAR (C.sNonceGenerator s0) (C.sEventChannel s0) filename
      let s1 = s0 & C.lLoader .~ Just loader
                  & C.lInputFile .~ Just filename
      B.continue $! C.State s1
    C.LoadLLVM filename -> do
      liftIO (F.traverse_ C.cancelLoader (C.sLoader s0))
      loader <- liftIO $ C.asynchronouslyLoadLLVM (C.sNonceGenerator s0) (C.sEventChannel s0) filename
      let s1 = s0 & C.lLoader .~ Just loader
                  & C.lInputFile .~ Just filename
      B.continue $! C.State s1

stateFromAnalysisResult :: forall arch0 arch s
                         . (C.Architecture arch s)
                        => C.S SBE.BrickUIExtension SBE.BrickUIState arch0 s
                        -> C.AnalysisResult arch s
                        -> Seq.Seq T.Text
                        -> C.AppState
                        -> C.SomeUIMode s
                        -> IO (C.S SBE.BrickUIExtension SBE.BrickUIState arch s)
stateFromAnalysisResult s0 ares newDiags state uiMode = do
  tcache <- C.newTranslationCache
  case () of
    () | Just PC.Refl <- PC.testEquality (s0 ^. C.lNonce) (C.archNonce ares) -> return ()
       | otherwise -> do
           -- When we get a new binary (and have a fresh state), queue up a
           -- default function to view (the first function we found).
           --
           -- We have to do this in a separate thread, as constructing the
           -- function viewer can be a bit expensive (since we are translating
           -- the function on-demand).
           --
           -- If there is no function, then just don't do anything.
           case C.functions ares of
             [] -> return ()
             defFunc : _ -> do
               let pushContext (newContext, sessState) oldState =
                     oldState & C.lArchState . _Just . C.contextL %~ C.pushContext newContext
                              & C.lArchState . _Just . C.symExStateL %~ (<> sessState)
               C.asynchronously (C.archNonce ares) (C.sEmitEvent s0) pushContext $ do
                 case C.functionBlocks ares defFunc of
                   b0 : _ -> do
                     let ng = C.sNonceGenerator s0
                     C.makeContext ng tcache ares defFunc C.BaseRepr b0
                   _ -> error ("No blocks in function " ++ show defFunc)
  let ng = C.sNonceGenerator s0
  ses <- C.defaultSymbolicExecutionConfig ng
  let appendTextLog ls t = do
        msg <- C.timestamp (C.msgWith { C.logText = [t], C.logSource = C.Loader })
        return (C.appendLog msg ls)
  nextLogStore <- F.foldlM appendTextLog (C.sLogStore s0) newDiags
  return C.S { C.sLogStore = nextLogStore
             , C.sDiagnosticLevel = C.sDiagnosticLevel s0
             , C.sLogActions = C.LoggingActions { C.sStateLogger = C.logToState (C.sEventChannel s0)
                                                , C.sFileLogger = C.sFileLogger (C.sLogActions s0)
                                                }
             , C.sUIMode = uiMode
             , C.sAppState = state
             , C.sEventChannel = C.sEventChannel s0
             , C.sNonceGenerator = C.sNonceGenerator s0
             , C.sEchoArea = C.sEchoArea s0
             , C.sInputFile = C.sInputFile s0
             , C.sLoader = C.sLoader s0
             , C.sKeymap = keymap
             , C.sUIExtension = uiExt
             , C.sArchNonce = C.archNonce ares
             , C.sArchState =
               case () of
                 () | Just oldArchState <- C.sArchState s0
                    , Just PC.Refl <- PC.testEquality (s0 ^. C.lNonce) (C.archNonce ares) ->
                      Just (oldArchState { C.sAnalysisResult = ares })
                    | otherwise -> do
                        let blockViewers = (MapF.Pair C.BaseRepr (BV.blockViewer InteractiveBlockViewer C.BaseRepr))
                                         : [ MapF.Pair rep (BV.blockViewer InteractiveBlockViewer rep)
                                           | C.SomeIRRepr rep <- C.alternativeIRs (Proxy @(arch, s))
                                           ]
                        let archDicts =   MapF.Pair C.BaseRepr C.ArchDict
                                        : [ MapF.Pair rep C.ArchDict
                                          | C.SomeIRRepr rep <- C.alternativeIRs (Proxy @(arch, s))
                                          ]
                        let funcViewerCallback :: forall ir . (C.ArchConstraints ir s) => C.IRRepr arch ir -> C.FunctionHandle arch s -> C.Block ir s -> IO ()
                            funcViewerCallback rep fh b = do
                              C.sEmitEvent s0 (C.PushContext (C.archNonce ares) fh rep b)
                              C.sEmitEvent s0 (C.ViewBlock (C.archNonce ares) rep)
                        let funcViewers = (MapF.Pair C.BaseRepr (FV.functionViewer (funcViewerCallback C.BaseRepr) FunctionCFGViewer C.BaseRepr))
                                          : [ MapF.Pair rep (FV.functionViewer (funcViewerCallback rep) FunctionCFGViewer rep)
                                            | C.SomeIRRepr rep <- C.alternativeIRs (Proxy @(arch, s))
                                            ]
                        let uiState = SBE.BrickUIState { SBE.sBlockSelector = BS.emptyBlockSelector
                                                       , SBE.sBlockViewers = MapF.fromList blockViewers
                                                       , SBE.sFunctionViewer = MapF.fromList funcViewers
                                                       , SBE.sFunctionSelector = FS.functionSelector (const (return ())) focusedListAttr []
                                                       , SBE.sSymbolicExecutionManager =
                                                         SEM.symbolicExecutionManager (Some (C.Configuring ses))
                                                       }
                        return C.ArchState { C.sAnalysisResult = ares
                                           , C.sUIState = uiState
                                           , C.sContext = C.emptyContextStack
                                           , C.sSymExState = mempty
                                           , C.sArchDicts = MapF.fromList archDicts
                                           , C.sIRCache = tcache
                                           }
             }
  where
    addrParser s = C.SomeAddress (C.archNonce ares) <$> C.parseAddress s
    uiExt = SBE.BrickUIExtension { SBE.sMinibuffer = MB.minibuffer addrParser (SBE.updateMinibufferCompletions (C.sEmitEvent s0) (C.archNonce ares)) MinibufferEditor MinibufferCompletionList "M-x" (C.allCommands ++ BC.extraCommands)
                                 }

    keymap = SBK.defaultKeymap (Just (C.archNonce ares))
