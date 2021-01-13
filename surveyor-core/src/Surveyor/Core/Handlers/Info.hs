{-# LANGUAGE GADTs #-}
module Surveyor.Core.Handlers.Info (
  handleInfoEvent
  ) where

import qualified Control.Concurrent.Async as CCA
import qualified Control.Exception as X
import           Control.Lens ( (&), (^.), (.~) )
import           Control.Monad.IO.Class ( MonadIO, liftIO )
import qualified Data.GraphViz as DG
import qualified Data.Text as T
import qualified Data.Text.Prettyprint.Doc as PP
import           System.FilePath ( (<.>) )

import qualified Surveyor.Core.Architecture as SCA
import qualified Surveyor.Core.Command as SCC
import qualified Surveyor.Core.Events as SCE
import qualified Surveyor.Core.GraphViz as SCG
import qualified Surveyor.Core.Keymap as SCK
import qualified Surveyor.Core.Logging as SCL
import qualified Surveyor.Core.Mode as SCM
import qualified Surveyor.Core.State as SCS

handleInfoEvent :: ( SCA.Architecture arch s
                   , SCA.CrucibleExtension arch
                   , MonadIO m
                   )
                => SCS.S e u arch s
                -> SCE.InfoEvent s (SCS.S e u)
                -> m (SCS.State e u s)
handleInfoEvent s0 evt =
  case evt of
    SCE.DescribeCommand (SCC.SomeCommand cmd) -> do
      let msg = SCL.msgWith { SCL.logLevel = SCL.Requested
                            , SCL.logSource = SCL.EventHandler (T.pack "DescribeCommand")
                            , SCL.logText = [PP.pretty (SCC.cmdName cmd) <> PP.pretty ": " <> SCC.cmdDocstring cmd]
                            }
      liftIO $ SCS.logMessage s0 msg
      return $! SCS.State s0
    SCE.DescribeKeys -> do
      withBaseMode (s0 ^. SCS.lUIMode) $ \normalMode -> do
        let keys = SCK.modeKeybindings (s0 ^. SCS.lKeymap) (SCM.SomeUIMode normalMode)
        let formatKey (k, SCC.SomeCommand cmd) =
              PP.pretty "  " <> PP.pretty k <> PP.pretty ": " <> PP.pretty (SCC.cmdName cmd)
        liftIO $ SCS.logMessage s0 (SCL.msgWith { SCL.logLevel = SCL.Requested
                                                , SCL.logSource = SCL.EventHandler (T.pack "DescribeKeys")
                                                , SCL.logText = ( (PP.pretty "Keybindings for " <> SCM.prettyMode normalMode)
                                                                : map formatKey keys
                                                                )
                                                })
      let s1 = s0 & SCS.lUIMode .~ SCM.SomeUIMode SCM.Diags
      return $! SCS.State s1

    SCE.VisualizeSymbolicTerm regEntry mPath -> do
      gvTest <- liftIO $ DG.isGraphvizInstalled
      case gvTest of
        True -> do
          -- Run the term rendering and graphviz process in another thread so
          -- that we don't block the UI
          _task <- liftIO $ CCA.async $ flip X.onException (reportGraphvizError s0) $ do
            let dot = SCG.regEntryToGraphViz regEntry
            let cmd = DG.Dot
            case mPath of
              Nothing -> DG.runGraphvizCanvas cmd dot DG.Gtk
              Just path -> do
                let msg = SCL.msgWith { SCL.logLevel = SCL.Info
                                      , SCL.logSource = SCL.EventHandler (T.pack "VisualizeSymbolicTerm")
                                      , SCL.logText = [ PP.pretty "Saving to SVG " <> PP.pretty path ]
                                      }
                SCS.logMessage s0 msg
                _ <- DG.runGraphvizCommand cmd dot DG.Svg path
                _ <- DG.runGraphvizCommand cmd dot (DG.XDot Nothing) (path <.> "dot")
                let msg2 = SCL.msgWith { SCL.logLevel = SCL.Info
                                       , SCL.logSource = SCL.EventHandler (T.pack "VisualizeSymbolicTerm")
                                       , SCL.logText = [ PP.pretty "Finished rendering symbolic term to " <> PP.pretty path ]
                                       }
                SCS.logMessage s0 msg2
                return ()
          -- FIXME: Stash the task handle away so that we can monitor it from a
          -- UI somewhere (and clean them up periodically, and on program
          -- termination)
          return ()
        False -> do
          let msg = SCL.msgWith { SCL.logLevel = SCL.Warn
                                , SCL.logSource = SCL.EventHandler (T.pack "VisualizeSymbolicTerm")
                                , SCL.logText = [ PP.pretty "Graphviz is not installed" ]
                                }
          liftIO $ SCS.logMessage s0 msg
      return $! SCS.State s0

reportGraphvizError :: SCS.S e u arch s -> IO ()
reportGraphvizError s0 = do
  let msg = SCL.msgWith { SCL.logLevel = SCL.Error
                        , SCL.logSource = SCL.EventHandler (T.pack "VisualizeSymbolicTerm")
                        , SCL.logText = [ PP.pretty "Error while running graphviz" ]
                        }
  SCS.logMessage s0 msg

-- | Get the current mode (looking through the minibuffer if necessary)
withBaseMode :: SCM.SomeUIMode s -> (SCM.UIMode s SCM.NormalK -> a) -> a
withBaseMode sm k =
  case sm of
    SCM.SomeUIMode m -> k m
    SCM.SomeMiniBuffer (SCM.MiniBuffer m) -> k m
