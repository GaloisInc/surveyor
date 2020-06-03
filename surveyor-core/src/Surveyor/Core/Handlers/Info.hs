{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Surveyor.Core.Handlers.Info (
  handleInfoEvent
  ) where

import           Control.Lens ( (&), (^.), (.~), (%~) )
import           Control.Monad.IO.Class ( MonadIO, liftIO )
import qualified Data.Text as T
import qualified Data.Text.Prettyprint.Doc as PP
import           Fmt ( (+|), (|+), (||+) )
import qualified Fmt as Fmt
import           Text.Printf ( printf )

import qualified Surveyor.Core.Architecture as SCA
import qualified Surveyor.Core.Command as SCC
import qualified Surveyor.Core.EchoArea as SCEA
import qualified Surveyor.Core.Events as SCE
import qualified Surveyor.Core.Keymap as SCK
import qualified Surveyor.Core.Logging as SCL
import qualified Surveyor.Core.Mode as SCM
import qualified Surveyor.Core.State as SCS

handleInfoEvent :: (SCA.Architecture arch s, MonadIO m)
                => SCS.S e u arch s
                -> SCE.InfoEvent s (SCS.S e u)
                -> m (SCS.State e u s)
handleInfoEvent s0 evt =
  case evt of
    SCE.DescribeCommand (SCC.SomeCommand cmd) -> do
      let msg = T.pack (printf "%s: %s" (SCC.cmdName cmd) (SCC.cmdDocstring cmd))
      liftIO (SCS.sEmitEvent s0 (SCE.EchoText msg))
      return $! SCS.State s0
    SCE.EchoText txt -> do
      -- All echo area text is mirrored into the log
      liftIO $ SCS.logMessage s0 (SCL.msgWith { SCL.logLevel = SCL.Requested
                                              , SCL.logSource = SCL.EchoAreaUpdate
                                              , SCL.logText = [txt]
                                              })
      ea' <- liftIO (SCEA.setEchoAreaText (SCS.sEchoArea s0) txt)
      return $! SCS.State (s0 & SCS.lEchoArea .~ ea')
    SCE.ResetEchoArea -> return $! SCS.State (s0 & SCS.lEchoArea %~ SCEA.resetEchoArea)
    SCE.DescribeKeys -> do
      withBaseMode (s0 ^. SCS.lUIMode) $ \normalMode -> do
        let keys = SCK.modeKeybindings (s0 ^. SCS.lKeymap) (SCM.SomeUIMode normalMode)
        let formatKey (k, SCC.SomeCommand cmd) =
              Fmt.fmt ("  "+| PP.pretty k ||+ ": " +| SCC.cmdName cmd |+ "")
        liftIO $ SCS.logMessage s0 (SCL.msgWith { SCL.logLevel = SCL.Requested
                                                , SCL.logSource = SCL.EventHandler "DescribeKeys"
                                                , SCL.logText = ( Fmt.fmt ("Keybindings for " +| SCM.prettyMode normalMode |+ ":")
                                                                  : map formatKey keys
                                                                )
                                                })
      let s1 = s0 & SCS.lUIMode .~ SCM.SomeUIMode SCM.Diags
      return $! SCS.State s1

-- | Get the current mode (looking through the minibuffer if necessary)
withBaseMode :: SCM.SomeUIMode s -> (SCM.UIMode s SCM.NormalK -> a) -> a
withBaseMode sm k =
  case sm of
    SCM.SomeUIMode m -> k m
    SCM.SomeMiniBuffer (SCM.MiniBuffer m) -> k m
