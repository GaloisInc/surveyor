{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Surveyor.Brick.Handlers.Info (
  handleInfoEvent
  ) where

import qualified Brick as B
import           Control.Lens ( (&), (^.), (.~), (%~) )
import           Control.Monad.IO.Class ( liftIO )
import qualified Data.Text as T
import qualified Data.Text.Prettyprint.Doc as PP
import           Fmt ( (+|), (|+), (||+) )
import qualified Fmt as Fmt
import           Surveyor.Brick.Names ( Names(..) )
import qualified Surveyor.Core as C
import           Text.Printf ( printf )

import qualified Surveyor.Brick.Extension as SBE

handleInfoEvent :: (C.Architecture arch s)
                => C.S SBE.BrickUIExtension SBE.BrickUIState arch s
                -> C.InfoEvent s (C.S SBE.BrickUIExtension SBE.BrickUIState)
                -> B.EventM Names (B.Next (C.State SBE.BrickUIExtension SBE.BrickUIState s))
handleInfoEvent s0 evt =
  case evt of
    C.DescribeCommand (C.SomeCommand cmd) -> do
      let msg = T.pack (printf "%s: %s" (C.cmdName cmd) (C.cmdDocstring cmd))
      liftIO (C.sEmitEvent s0 (C.EchoText msg))
      B.continue $! C.State s0
    C.EchoText txt -> do
      -- All echo area text is mirrored into the log
      liftIO $ C.logMessage s0 (C.msgWith { C.logLevel = C.Requested
                                          , C.logSource = C.EchoAreaUpdate
                                          , C.logText = [txt]
                                          })
      ea' <- liftIO (C.setEchoAreaText (C.sEchoArea s0) txt)
      B.continue $! C.State (s0 & C.lEchoArea .~ ea')
    C.ResetEchoArea -> B.continue $! C.State (s0 & C.lEchoArea %~ C.resetEchoArea)
    C.DescribeKeys -> do
      withBaseMode (s0 ^. C.lUIMode) $ \normalMode -> do
        let keys = C.modeKeybindings (s0 ^. C.lKeymap) (C.SomeUIMode normalMode)
        let formatKey (k, C.SomeCommand cmd) =
              Fmt.fmt ("  "+| PP.pretty k ||+ ": " +| C.cmdName cmd |+ "")
        liftIO $ C.logMessage s0 (C.msgWith { C.logLevel = C.Requested
                                            , C.logSource = C.EventHandler "DescribeKeys"
                                            , C.logText = ( Fmt.fmt ("Keybindings for " +| C.prettyMode normalMode |+ ":")
                                                          : map formatKey keys
                                                          )
                                            })
      let s1 = s0 & C.lUIMode .~ C.SomeUIMode C.Diags
      B.continue $! C.State s1

-- | Get the current mode (looking through the minibuffer if necessary)
withBaseMode :: C.SomeUIMode s -> (C.UIMode s C.NormalK -> a) -> a
withBaseMode sm k =
  case sm of
    C.SomeUIMode m -> k m
    C.SomeMiniBuffer (C.MiniBuffer m) -> k m
