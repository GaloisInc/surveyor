module Surveyor.QML ( surveyor) where

import qualified Brick.BChan as B
import qualified Data.Parameterized.Nonce as PN
import qualified Data.Traversable as T
import qualified Graphics.QML as Q

import           Surveyor.Loader ( AsyncLoader, asynchronouslyLoad )

surveyor :: Maybe FilePath
         -> Maybe FilePath
         -> IO ()
surveyor mInitialInput mUIFile = PN.withIONonceGenerator $ \ng -> do
  customEventChan <- B.newBChan 100
  mloader <- T.traverse (asynchronouslyLoad ng customEventChan) mInitialInput
  case mUIFile of
    Nothing -> error "No UI file"
    Just uiFile -> do
      let cfg = Q.defaultEngineConfig { Q.initialDocument = Q.fileDocument uiFile
                                      }
      Q.runEngineLoop cfg
  return ()
