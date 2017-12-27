{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Surveyor.Loader (
  asynchronouslyLoad,
  loadElf
  ) where

import qualified Brick.BChan as B
import qualified Control.Concurrent.Async as A
import qualified Control.Exception as X
import qualified Data.ByteString as BS
import qualified Data.ElfEdit as E
import qualified Data.Parameterized.NatRepr as NR
import qualified Data.Parameterized.Nonce as NG
import           Data.Proxy ( Proxy(..) )

import qualified Data.Macaw.CFG.Core as MC
import qualified Data.Macaw.Memory as MM
import qualified Data.Macaw.Memory.ElfLoader as MM
import qualified Renovate as R
import qualified Renovate.Arch.X86_64 as X86
import qualified Renovate.Arch.PPC as PPC

import qualified Surveyor.Architecture as A
import           Surveyor.BinaryAnalysisResult
import           Surveyor.Events ( Events(..) )
import qualified Surveyor.Loader.RenovateAnalysis as RA
import qualified Surveyor.Loader.PPCConfig as LP

import Data.Macaw.PPC.PPCReg ()
import Data.Macaw.PPC.Arch ()

-- | Start a thread to load an input file in the background
--
-- The thread sends the result of loading to the main thread through the
-- provided event channel
asynchronouslyLoad :: NG.NonceGenerator IO s -> B.BChan (Events s) -> FilePath -> IO ()
asynchronouslyLoad ng customEventChan exePath = do
  _thread <- A.async $ do
    -- We spawn off a second worker so that we can catch any exceptions it
    -- throws without blocking the caller.
    worker <- A.async $ do
      bs <- BS.readFile exePath
      case E.parseElf bs of
        E.ElfHeaderError off msg ->
          B.writeBChan customEventChan (ErrorLoadingELFHeader off msg)
        E.Elf32Res [] e32 -> loadElf ng customEventChan (E.Elf32 e32)
        E.Elf64Res [] e64 -> loadElf ng customEventChan (E.Elf64 e64)
        E.Elf32Res errs _ -> B.writeBChan customEventChan (ErrorLoadingELF errs)
        E.Elf64Res errs _ -> B.writeBChan customEventChan (ErrorLoadingELF errs)
    eres <- A.waitCatch worker
    case eres of
      Right () -> return ()
      Left exn -> B.writeBChan customEventChan (AnalysisFailure exn)
  return ()

-- | We generate one nonce at the beginning of a single load and re-use it
-- across all of the streamed results for the same executable.  We need to
-- generate the nonce under 'withElfConfig' so that we can capture the
-- appropriate value of w.
loadElf :: NG.NonceGenerator IO s -> B.BChan (Events s) -> E.SomeElf E.Elf -> IO ()
loadElf ng customEventChan someElf = do
  let elfLoadOpts = MM.LoadOptions { MM.loadStyle = MM.LoadBySegment
                                   , MM.includeBSS = False
                                   }
  rcfgs <- case someElf of
    E.Elf32 e32 -> do
      ppc32cfg <- LP.ppcConfig (Proxy @PPC.PPC32) customEventChan ng e32 PPC.config32 A.mkPPC32Result
      return [ (R.PPC32, ppc32cfg)
             ]
    E.Elf64 e64 -> do
      let Right (_, mem) = MM.memoryForElf elfLoadOpts e64
      nonceWx86 <- NG.freshNonce ng
      nonceIx86 <- NG.freshNonce ng
      nonceAx86 <- NG.freshNonce ng
      ppc64cfg <- LP.ppcConfig (Proxy @PPC.PPC64) customEventChan ng e64 PPC.config64 A.mkPPC64Result
      let x86cfg0 = X86.config (RA.analysis A.mkX86Result (nonceWx86, nonceIx86, nonceAx86)) undefined
      let x86callback addr ebi =
            case ebi of
              Left ex -> B.writeBChan customEventChan (AnalysisFailure ex)
              Right bi ->
                let res = BinaryAnalysisResult { rBlockInfo = bi
                                               , rMemory = mem
                                               , rISA = R.rcISA x86cfg0
                                               , rBlockMap = indexBlocksByAddress (R.rcISA x86cfg0) mem bi
                                               , rNonces = (nonceWx86, nonceIx86, nonceAx86)
                                               }
                    sr = A.mkX86Result res
                in B.writeBChan customEventChan (AnalysisProgress sr)
      let x86cfg = x86cfg0 { R.rcFunctionCallback = x86callback }
      return [ (R.PPC64, ppc64cfg)
             , (R.X86_64, R.SomeConfig NR.knownNat x86cfg)
             ]
  R.withElfConfig someElf rcfgs $ \rc e0 m -> do
    (res, diags) <- R.analyzeElf rc e0 m
    B.writeBChan customEventChan (AnalysisFinished res diags)
