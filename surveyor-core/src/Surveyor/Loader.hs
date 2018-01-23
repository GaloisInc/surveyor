{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Surveyor.Loader (
  AsyncLoader,
  cancelLoader,
  asynchronouslyLoad,
  asynchronouslyLoadElf,
  asynchronouslyLoadLLVM,
  asynchronouslyLoadJAR,
  loadElf
  ) where

import qualified Brick.BChan as B
import qualified Control.Concurrent.Async as A
import qualified Control.Concurrent.MVar as C
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as LB8
import qualified Data.ElfEdit as E
import qualified Data.Foldable as F
import qualified Data.List.Split as L
import qualified Data.Map.Strict as M
import           Data.Maybe ( catMaybes )
import qualified Data.Parameterized.NatRepr as NR
import qualified Data.Parameterized.Nonce as NG
import           Data.Proxy ( Proxy(..) )
import           System.FilePath ( takeExtension)

import qualified SemMC.Architecture.PPC32.Opcodes as PPC32
import qualified SemMC.Architecture.PPC64.Opcodes as PPC64

import qualified Data.LLVM.BitCode as LL
import qualified Data.Macaw.Memory.ElfLoader as MM
import qualified Language.JVM.JarReader as J
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

data AsyncLoader =
  AsyncLoader { errorCatcher :: A.Async ()
              , workerThread :: A.Async ()
              }

cancelLoader :: AsyncLoader -> IO ()
cancelLoader al = do
  A.cancel (errorCatcher al)
  A.cancel (workerThread al)

-- | Try to load the given file, attempting to determine its type based on the
-- filename
asynchronouslyLoad :: NG.NonceGenerator IO s -> B.BChan (Events s) -> FilePath -> IO AsyncLoader
asynchronouslyLoad ng customEventChan path
  | takeExtension path == ".bc" = asynchronouslyLoadLLVM ng customEventChan path
  | takeExtension path == ".jar" = asynchronouslyLoadJAR ng customEventChan path
  | otherwise = asynchronouslyLoadElf ng customEventChan path

-- | Load a JAR file
asynchronouslyLoadJAR :: NG.NonceGenerator IO s -> B.BChan (Events s) -> FilePath -> IO AsyncLoader
asynchronouslyLoadJAR ng customEventChan jarPath = do
  nonce <- NG.freshNonce ng
  mv <- C.newEmptyMVar
  errThread <- A.async $ do
    worker <- A.async $ do
      jr <- J.newJarReader [jarPath]
      let chunks = L.chunksOf 10 (M.keys (J.unJR jr))
      lastRes <- F.foldlM (addJARChunk nonce jr) Nothing chunks
      let lastRes' = A.mkJVMResult nonce jr lastRes []
      B.writeBChan customEventChan (AnalysisFinished (A.SomeResult lastRes') [])
    C.putMVar mv worker
    eres <- A.waitCatch worker
    case eres of
      Right () -> return ()
      Left exn -> B.writeBChan customEventChan (AnalysisFailure exn)
  worker <- C.takeMVar mv
  return AsyncLoader { errorCatcher = errThread
                     , workerThread = worker
                     }
  where
    addJARChunk nonce jr mres classNames = do
      let readClass className = J.loadClassFromJar (LB8.unpack className) jr
      classes <- catMaybes <$> mapM readClass classNames
      let res' = A.mkJVMResult nonce jr mres classes
      B.writeBChan customEventChan (AnalysisProgress (A.SomeResult res'))
      return (Just res')

asynchronouslyLoadLLVM :: NG.NonceGenerator IO s -> B.BChan (Events s) -> FilePath -> IO AsyncLoader
asynchronouslyLoadLLVM ng customEventChan bcPath = do
  mv <- C.newEmptyMVar
  errThread <- A.async $ do
    worker <- A.async $ do
      bs <- BS.readFile bcPath
      em <- LL.parseBitCode bs
      case em of
        Left err ->
          B.writeBChan customEventChan (ErrorLoadingLLVM (LL.formatError err))
        Right m -> do
          nonce <- NG.freshNonce ng
          B.writeBChan customEventChan (AnalysisFinished (A.mkLLVMResult nonce m) [])
    C.putMVar mv worker
    eres <- A.waitCatch worker
    case eres of
      Right () -> return ()
      Left exn -> B.writeBChan customEventChan (AnalysisFailure exn)
  worker <- C.takeMVar mv
  return AsyncLoader { errorCatcher = errThread
                     , workerThread = worker
                     }

-- | Start a thread to load an input file in the background
--
-- The thread sends the result of loading to the main thread through the
-- provided event channel
asynchronouslyLoadElf :: NG.NonceGenerator IO s -> B.BChan (Events s) -> FilePath -> IO AsyncLoader
asynchronouslyLoadElf ng customEventChan exePath = do
  mv <- C.newEmptyMVar
  thread <- A.async $ do
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
    C.putMVar mv worker
    eres <- A.waitCatch worker
    case eres of
      Right () -> return ()
      Left exn -> B.writeBChan customEventChan (AnalysisFailure exn)
  worker <- C.takeMVar mv
  return AsyncLoader { errorCatcher = thread
                     , workerThread = worker
                     }

-- | We generate one nonce at the beginning of a single load and re-use it
-- across all of the streamed results for the same executable.  We need to
-- generate the nonce under 'withElfConfig' so that we can capture the
-- appropriate value of w.
loadElf :: NG.NonceGenerator IO s -> B.BChan (Events s) -> E.SomeElf E.Elf -> IO ()
loadElf ng customEventChan someElf = do
  let elfLoadOpts = MM.LoadOptions { MM.loadStyle = MM.LoadBySegment
                                   , MM.includeBSS = False
                                   , MM.loadRegionIndex = 0
                                   }
  rcfgs <- case someElf of
    E.Elf32 e32 -> do
      ppc32cfg <- LP.ppcConfig (Proxy @PPC.PPC32) customEventChan ng PPC32.allSemantics e32 PPC.config32 A.mkPPC32Result
      return [ (R.PPC32, ppc32cfg)
             ]
    E.Elf64 e64 -> do
      let Right (_, mem) = MM.memoryForElf elfLoadOpts e64
      nonceAx86 <- NG.freshNonce ng
      ppc64cfg <- LP.ppcConfig (Proxy @PPC.PPC64) customEventChan ng PPC64.allSemantics e64 PPC.config64 A.mkPPC64Result
      let x86cfg0 = X86.config (RA.analysis A.mkX86Result nonceAx86 Nothing) undefined
      let x86callback _addr ebi =
            case ebi of
              Left ex -> B.writeBChan customEventChan (AnalysisFailure ex)
              Right bi ->
                let res = BinaryAnalysisResult { rBlockInfo = bi
                                               , rMemory = mem
                                               , rISA = R.rcISA x86cfg0
                                               , rBlockMap = indexBlocksByAddress (R.rcISA x86cfg0) mem bi
                                               , rNonce = nonceAx86
                                               , rSemantics = Nothing
                                               }
                    sr = A.mkX86Result res
                in B.writeBChan customEventChan (AnalysisProgress sr)
      let x86cfg = x86cfg0 { R.rcFunctionCallback = Just (10, x86callback) }
      return [ (R.PPC64, ppc64cfg)
             , (R.X86_64, R.SomeConfig NR.knownNat x86cfg)
             ]
  R.withElfConfig someElf rcfgs $ \rc e0 m -> do
    (res, diags) <- R.analyzeElf rc e0 m
    B.writeBChan customEventChan (AnalysisFinished res diags)
