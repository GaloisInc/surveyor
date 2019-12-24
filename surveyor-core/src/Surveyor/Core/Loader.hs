{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Surveyor.Core.Loader (
  AsyncLoader,
  cancelLoader,
  asynchronouslyLoad,
  asynchronouslyLoadElf,
  asynchronouslyLoadLLVM,
  asynchronouslyLoadJAR,
  loadElf
  ) where

import qualified Control.Concurrent.Async as A
import qualified Control.Concurrent.MVar as C
import qualified Control.DeepSeq as DS
import qualified Control.Exception as X
import           Control.Monad ( void )
import qualified Data.ByteString as BS
import qualified Data.ElfEdit as E
import qualified Data.Foldable as F
import qualified Data.List.Split as L
import           Data.Maybe ( catMaybes )
import qualified Data.Parameterized.NatRepr as NR
import qualified Data.Parameterized.Nonce as NG
import           System.FilePath ( takeExtension)

import qualified SemMC.Architecture.PPC32.Opcodes as PPC32
import qualified SemMC.Architecture.PPC64.Opcodes as PPC64

import qualified Data.LLVM.BitCode as LL
import qualified Data.Macaw.BinaryLoader as MBL
import           Data.Macaw.BinaryLoader.X86 ()
import           Data.Macaw.X86.Symbolic ()
import qualified Lang.Crucible.Backend.Simple as SB
import qualified Lang.Crucible.FunctionHandle as CFH
import qualified Language.JVM.JarReader as J
import qualified Renovate as R
import qualified Renovate.Arch.X86_64 as X86
import qualified Renovate.Arch.PPC as PPC
import qualified What4.Expr.Builder as WEB

import qualified Surveyor.Core.Architecture as A
import           Surveyor.Core.BinaryAnalysisResult
import qualified Surveyor.Core.Chan as C
import           Surveyor.Core.Events ( Events(..) )
import qualified Surveyor.Core.Loader.RenovateAnalysis as RA
import qualified Surveyor.Core.Loader.PPCConfig as LP

-- These modules supply orphan instances; we seem to get some strange errors if
-- we don't explicitly import them, even though they are transitively included.
import           Data.Macaw.PPC.PPCReg ()
import           Data.Macaw.PPC.Arch ()

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
asynchronouslyLoad :: NG.NonceGenerator IO s -> C.Chan (Events s st) -> FilePath -> IO AsyncLoader
asynchronouslyLoad ng customEventChan path
  | takeExtension path == ".bc" = asynchronouslyLoadLLVM ng customEventChan path
  | takeExtension path == ".jar" = asynchronouslyLoadJAR ng customEventChan path
  | otherwise = asynchronouslyLoadElf ng customEventChan path

-- | Load a JAR file
asynchronouslyLoadJAR :: NG.NonceGenerator IO s -> C.Chan (Events s st) -> FilePath -> IO AsyncLoader
asynchronouslyLoadJAR ng customEventChan jarPath = do
  nonce <- NG.freshNonce ng
  mv <- C.newEmptyMVar
  errThread <- A.async $ do
    worker <- A.async $ do
      jr <- J.newJarReader [jarPath]
      let chunks = L.chunksOf 10 (J.jarClasses jr)
      lastRes <- F.foldlM (addJARChunk nonce jr) Nothing chunks
      let lastRes' = A.mkJVMResult nonce jr lastRes []
      C.writeChan customEventChan (AnalysisFinished (A.SomeResult lastRes') [])
    C.putMVar mv worker
    eres <- A.waitCatch worker
    case eres of
      Right () -> return ()
      Left exn -> C.writeChan customEventChan (AnalysisFailure exn)
  worker <- C.takeMVar mv
  return AsyncLoader { errorCatcher = errThread
                     , workerThread = worker
                     }
  where
    addJARChunk nonce jr mres classNames = do
      let readClass className = J.loadClassFromJar className jr
      classes <- catMaybes <$> mapM readClass classNames
      let res' = A.mkJVMResult nonce jr mres classes
      C.writeChan customEventChan (AnalysisProgress (A.SomeResult res'))
      return (Just res')

asynchronouslyLoadLLVM :: NG.NonceGenerator IO s -> C.Chan (Events s st) -> FilePath -> IO AsyncLoader
asynchronouslyLoadLLVM ng customEventChan bcPath = do
  mv <- C.newEmptyMVar
  errThread <- A.async $ do
    worker <- A.async $ do
      bs <- BS.readFile bcPath
      em <- LL.parseBitCode bs
      case em of
        Left err ->
          C.writeChan customEventChan (ErrorLoadingLLVM (LL.formatError err))
        Right m -> do
          nonce <- NG.freshNonce ng
          C.writeChan customEventChan (AnalysisFinished (A.mkLLVMResult nonce m) [])
    C.putMVar mv worker
    eres <- A.waitCatch worker
    case eres of
      Right () -> return ()
      Left exn -> C.writeChan customEventChan (AnalysisFailure exn)
  worker <- C.takeMVar mv
  return AsyncLoader { errorCatcher = errThread
                     , workerThread = worker
                     }

-- | Start a thread to load an input file in the background
--
-- The thread sends the result of loading to the main thread through the
-- provided event channel
asynchronouslyLoadElf :: NG.NonceGenerator IO s -> C.Chan (Events s st) -> FilePath -> IO AsyncLoader
asynchronouslyLoadElf ng customEventChan exePath = do
  mv <- C.newEmptyMVar
  thread <- A.async $ do
    -- We spawn off a second worker so that we can catch any exceptions it
    -- throws without blocking the caller.
    worker <- A.async $ do
      bs <- BS.readFile exePath
      case E.parseElf bs of
        E.ElfHeaderError off msg ->
          C.writeChan customEventChan (ErrorLoadingELFHeader off msg)
        E.Elf32Res [] e32 -> loadElf ng customEventChan (E.Elf32 e32)
        E.Elf64Res [] e64 -> loadElf ng customEventChan (E.Elf64 e64)
        E.Elf32Res errs _ -> C.writeChan customEventChan (ErrorLoadingELF errs)
        E.Elf64Res errs _ -> C.writeChan customEventChan (ErrorLoadingELF errs)
    C.putMVar mv worker
    eres <- A.waitCatch worker
    case eres of
      Right () -> return ()
      Left exn -> C.writeChan customEventChan (AnalysisFailure exn)
  worker <- C.takeMVar mv
  return AsyncLoader { errorCatcher = thread
                     , workerThread = worker
                     }

-- | We generate one nonce at the beginning of a single load and re-use it
-- across all of the streamed results for the same executable.  We need to
-- generate the nonce under 'withElfConfig' so that we can capture the
-- appropriate value of w.
loadElf :: NG.NonceGenerator IO s -> C.Chan (Events s st) -> E.SomeElf E.Elf -> IO ()
loadElf ng customEventChan someElf = do
  -- FIXME: Make the float mode configurable
  sym <- SB.newSimpleBackend WEB.FloatRealRepr ng
  nonceAx86 <- NG.freshNonce ng
  let x86cfg0 = X86.config (R.AnalyzeOnly (RA.analysis sym X86.isa A.mkX86Result ng nonceAx86 Nothing))
  let x86callback loadedBinary _addr bi = do
            let res = BinaryAnalysisResult { rBlockInfo = bi
                                           , rLoadedBinary = loadedBinary
                                           , rISA = R.rcISA x86cfg0
                                           , rAddressIndex = indexAddresses (R.rcISA x86cfg0) bi
                                           , rNonce = nonceAx86
                                           , rSemantics = Nothing
                                           , rSym = sym
                                           , rNonceGen = ng
                                           }
            let sr = A.mkX86Result res
            void $ X.evaluate (DS.force sr)
            C.writeChan customEventChan (AnalysisProgress sr)
  let x86cfg = x86cfg0 { R.rcFunctionCallback = Just (10, x86callback) }
  ppc32cfg <- LP.ppcConfig sym MBL.Elf32Repr customEventChan ng PPC32.allDefinedFunctions PPC32.allSemantics PPC.config32 A.mkPPC32Result
  ppc64cfg <- LP.ppcConfig sym MBL.Elf64Repr customEventChan ng PPC64.allDefinedFunctions PPC64.allSemantics PPC.config64 A.mkPPC64Result
  let rcfgs = [ (R.PPC32, ppc32cfg)
              , (R.PPC64, ppc64cfg)
              , (R.X86_64, R.SomeConfig NR.knownNat MBL.Elf64Repr x86cfg)
              ]
  R.withElfConfig someElf rcfgs $ \rc elf loadedBinary -> do
    hdlAlloc <- CFH.newHandleAllocator
    (res, diags) <- R.analyzeElf rc hdlAlloc elf loadedBinary
    C.writeChan customEventChan (AnalysisFinished res diags)
