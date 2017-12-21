{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
module Surveyor.Loader (
  asynchronouslyLoad,
  loadElf
  ) where

import qualified Brick.BChan as B
import qualified Control.Concurrent.Async as A
import qualified Data.ByteString as BS
import qualified Data.ElfEdit as E
import qualified Data.Parameterized.NatRepr as NR
import           Data.Proxy ( Proxy(..) )

import qualified Data.Macaw.Memory as MM
import qualified Renovate as R
import qualified Renovate.Arch.X86_64 as X86
import qualified Renovate.Arch.PPC as PPC

import           Surveyor.BinaryAnalysisResult ( BinaryAnalysisResult(..)
                                               , BinaryAnalysisResultWrapper(..)
                                               )
import           Surveyor.Events ( Events(..) )

-- | Start a thread to load an input file in the background
--
-- The thread sends the result of loading to the main thread through the
-- provided event channel
asynchronouslyLoad :: B.BChan Events -> FilePath -> IO ()
asynchronouslyLoad customEventChan exePath = do
  _thread <- A.async $ do
    -- We spawn off a second worker so that we can catch any exceptions it
    -- throws without blocking the caller.
    worker <- A.async $ do
      bs <- BS.readFile exePath
      case E.parseElf bs of
        E.ElfHeaderError off msg ->
          B.writeBChan customEventChan (ErrorLoadingELFHeader off msg)
        E.Elf32Res [] e32 -> loadElf customEventChan (E.Elf32 e32)
        E.Elf64Res [] e64 -> loadElf customEventChan (E.Elf64 e64)
        E.Elf32Res errs _ -> B.writeBChan customEventChan (ErrorLoadingELF errs)
        E.Elf64Res errs _ -> B.writeBChan customEventChan (ErrorLoadingELF errs)
    eres <- A.waitCatch worker
    case eres of
      Right () -> return ()
      Left exn -> B.writeBChan customEventChan (AnalysisFailure exn)
  return ()

analysis :: (MM.MemWidth w)
         => R.ISA i a w
         -> MM.Memory w
         -> R.BlockInfo i w arch
         -> BinaryAnalysisResultWrapper
analysis isa mem bi =
  BinaryAnalysisResultWrapper $ BinaryAnalysisResult { rBlockInfo = bi
                       , rMemory = mem
                       , rISA = isa
                       }

loadElf :: B.BChan Events -> E.SomeElf E.Elf -> IO ()
loadElf customEventChan someElf = do
  let rcfgs = case someElf of
        E.Elf32 e32 ->
          let tocBase = PPC.tocBaseForELF (Proxy @PPC.PPC32) e32
          in [ (R.PPC32, R.SomeConfig NR.knownNat (PPC.config32 tocBase analysis undefined))
             ]
        E.Elf64 e64 ->
          let tocBase = PPC.tocBaseForELF (Proxy @PPC.PPC64) e64
          in [ (R.PPC64, R.SomeConfig NR.knownNat (PPC.config64 tocBase analysis undefined))
             , (R.X86_64, R.SomeConfig NR.knownNat (X86.config analysis undefined))
             ]
  R.withElfConfig someElf rcfgs $ \rc e0 m -> do
    let rc' = rc { R.rcFunctionCallback = \addr ebi ->
                     case ebi of
                       Left ex -> B.writeBChan customEventChan (AnalysisFailure ex)
                       Right bi ->
                         let res = BinaryAnalysisResult { rBlockInfo = bi
                                                        , rMemory = m
                                                        , rISA = R.rcISA rc
                                                        }
                         in B.writeBChan customEventChan (AnalysisProgress (MM.relativeSegmentAddr addr) (BinaryAnalysisResultWrapper res))
                 }
    (res, diags) <- R.analyzeElf rc' e0 m
    B.writeBChan customEventChan (AnalysisFinished res diags)

