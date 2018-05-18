{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Surveyor.Loader.PPCConfig (
  ppcConfig
  ) where

import           GHC.TypeLits

import qualified Brick.BChan as B
import qualified Control.Concurrent.Async as CA
import qualified Data.ByteString as BS
import qualified Data.ElfEdit as E
import qualified Data.Macaw.AbsDomain.AbsState as MA
import qualified Data.Macaw.CFG as MC
import qualified Data.Macaw.Memory as MM
import qualified Data.Macaw.Memory.ElfLoader as MM
import qualified Data.Macaw.Types as MT
import qualified Data.Parameterized.TraversableF as TF
import qualified Data.Parameterized.NatRepr as NR
import qualified Data.Parameterized.Nonce as NG
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Text as T

import qualified Data.Parameterized.HasRepr as HR
import qualified Lang.Crucible.Solver.SimpleBackend as SB
import qualified SemMC.Architecture as SA
import qualified SemMC.Formula as SF
import qualified SemMC.Log as SL
import qualified Dismantle.PPC as DPPC
import qualified Renovate as R
import qualified Renovate.Arch.PPC as PPC

import qualified Surveyor.Architecture as A
import           Surveyor.BinaryAnalysisResult
import           Surveyor.Events
import qualified Surveyor.Loader.RenovateAnalysis as RA

elfLoadOpts :: MM.LoadOptions
elfLoadOpts = MM.LoadOptions { MM.loadRegionBaseOffset = 0
                             , MM.loadRegionIndex = Just 0
                             }

ppcConfig :: (w ~ MC.RegAddrWidth (MC.ArchReg arch),
              MM.MemWidth w,
              R.ArchInfo arch,
              A.Architecture arch s,
              KnownNat w,
              TF.FoldableF (MC.ArchStmt arch),
              R.InstructionConstraints arch,
              MC.IsArchStmt (MC.ArchStmt arch),
              MC.PrettyF (MC.ArchTermStmt arch),
              MC.FoldableFC (MC.ArchFn arch),
              MC.IsArchFn (MC.ArchFn arch),
              SA.Architecture arch,
              HR.HasRepr (DPPC.Opcode DPPC.Operand) (SA.ShapeRepr arch),
              Show (MC.ArchReg arch (MT.BVType w)),
              MC.RegisterInfo (MC.ArchReg arch))
          => proxy arch
          -> B.BChan (Events s)
          -> NG.NonceGenerator IO s
          -> [(Some (DPPC.Opcode DPPC.Operand), BS.ByteString)]
          -> E.Elf w
          -> ((MC.ArchSegmentOff arch -> Maybe (MA.AbsValue w (MT.BVType w))) ->
              (R.ISA arch -> MM.Memory w -> R.BlockInfo arch -> A.SomeResult s arch) ->
              (A.SomeResult s arch -> R.ISA arch -> MM.Memory w -> R.SymbolicBlock arch -> R.RewriteM arch (Maybe [R.TaggedInstruction arch a])) ->
              R.RenovateConfig arch (A.SomeResult s))
          -> (BinaryAnalysisResult s (DPPC.Opcode DPPC.Operand) arch -> A.SomeResult s arch)
          -> IO (R.SomeConfig R.TrivialConfigConstraint (A.SomeResult s))
ppcConfig proxy customEventChan ng semantics elf mkCfg0 mkRes = do
  sym <- SB.newSimpleBackend ng
  logCfg <- mkLogCfg customEventChan
  let ?logCfg = logCfg
  formulas <- SF.loadFormulas sym semantics
  let Right (_sim, mem, _warnings) = MM.memoryForElf elfLoadOpts elf
  nonceA <- NG.freshNonce ng
  let tocBase = PPC.tocBaseForELF proxy elf
  let cfg0 = mkCfg0 tocBase (RA.analysis mkRes nonceA (Just formulas)) undefined
  let callback _addr bi = do
            let isa = R.rcISA cfg0
            let res = BinaryAnalysisResult { rBlockInfo = bi
                                           , rMemory = mem
                                           , rISA = isa
                                           , rBlockMap = indexBlocksByAddress isa bi
                                           , rNonce = nonceA
                                           , rSemantics = Just formulas
                                           }
            let sr = mkRes res
            B.writeBChan customEventChan (AnalysisProgress sr)
  let cfg = cfg0 { R.rcFunctionCallback = Just (10, callback) }
  return (R.SomeConfig NR.knownNat cfg)

mkLogCfg :: B.BChan (Events s) -> IO SL.LogCfg
mkLogCfg customEventChan = do
  lcfg <- SL.mkLogCfg "loader"
  _ <- CA.async (SL.consumeUntilEnd (const True) (translateMessage customEventChan) lcfg)
  return lcfg

translateMessage :: B.BChan (Events s) -> SL.LogEvent -> IO ()
translateMessage customEventChan evt =
  B.writeBChan customEventChan (LogDiagnostic (T.pack (SL.prettyLogEvent evt)))
