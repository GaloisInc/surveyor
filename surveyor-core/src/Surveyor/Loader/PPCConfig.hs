{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Surveyor.Loader.PPCConfig (
  ppcConfig
  ) where

import           GHC.TypeLits

import qualified Control.Concurrent.Async as CA
import qualified Data.ByteString as BS
import qualified Data.Macaw.BinaryLoader as MBL
import qualified Data.Macaw.CFG as MC
import qualified Data.Macaw.Memory as MM
import qualified Data.Macaw.Types as MT
import qualified Data.Parameterized.TraversableF as TF
import qualified Data.Parameterized.NatRepr as NR
import qualified Data.Parameterized.Nonce as NG
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Text as T

import qualified Data.Parameterized.HasRepr as HR
import qualified Lang.Crucible.Backend.Simple as SB
import qualified SemMC.Architecture as SA
import qualified SemMC.Formula as SF
import qualified SemMC.Log as SL
import qualified Dismantle.PPC as DPPC
import qualified Renovate as R
import qualified Renovate.Arch.PPC as RP

import qualified Surveyor.Architecture as A
import           Surveyor.BinaryAnalysisResult
import qualified Surveyor.Chan as C
import           Surveyor.Events
import qualified Surveyor.Loader.RenovateAnalysis as RA

ppcConfig :: (w ~ MC.RegAddrWidth (MC.ArchReg arch),
              R.InstructionAnnotation arch ~ RP.TargetAddress arch,
              R.Instruction arch ~ RP.Instruction,
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
              MBL.BinaryLoader arch binFmt,
              HR.HasRepr (DPPC.Opcode DPPC.Operand) (SA.ShapeRepr arch),
              Show (MC.ArchReg arch (MT.BVType w)),
              MC.RegisterInfo (MC.ArchReg arch))
          => MBL.BinaryRepr binFmt
          -> C.Chan (Events s)
          -> NG.NonceGenerator IO s
          -> [(Some (DPPC.Opcode DPPC.Operand), BS.ByteString)]
          -> ((R.RewriteEnv arch -> MBL.LoadedBinary arch binFmt -> A.SomeResult s arch) ->
              (A.SomeResult s arch -> R.ISA arch -> MBL.LoadedBinary arch binFmt -> R.SymbolicBlock arch -> R.RewriteM arch (Maybe [R.TaggedInstruction arch (R.InstructionAnnotation arch)])) ->
              R.RenovateConfig arch binFmt (A.SomeResult s))
          -> (BinaryAnalysisResult s (DPPC.Opcode DPPC.Operand) arch -> A.SomeResult s arch)
          -> IO (R.SomeConfig R.TrivialConfigConstraint (A.SomeResult s))
ppcConfig binRep customEventChan ng semantics mkCfg0 mkRes = do
  sym <- SB.newSimpleBackend ng
  logCfg <- mkLogCfg customEventChan
  let ?logCfg = logCfg
  formulas <- SF.loadFormulas sym semantics
  nonceA <- NG.freshNonce ng
  let cfg0 = mkCfg0 (RA.analysis RP.isa mkRes nonceA (Just formulas)) R.nop
  let callback loadedBinary _addr bi = do
            let isa = R.rcISA cfg0
            let res = BinaryAnalysisResult { rBlockInfo = bi
                                           , rLoadedBinary = loadedBinary
                                           , rISA = isa
                                           , rBlockMap = indexBlocksByAddress isa bi
                                           , rNonce = nonceA
                                           , rSemantics = Just formulas
                                           }
            let sr = mkRes res
            C.writeChan customEventChan (AnalysisProgress sr)
  let cfg = cfg0 { R.rcFunctionCallback = Just (10, callback) }
  return (R.SomeConfig NR.knownNat binRep cfg)

mkLogCfg :: C.Chan (Events s) -> IO SL.LogCfg
mkLogCfg customEventChan = do
  lcfg <- SL.mkLogCfg "loader"
  _ <- CA.async (SL.consumeUntilEnd (const True) (translateMessage customEventChan) lcfg)
  return lcfg

translateMessage :: C.Chan (Events s) -> SL.LogEvent -> IO ()
translateMessage customEventChan evt =
  C.writeChan customEventChan (LogDiagnostic (T.pack (SL.prettyLogEvent evt)))

