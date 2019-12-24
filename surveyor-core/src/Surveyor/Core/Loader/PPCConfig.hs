{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Surveyor.Core.Loader.PPCConfig (
  ppcConfig
  ) where

import           GHC.TypeLits

import qualified Control.Concurrent.Async as CA
import qualified Control.DeepSeq as DS
import qualified Control.Exception as X
import           Control.Monad ( void )
import qualified Data.ByteString as BS
import qualified Data.Macaw.BinaryLoader as MBL
import qualified Data.Macaw.CFG as MC
import qualified Data.Macaw.Symbolic as MS
import qualified Data.Parameterized.NatRepr as NR
import qualified Data.Parameterized.Nonce as NG
import           Data.Parameterized.Some ( Some(..) )
import           Data.Proxy ( Proxy(..) )

import qualified Data.Parameterized.HasRepr as HR
import qualified Lang.Crucible.Backend.Simple as SB
import qualified SemMC.Architecture as SA
import qualified SemMC.Formula as SF
import qualified SemMC.Log as SL
import qualified Dismantle.PPC as DPPC
import qualified Renovate as R
import qualified Renovate.Arch.PPC as RP
import qualified What4.InterpretedFloatingPoint as WI
import qualified What4.Expr as WE

import qualified Surveyor.Core.Architecture as A
import           Surveyor.Core.BinaryAnalysisResult
import qualified Surveyor.Core.Chan as C
import           Surveyor.Core.Events
import qualified Surveyor.Core.Loader.RenovateAnalysis as RA

ppcConfig :: forall sym w arch s fs st binFmt
           . ( w ~ MC.RegAddrWidth (MC.ArchReg arch)
             , sym ~ SB.SimpleBackend s fs
             , R.InstructionAnnotation arch ~ RP.TargetAddress arch
             , R.Instruction arch ~ RP.Instruction
             , A.Architecture arch s
             , KnownNat w
             , MS.SymArchConstraints arch
             , R.InstructionConstraints arch
             , SA.Architecture arch
             , MC.IPAlignment arch
             , MBL.BinaryLoader arch binFmt
             , HR.HasRepr (DPPC.Opcode DPPC.Operand) (SA.ShapeRepr arch)
             , WI.IsInterpretedFloatExprBuilder (WE.ExprBuilder s SB.SimpleBackendState fs)
             )
          => sym
          -> MBL.BinaryRepr binFmt
          -> C.Chan (Events s st)
          -> NG.NonceGenerator IO s
          -> [(String, BS.ByteString)]
          -> [(Some (DPPC.Opcode DPPC.Operand), BS.ByteString)]
          -> (R.AnalyzeOnly arch binFmt (A.SomeResult s) -> R.RenovateConfig arch binFmt R.AnalyzeOnly (A.SomeResult s))
          -> (BinaryAnalysisResult s (DPPC.Opcode DPPC.Operand) arch -> A.SomeResult s arch)
          -> IO (R.SomeConfig R.AnalyzeOnly (A.SomeResult s))
ppcConfig sym binRep customEventChan ng library semantics mkCfg0 mkRes = do
  logCfg <- mkLogCfg customEventChan
  let ?logCfg = logCfg
  fenv <- SF.formulaEnv (Proxy @arch) sym
  lib <- SF.loadLibrary (Proxy @arch) sym fenv library
  formulas <- SF.loadFormulas sym fenv lib semantics
  nonceA <- NG.freshNonce ng
  let ao = R.AnalyzeOnly (RA.analysis sym RP.isa mkRes ng nonceA (Just formulas))
  let cfg0 = mkCfg0 ao
  let callback loadedBinary _addr bi = do
            let isa = R.rcISA cfg0
            let res = BinaryAnalysisResult { rBlockInfo = bi
                                           , rLoadedBinary = loadedBinary
                                           , rISA = isa
                                           , rAddressIndex = indexAddresses isa bi
                                           , rNonce = nonceA
                                           , rSemantics = Just formulas
                                           , rSym = sym
                                           , rNonceGen = ng
                                           }
            let sr = mkRes res
            void $ X.evaluate (DS.force sr)
            C.writeChan customEventChan (AnalysisProgress sr)
  let cfg = cfg0 { R.rcFunctionCallback = Just (10, callback) }
  return (R.SomeConfig NR.knownNat binRep cfg)

mkLogCfg :: C.Chan (Events s st) -> IO SL.LogCfg
mkLogCfg customEventChan = do
  lcfg <- SL.mkLogCfg "loader"
  _ <- CA.async (SL.consumeUntilEnd (const True) (translateMessage customEventChan) lcfg)
  return lcfg

-- | Drop the log messages from semmc
translateMessage :: C.Chan (Events s st) -> SL.LogEvent -> IO ()
translateMessage _customEventChan _evt =
  return ()

