{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | The 'Void' architecture is an unfortunate artifact, but we need it to
-- instantiate the @arch@ type parameter to something before we have loaded a
-- binary.  Unfortunately, several constraints require the @arch@ type to
-- implement the 'Architecture' class, so we have this instance.
module Surveyor.Core.Architecture.Void () where

import           Control.DeepSeq ( NFData, rnf )
import           Data.Void

import           Surveyor.Core.Architecture.Class

instance IR Void s where
  data Instruction Void s = VoidInstruction Void
  data Operand Void s = VoidOperand Void
  data Opcode Void s = VoidOpcode Void
  data Address Void s = VoidAddress Void

  prettyAddress (VoidAddress v) = absurd v
  prettyInstruction _ (VoidInstruction v) = absurd v
  opcode (VoidInstruction v) = absurd v
  operands (VoidInstruction v) = absurd v
  boundValue (VoidInstruction v) = absurd v
  prettyOperand _ (VoidOperand v) = absurd v
  prettyOpcode (VoidOpcode v) = absurd v
  parseAddress _ = Nothing
  rawRepr = Nothing
  showInstructionAddresses _ = False
  operandSelectable _ = False


type instance CruciblePersonality Void sym = ()

instance SymbolicArchitecture Void s where
  loadConcreteString _ _ _ _ = return Nothing
  archNonceNames _ _ _ _ _ = []

instance Architecture Void s where
  data ArchResult Void s = VoidAnalysisResult Void
  type CrucibleExt Void = ()

  summarizeResult (AnalysisResult (VoidAnalysisResult v) _) = absurd v
  archNonce (AnalysisResult (VoidAnalysisResult v) _) = absurd v
  containingBlocks (AnalysisResult (VoidAnalysisResult v) _) _ = absurd v
  functions (AnalysisResult (VoidAnalysisResult v) _) = absurd v
  genericSemantics (AnalysisResult (VoidAnalysisResult v) _) _ = absurd v
  functionBlocks (AnalysisResult (VoidAnalysisResult v) _) _ = absurd v
  alternativeIRs _ = []
  asAlternativeIR _ (AnalysisResult (VoidAnalysisResult v) _) _ = absurd v
  crucibleCFG _ _ = return Nothing
  freshSymbolicEntry _ _ _ = Nothing
  symbolicInitializers (AnalysisResult (VoidAnalysisResult v) _) _ = absurd v
  fromCrucibleBlock = Nothing

data VoidJVMOperand s = VoidJVMOperand Void

instance CrucibleExtension Void where
  type CrucibleExtensionOperand Void = VoidJVMOperand
  extensionOperandSelectable _ _ = False
  extensionStmtOperands _ _ _ = error "Impossible"
  extensionExprOperands _ _ _ = error "Impossible"
  prettyExtensionStmt _ _ = error "Impossible"
  prettyExtensionApp _ _  = error "Impossible"
  prettyExtensionOperand _ (VoidJVMOperand v) = absurd v

instance Eq (Address Void s) where
  VoidAddress v == _ = absurd v

instance Ord (Address Void s) where
  compare (VoidAddress v) _ = absurd v

instance Show (Address Void s) where
  show (VoidAddress v) = absurd v

instance NFData (Address Void s) where
  rnf (VoidAddress v) = absurd v

instance NFData (Instruction Void s) where
  rnf (VoidInstruction v) = absurd v

instance NFData (Operand Void s) where
  rnf (VoidOperand v) = absurd v
