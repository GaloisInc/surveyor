module Surveyor.Names (
  Names(..)
  ) where

data Names = DiagnosticView
           | DiagnosticContent
           | FunctionList
           | BlockList
           | MinibufferEditor
           | MinibufferCompletionList
           | BlockSelectEditor
           | BlockSelectList
           | FunctionSelectEditor
           | FunctionSelectList
  deriving (Eq, Ord, Show)
