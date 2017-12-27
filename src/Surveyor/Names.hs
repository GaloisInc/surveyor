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
  deriving (Eq, Ord, Show)
