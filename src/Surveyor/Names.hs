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
           | BlockViewerList
  deriving (Eq, Ord, Show)
