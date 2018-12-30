{-# LANGUAGE FlexibleContexts #-}
module Surveyor.Core.TranslationCache (
  TranslationCache,
  newTranslationCache,
  flushTranslationCache,
  translateFunctionBlocks
  ) where

import qualified Data.IORef as IOR
import qualified Data.Map as Map
import qualified Data.Parameterized.Map as MapF

import qualified Surveyor.Core.Architecture as CA
import           Surveyor.Core.IRRepr ( IRRepr )

newtype TranslationCache arch s =
  TranslationCache (IOR.IORef (MapF.MapF (IRRepr arch) (TranslatedBlocks arch s)))

newtype TranslatedBlocks arch s ir =
  TranslatedBlocks (Map.Map (CA.FunctionHandle arch s) (Map.Map (CA.Address arch s) (CA.Block arch s, CA.Block ir s)))

-- | Construct a new (empty) 'TranslationCache'
newTranslationCache :: IO (TranslationCache arch s)
newTranslationCache = TranslationCache <$> IOR.newIORef MapF.empty

-- | Flush all of the entries from the 'TranslationCache'
flushTranslationCache :: TranslationCache arch s -> IO ()
flushTranslationCache (TranslationCache cacheRef) = IOR.writeIORef cacheRef MapF.empty

-- | Translate the blocks from the native representation of the architecture
-- into the requested IR (based on the 'IRRepr' type).  Caches results for
-- future lookups.
translateFunctionBlocks :: (CA.Architecture arch s, CA.ArchConstraints arch s)
                        => TranslationCache arch s
                        -> CA.AnalysisResult arch s
                        -> IRRepr arch ir
                        -> CA.FunctionHandle arch s
                        -> IO (Map.Map (CA.Address arch s) (CA.Block arch s, CA.Block ir s))
translateFunctionBlocks (TranslationCache cacheRef) ares rep fh = do
  m0 <- IOR.readIORef cacheRef
  case MapF.lookup rep m0 of
    Just (TranslatedBlocks m1) ->
      case Map.lookup fh m1 of
        Just tr -> return tr
        Nothing -> do
          blocks <- CA.asAlternativeIR rep ares fh
          let tr = toBlockMap blocks
          let m1' = Map.insert fh tr m1
          IOR.writeIORef cacheRef (MapF.insert rep (TranslatedBlocks m1') m0)
          return tr
    Nothing -> do
      -- There is no entry at all for this repr, so build a fresh block translation cache
      blocks <- CA.asAlternativeIR rep ares fh
      let tr = toBlockMap blocks
      let m1 = Map.singleton fh tr
      IOR.writeIORef cacheRef (MapF.insert rep (TranslatedBlocks m1) m0)
      return tr
  where
    toBlockMap pairs = Map.fromList [ (CA.blockAddress origBlock, (origBlock, trBlock))
                                    | (origBlock, trBlock) <- pairs
                                    ]
