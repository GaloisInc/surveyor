{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
-- | A viewer for Crux models
module Surveyor.Brick.Widget.ModelViewer
  ( renderModelViewer,
  ) where

import qualified Brick as B
import qualified Crux.Types as CT
import qualified Data.Binary.IEEE754 as IEEE754
import qualified Data.BitVector.Sized as BV
import qualified Data.Parameterized.Map as MapF
import qualified Data.Text as T
import qualified Lang.Crucible.Types as LCT
import qualified Surveyor.Brick.Names as SBN
import qualified Data.Text.Prettyprint.Doc as PP
import qualified Data.Text.Prettyprint.Doc.Render.Text as PPT

renderModelViewer :: CT.ModelView -> B.Widget SBN.Names
renderModelViewer mv = B.txt (ppModel mv)

ppModel :: CT.ModelView -> T.Text
ppModel (CT.ModelView vals) = case ents of
                [] -> "[]"
                _  -> T.unlines $ (zipWith T.append pre ents) ++ ["]"]
  where ents = MapF.foldrWithKey (\k v rest -> ppVals k v ++ rest) [] vals
        pre  = "[ " : repeat ", "

ppVals :: LCT.BaseTypeRepr ty -> CT.Vals ty -> [T.Text]
ppVals ty (CT.Vals xs) =
  let showEnt = case ty of
        LCT.BaseBVRepr n -> showEnt' show n
        LCT.BaseFloatRepr (LCT.FloatingPointPrecisionRepr eb sb)
          | LCT.natValue eb == 8, LCT.natValue sb == 24 -> showEnt'
            (show . IEEE754.wordToFloat . fromInteger . BV.asUnsigned)
            (LCT.knownNat @32)
        LCT.BaseFloatRepr (LCT.FloatingPointPrecisionRepr eb sb)
          | LCT.natValue eb == 11, LCT.natValue sb == 53 -> showEnt'
            (show . IEEE754.wordToDouble . fromInteger . BV.asUnsigned)
            (LCT.knownNat @64)
        LCT.BaseRealRepr -> showEnt' (show . fromRational @Double) (LCT.knownNat @64)
        _ -> error ("Type not implemented: " ++ show ty)
  in map showEnt xs

  where
    showEnt' :: Show b => (a -> String) -> b -> CT.Entry a -> T.Text
    showEnt' repr n e =
      PPT.renderStrict . PP.layoutCompact $
      "name:" PP.<+> PP.pretty (CT.entryName e) PP.<+>
      "value:" PP.<+> PP.pretty (repr (CT.entryValue e)) PP.<+>
      "bits:" PP.<+> PP.pretty (show n)
