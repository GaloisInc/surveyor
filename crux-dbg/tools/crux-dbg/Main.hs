{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
module Main ( main ) where

import qualified System.FilePath as SFP
import qualified System.Exit as SE

import qualified Crux as C
import qualified Crux.Log as CL
import qualified Crux.Config.Common as CCC
import qualified Surveyor.Brick as SB

import qualified Crux.Debug.Config as CDC
import qualified Crux.Debug.LLVM as CDL

main :: IO ()
main = do
  let outCfg = CL.defaultOutputConfig
  let ?outputConfig = outCfg
  C.loadOptions outCfg "crux-dbg" "0.1" CDC.debugConfig $ \(cruxOpts, dbgOpts) -> do
    case CCC.inputFiles cruxOpts of
      [] | CDC.interactive dbgOpts -> SB.surveyor Nothing
         | otherwise -> error "Expected input files or interactive mode"
      [inp] | CDC.interactive dbgOpts -> SB.surveyor (Just inp)
            | otherwise -> do
                let ext = SFP.takeExtension inp
                if | ext == ".bc" -> CDL.debugLLVM cruxOpts dbgOpts inp >>= SE.exitWith
                   | otherwise -> error ("Unsupported input file: " ++ inp)
      _ -> error "Multiple input files are not supported"
