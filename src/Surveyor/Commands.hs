{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
module Surveyor.Commands (
  exitC,
  showSummaryC,
  showDiagnosticsC,
  findBlockC,
  allCommands
  ) where

import qualified Brick.BChan as B
import qualified Data.Functor.Const as C
import qualified Data.Parameterized.List as PL
import           Data.Parameterized.Some ( Some(..) )

import qualified Brick.Command as C
import qualified Surveyor.Minibuffer as MB
import           Surveyor.Events ( Events(..) )

allCommands :: B.BChan (Events s) -> [Some (C.Command MB.Argument MB.TypeRepr)]
allCommands customEventChan =
  [ Some (showSummaryC customEventChan)
  , Some (exitC customEventChan)
  , Some (showDiagnosticsC customEventChan)
  , Some (findBlockC customEventChan)
  ]

exitC :: B.BChan (Events s) -> C.Command MB.Argument MB.TypeRepr '[]
exitC customEventChan =
  C.Command "exit" PL.Nil PL.Nil callback
  where
    callback = \PL.Nil -> B.writeBChan customEventChan Exit

showSummaryC :: B.BChan (Events s) -> C.Command MB.Argument MB.TypeRepr '[]
showSummaryC customEventChan =
  C.Command "summary" PL.Nil PL.Nil callback
  where
    callback = \PL.Nil -> B.writeBChan customEventChan ShowSummary

showDiagnosticsC :: B.BChan (Events s) -> C.Command MB.Argument MB.TypeRepr '[]
showDiagnosticsC customEventChan =
  C.Command "log" PL.Nil PL.Nil callback
  where
    callback = \PL.Nil -> B.writeBChan customEventChan ShowDiagnostics

findBlockC :: B.BChan (Events s) -> C.Command MB.Argument MB.TypeRepr '[MB.AddressType]
findBlockC customEventChan =
  C.Command "find-block" names rep callback
  where
    names = C.Const "address" PL.:< PL.Nil
    rep = MB.AddressTypeRepr PL.:< PL.Nil
    callback = \(MB.AddressArgument addr PL.:< PL.Nil) ->
      B.writeBChan customEventChan (FindBlockContaining addr)
