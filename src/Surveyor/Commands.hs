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

import qualified Surveyor.Minibuffer as MB
import           Surveyor.Events ( Events(..) )

allCommands :: B.BChan (Events s) -> [Some (MB.Command MB.Argument MB.TypeRepr)]
allCommands customEventChan =
  [ Some (showSummaryC customEventChan)
  , Some (exitC customEventChan)
  , Some (showDiagnosticsC customEventChan)
  , Some (findBlockC customEventChan)
  ]

exitC :: B.BChan (Events s) -> MB.Command MB.Argument MB.TypeRepr '[]
exitC customEventChan =
  MB.Command "exit" PL.Nil PL.Nil callback
  where
    callback = \PL.Nil -> B.writeBChan customEventChan Exit

showSummaryC :: B.BChan (Events s) -> MB.Command MB.Argument MB.TypeRepr '[]
showSummaryC customEventChan =
  MB.Command "summary" PL.Nil PL.Nil callback
  where
    callback = \PL.Nil -> B.writeBChan customEventChan ShowSummary

showDiagnosticsC :: B.BChan (Events s) -> MB.Command MB.Argument MB.TypeRepr '[]
showDiagnosticsC customEventChan =
  MB.Command "log" PL.Nil PL.Nil callback
  where
    callback = \PL.Nil -> B.writeBChan customEventChan ShowDiagnostics

findBlockC :: B.BChan (Events s) -> MB.Command MB.Argument MB.TypeRepr '[MB.AddressType]
findBlockC customEventChan =
  MB.Command "find-block" names rep callback
  where
    names = C.Const "address" PL.:< PL.Nil
    rep = MB.AddressTypeRepr PL.:< PL.Nil
    callback = \(MB.AddressArgument addr PL.:< PL.Nil) ->
      B.writeBChan customEventChan (FindBlockContaining addr)
