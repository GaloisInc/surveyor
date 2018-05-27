{-# LANGUAGE OverloadedStrings #-}
module Surveyor.Brick.Attributes (
  focusedListAttr,
  statusBarAttr
  ) where

import qualified Brick as B

focusedListAttr :: B.AttrName
focusedListAttr = "focusedListItem"

statusBarAttr :: B.AttrName
statusBarAttr = "statusBar"
