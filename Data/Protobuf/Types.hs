{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Data.Protobuf.Types where

import Data.Monoid
import Data.ByteString
import Data.Protobuf.Input
import Data.Protobuf.Output

class Monoid (Builder m) => Message m where
  data Builder m
  initBuilder :: m -> Builder m
  build :: Builder m -> m
  writeMessage :: m -> Output ()
  readField :: Input (Builder m)

readMessage :: Message m => Input m
readMessage = go mempty where
  go currentBuilder = do
    done <- isEOF
    if done then return (build currentBuilder) else do
      nextField <- readField
      go (currentBuilder `mappend` nextField)

serializeMessage :: Message m => m -> ByteString
serializeMessage m = execOutput (writeMessage m)

deserializeMessage :: Message m => ByteString -> m
deserializeMessage = execInput readMessage