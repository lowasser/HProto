{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
{-# OPTIONS -funbox-strict-fields #-}
module Data.Protobuf.Input (Input, readPrim, readBytes) where

import Control.Monad
import Data.Word
import Data.ByteString
import Data.ByteString.Unsafe
import Data.ByteString.Internal
import Foreign.Storable
import Prelude hiding (length, splitAt)

data InputState a = Failure String | Success a !ByteString

newtype Input a = Input {runInput :: ByteString -> InputState a}

instance Monad Input where
  return x = Input $ Success x
  m >>= k = Input $ \ src -> case runInput m src of
    Failure msg -> Failure msg
    Success a src' -> runInput (k a) src'

instance MonadPlus Input where
  mzero = Input $ \ _ -> Failure "no valid alternatives"
  m1 `mplus` m2 = Input $ \ src -> case runInput m1 src of
    result@Success{} -> result
    Failure{} -> runInput m2 src

{-# SPECIALIZE readPrim :: Input Word8, Input Word64 #-}
readPrim :: forall a . Storable a => Input a
readPrim = Input $ \ src -> if length src < sz then Failure "not enough bytes" else 
  inlinePerformIO $ unsafeUseAsCString src $ \ ptr -> do
    !result <- peekByteOff ptr 0
    return (Success result (unsafeDrop sz src))
  where !sz = sizeOf (undefined :: a)

readBytes :: Int -> Input ByteString
readBytes !n = Input $ \ src ->
  if length src < n then Failure "not enough bytes" else case splitAt n src of
    (res, src') -> Success res src'