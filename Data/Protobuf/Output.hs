{-# LANGUAGE BangPatterns, RecordWildCards #-}
{-# OPTIONS -funbox-strict-fields #-}
module Data.Protobuf.Output (Output, writePrim, writeBytes) where

import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Data.ByteString
import Data.ByteString.Internal
import Data.ByteString.Unsafe
import Data.Word
import Foreign.Ptr
import Foreign.ForeignPtr.Safe
import Foreign.Storable
import Foreign.Marshal.Array

import Prelude hiding (length)

{-# INLINE chunkSize #-}
chunkSize :: Int
chunkSize = 64 * 1024

data OutputBuffer = OutBuf {
  currentChunk :: !(ForeignPtr Word8),
  currentChunkIndex :: !Int,
  doneChunks :: Seq ByteString}

data OutputResult a = OutResult !OutputBuffer a

newtype Output a = Output {runOutput :: OutputBuffer -> IO (OutputResult a)}

instance Monad Output where
  return a = Output $ \ buf -> return $ OutResult buf a
  m >>= k = Output $ \ buf -> do
    OutResult buf' a <- runOutput m buf
    runOutput (k a) buf'

{-# SPECIALIZE writePrim :: Word8 -> Output () #-}
{-# SPECIALIZE writePrim :: Word64 -> Output () #-}
writePrim :: Storable a => a -> Output ()
writePrim !x = Output $ \ buf@OutBuf{..} -> 
  if currentChunkIndex + sz <= chunkSize
    then withForeignPtr currentChunk $ \ ptr -> do
      pokeByteOff ptr currentChunkIndex x
      return (OutResult buf{currentChunkIndex = currentChunkIndex + sz} ())
    else do
      let doneChunk = fromForeignPtr currentChunk 0 currentChunkIndex
      newChunk <- mallocForeignPtrBytes chunkSize
      withForeignPtr newChunk $ \ ptr -> do
	pokeByteOff ptr 0 x
	return (OutResult OutBuf{currentChunk = newChunk, currentChunkIndex = sz, doneChunks = doneChunks |> doneChunk} ())
  where !sz = sizeOf x

writeBytes :: ByteString -> Output ()
writeBytes !bytes = Output $ \ buf@OutBuf{..} -> unsafeUseAsCStringLen bytes $ \ (srcPtr, len) ->
  if currentChunkIndex + len <= chunkSize
    then withForeignPtr currentChunk $ \ dstPtr -> do
      copyArray dstPtr (castPtr srcPtr) len
      return $ OutResult buf{currentChunkIndex = currentChunkIndex + len} ()
    else withForeignPtr currentChunk $ \ dstPtr -> do
      let !space = chunkSize - currentChunkIndex - len
      copyArray dstPtr (castPtr srcPtr) space
      let doneChunk = fromForeignPtr currentChunk 0 chunkSize
      newChunk <- mallocForeignPtrBytes chunkSize
      runOutput (writeBytes $ unsafeDrop space bytes) OutBuf{currentChunk = newChunk, currentChunkIndex = 0,
	doneChunks = doneChunks |> doneChunk}