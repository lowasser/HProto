{-# LANGUAGE NamedFieldPuns, BangPatterns, RecordWildCards, TemplateHaskell #-}
{-# OPTIONS -funbox-strict-fields #-}
module Data.Protobuf.Wire (WireType(..), SerialWire(..), writeSerialWire, readSerialWire, getSerialCon) where

import Data.Bits
import Data.Word
import Data.Protobuf.Input
import Data.Protobuf.Output
import Language.Haskell.TH
import qualified Data.ByteString as BS

data WireType =
    VarInt
  | Fixed64
  | LengthDelimited
  | Fixed32

-- | Context-agnostic serialized field, independent of any message specification.
data SerialWire =
    WireVarInt {fieldIndex :: !Word, wireVarInt :: !Word64}
  | WireFixed64 {fieldIndex :: !Word, wireFixed64 :: !Word64}
  | WireLengthDelimited {fieldIndex :: !Word, wireLengthDelimited :: !BS.ByteString}
  | WireFixed32 {fieldIndex :: !Word, wireFixed32 :: !Word32}

getSerialCon :: WireType -> Name
getSerialCon VarInt = 'WireVarInt
getSerialCon Fixed64 = 'WireFixed64
getSerialCon LengthDelimited = 'WireLengthDelimited
getSerialCon Fixed32 = 'WireFixed32

{-# SPECIALIZE writeVarInt :: Word -> Output (), Word8 -> Output (), Word64 -> Output () #-}
writeVarInt :: (Integral a, Bits a) => a -> Output ()
writeVarInt !x
  | x' == 0	= writePrim outBits
  | otherwise	= writePrim (outBits .|. 0x80) >> writeVarInt x'
  where outBits = fromIntegral x .&. (0x7f :: Word8)
	x' = x `shiftR` 7

getWireType :: SerialWire -> WireType
getWireType WireVarInt{} = VarInt
getWireType WireFixed64{} = Fixed64
getWireType WireLengthDelimited{} = LengthDelimited
getWireType WireFixed32{} = Fixed32

wireTypeBits :: WireType -> Word
wireTypeBits VarInt = 0
wireTypeBits Fixed64 = 1
wireTypeBits LengthDelimited = 2
wireTypeBits Fixed32 = 5

writeSerialWire :: SerialWire -> Output ()
writeSerialWire wire =
  case wire of
    WireVarInt{wireVarInt} -> writeKey >> writeVarInt wireVarInt
    WireFixed64{wireFixed64} -> writeKey >> writePrim wireFixed64
    WireFixed32{wireFixed32} -> writeKey >> writePrim wireFixed32
    WireLengthDelimited{wireLengthDelimited} -> do
      writeKey
      writeVarInt ((fromIntegral :: Int -> Word) $ BS.length wireLengthDelimited)
      writeBytes wireLengthDelimited
  where {-# INLINE writeKey #-}
	writeKey = writeVarInt ((fieldIndex wire `shiftL` 3) .|. wireTypeBits (getWireType wire))

{-# SPECIALIZE INLINE readVarInt :: (Word -> Input a) -> Input a, (Word64 -> Input a) -> Input a #-}
readVarInt :: (Integral a, Bits a) => (a -> Input b) -> Input b
readVarInt cont = go 0 0 where
  go !accum !shift = do
    !w8 <- readPrim :: Input Word8
    let !accum' = accum .|. (fromIntegral (w8 .&. 0x7f) `shiftL` shift)
    if testBit w8 7 then go accum' (shift + 7) else cont accum'

readSerialWire :: Input SerialWire
readSerialWire = readVarInt $ \ wireKey -> do
  let theWireType = wireKey .&. 0x7
      !fieldIndex = wireKey `shiftR` 3
  case theWireType of
    0 -> readVarInt $ \ wireVarInt -> return WireVarInt{..}
    1 -> do
      wireFixed64 <- readPrim
      return WireFixed64{..}
    2 -> readVarInt $ \ len -> do
      wireLengthDelimited <- readBytes ((fromIntegral :: Word -> Int) len)
      return WireLengthDelimited{..}
    5 -> do
      wireFixed32 <- readPrim
      return WireFixed32{..}
    _ -> fail "Unknown wire type"