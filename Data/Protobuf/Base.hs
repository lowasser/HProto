{-# LANGUAGE QuasiQuotes, TemplateHaskell, RecordWildCards, BangPatterns #-}
{-# OPTIONS -funbox-strict-fields #-}
module Data.Protobuf.Base (SerialType(..), ProtoBaseMessage(..), haskType, serialTypeToWireType,
  ProtoBaseField(..), toWireExpr, fromWireExpr, defaultForTy) where

import Language.Haskell.TH
import Data.Bits
import Data.Int
import Data.Word
import Data.ByteString
import Data.ByteString.Internal
import Data.Protobuf.Wire
import Data.Protobuf.Types
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc

data SerialType =
    SerialInt32
  | SerialUInt32
  | SerialSInt32
  | SerialInt64
  | SerialSInt64
  | SerialUInt64
  | SerialBool
  | SerialFixed64
  | SerialSFixed64
  | SerialDouble
  | SerialString
  | SerialBytes
  | SerialFixed32
  | SerialSFixed32
  | SerialFloat
  | SerialMessage Type

serialTypeToWireType :: SerialType -> WireType
serialTypeToWireType ty = case ty of
  SerialInt32 -> VarInt
  SerialUInt32 -> VarInt
  SerialSInt32 -> VarInt
  SerialInt64 -> VarInt
  SerialSInt64 -> VarInt
  SerialUInt64 -> VarInt
  SerialBool -> VarInt
  SerialFixed32 -> Fixed32
  SerialSFixed32 -> Fixed32
  SerialFloat -> Fixed32
  SerialBytes -> LengthDelimited
  SerialString -> LengthDelimited
  SerialFixed64 -> Fixed64
  SerialSFixed64 -> Fixed64
  SerialDouble -> Fixed64
  SerialMessage _ -> LengthDelimited

defaultForTy :: SerialType -> Q Exp
defaultForTy ty = case ty of
  SerialInt32 -> [| 0 :: Int32 |]
  SerialUInt32 -> [| 0 :: Word32 |]
  SerialSInt32 -> [| 0 :: Int32 |]
  SerialFixed32 -> [| 0 :: Word32 |]
  SerialSFixed32 -> [| 0 :: Int32 |]
  SerialBool -> [| False :: Bool |]
  SerialString -> [| empty :: ByteString |]
  SerialBytes -> [| empty :: ByteString |]
  SerialDouble -> [| 0.0 :: Double |]
  SerialFloat -> [| 0.0 :: Float |]
  SerialInt64 -> [| 0 :: Int64 |]
  SerialUInt64 -> [| 0 :: Word64 |]
  SerialSInt64 -> [| 0 :: Int64 |]
  SerialFixed64 -> [| 0 :: Word64 |]
  SerialSFixed64 -> [| 0 :: Int64 |]

data ProtoBaseMessage = ProtoBaseMessage {
    baseProtoName :: String,
    baseProtoFields :: [ProtoBaseField]}

data ProtoBaseField = 
    BaseReqField {
      baseFieldName :: String,
      baseFieldIndex :: !Word,
      baseFieldType :: SerialType}
  | BaseOptField {
      baseFieldName :: String,
      baseFieldIndex :: !Word,
      baseFieldType :: SerialType,
      baseFieldDefault :: Maybe Exp}
  | BaseRptField {
      baseFieldName :: String,
      baseFieldIndex :: !Word,
      baseFieldType :: SerialType}

haskType :: SerialType -> Q Type
haskType ty = case ty of
  SerialInt32 -> [t| Int32 |]
  SerialUInt32 -> [t| Word32 |]
  SerialSInt32 -> [t| Int32 |]
  SerialBool -> [t| Bool |]
  SerialDouble -> [t| Double |]
  SerialFloat -> [t| Float |]
  SerialString -> [t| ByteString |]
  SerialBytes -> [t| ByteString |]
  SerialFixed32 -> [t| Word32 |]
  SerialSFixed32 -> [t| Int32 |]
  SerialFixed64 -> [t| Word64 |]
  SerialSFixed64 -> [t| Int64 |]
  SerialMessage ty -> return ty

toZigZag32 :: Int32 -> Word64
toZigZag32 !x = fromIntegral $ (x `shiftL` 1) `xor` (x `shiftR` 31)

toZigZag64 :: Int64 -> Word64
toZigZag64 !x = fromIntegral $ (x `shiftL` 1) `xor` (x `shiftR` 63)

fromZigZag32 :: Word64 -> Int32
fromZigZag32 !w64 = fromIntegral $ (w64 `shiftR` 1) `xor` (-(w64 .&. 1))

fromZigZag64 :: Word64 -> Int64
fromZigZag64 !w64 = fromIntegral $ (w64 `shiftR` 1) `xor` (-(w64 .&. 1))

toWireExpr :: SerialType -> Q Exp -- returned exp has type Word -> haskType ty -> SerialWire
toWireExpr SerialInt32 = [| \ ix i32 -> WireVarInt ix ((fromIntegral :: Int32 -> Word64) i32) |]
toWireExpr SerialUInt32 = [| \ ix w32 -> WireVarInt ix ((fromIntegral :: Word32 -> Word64) w32) |]
toWireExpr SerialSInt32 = [| \ ix si32 -> WireVarInt ix (toZigZag32 si32) |]
toWireExpr SerialFixed32 = [| WireFixed32 |]
toWireExpr SerialSFixed32 = [| \ ix sf32 -> WireFixed32 ix ((fromIntegral :: Int32 -> Word32) sf32) |]
toWireExpr SerialInt64 = [| \ ix i64 -> WireVarInt ix ((fromIntegral :: Int64 -> Word64) i64) |]
toWireExpr SerialUInt64 = [| WireVarInt |]
toWireExpr SerialSInt64 = [| \ ix si64 -> WireVarInt ix (toZigZag64 si64) |]
toWireExpr SerialFixed64 = [| WireFixed64 |]
toWireExpr SerialSFixed64 = [| \ ix sf64 -> WireFixed64 ix ((fromIntegral :: Int64 -> Word64) sf64) |]
toWireExpr SerialBool = [| \ ix b -> WireVarInt ix (if b then 1 else 0) |]
toWireExpr SerialBytes = [| WireLengthDelimited |]
toWireExpr SerialString = [| WireLengthDelimited |]
toWireExpr SerialFloat = [| \ ix f -> WireFixed32 ix ((storableCast :: Float -> Word32) f) |]
toWireExpr SerialDouble = [| \ ix d -> WireFixed64 ix ((storableCast :: Double -> Word64) d) |]
toWireExpr (SerialMessage ty) = [| \ ix message -> WireLengthDelimited ix (serializeMessage message) |]

fromWireExpr :: SerialType -> Q Exp -- returned exp has type SerialWire -> (haskType ty)
fromWireExpr SerialInt32 = [| fromIntegral :: Word64 -> Int32 |]
fromWireExpr SerialUInt32 = [| fromIntegral :: Word64 -> Word32 |]
fromWireExpr SerialSInt32 = [| fromZigZag32 |]
fromWireExpr SerialFixed32 = [| id |]
fromWireExpr SerialSFixed32 = [| fromIntegral :: Word32 -> Int32 |]
fromWireExpr SerialBool = [| (\ x -> if x == 0 then False else True) |]
fromWireExpr SerialInt64 = [| (fromIntegral :: Word64 -> Int64) |]
fromWireExpr SerialUInt64 = [| id |]
fromWireExpr SerialSInt64 = [| fromZigZag64 |]
fromWireExpr SerialFixed64 = [| id |]
fromWireExpr SerialSFixed64 = [| (fromIntegral :: Word64 -> Int64) |]
fromWireExpr SerialBytes = [| id |]
fromWireExpr SerialString = [| id |]
fromWireExpr SerialFloat = [| (storableCast :: Word32 -> Float) |]
fromWireExpr SerialDouble = [| (storableCast :: Word64 -> Double) |]
fromWireExpr (SerialMessage ty) = [| deserializeMessage |]

{-# SPECIALIZE storableCast :: Float -> Word32, Word32 -> Float, Double -> Word64, Word64 -> Double #-}
storableCast :: (Storable a, Storable b) => a -> b
storableCast !x = inlinePerformIO $ alloca $ \ p -> do
  poke p x
  peek (castPtr p)