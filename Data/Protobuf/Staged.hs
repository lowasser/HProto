{-# LANGUAGE QuasiQuotes, TemplateHaskell, TupleSections, RecordWildCards #-}
module Data.Protobuf.Staged (genDecs) where

import Control.Monad

import Debug.Trace
import Data.Monoid
import Data.Maybe
import Data.Word
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Fold
import Data.Protobuf.Base
import Data.Protobuf.Types
import Data.Protobuf.Wire
import Language.Haskell.TH
import qualified Data.Vector as Vec
import qualified Data.Vector.Unboxed as UVec
import qualified Data.Vector.Generic as GVec

data ProtoMessage = ProtoMessage {
      protoTyName :: Name,
      protoConName :: Name,
      builderConName :: Name,
      protoClassName :: Name,
      protoFields :: [ProtoField]}

data ProtoField =
    ReqField {
      protoFieldName :: Name,
      builderFieldName :: Name,
      protoFieldIndex :: Word,
      hasFieldFun :: Name,
      getFieldFun :: Name,
      fieldValueTy :: SerialType,
      defaultValue :: Exp}
  | OptField {
      protoFieldName :: Name,
      builderFieldName :: Name,
      protoFieldIndex :: Word,
      hasFieldFun :: Name,
      getFieldFun :: Name,
      fieldValueTy :: SerialType,
      defaultValue :: Exp}
  | RptField {
      protoFieldName :: Name,
      protoFieldIndex :: Word,
      builderFieldName :: Name,
      getCountFun :: Name,
      getAtFun :: Name,
      getVectorFun :: Name,
      fieldValueTy :: SerialType,
      fullVectorTy :: Type}

stageMessage :: ProtoBaseMessage -> Q ProtoMessage
stageMessage ProtoBaseMessage{..} = do
  let protoTyName = mkName baseProtoName
  protoConName <- newName (baseProtoName ++ "Con")
  builderConName <- newName (baseProtoName ++ "BuilderCon")
  let protoClassName = mkName (baseProtoName ++ "OrBuilder")
  protoFields <- mapM stageField baseProtoFields
  return ProtoMessage{..}

stageField :: ProtoBaseField -> Q ProtoField
stageField field = do
  protoFieldName <- newName $ "proto_" ++ baseFieldName field
  builderFieldName <- newName $ "builder_" ++ baseFieldName field
  let protoFieldIndex = baseFieldIndex field
      fieldValueTy = baseFieldType field
  case field of
    BaseReqField{..} -> do
      let hasFieldFun = mkName $ "has_" ++ baseFieldName
	  getFieldFun = mkName $ "get_" ++ baseFieldName
      defaultValue <- defaultForTy fieldValueTy
      return ReqField{..}
    BaseOptField{..} -> do
      typeDefault <- defaultForTy fieldValueTy
      let hasFieldFun = mkName $ "has_" ++ baseFieldName
	  getFieldFun = mkName $ "get_" ++ baseFieldName
	  defaultValue = fromMaybe typeDefault baseFieldDefault
      return OptField{..}
    BaseRptField{..} -> do
      haskTy <- haskType fieldValueTy
      isUnbox <- isClassInstance ''UVec.Unbox [haskTy]
      fullVectorTy <- appT (if isUnbox then [t| UVec.Vector |] else [t| Vec.Vector |]) (return haskTy)
      let getVectorFun = mkName $ "get_" ++ baseFieldName ++ "_vector"
	  getAtFun = mkName $ "get_" ++ baseFieldName
	  getCountFun = mkName $ "get_" ++ baseFieldName ++ "_count"
      return RptField{..}
      

genBuilderTypeDec :: ProtoMessage -> Q Dec
genBuilderTypeDec ProtoMessage{..} = 
  dataInstD (return []) ''Builder
    [conT protoTyName]
    [recC builderConName 
      [liftM (builderFieldName field, IsStrict, ) $ case field of
	ReqField{..} -> [t| Last $(haskType fieldValueTy) |]
	OptField{..} -> [t| Last $(haskType fieldValueTy) |]
	RptField{..} -> [t| Seq $(haskType fieldValueTy) |]
	  | field <- protoFields ]]
    []

genProtoTypeDec :: ProtoMessage -> Q Dec
genProtoTypeDec ProtoMessage{..} =
  dataD (return []) protoTyName []
    [recC protoConName
      [liftM (protoFieldName field, IsStrict, ) $ case field of
	  ReqField{..} -> haskType fieldValueTy
	  OptField{..} -> [t| Maybe $(haskType fieldValueTy) |]
	  RptField{..} -> return fullVectorTy
	| field <- protoFields]]
    []

infixr 6 #=>

(#=>) :: TypeQ -> TypeQ -> TypeQ
argQ #=> resQ = do
  arg <- argQ
  res <- resQ
  return (ArrowT `AppT` arg `AppT` res)

genProtoClassDec :: ProtoMessage -> Q Dec
genProtoClassDec ProtoMessage{..} = do
  tyv <- newName "proto_or_builder"
  liftM (ClassD [] protoClassName [PlainTV tyv] []) $ sequence $ concat 
    [case field of
      ReqField{..} -> 
	[sigD hasFieldFun (varT tyv #=> [t| Bool |]),
	  sigD getFieldFun (varT tyv #=> haskType fieldValueTy)]
      OptField{..} ->
	[sigD hasFieldFun (varT tyv #=> [t| Bool |]),
	  sigD getFieldFun (varT tyv #=> haskType fieldValueTy)]
      RptField{..} ->
	[sigD getCountFun (varT tyv #=> [t| Int |]),
	  sigD getAtFun (varT tyv #=> [t| Int |] #=> haskType fieldValueTy),
	  sigD getVectorFun (varT tyv #=> return fullVectorTy),
	  valD (varP getCountFun) (normalB [| GVec.length . $(varE getVectorFun) |]) [],
	  valD (varP getAtFun) (normalB [| (GVec.!) . $(varE getVectorFun) |]) []]
      | field <- protoFields]

genMonoidDec :: ProtoMessage -> Q Dec
genMonoidDec ProtoMessage{..} = 
  instanceD (return []) (conT ''Monoid `appT` (conT ''Builder `appT` conT protoTyName))
    [valD
      (varP 'mempty)
      (normalB $ recConE builderConName $
	[liftM (builderFieldName field, ) [| mempty |] | field <- protoFields]) [],
     do	builder1 <- newName "builder"
	builder2 <- newName "builder"
	funD 'mappend
	  [clause [varP builder1, varP builder2]
	    (normalB $ recConE builderConName $
	      [liftM (fn, ) 
		[| $(varE $ fn) $(varE builder1) `mappend` $(varE fn) $(varE builder2) |]
		| field <- protoFields, let fn = builderFieldName field])
	    []]]

writeFieldValueExpr :: ProtoField -> Q Exp -- has type fieldTy -> Output ()
writeFieldValueExpr field =
  [| writeSerialWire . $(toWireExpr (fieldValueTy field)) $(litE $ integerL $ fromIntegral $ protoFieldIndex field) |]

genMessageWriteExpr :: ProtoMessage -> [Q Exp]
genMessageWriteExpr ProtoMessage{..} = [
  case field of
    ReqField{..} -> [| $(writeFieldValueExpr field) . $(varE protoFieldName) |]
    OptField{..} -> [| maybe (return ()) $(writeFieldValueExpr field) . $(varE protoFieldName) |]
    RptField{..} -> [| GVec.mapM_ $(writeFieldValueExpr field) . $(varE protoFieldName) |]
  | field <- protoFields]

genMessageWriteDec :: ProtoMessage -> Q Dec
genMessageWriteDec msg = do
  proto <- newName "proto"
  funD 'writeMessage
    [clause [bangP $ varP proto]
      (normalB $ doE 
	[noBindS (appE writeField (varE proto)) | writeField <- genMessageWriteExpr msg])
      []]

toWireCon :: ProtoField -> Name
toWireCon = getSerialCon . serialTypeToWireType . fieldValueTy

withSingleValue :: ProtoField -> Q Exp -- result has type fieldValueTy -> Builder message
withSingleValue ReqField{..} = 
  [| \ value -> $(recUpdE [| mempty |] [liftM (builderFieldName,) [| Last (Just value) |]]) |]
withSingleValue OptField{..} =
  [| \ value -> $(recUpdE [| mempty |] [liftM (builderFieldName,) [| Last (Just value) |]]) |]
withSingleValue RptField{..} =
  [| \ value -> $(recUpdE [| mempty |] [liftM (builderFieldName,) [| Seq.singleton value |]]) |]

genMessageReadMatch :: ProtoField -> Q Match -- values are of the form 
genMessageReadMatch field = do
  valueName <- newName "value"
  match (conP (toWireCon field) 
      [litP (integerL (fromIntegral (protoFieldIndex field))), varP valueName])
    (normalB [| $(withSingleValue field) ($(fromWireExpr (fieldValueTy field)) $(varE valueName)) |])
    []

genMessageReadDec :: ProtoMessage -> Q Dec
genMessageReadDec ProtoMessage{..} = do
  someField <- newName "someField"
  valD (varP 'readField)
    (normalB $ doE 
      [bindS (bangP $ varP someField) [| readSerialWire |],
	noBindS [| return $(caseE (varE someField) $
	  [genMessageReadMatch field | field <- protoFields] ++ [defaultMatch]) |]])
    []
  where defaultMatch = match wildP (normalB [| trace "Unknown field; skipping." mempty |]) []

builderValueToProtoValue :: ProtoField -> Q Exp
builderValueToProtoValue ReqField{..} =
  [| fromMaybe (error ("Error: no value provided for field " ++ $(litE $ stringL $ nameBase builderFieldName))) . getLast |]
builderValueToProtoValue OptField{} = [| getLast |]
builderValueToProtoValue RptField{..} = [| \ xs -> GVec.fromListN (Seq.length xs) (Fold.toList xs) |]

genBuildDec :: ProtoMessage -> Q Dec
genBuildDec ProtoMessage{..} = do
  builder <- newName "builder"
  funD 'build
    [clause [bangP $ varP builder]
      (normalB $ recConE protoConName
	[liftM (protoFieldName field,) [| $(builderValueToProtoValue field) ($(varE $ builderFieldName field) $(varE builder)) |]
	  | field <- protoFields])
      []]

protoValueToBuilderValue :: ProtoField -> Q Exp
protoValueToBuilderValue ReqField{..} = [| Last . Just |]
protoValueToBuilderValue OptField{..} = [| Last |]
protoValueToBuilderValue RptField{..} = [| Seq.fromList . GVec.toList |]

genInitBuilderDec :: ProtoMessage -> Q Dec
genInitBuilderDec ProtoMessage{..} = do
  proto <- newName "proto"
  funD 'initBuilder
    [clause [bangP $ varP proto]
      (normalB $ recConE builderConName
	[liftM (builderFieldName field,) [| $(protoValueToBuilderValue field) ($(varE $ protoFieldName field) $(varE proto)) |]
	  | field <- protoFields])
      []]

genMessageInstance :: ProtoMessage -> Q Dec
genMessageInstance msg@ProtoMessage{..} =
  instanceD (return []) (conT ''Message `appT` conT protoTyName)
    [genBuilderTypeDec msg,
     genMessageWriteDec msg,
     genMessageReadDec msg,
     genBuildDec msg,
     genInitBuilderDec msg]

genBuilderClassInstanceDec :: ProtoMessage -> Q Dec
genBuilderClassInstanceDec ProtoMessage{..} =
  instanceD (return []) (conT protoClassName `appT` (conT ''Builder `appT` conT protoTyName)) . concat =<< sequence
    [do	value <- newName "value"
	case field of
	  ReqField{..} ->
	    return [funD getFieldFun [clause [recP builderConName [return (builderFieldName, VarP value)]]
		(normalB [| fromMaybe $(return defaultValue) (getLast $(varE value)) |]) []],
	      funD hasFieldFun [clause [recP builderConName [return (builderFieldName, VarP value)]]
		(normalB [| isJust (getLast $(varE value)) |]) []]]
	  OptField{..} -> 
	    return [funD getFieldFun [clause [recP builderConName [return (builderFieldName, VarP value)]]
		(normalB [| fromMaybe $(return defaultValue) (getLast $(varE value)) |]) []],
	      funD hasFieldFun [clause [recP builderConName [return (builderFieldName, VarP value)]]
		(normalB [| isJust (getLast $(varE value)) |]) []]]
	  RptField{..} ->
	    return [funD getVectorFun [clause [recP builderConName [return (builderFieldName, VarP value)]]
		(normalB [| GVec.fromListN (Seq.length $(varE value)) (Fold.toList $(varE value)) |]) []]]
      | field <- protoFields]

genProtoClassInstanceDec :: ProtoMessage -> Q Dec
genProtoClassInstanceDec ProtoMessage{..} =
  instanceD (return []) (conT protoClassName `appT` conT protoTyName) . concat =<< sequence
    [do	value <- newName "value"
	case field of
	  ReqField{..} -> return
	    [valD (varP getFieldFun) (normalB $ varE protoFieldName) [],
	     valD (varP hasFieldFun) (normalB [| const True |]) []]
	  OptField{..} -> return
	    [funD getFieldFun [clause [recP protoConName [return (protoFieldName, VarP value)]]
	      (normalB [| fromMaybe $(return defaultValue) $(varE value) |]) []],
	     funD hasFieldFun [clause [recP protoConName [return (protoFieldName, VarP value)]]
	      (normalB [| isJust $(varE value) |]) []]]
	  RptField{..} -> return
	    [valD (varP getVectorFun) (normalB $ varE protoFieldName) []]
      | field <- protoFields]

genDecs :: ProtoBaseMessage -> Q [Dec]
genDecs baseMsg = do
  msg <- stageMessage baseMsg
  sequence
    [genProtoClassDec msg,
      genProtoTypeDec msg,
      genMessageInstance msg,
      genMonoidDec msg,
      genBuilderClassInstanceDec msg,
      genProtoClassInstanceDec msg]