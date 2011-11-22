{-# LANGUAGE TemplateHaskell, QuasiQuotes, TypeFamilies, FlexibleInstances #-}
module Data.Protobuf.Test where

import Language.Haskell.TH
import Data.Protobuf.Staged
import Data.Protobuf.Base

$(genDecs $ ProtoBaseMessage {
  baseProtoName = "SerialTeacher",
  baseProtoFields = [
    BaseReqField {baseFieldName = "teacher_id", baseFieldIndex = 0, baseFieldType = SerialInt32},
    BaseOptField {baseFieldName = "name", baseFieldIndex = 1, baseFieldType = SerialString, baseFieldDefault = Nothing},
    BaseRptField {baseFieldName = "available_period", baseFieldIndex = 2, baseFieldType = SerialUInt32}]})

$(genDecs $ ProtoBaseMessage {
  baseProtoName = "SerialSection",
  baseProtoFields = [
    BaseReqField {baseFieldName = "section_id", baseFieldIndex = 0, baseFieldType = SerialInt32},
    BaseReqField {baseFieldName = "course_id", baseFieldIndex = 1, baseFieldType = SerialInt32},
    BaseRptField {baseFieldName = "teacher", baseFieldIndex = 2, baseFieldType = SerialMessage (ConT ''SerialTeacher)},
    BaseOptField {baseFieldName = "course_title", baseFieldIndex = 3, baseFieldType = SerialString, baseFieldDefault = Nothing}]})