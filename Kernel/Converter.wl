BeginPackage["JerryI`Tinyweb`Converter`"]


DecodeMultipart::usage = 
"DecodeMultipart[body]"


EncodeMultipart::usage = 
"EncodeMultipart[expr]"


Begin["`Private`"]


DecodeMultipart[data_ByteArray] := 
data


EncodeMultipart[expr_] := 
BinarySerialize[expr] 


End[]


BeginPackage[]