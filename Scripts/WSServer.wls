#!/usr/bin/env wolframscript

$HisttoryLength = 0
PacletDirectoryLoad[Directory[]]
Get["JerryI`Tinyweb`"]


ws = WSHandler[
	"Pipeline" -> <|
		"Code" -> Function[True] -> Function[Print[ToExpression[#2]]]
	|>, 
	"Deserializer" -> Function[ByteArrayToString[#Data]]
]


tcp = TCPServer[
	"CompleteHandler" -> <|
		"WS" -> WSQ -> WSLength, 
		"Error" > Function[Print["Erro"]; True] -> Function[Length[#2]]
	|>, 
	"MessageHandler" -> <|
		"WS" -> WSQ -> ws, 
		"Error" -> Function[True] -> Function[Close[#2]]
	|>
]


SocketListen[8000, Function[tcp[#]]]


While[True, Pause[0.001]]
