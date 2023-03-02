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
		"WS" -> WSQ -> WSLength
	|>, 
	"MessageHandler" -> <|
		"WS" -> WSQ -> ws
	|>
]


SocketListen[8080, Function[tcp[#]]]


While[True, Pause[0.001]]
