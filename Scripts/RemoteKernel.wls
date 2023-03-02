#!/usr/bin/env wolframscript

$HisttoryLength = 0
PacletDirectoryLoad[Directory[]]
Get["JerryI`Tinyweb`"]


tcp = TCPServer[
	"CompleteHandler" -> <|
		"LTP" -> LTPQ -> LTPLength
	|>, 
	"MessageHandler" -> <|
		"LTP" -> LTPQ -> LTPHandler
	|>
]


SocketListen[8080, Function[tcp[#]]]


While[True, Pause[0.001]]
