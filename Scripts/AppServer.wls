#!/usr/bin/env wolframscript

PacletDirectoryLoad[Directory[]]; 


Get["JerryI`Tinyweb`"]; 


importUrlPath[path_String] := 
Import[FileNameJoin[Flatten[{Directory[], FileNameSplit[path]}]], "String"]


http = HTTPHandler[
	"Pipeline" -> <|
		"HTML" -> AssociationMatchQ[<|"Method" -> "GET", "Path" -> __ ~~ ".html"|>] -> Function[importUrlPath[#Path]], 
		"WSP" -> AssociationMatchQ[<|"Method" -> "GET", "Path" -> __ ~~ ".wsp"|>] -> Function[importUrlPath[#Path]], 
		"IMG" -> AssociationMatchQ[<|"Method" -> "GET", "Path" -> __ ~~ {".png", ".jpg"}|>] -> Function[importUrlPath[#Path]], 

		(*signature*)
		"Not found" -> Function[assoc, True] -> Function[assoc, <|"Code" -> 404, "Body" -> "Not found"|>]
	|>
]


tcp = TCPServer[
	"CompleteHandler" -> <|
		"HTTP" -> HTTPQ -> HTTPLength
	|>, 
	"MessageHandler" -> <|
		"HTTP" -> HTTPQ -> http
	|>
]


SocketListen[8080, Function[tcp[#]]]


While[True, Pause[0.001]]
