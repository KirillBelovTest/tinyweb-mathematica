#!/usr/bin/env wolframscript

PacletDirectoryLoad[Directory[]]; 


Get["JerryI`Tinyweb`"]; 


http = HTTPHandler[
	"Pipeline" -> <|
		"HTML" -> HTTPMatchQ[<|"Method" -> "GET", "Path" -> __ ~~ ".html"|>] -> Function[#Path <> " HTML page"], 
		"WSP" -> HTTPMatchQ[<|"Method" -> "GET", "Path" -> __ ~~ ".wsp"|>] -> Function[#Path <> " WSP page"], 

		(*to demonstrate signatures*)
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
