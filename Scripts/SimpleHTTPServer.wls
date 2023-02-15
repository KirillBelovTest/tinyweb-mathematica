#!/usr/bin/env wolframscript

$HisttoryLength = 0
PacletDirectoryLoad[Directory[]]
Get["JerryI`Tinyweb`"]

okResponseTemplate = 
StringTemplate["HTTP/1.1 200 OK\r\nDate: <* Now *>\r\nContent-Length: `1`\r\nContent-Type: text/html\nConnection: keep-alive\r\n\r\n`2`"];

createResponse[expr_] := okResponseTemplate[
	StringLength[ExportString[expr, "Text"]], 
	ExportString[expr, "Text"]
];

server = TCPServer[
	"CompleteChecker" -> <|
		"GET" -> HTTPGetQ -> HTTPGetDataLength, 
		"ERR" -> (True &) -> (<|"ExpectedLength" -> 0|> &)
	|>, 
	"MessageHandler" -> <|
		"GET" -> HTTPGetQ -> (createResponse["Hello!"] &), 
    	"ERR" -> (True &) -> ("ERROR" &)
	|>
]

SocketListen[8000, server @* TCPPacketHandle]


While[True, Pause[0.1]]