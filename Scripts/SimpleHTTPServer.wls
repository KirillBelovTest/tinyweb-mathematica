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


loadPageQ[parsed_?AssociationQ] := 
parsed["Query", "Method"] == "GET" && 
StringMatchQ[parsed["Query", "Path"], "/" ~~ __ ~~ {".html", ".wsp"}]


loadPage[parsed_?AssociationQ] := 
Import[FileNameJoin[Join[{Directory[]}, FileNameSplit[parsed["Query", "Path"]]]], "String"]


evaluateQ[parsed_?AssociationQ] := 
StringMatchQ[parsed["Query", "Path"], __ ~~ "?code=" ~~ __]


evaluate[parsed_?AssociationQ] := 
ToExpression[URLDecode[First[StringCases[parsed["Query", "Path"], Shortest[__ ~~ "?code="] ~~ code__ :> code]]]]


handler = HTTPHandleMessage[<|
	"LoadPage" -> loadPageQ -> createResponse @* loadPage, 
	"Evaluate" -> evaluateQ -> createResponse @* evaluate, 
	"Error" -> (True&) -> (createResponse["NOT FOUND"]&)
|>]


server = TCPServer[
	"CompleteHandler" -> <|
		"GET" -> HTTPGetQ -> HTTPGetExpectedLength, 
		"POST" -> HTTPPostQ -> HTTPPostExpectedLength
	|>, 
	"MessageHandler" -> <|
		"HTTP" -> HTTPQ -> ((HTTPParse[#2]; handler[#2])&)
	|>
]

SocketListen[8000, server @* TCPPacketHandle]


While[True, Pause[0.01]]