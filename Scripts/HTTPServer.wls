#!/usr/bin/env wolframscript

PacletDirectoryLoad[Directory[]]; 


Get["JerryI`Tinyweb`"]; 


evaluateRequestCode[request_Association] := 
serializeCodeResponse[ToExpression[request["Query", "code"]]]


serializeCodeResponse[image_Graphics] := 
ExportString[image, "PNG"]


serializeCodeResponse[expr_] := 
ExportString[expr, "String"]


pathExistsQ[path_String] := 
FileExistsQ[FileNameJoin[{Directory[], FileNameSplit[path]}]] && 
Not[DirectoryQ[FileNameJoin[{Directory[], FileNameSplit[path]}]]]


importFile[request_Association] := 
ImportString[FileNameJoin[{Directory[], FileNameSplit[request["Path"]]}], "String"]


http = HTTPHandler[
	"Pipeline" -> <|
		"Code" -> AssocMatchQ[<|"Method" -> "GET", "Query" -> KeyExistsQ["code"]|>] -> evaluateRequestCode, 
		"File" -> AssocMatchQ[<|"Method" -> "GET", "Path" -> pathExistsQ|>] -> importFile, 
		"ListPlot" -> AssocMatchQ[<|"Method" -> "POST", "Query" -> <|"code" -> "ListPlot"|>|>] -> Function[ExportString[ListPlot[ImportByteArray[#Body, "Table"]], "PNG"]], 

		(*Pipeline with signature example*)
		"NotFound" -> Function[assoc, True] -> Function[assoc, <|"Code" -> 404, "Body" -> "Not found"|>]
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


PrintReturn[SocketListen[8080, Function[tcp[#]]], "STARTING TCP SERVER", "Server listening on port `1`", 8080&]


While[True, Pause[0.001]]
