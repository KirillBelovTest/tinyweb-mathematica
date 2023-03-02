#!/usr/bin/env wolframscript

$HisttoryLength = 0
PacletDirectoryLoad[Directory[]]
Get["JerryI`Tinyweb`"]


getPath[request_Association] := 
FileNameJoin[Flatten[{Directory[], request[["HTTPHeader", "URL", "Path"]]}]]


getPath[request_Association, fileName_String] := 
FileNameJoin[Flatten[{Directory[], request[["HTTPHeader", "URL", "Path"]], fileName}]]


(*HTML*)


htmlQ[request_Association] := 
StringMatchQ[getPath[request], __ ~~ ".html", IgnoreCase -> True] &&
FileExistsQ[getPath[request]]


html[request_Association] := 
Import[getPath[request], "String"]


html[request_Association, path_String] := 
Import[path, "String"]


(*WSP*)


wspQ[request_Association] := 
StringMatchQ[getPath[request], __ ~~ ".wsp", IgnoreCase -> True] && 
FileExistsQ[getPath[request]]


wsp[request_Association] := 
ImportWSP[getPath[request]]


wsp[request_Association, path_String] := 
ImportWSP[request, path]


(*Index*)


indexQ[request_Association] := 
FileExistsQ[getPath[request, "index.html"]] || 
FileExistsQ[getPath[request, "index.wsp"]]


index[request_Association] := 
Which[
	FileExistsQ[getPath[request, "index.wsp"]], wsp[request, getPath[request, "index.wsp"]], 
	FileExistsQ[getPath[request, "index.html"]], html[request, getPath[request, "index.html"]]
]

(*HTTP*)


http = HTTPHandler[
	"Pipeline" -> <|
		"Index" -> indexQ -> index, 
		"HTML" -> htmlQ -> html, 
		"WSP" -> wspQ -> wsp, 
		"Image" -> imageQ -> image, 

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
