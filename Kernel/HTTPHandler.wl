(* ::Package:: *)

(* ::Chapter:: *)
(*HTTP Handler*)


(*+-------------------------------------------------+
  |                 HTTP HANDLER                    |
  |                                                 |
  |               (reseive message)                 |
  |                       |                         |
  |            [parse message to assoc]             |
  |                       |                         |
  |               <select pipeline>                 |
  |      /       /        |        \         \      |
  |     ..   [get..]  [post..]  [delete..]   ..     |
  |              \        |        /                |
  |           [create string response]              |
  |                       |                         |
  |                {return to tcp}                  |  
  +-------------------------------------------------+*)


(* ::Section::Closed:: *)
(*Begin packge*)


BeginPackage["JerryI`Tinyweb`HTTPHandler`", {"JerryI`Tinyweb`Internal`", "KirillBelov`Objects`"}]; 


(* ::Section::Closed:: *)
(*Names*)


ClearAll["`*"]


HTTPQ::usage = 
"HTTPQ[client, message] check that message was sent via HTTP protocol"; 


HTTPLength::usage = 
"HTTPLength[client, message] returns expected message length"; 


HTTPHandler::usage = 
"HTTPHandler[opts] mutable type for the handling HTTP messages"; 


(* ::Section::Closed:: *)
(*Begin private context*)


Begin["`Private`"]; 


(* ::Section::Closed:: *)
(*HTTPQ*)


HTTPQ[client_SocketObject, message_ByteArray] := 
Module[{messageHeader, result}, 
	messageString = ByteArrayToString[message]; 
	messageStringHeader = StringExtract[messageString, "\r\n\r\n" -> 1]; 
	
	result = And[
		StringContainsQ[messageString, "\r\n\r\n"], 
		StringContainsQ[messageStringHeader, StartOfString ~~ $httpMethods], 
		Or[
			StringContainsQ[messageStringHeader, StartOfLine ~~ "Connection: keep-alive", IgnoreCase -> True], 
			StringContainsQ[messageStringHeader, StartOfLine ~~ "Connection: close", IgnoreCase -> True]
		]
	]; 

	If[result, Print["[PROTOCOL]: HTTP"]]; 

	(*Return: True | False*)
	result
]; 


(* ::Section::Closed:: *)
(*HTTPLength*)


HTTPLength[client_SocketObject, message_ByteArray] := 
Module[{messageString}, 
	messageString = ByteArrayToString[message]; 

	(*Return: _Integer*)
	Which[
		StringContainsQ[messageString, "Content-Length: ", IgnoreCase -> True], 
			ToExpression[StringExtract[messageString, {"Content-Length: ", "content-length: "} -> 2, "\r\n" -> 1]], 
		True, 
			Length[message]
	]
]; 


(* ::Section::Closed:: *)
(*HTTPHandler*)


CreateType[HTTPHandler, {"Pipeline" -> <||>}]; 


handler_HTTPHandler[client_SocketObject, message_ByteArray] := 
Module[{messageAssoc, pipeline, result}, 
	messageAssoc = parseRequest[message]; 
	pipeline = handler["Pipeline"]; 

	(*Result: _String | _Association?responseQ*)
	result = ConditionApply[pipeline][messageAssoc]; 

	(*Return: _String*)
	createResponse[result]
]


(* ::Section::Closed:: *)
(*Internal*)


$httpMethods = {"GET", "PUT", "DELETE", "HEAD", "POST", "CONNECT", "OPTIONS", "TRACE", "PATCH"}; 


parseRequest[message_ByteArray] := 
Module[{messageString, httpHeader, headers, body}, 
	messageString = ByteArrayToString[message]; 
	
	httpHeader = First @ StringCases[
		StringExtract[messageString, "\r\n" -> 1], 
		method__ ~~ " " ~~ url__ ~~ " " ~~ version__ :> <|"Method" -> method, "URL" -> <|URLParse[url]|>, "Version" -> version|>, 
		IgnoreCase -> True
	]; 

	headers = Association[
		Map[Rule[#1, StringRiffle[{##2}, ":"]]& @@ Map[StringTrim]@StringSplit[#, ":"] &]@
  		StringExtract[messageString, "\r\n\r\n" -> 1, "\r\n" -> 2 ;; ]
	]; 

	body = StringRiffle[StringExtract[messageString, "\r\n\r\n" -> 2 ;; ], "\r\n\r\n"]; 

	(*Return: _Association?requestQ*)
	<|
		"HTTPHeader" -> httpHeader, 
		"Headers" -> headers, 
		"Body" -> body
	|>
]; 


createResponse[body_String] := 
createResponse[<|
	"Code" -> 200, 
	"Body" -> body
|>]


createResponse[response_Association?responseQ] := 
Module[{assoc = response}, 
	If[Not[KeyExistsQ[assoc, "Message"]], assoc["Message"] = "OK"]; 
	If[Not[KeyExistsQ[assoc, "Headers"]], assoc["Headers"] = <|
		"Content-Length" -> StringLength[assoc["Body"]]
	|>]; 

	(*Return: _String*)
	StringTemplate["HTTP/1.1 `Code` `Message`\r\n"][assoc] <> 
	StringRiffle[KeyValueMap[StringRiffle[{#1, #2}, ": "]&] @ assoc["Headers"], "\r\n"] <> 
	"\r\n\r\n" <> 
	assoc["Body"]
]


responseQ[assoc_Association] := 
And[
	KeyExistsQ[assoc, "Code"], 
	KeyExistsQ[assoc, "Body"]
]


(* ::Section::Closed:: *)
(*End private context*)


End[]; 


(* ::Section::Closed:: *)
(*End packet*)


EndPackage[]; 
