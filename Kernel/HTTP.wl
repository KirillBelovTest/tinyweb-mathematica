(* ::Package:: *)

(* ::Chapter:: *)
(*HTTP Handler*)


(*
	message - ByteArray passed from TCPServer handler
	request - Association parsed from message
	response - Null | String | ByteArray for further sending to TCPServer handler
*)


(*+-------------------------------------------------+
  |                 HTTP HANDLER                    |
  |                                                 |
  |               (reseive request)                 |
  |                       |                         |
  |            [parse request to assoc]             |
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


BeginPackage["JerryI`Tinyweb`HTTP`", {"JerryI`Tinyweb`Internal`", "KirillBelov`Objects`"}]; 


(* ::Section::Closed:: *)
(*Names*)


ClearAll["`*"]


HTTPQ::usage = 
"HTTPQ[client, message] check that message was sent via HTTP protocol"; 


HTTPLength::usage = 
"HTTPLength[client, message] returns expected message length"; 


HTTPHandler::usage = 
"HTTPHandler[opts] mutable type for the handling HTTP request"; 


(* ::Section::Closed:: *)
(*Begin private context*)


Begin["`Private`"]; 


(* ::Section::Closed:: *)
(*HTTPQ*)


HTTPQ[client_SocketObject, message_ByteArray] := 
Module[{head}, 
	head = ByteArrayToString[ByteArraySplit[message, $httpEndOfHead -> 1][[1]]]; 
	
	(*Return: True | False*)
	And[
		StringLength[head] != Length[message], (* equivalent of the StringContainsQ[message, $httpEndOfHead] *)
		StringContainsQ[head, StartOfString ~~ $httpMethods], 
		Or[
			StringContainsQ[head, StartOfLine ~~ "Connection: keep-alive", IgnoreCase -> True], 
			StringContainsQ[head, StartOfLine ~~ "Connection: close", IgnoreCase -> True]
		]
	]
]; 


(* ::Section::Closed:: *)
(*HTTPLength*)


HTTPLength[client_SocketObject, message_ByteArray] := 
Module[{head}, 
	head = ByteArrayToString[ByteArraySplit[message, $httpEndOfHead -> 1][[1]]]; 

	(*Return: _Integer*)
	Which[
		StringContainsQ[head, "Content-Length: ", IgnoreCase -> True], 
			ToExpression[StringExtract[head, {"Content-Length: ", "content-length: "} -> 2, "\r\n" -> 1]], 
		True, 
			Length[message]
	]
]; 


(* ::Section::Closed:: *)
(*HTTPHandler*)


CreateType[HTTPHandler, {
	"Pipeline" -> <||>, 
	"Default" -> Function[$errorResponse], 
	"Deserializers" -> $deserializers, 
	"Serializers" -> $serializers
}]; 


handler_HTTPHandler[client_SocketObject, message_ByteArray] := 
Module[{request, pipeline, result}, 
	request = PrintReturn[parseRequest[message, handler["Deserializers"]], "HTTP REQUEST PARSING", "Parsing `1`", #&]; 
	pipeline = handler["Pipeline"]; 

	(*Result: _String | _Association?responseQ*)
	result = AssocApply[pipeline, handler["Default"]][request]; 

	(*Return: _String | _ByteArray*)
	createResponse[result, handler["Serializers"]]
]


(* ::Section::Closed:: *)
(*Internal*)


$httpMethods = {"GET", "PUT", "DELETE", "HEAD", "POST", "CONNECT", "OPTIONS", "TRACE", "PATCH"}; 


$httpEndOfHead = StringToByteArray["\r\n\r\n"]; 


$errorResponse = <|"Code" -> 404, "Body" -> "Not found"|>; 


$deserializers = <|
	"JSON" -> 
		Function[AssocMatchQ[#1, <|"Content-Type" -> "application/json"|>] -> 
			Function[ImportString[#2, "RawJSON"]]], 
	"QueryEncoded" -> 
		Function[AssocMatchQ[#1, <|"Content-Type" -> "application/x-www-form-urlencoded"|>] -> 
			Function[Association[URLQueryDecode[#2]]]]
|>; 


$serializers = ToString


parseRequest[message_ByteArray, deserializers_Association] := 
Module[{headBytes, head, headline, headers, body, bodyExpr}, 
	{headBytes, body} = ByteArraySplit[message, $httpEndOfHead -> 1]; 
	head = ByteArrayToString[headBytes]; 
	
	headline = First @ StringCases[
		StringExtract[head, "\r\n" -> 1], 
		method__ ~~ " " ~~ url__ ~~ " " ~~ version__ :> Join[
			<|"Method" -> method|>, 
			MapAt[Association, Key["Query"]] @ 
			MapAt[URLBuild, Key["Path"]] @ 
			<|URLParse[url]|>[[{"Path", "Query"}]], 
			<|"Version" -> version|>
		], 
		IgnoreCase -> True
	]; 

	headers = Association[
		Map[Rule[#1, StringRiffle[{##2}, ":"]]& @@ Map[StringTrim]@StringSplit[#, ":"] &]@
  		StringExtract[head, "\r\n\r\n" -> 1, "\r\n" -> 2 ;; ]
	]; 

	bodyExpr = AssocApply[deserializers, #2&][headers, body]; 

	(*Return: _Association*)
	Join[
		headline, 
		<|"Headers" -> headers, "Body" -> bodyExpr|>
	]
]; 


createResponse[body_String] := 
createResponse[<|
	"Code" -> 200, 
	"Body" -> body
|>]


createResponse[result_Association?responseQ, serializers_Association] := 
Module[{assoc = result, bodyString}, 
	bodyString = AssocApply[serializers, ToString][assoc["Body"]]; 
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


responseQ[result_Association] := 
And[
	KeyExistsQ[result, "Code"], 
	KeyExistsQ[result, "Body"]
]


(* ::Section::Closed:: *)
(*End private context*)


End[]; 


(* ::Section::Closed:: *)
(*End packet*)


EndPackage[]; 
