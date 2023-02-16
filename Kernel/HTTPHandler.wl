(* ::Package:: *)

(* ::Chapter:: *)
(*HTTP Handler*)


(* ::Section::Closed:: *)
(*Begin packge*)


BeginPackage["JerryI`Tinyweb`HTTPHandler`", {"KirillBelov`Objects`"}]; 


(* ::Section::Closed:: *)
(*Names*)


ClearAll["`*"];


HTTPQ::usage = 
"HTTPQ[packetDataString] POST or GET"


HTTPGetQ::usage = 
"HTTPGetQ[packetDataString] checks that received message is via HTTP GET method"; 


HTTPGetExpectedLength::usage = 
"HTTPGetExpectedLength[packetDataString] full message length"


HTTPPostQ::usage = 
"HTTPPostQ[packetDataString] checks that received message is via HTTP POST method"; 


HTTPPostExpectedLength::usage = 
"HTTPPostExpectedLength[packetDataString] full message length"


HTTPParse::usage = 
"HTTPParse[message] parse message to assoc"


HTTPHandleMessage::usage = 
"HTTPHandleMessage[assoc][message]"


(* ::Section::Closed:: *)
(*Begin private context*)


Begin["`Private`"]; 


(* ::Section::Closed:: *)
(*Check HTTP GET Request*)


HTTPGetQ[message_String] := 
StringContainsQ[message, "GET "]


HTTPGetExpectedLength[message_String] := 
StringLength[message]


(* ::Section::Closed:: *)
(*Check HTTP POST Request*)


HTTPPostQ[message_String] := 
StringContainsQ[message, "POST "]


HTTPPostExpectedLength[message_String] := 
4 + StringLength[StringExtract[message, "\r\n\r\n" -> 1]] + 
ToExpression[StringExtract[message, "content-length: " -> 2, "\r\n"  -> 1]]


(* ::Section::Closed:: *)
(*Parametrized Handler*)


HTTPHandleMessage[assoc_?AssociationQ][message_String] := 
conditionApply[assoc] @ HTTPParse[message]


(* ::Section::Closed:: *)
(*HTTPQ*)


HTTPQ[message_String] := 
HTTPGetQ[message] || HTTPPostQ[message]


(* ::Section::Closed:: *)
(*Parser*)


HTTPParse[message_String] := 
Module[{query, headers, body}, 
	query = StringCases[
		StringExtract[message, "\r\n" -> 1], 
		method__ ~~ " " ~~ path__ ~~ " " ~~ version__ :> <|"Method" -> method, "Path" -> path, "Version" -> version|>, 
		IgnoreCase -> True
	][[1]]; 

	headers = Association[
		Map[Rule[#1, StringJoin[##2]]& @@ Map[StringTrim]@StringSplit[#, ":"] &]@
  		StringExtract[message, "\r\n\r\n" -> 1, "\r\n" -> 2 ;; ]
	]; 

	body = StringJoin[StringExtract[message, "\r\n\r\n" -> 2 ;; ]]; 

	(*Return*)
	parsed =  <|
		"Query" -> query, 
		"Headers" -> headers, 
		"Body" -> body
	|>; 

	Print["Parsed request: \n", ToString[parsed], "\n"]; 

	parsed
]


(* ::Section::Closed::*)
(*Internal*)


conditionApply[conditionAndFunctions: _Association | _List] := 
Function[Last[SelectFirst[conditionAndFunctions, Function[cf, First[cf][##]]]][##]]


(* ::Section::Closed:: *)
(*End private context*)


End[]; 


(* ::Section::Closed:: *)
(*End packe*)


EndPackage[]; 
