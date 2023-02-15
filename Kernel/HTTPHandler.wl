(* ::Package:: *)

(* ::Chapter:: *)
(*HTTP Handler*)


(* ::Section::Closed:: *)
(*Begin packge*)


BeginPackage["JerryI`Tinyweb`HTTPHandler`", {"KirillBelov`Objects`", "JerryI`Tinyweb`TCPServer`"}]; 


HTTPGetQ::usage = 
"HTTPGetQ[message] checks that received message is via HTTP GET method"; 


HTTPGetCompletedQ::usage = 
"HTTPGetCompletedQ[message] check that message is completed"


(* ::Section::Closed:: *)
(*Names*)


ClearAll["`*"];


(* ::Section::Closed:: *)
(*Begin private context*)


Begin["`Private`"]; 


(* ::Section::Closed:: *)
(*Check HTTP GET Request*)


HTTPGetQ[message_String] := 
StringContainsQ[message, "GET /"]


HTTPGetCompletedQ[message_String] := 
<|
	"ExpectedLength" -> StringLength[message]
|>


(* ::Section::Closed:: *)
(*End private context*)


End[]; 


(* ::Section::Closed:: *)
(*End packe*)


EndPackage[]; 
