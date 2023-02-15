(* ::Package:: *)

(* ::Chapter:: *)
(*HTTP Handler*)


(* ::Section::Closed:: *)
(*Begin packge*)


BeginPackage["JerryI`Tinyweb`HTTPHandler`", {"KirillBelov`Objects`", "JerryI`Tinyweb`TCPServer`"}]; 


(* ::Section::Closed:: *)
(*Names*)


ClearAll["`*"];


HTTPGetQ::usage = 
"HTTPGetQ[packetDataString] checks that received message is via HTTP GET method"; 


HTTPGetDataLength::usage = 
"HTTPGetDataLength[packetDataString] full message length"


(* ::Section::Closed:: *)
(*Begin private context*)


Begin["`Private`"]; 


(* ::Section::Closed:: *)
(*Check HTTP GET Request*)


HTTPGetQ[message_String] := 
StringContainsQ[message, "GET /"]


HTTPGetDataLength[message_String] := 
<|
	"ExpectedLength" -> StringLength[message]
|>


(* ::Section::Closed:: *)
(*End private context*)


End[]; 


(* ::Section::Closed:: *)
(*End packe*)


EndPackage[]; 
