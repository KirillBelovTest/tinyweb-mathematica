(* ::Package:: *)

(* ::Chapter:: *)
(*TCP Server*)


(* ::Section::Closed:: *)
(*Begin package*)


BeginPackage["JerryI`Tinyweb`TCPServer`", {"KirillBelov`Objects`"}]; 


(* ::Section::Closed:: *)
(*Names*)


ClearAll["`*"]; 


TCPPacketBufferingServer::usage = 
"TCPPacketBufferingServer[opts] packet buffer"; 


TCPPacketHandler::usage = 
"TCPPacketHandler[buffer] handler received TCP packet"; 


(* ::Section::Closed:: *)
(*Private context*)


Begin["`Private`"]; 


(* ::Section::Closed:: *)
(*Server*)


(*
	Buffering Server for TCP packets that come into an open connection. 
	The buffer checks to see if the packet is complete. 
	It is assumed that all packets from one client go in order and 
	packets from different requests are not mixed up.
*)


CreateType[TCPPacketBufferingServer, Object, Identity, {
	"ClientStore" -> <||>, 
	"CompleteChecker" -> <||>, 
	"MessageHandler" -> <||>
}]; 


(* ::Section::Closed:: *)
(*Handler*)


(*
	Parameterized TCP packet handler. 
	Needed to store data if the request does not fit into one TCP packet. 
	By default it does nothing. 
	All packet buffering logic must be implemented in a specific handler.
*)


TCPPacketBufferingServer /: TCPPacketHandler[buffer_TCPPacketBufferingServer, packet_?AssociationQ] := 
Module[{packetState, message, result}, 

	packetState = checkPacket[buffer, packet]; 

	If[packetState["Completed"], 
		message = mergeMessage[buffer, packet]; 
		clearClientBuffer[buffer, packet]; 
		result = invokeMessageHandler[buffer, message]; 
		sendResponse[buffer, packet, result], 
	(*Else*)
		saveClientBuffer[buffer, Join[packet, packetState]]
	]
]; 


TCPPacketBufferingServer /: TCPPacketHandler[buffer_TCPPacketBufferingServer] := 
Function[packet, TCPPacketHandler[buffer, packet]]


(* ::Section::Closed:: *)
(*Internal functions*)


TCPPacketBufferingServer /: checkPacket[buffer_TCPPacketBufferingServer, packet_Association] := 
Module[{
	packetData, 
	uuid, clientStore, 
	lastItem, packetLength, storedLength, expectedLength, 
	completed, completeChecker, checkResult
}, 
	uuid = packet["SourceSocket"][[1]]; 
	packetData = packet["DataByteArray"]; 
	packetLength = Length[packetData]; 

	If[KeyExistsQ[buffer["ClientStore"], uuid] && buffer["ClientStore", uuid]["Length"] > 0, 
		clientStore = buffer["ClientStore", client]; 
		lastItem = clientStore["Part", -1]; 
		expectedLength = lastItem["ExpectedLength"]; 
		storedLength = lastItem["StoredLength"]; , 
	(*Else*)
		completeChecker = SelectFirst[#[[1]][ByteArrayToString[packetData]]&] @ buffer["CompleteChecker"]; 
		checkResult = completeChecker[[2]][ByteArrayToString[packetData]]; 
		expectedLength = checkResult["ExpectedLength"]; 
		storedLength = 0; 
	]; 

	completed = storedLength + packetLength >= expectedLength; 

	(*Return*)
	<|
		"Completed" -> completed, 
		"ExpectedLength" -> expectedLength, 
		"StoredLength" -> storedLength
	|>
]


TCPPacketBufferingServer /: mergeMessage[buffer_TCPPacketBufferingServer, packet_Association] := 
Module[{uuid = packet["SourceSocket"][[1]]}, 
	ByteArrayToString @ 
	If[KeyExistsQ[buffer["ClientStore"], uuid], 
		Apply[Join] @ 
		Append[packet["DataByteArray"]] @ 
		buffer["ClientStore", uuid]["Elements"], 
	(*Else*)
		packet["DataByteArray"]
	]
]


TCPPacketBufferingServer /: invokeMessageHandler[buffer_TCPPacketBufferingServer, message_String] := 
#[[2]][message]& @ SelectFirst[#[[1]][message]&] @ buffer["MessageHandler"]


TCPPacketBufferingServer /: sendResponse[buffer_TCPPacketBufferingServer, packet_Association, result_String] := 
Module[{client = packet["SourceSocket"]}, 
	WriteString[client, result]
]


TCPPacketBufferingServer /: saveClientBuffer[buffer_TCPPacketBufferingServer, packetExtended_Association] := 
With[{uuid = packetExtended["SourceSocket"][[1]]}, 
	If[KeyExistsQ[buffer["ClientStore"], uuid], 
		buffer["ClientStore", uuid]["Append", packetExtended], 
		buffer["ClientStore", uuid] = CreateDataStructure["DynamicArray", {packetExtended}]
	]; 
]


TCPPacketBufferingServer /: clearClientBuffer[buffer_TCPPacketBufferingServer, packet_Association] := 
With[{uuid = packet["SourceSocket"][[1]]}, 
	If[KeyExistsQ[buffer["ClientStore"], uuid], 
		buffer["ClientStore", uuid]["DropAll"]
	]; 
]


(* ::Section::Closed:: *)
(*End private context*)


End[]; 


(* ::Section::Closed:: *)
(*End package*)


EndPackage[]; 
