(* ::Package:: *)

(* ::Chapter:: *)
(*TCP Server*)


(* ::Section::Closed:: *)
(*Begin package*)


BeginPackage["JerryI`Tinyweb`TCPServer`", {"KirillBelov`Objects`"}]; 


(* ::Section::Closed:: *)
(*Names*)


ClearAll["`*"]; 


TCPServer::usage = 
"TCPServer[opts] TCP server"; 


TCPPacketHandle::usage = 
"TCPPacketHandle[server, packet] TCP server"; 


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


CreateType[TCPServer, Object, Identity, {
	"ClientStore" -> <||>, 
	"CompleteChecker" -> <||>, 
	"MessageHandler" -> <||>
}]; 


(* ::Section::Closed:: *)
(*Handler*)


(*
	Server definition overload when processing TCP packets. 
	Packets are processed as follows: 
	1. As soon as a packet is received, its status is first checked. 
	2. If the packet is complete, processing begins.
	3. Functions that check the message type and 
	   perform processing and forming the response are stored as server properties
	4. If the packet is not completed - it is stored in the buffer along with 
	   extended information about its expected length and current state
*)


TCPServer /: TCPPacketHandle[server_TCPServer, packet_Association] := 
Module[{packetState, client, message, result}, 

	client = packet["SourceSocket"]; 
	packetState = checkPacket[server, packet]; 

	If[packetState["Completed"], 
		message = mergeMessage[server, packet]; 
		clearClientBuffer[server, packet]; 
		result = invokeMessageHandler[server, client, message]; 
		sendResponse[server, client, result], 
	(*Else*)
		saveClientBuffer[server, Join[packet, packetState]]
	]
]; 


(* ::Section::Closed:: *)
(*Internal functions*)


TCPServer /: checkPacket[buffer_TCPServer, packet_Association] := 
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

	(*Log*)
	Print[DateString[]]; 
	Print[StringTemplate["Received packet state: \n\tCompleted = `1`; \n\tPacketLength = `2`; \n\tStoredLength = `3`; \n\tExpectedLength = `4`"][
		completed, 
		packetLength, 
		storedLength, 
		expectedLength
	]]; 
	Print[]; 

	(*Return*)
	<|
		"Completed" -> completed, 
		"ExpectedLength" -> expectedLength, 
		"StoredLength" -> storedLength, 
		"PacketLength" -> packetLength
	|>
]


TCPServer /: mergeMessage[buffer_TCPServer, packet_Association] := 
Module[{uuid = packet["SourceSocket"][[1]]}, 
	
	(*Log*)
	Print[DateString[]]; 
	(Print[StringTemplate["Request: \n`1`"][#]]; Print[]; #)& @  

	ByteArrayToString @ 
	If[KeyExistsQ[buffer["ClientStore"], uuid], 
		Apply[Join] @ 
		Append[packet["DataByteArray"]] @ 
		buffer["ClientStore", uuid]["Elements"], 
	(*Else*)
		packet["DataByteArray"]
	]
]


TCPServer /: invokeMessageHandler[buffer_TCPServer, client_SocketObject, message_String] := 
#[[2]][client, message]& @ SelectFirst[#[[1]][message]&] @ buffer["MessageHandler"]


TCPServer /: sendResponse[buffer_TCPServer, client_SocketObject, result_String] := (
	Print[DateString[]]; 
	Print[StringTemplate["Response: \n`1`"][result]]; 
	Print[]; 
	WriteString[client, result]
)


TCPServer /: saveClientBuffer[buffer_TCPServer, packetExtended_Association] := 
With[{uuid = packetExtended["SourceSocket"][[1]]}, 
	If[KeyExistsQ[buffer["ClientStore"], uuid], 
		buffer["ClientStore", uuid]["Append", packetExtended], 
		buffer["ClientStore", uuid] = CreateDataStructure["DynamicArray", {packetExtended}]
	]; 
]


TCPServer /: clearClientBuffer[buffer_TCPServer, packet_Association] := 
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
