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
"TCPPacketHandle[server, packet] packet handler"; 


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
	"ClientBuffer" -> <||>, 
	"CompleteHandler" -> <||>, 
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
Module[{packetCompletionState, packetExtended, client, message, result}, 

	Print[DateString[], "\n"]; 

	client = packet["SourceSocket"]; 

	packetCompletionState = checkCompletion[server, packet]; 

	Print["Received: ", packetCompletionState["PacketLength"], " bytes\n"]; 

	If[packetCompletionState["Completed"], 

		message = mergeMessage[server, packet]; 

		Print["Message: \n- - - - - - - - - - - -\n", message, "\n- - - - - - - - - - - -\n"]; 

		clearClientBuffer[server, packet]; 
		result = invokeMessageHandler[server, client, message]; 

		Print["Response: \n- - - - - - - - - - - -\n", result, "\n- - - - - - - - - - - -\n"]; 

		sendResponse[server, client, result], 
	(*Else*)
		packetExtended = Join[packet, packetCompletionState]; 
		saveClientBuffer[server, packetExtended]
	]
]; 


(* ::Section::Closed:: *)
(*Internal functions*)


TCPServer /: checkCompletion[server_TCPServer, packet_Association] := 
Module[{ 
	uuid, clientBuffer, 
	lastItem, packetLength, storedLength, expectedLength, 
	completed, checkResult, packetDataString
}, 
	uuid = packet["SourceSocket"][[1]]; 
	packetDataString = ByteArrayToString[packet["DataByteArray"]]; 
	packetLength = Length[packet["DataByteArray"]]; 

	If[KeyExistsQ[server["ClientBuffer"], uuid] && server["ClientBuffer", uuid]["Length"] > 0, 
		clientBuffer = server["ClientBuffer", uuid]; 
		lastItem = clientBuffer["Part", -1]; 
		expectedLength = lastItem["ExpectedLength"]; 
		storedLength = lastItem["StoredLength"]; , 
	(*Else*)
		expectedLength = conditionApply[server["CompleteHandler"]] @ packetDataString; 
		storedLength = 0; 
	]; 

	completed = storedLength + packetLength >= expectedLength; 

	(*Return*)
	<|
		"Completed" -> completed, 
		"ExpectedLength" -> expectedLength, 
		"StoredLength" -> storedLength, 
		"PacketLength" -> packetLength
	|>
]


TCPServer /: mergeMessage[server_TCPServer, packet_Association] := 
Module[{uuid = packet["SourceSocket"][[1]]}, 
	
	If[KeyExistsQ[server["ClientBuffer"], uuid], 
		ByteArrayToString @ 
		Apply[Join] @ 
		Append[packet["DataByteArray"]] @ 
		server["ClientBuffer", uuid]["Elements"][[All, "DataByteArray"]], 
	(*Else*)
		ByteArrayToString @ 
		packet["DataByteArray"]
	]
]


TCPServer /: invokeMessageHandler[server_TCPServer, client_SocketObject, message_String] := 
Last[#][client, message]& @ SelectFirst[First[#][message]&] @ server["MessageHandler"]


TCPServer /: sendResponse[server_TCPServer, client_SocketObject, result_String] := 
WriteString[client, result]


TCPServer /: saveClientBuffer[server_TCPServer, packetExtended_Association] := 
With[{uuid = packetExtended["SourceSocket"][[1]], 
	packetExtendedUpdated = Append[packetExtended, 
		"StoredLength" -> packetExtended["StoredLength"] + packetExtended["PacketLength"]]
}, 
	If[KeyExistsQ[server["ClientBuffer"], uuid], 
		server["ClientBuffer", uuid]["Append", packetExtendedUpdated], 
		server["ClientBuffer", uuid] = CreateDataStructure["DynamicArray", {packetExtendedUpdated}]
	]; 
]


TCPServer /: clearClientBuffer[server_TCPServer, packet_Association] := 
With[{uuid = packet["SourceSocket"][[1]]}, 
	If[KeyExistsQ[server["ClientBuffer"], uuid], 
		server["ClientBuffer", uuid]["DropAll"]
	]; 
]


(* ::Section::Closed:: *)
(*Extension*)


conditionApply[conditionAndFunctions: _Association | _List] := 
Function[Last[SelectFirst[conditionAndFunctions, Function[cf, First[cf][##]]]][##]]


(* ::Section::Closed:: *)
(*End private context*)


End[]; 


(* ::Section::Closed:: *)
(*End package*)


EndPackage[]; 
