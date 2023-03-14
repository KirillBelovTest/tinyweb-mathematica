(* ::Package:: *)

(* ::Chapter:: *)
(*TCP Server*)


(*+-------------------------------------------------+
  |                HANDLE PACKET                    |
  |                                                 |
  |              (receive packet)                   |
  |                      |                          |
  |            [get extended packet]                |
  |                      |                          |
  |                <is complete>                    |
  |           yes /             \ no                |
  |    [get message]      [save packet to buffer]   |
  |          |                   /                  |
  |   [invoke handler]          /                   |
  |          |                 /                    |
  |   [send response]         /                     |
  |          |               /                      |
  |    [clear buffer]       /                       |
  |                 \      /                        |
  |                  {next}                         |
  +-------------------------------------------------+*)


(* ::Section::Closed:: *)
(*Begin package*)


BeginPackage["JerryI`Tinyweb`TCP`", {"JerryI`Tinyweb`Internal`", "KirillBelov`Objects`"}]; 


(* ::Section::Closed:: *)
(*Names*)


TCPServer::usage = 
"TCPServer[opts] TCP server"; 


(* ::Section::Closed:: *)
(*Private context*)


Begin["`Private`"]; 


(* ::Section::Closed:: *)
(*Server*)


CreateType[TCPServer, {
	"Buffer" -> <||>, 
	"CompleteHandler" -> <||>, 
	"MessageHandler" -> <||>
}]; 


server_TCPServer[packet_Association] := 
Module[{client, extendedPacket, message, result}, 
	client = packet["SourceSocket"]; 
	extendedPacket = getExtendedPacket[server, client, packet]; 

	If[extendedPacket["Completed"], 
		message = getMessage[server, client, extendedPacket]; 
		result = invokeHandler[server, client, message]; 
		sendResponse[server, client, result]; 
		clearBuffler[server, client], 
	(*Else*)
		savePacketToBuffer[server, client, extendedPacket]
	]; 
]; 


(* ::Section::Closed:: *)
(*Internal*)


TCPServer /: getExtendedPacket[server_TCPServer, client: SocketObject[uuid_String], packet_Association] := 
Module[{data, dataLength, buffer, last, expectedLength, storedLength, completed}, 
	
	data = packet["DataByteArray"]; 
	dataLength = Length[data]; 

	If[KeyExistsQ[server["Buffer"], uuid] && server["Buffer", uuid]["Length"] > 0, 
		buffer = server["Buffer", uuid]; 
		last = buffer["Part", -1]; 
		expectedLength = last["ExpectedLength"]; 
		storedLength = last["StoredLength"]; , 
	(*Else*)
		expectedLength = ConditionApply[server["CompleteHandler"]][client, data]; 
		storedLength = 0; 
	]; 

	completed = storedLength + dataLength >= expectedLength; 

	(*Return: _Association*)
	Join[packet, <|
		"Completed" -> completed, 
		"ExpectedLength" -> expectedLength, 
		"StoredLength" -> storedLength + dataLength, 
		"DataLength" -> dataLength
	|>]
]; 


TCPServer /: getMessage[server_TCPServer, client: SocketObject[uuid_String], extendedPacket_Association] := 
If[KeyExistsQ[server["Buffer"], uuid] && server["Buffer", uuid]["Length"] > 0, 
	Apply[Join] @ 
	Append[extendedPacket["DataByteArray"]] @ 
	server["Buffer", uuid]["Elements"][[All, "DataByteArray"]], 
(*Else*)
	extendedPacket["DataByteArray"]
]; 


TCPServer /: invokeHandler[server_TCPServer, client_SocketObject, message_ByteArray] := 
ConditionApply[server["MessageHandler"]][client, message]


TCPServer /: sendResponse[server_TCPServer, client_SocketObject, result: _String | _ByteArray | Null] := 
Module[{t = AbsoluteTime[]}, 
	Switch[result, 
		_String, WriteString[client, result], 
		_ByteArray, BinaryWrite[client, result], 
		Null, Null
	]
]; 


TCPServer /: savePacketToBuffer[server_TCPServer, SocketObject[uuid_String], extendedPacket_Association] := 
If[KeyExistsQ[server["Buffer"], uuid], 
	server["Buffer", uuid]["Append", extendedPacket], 
	server["Buffer", uuid] = CreateDataStructure["DynamicArray", {extendedPacket}]
]; 


TCPServer /: clearBuffer[server_TCPServer, SocketObject[uuid_String]] := 
If[KeyExistsQ[server["Buffer"], uuid], 
	server["Buffer", uuid]["DropAll"]
]; 


(* ::Section::Closed:: *)
(*End private context*)


End[]; 


(* ::Section::Closed:: *)
(*End package*)


EndPackage[]; 
