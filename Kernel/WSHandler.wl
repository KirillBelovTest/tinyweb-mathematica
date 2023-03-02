(* ::Package:: *)

(* ::Chapter:: *)
(*WS Handler*)


(*+-------------------------------------------------+
  |                  WS HANDLER                     |
  |                                                 |
  |               (reseive message)                 |
  |                       |                         |
  |              <check message type>               |
  |             /         |          \              |
  |  [handshake]       [frame]        [close frame] |
  |      |                |                 |       |
  | [send accept]     [decode]           {close}    |
  |      |                |                         |
  |   {to tcp}      [deserialize]                   |
  |                       |                         |
  |                <select pipeline>                |
  |      /        /                 \       \       |
  |    ..    [callback]         [subscribe]  ..     |
  |                  \            /                 |
  |                   <check type>                  |
  |             null /            \ data            |
  |            {next}            [serialize]        |
  |                                   |             |
  |                                {to tcp}         |
  +-------------------------------------------------+*)


(*::Section::Close::*)
(*Begin package*)


BeginPackage["JerryI`Tinyweb`WSHandler`", {"JerryI`Tinyweb`Internal`", "KirillBelov`Objects`"}]; 


(*::Section::Close::*)
(*Names*)


ClearAll["`*"]


WSQ::usage = 
"WSHandshakeQ[client, message] check that message sent via WebSocket protocol"


WSLength::usage = 
"WSLength[client, message] get expected message length"


WSHandler::usage = 
"WSHandler[opts] handle messages received via WS protocol"


(*::Section::Close::*)
(*Begin private*)


Begin["`Private`"]; 


WSQ[client_SocketObject, message_ByteArray] := 
Module[{result}, 
	result = frameQ[client, message] || handshakeQ[client, message]; 
	If[result, Print["[PROTOCOL]: WS"]]; 

	(*Return*)
	result
]; 


WSLength[client_SocketObject, message_ByteArray] := 
If[frameQ[client, message], 
	getFrameLength[client, message], 
	Length[message]
]; 


CreateType[WSHandler, {
	"Pipeline" -> <||>, 
	"Deserializer" -> Identity, 
	"Serializer" -> Identity
}]; 


handler_WSHandler[client_SocketObject, message_ByteArray] := 
Module[{deserialize, serialize, frame, data, result}, 
	deserialize = handler["Deserializer"]; 
	serialize = handler["Serializer"]; 
	
	Which[
		(*Return: Null*)
		closeQ[client, message], 
			Close[client]; 
			DeleteCases[$connections, client], 

		(*Return: _ByteArray | Null*)
		frameQ[client, message], 
			frame = decodeFrame[message]; 
			data = deserialize[frame]; 
			result = ConditionApply[handler["Pipeline"]][client, data]; 
			If[result != Null, serialize[result], Null], 

		(*Return: _String*)
		handshakeQ[client, message], 
			handshake[client, message]
	]
]; 


(*::Section::Close::*)
(*Internal*)


$guid = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"; 


$connections = {}; 


handshakeQ[client_SocketObject, message_ByteArray] := 
Module[{messageString}, 

	(*Return: True | False*)
	If[frameQ[client, message], 
		False, 
	(*Else*)
		messageString = ByteArrayToString[message]; 
	
		StringContainsQ[messageString, StartOfString ~~ "GET /"] && 
		StringContainsQ[messageString, StartOfLine ~~ "Upgrade: websocket"] && 
		StringContainsQ[messageString, "\r\n\r\n"]
	]
]; 


frameQ[client_SocketObject, message_ByteArray] := 
(*Return: True | False*)
MemberQ[$connections, client]; 


closeQ[client_SocketObject, message_ByteArray] := 
(*Return: True | False*)
FromDigits[IntegerDigits[message[[1]], 2, 8][[2 ;; ]], 2] == 8


handshake[client_SocketObject, message_ByteArray] := 
Module[{messageString, key, acceptKey}, 
	messageString = ByteArrayToString[message]; 
	$connections = DeleteDuplicates[Append[$connections, client]]; 

	key = StringExtract[messageString, "Sec-WebSocket-Key: " -> 2, "\r\n" -> 1]; 
	acceptKey = createAcceptKey[key]; 

	(*Return: _String*)
	"HTTP/1.1 101 Switching Protocols\r\n" <> 
	"Connection: upgrade\r\n" <> 
	"Upgrade: websocket\r\n" <> 
	"Sec-WebSocket-Accept: " <> acceptKey <> "\r\n\r\n"
]; 


getFrameLength[client_SocketObject, message_ByteArray] := 
Module[{len}, 
	len = FromDigits[IntegerDigits[message[[2]], 2, 8][[2 ;; ]], 2]; 

	Which[
		len == 126, len = FromDigits[Normal[message[[3 ;; 4]]], 256], 
		len == 127, len = FromDigits[Normal[message[[3 ;; 10]]], 256]
	]; 

	(*Return: _Integer*)
	len
]; 


decodeFrame[message_ByteArray] := 
Module[{header, payload, data},
	header = getFrameHeader[message]; 
	payload = Normal[message[[header["PayloadPosition"]]]]; 
	data = ByteArray[unmask[header["MaskingKey"], payload]]; 

	(*Return: _Association*)
	Append[header, "Data" -> data]
]; 


createAcceptKey[key_String] := 
(*Return: _String*)
BaseEncode[Hash[key <> $guid, "SHA1", "ByteArray"], "Base64"]; 


getFrameHeader[message_ByteArray] := 
Module[{byte1, byte2, fin, opcode, mask, len, maskingKey, nextPosition, payload, data}, 
	byte1 = IntegerDigits[message[[1]], 2, 8]; 
	byte2 = IntegerDigits[message[[2]], 2, 8]; 

	fin = byte1[[1]] == 1; 
	opcode = Switch[FromDigits[byte1[[2 ;; ]], 2], 
		1, "Part", 
		2, "Text", 
		4, "Binary", 
		8, "Close"
	]; 

	mask = byte2[[1]] == 1; 

	len = FromDigits[byte2[[2 ;; ]], 2]; 

	nextPosition = 3; 

	Which[
		len == 126, len = FromDigits[Normal[message[[3 ;; 4]]], 256]; nextPosition = 5, 
		len == 127, len = FromDigits[Normal[message[[3 ;; 10]]], 256]; nextPosition = 11
	]; 

	If[mask, 
		maskingKey = Normal[message[[nextPosition ;; nextPosition + 4]]]; nextPosition = nextPosition + 4, 
		maskingKey = {}
	]; 

	(*Return: _Association*)
	<|
		"Fin" -> fin, 
		"OpCode" -> opcode, 
		"Mask" -> mask, 
		"Len" -> len, 
		"MaskingKey" -> maskingKey, 
		"PayloadPosition" -> nextPosition ;; nextPosition + len - 1
	|>
]; 


unmask := unmask = 
Compile[{{maskingKey, _Integer, 1}, {payload, _Integer, 1}}, 
	Module[{result = payload}, 
		If[Length[maskingKey] > 0, 
			Table[
				result[[i]] = BitXor[payload[[i]], maskingKey[[Mod[i - 1, 4] + 1]]], 
				{i, 1, Length[payload]}
			]
		]; 

		(*Return: {__Integer}*)
		result
	]
]


(*::Section::Close::*)
(*End private*)


End[]; 


(*::Section::Close::*)
(*End package*)


EndPackage[]; 
