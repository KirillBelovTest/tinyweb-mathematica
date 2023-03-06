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


WSSender::usage = 
"WSSender"


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

	(*Input: <|.., "Data" -> _ByteArray|>*)
	"Deserializer" -> Identity, 

	(*Return: _ByteArray*)
	"Serializer" -> Identity
}]; 


handler_WSHandler[client_SocketObject, message_ByteArray] := 
Module[{deserialize, serialize, frame, data, result, sender}, 
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
			If[result != Null, encodeFrame[serialize[result]], Null], 

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


createAcceptKey[key_String] := 
(*Return: _String*)
BaseEncode[Hash[key <> $guid, "SHA1", "ByteArray"], "Base64"]; 


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


encodeFrame[message_ByteArray] := 
Module[{byte1, fin, opcode, length, mask, lengthBytes}, 
	fin = 1; 
	
	opcode = 2; 
	
	byte1 = Prepend[IntegerDigits[opcode, 2, 7], fin]; 

	length = Length[message]; 

	Which[
		length < 126, 
			lengthBytes = ByteArray[{length}], 
		126 <= length < 2^16, 
			lengthBytes = ByteArray[Join[{126}, IntegerDigits[length, 256, 2]]], 
		2^16 <= length < 2^64, 
			lengthBytes = ByteArray[Join[{127}, IntegerDigits[length, 256, 8]]]
	]; 

	(*Return: _ByteArray*)
	Join[byte1, lengthBytes, message]
]


encodeFrame[message_String] := 
encodeFrame[StringToByteArray[message]]


decodeFrame[message_ByteArray] := 
Module[{header, payload, data},
	header = getFrameHeader[message]; 
	payload = message[[header["PayloadPosition"]]]; 
	data = If[Length[header["MaskingKey"]] == 4, ByteArray[unmask[header["MaskingKey"], payload]], payload]; 

	(*Return: _Association*)
	Append[header, "Data" -> data]
]; 


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
		maskingKey = message[[nextPosition ;; nextPosition + 4]]; nextPosition = nextPosition + 4, 
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


unmask := unmask = FunctionCompile[
	Function[{
		Typed[maskingKey, "NumericArray"::["MachineInteger", 1]], 
		Typed[payload, "NumericArray"::["MachineInteger", 1]]
	}, 
		(*Return: PacketArray::[MachineInteger, 1]*)
		Table[BitXor[payload[[i]], maskingKey[[Mod[i - 1, 4] + 1]]], {i, 1, Length[payload]}]
	]
]


(*::Section::Close::*)
(*End private*)


End[]; 


(*::Section::Close::*)
(*End package*)


EndPackage[]; 
