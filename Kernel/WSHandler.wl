BeginPackage["JerryI`Tinyweb`WSHandler`", {"JerryI`Tinyweb`Internal`", "KirillBelov`Objects`"}]; 


WSQ::usage = 
"WSQ[client, message] check that message sent via WebSocket protocol"


WSHandshakeQ::usage = 
"WSHandshakeQ[client, message] check that message received via WS protocol"


WSHandshake::usage = 
"WSHandshake[client, message] send handshake to the client"


WSFrameLength::usage = 
"WSFrameLength[client, message]"


WSFrameQ::usage = 
"WSFrameQ[client, message]"


WSLength::usage = 
"WSLength[client, message] get expected message length"


WSHandler::usage = 
"WSHandler[opts] handle messages received via WS protocol"


WSDecodeFrame::usage = 
"WSDecodeFrame[message]"


Begin["`Private`"]; 


WSQ[client_SocketObject, message_ByteArray] := 
Module[{result}, 
	result = MemberQ[$connections, client] || WSHandshakeQ[client, message]; 

	If[result, Print["[PROTOCOL]: WS"]]; 

	(*Return*)
	result
]; 


WSLength[client_SocketObject, message_ByteArray] := 
If[MemberQ[$connections, client], 
	WSFrameLength[client, message], 
	Length[message]
]; 


WSFrameLength[client_SocketObject, message_ByteArray] := 
Length[message]; 


WSHandshakeQ[client_SocketObject, message_ByteArray] := 
Module[{messageString}, 
	If[WSFrameQ[client, message], 
		False, 
	(*Else*)
		messageString = ByteArrayToString[message]; 
	
		StringContainsQ[messageString, StartOfString ~~ "GET /"] && 
		StringContainsQ[messageString, StartOfLine ~~ "Upgrade: websocket"] && 
		StringContainsQ[messageString, "\r\n\r\n"]
	]
]; 


WSFrameQ[client_SocketObject, message_ByteArray] := 
MemberQ[$connections, client]; 


WSHandshake[client_SocketObject, message_ByteArray] := 
Module[{messageString, key, acceptKey}, 
	messageString = ByteArrayToString[message]; 

	$connections = DeleteDuplicates[Append[$connections, client]]; 

	key = StringExtract[messageString, "Sec-WebSocket-Key: " -> 2, "\r\n" -> 1]; 
	acceptKey = createAcceptKey[key]; 

	(*Return*)
	"HTTP/1.1 101 Switching Protocols\r\n" <> 
	"Connection: upgrade\r\n" <> 
	"Upgrade: websocket\r\n" <> 
	"Sec-WebSocket-Accept: " <> acceptKey <> "\r\n\r\n"
]; 


WSDecodeFrame[client_SocketObject, message_ByteArray] := 
Module[{header, payload, data},
	header = getFrameHeader[message]; 
	payload = Normal[message[[header["PayloadPosition"]]]]; 
	data = ByteArray[unmask[header["MaskingKey"], payload]]; 

	(*Return*)
	Append[header, "Data" -> data]
]


CreateType[WSHandler, {
	"Handler" -> <||>
}]


handler_WSHandler[client_SocketObject, message_ByteArray] := 
Module[{}, 
	
	ConditionApply[handler["Handler"]][client, message]
]; 



(*::Section::Close::*)
(*Internal*)


$guid = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"; 


$connections = {}; 


createAcceptKey[key_String] := 
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

	If[mask == 1, 
		maskingKey = Normal[message[[nextPosition ;; nextPosition + 4]]]; nextPosition = nextPosition + 4, 
		maskingKey = {}
	]; 

	<|
		"Fin" -> fin, 
		"OpCode" -> opcode, 
		"Mask" -> mask, 
		"Len" -> len, 
		"MaskingKey" -> maskingKey, 
		"PayloadPosition" -> nextPosition ;; nextPosition + len - 1
	|>
]; 


unmask := 
unmask = Compile[{{maskingKey, _Integer, 1}, {payload, _Integer, 1}}, 
	Module[{result = payload}, 
		If[Length[maskingKey] > 0, 
			Table[
				result[[i]] = BitXor[payload[[i]], maskingKey[[Mod[i - 1, 4] + 1]]], 
				{i, 1, Length[payload]}
			]
		]; 

		(*Return*)
		result
	]
]


(*::Section::Close::*)
(*End private*)


End[]; 


(*::Section::Close::*)
(*End package*)


EndPackage[]; 
