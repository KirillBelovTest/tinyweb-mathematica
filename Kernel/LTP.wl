BeginPackage["JerryI`Tinyweb`LTP`"]


LTPQ::usage = 
"LTPQ[client, message]"


LTPLength::usage = 
"LTPLength[client, message]"


LTPHandler::usage = 
"LTPHandler[client, message]"


Begin["`Private`"]


LTPQ[client_SocketObject, message_ByteArray] := 
ByteArrayToString[message[[1 ;; 3]]] == "LTP"


LTPLength[client_SocketObject, message_ByteArray] :=
FromDigits[Normal[message[[4 ;; 7]]], 256] + 7


LTPHandler[client_SocketObject, message_ByteArray] := 
Module[{data, header}, 
	data = BinarySerialize[ReleaseHold[BinaryDeserialize[message[[8 ;; ]]]]]; 
	Join[StringToByteArray["LTP"], ByteArray[IntegerDigits[Length[data], 256, 4]], data]
]


End[]


EndPackage[]
