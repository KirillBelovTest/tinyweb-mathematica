(*:Package:*)

BeginPackage["JerryI`Tinyweb`Internal`"];


ClearAll["`*"];


ConditionApply::usage = 
"ConditionApply[assoc][args] select rule if key[args] is True and resturn value[args]";


Cache::usage = 
"Cache[expr] cache expr for one minute
Cache[expr, period] cache expr for specific period";


ByteArraySplit::usage = 
"ByteArraySplit[data, sep -> n] works like TakeDrop[StringSplit[text, sep], n]"


Begin["`Private`"];


ConditionApply[conditionAndFunctions: _Association | _List] := 
Function[Last[SelectFirst[conditionAndFunctions, Function[cf, First[cf][##]]]][##]];


SetAttributes[Cache, HoldFirst];


Cache[expr_, period_Integer: 60] := 
Module[{roundNow = Floor[AbsoluteTime[], period]}, 
	If[IntegerQ[Cache[expr, "Date"]] && Cache[expr, "Date"] == roundNow, 
		Cache[expr, "Value"], 
	(*Else*)
		Cache[expr, "Date"] = roundNow; 
		Cache[expr, "Value"] = expr
	]
]; 


ByteArrayPosition[byteArray_ByteArray, subByteArray_ByteArray, n: _Integer?Positive: 1] := 
cfByteArrayPosition[byteArray, subByteArray, n]


ByteArraySplit[byteArray_ByteArray, separator_ByteArray -> n_Integer?Positive] := 
Module[{position}, 
	position = ByteArrayPosition[byteArray, separator, n]; 
	If[position > 0, 
		{byteArray[[ ;; position - 1]], byteArray[[position + Length[separator] ;; ]]}, 
		{byteArray}
	]
]


cfByteArrayPosition := cfByteArrayPosition = 
FunctionCompile[Function[{
	Typed[byteArray, "NumericArray"::["UnsignedInteger8", 1]], 
	Typed[subByteArray, "NumericArray"::["UnsignedInteger8", 1]], 
	Typed[n, "MachineInteger"]
}, 
	Module[{m = 0, position = 0, len = Length[subByteArray]},
		Do[
			If[
				byteArray[[i ;; i + len - 1]] === subByteArray, 
					m++; 
					If[m === n,
						position = i;  
						Break[]
					]
			], 
			{i, 1, Length[byteArray] - len + 1}
		]; 
		
		(*Return: _Integer*)
		position
	]
]]


End[];


EndPackage[];
