(*:Package:*)

BeginPackage["JerryI`Tinyweb`Internal`"];


ClearAll["`*"];


ConditionApply::usage = 
"ConditionApply[assoc][args] select rule if key[args] is True and resturn value[args]";


Cache::usage = 
"Cache[expr] cache expr for one minute
Cache[expr, period] cache expr for specific period";


ByteArrayPosition::usage = 
"ByteArrayPosition[data, sep, n] n position of sep in data"; 


ByteArraySplit::usage = 
"ByteArraySplit[data, sep -> n] works like Map[StringJoin, TakeDrop[StringSplit[text, sep], n]]"; 


AssociationMatchQ::usage = 
"AssociationMatchQ[assoc, pattern] match assoc with pattern
AssociationMatchQ[assoc, key, valuePattern] check key from assoc
AssociationMatchQ[pattern] - function"; 


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
cfByteArrayPosition[byteArray, subByteArray, n]; 


ByteArraySplit[byteArray_ByteArray, separator_ByteArray -> n_Integer?Positive] := 
Module[{position}, 
	position = ByteArrayPosition[byteArray, separator, n]; 
	If[position > 0, 
		{byteArray[[ ;; position - 1]], byteArray[[position + Length[separator] ;; ]]}, 
	(*Else*)
		{byteArray}
	]
]; 


AssociationMatchQ[assoc_Association, pattern_Association] := 
Apply[And, KeyValueMap[AssociationMatchQ[assoc, #1, #2]&, pattern]]


AssociationMatchQ[pattern_Association] := 
Function[assoc, AssociationMatchQ[assoc, pattern]]


AssociationMatchQ[request_Association, key__String, test: _String | _StringExpression] := 
StringMatchQ[request[key], test, IgnoreCase -> True]


AssociationMatchQ[request_Association, key: _String | {__String}, test: _Function | _Symbol] := 
test[request[key]]


(*Internal*)


cfByteArrayPosition = 
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
]]; 


(*End private*)


End[]; 


(*End package*)


EndPackage[];
