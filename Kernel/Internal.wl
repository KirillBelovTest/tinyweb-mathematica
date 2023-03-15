(*:Package:*)

BeginPackage["JerryI`Tinyweb`Internal`"];


ClearAll["`*"]; 


PreCompile::usage = 
"PreCompile[name, func] load if library exists and compliling if not"


PrintReturn::usage = 
"PrintReturn[code, message, format] print info and return"


ConditionApply::usage = 
"ConditionApply[assoc][args] select rule if key[args] is True and resturn value[args]";


Cache::usage = 
"Cache[expr] cache expr for one minute
Cache[expr, period] cache expr for specific period";


ByteArrayPosition::usage = 
"ByteArrayPosition[data, sep, n] n position of sep in data"; 


ByteArraySplit::usage = 
"ByteArraySplit[data, sep -> n] works like Map[StringJoin, TakeDrop[StringSplit[text, sep], n]]"; 


AssocMatchQ::usage = 
"AssocMatchQ[assoc, pattern] match assoc with pattern
AssocMatchQ[assoc, key, valuePattern] check key from assoc
AssocMatchQ[pattern] - function"; 


Begin["`Private`"];


SetAttributes[PrintReturn, HoldFirst]


PrintReturn[code_, action_String, message_String, format_: ToString] := 
Module[{startTime, endTime, duration, result}, 
	startTime = AbsoluteTime[]; 
	
	Print["\n", $indent, "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -"]; 
	Print[$indent, "[  ACTION  ]: ", action]; 
	Print[$indent, "[Start Time]: ", DateString[startTime]]; 
	
	$indent = $indent <> "\t"; 
	result = code; 
	$indent = StringDrop[$indent, -1]; 

	endTime = AbsoluteTime[]; 
	duration = endTime - startTime; 

	Print[$indent, "[ End Time ]: ", DateString[endTime]]; 
	Print[$indent, "[ Duration ]: ", ToString[DecimalForm[duration]], " s"]; 

	Print[$indent, "[  Result  ]: ", StringTemplate[message][format[result]]]; 

	Print[$indent, "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -\n"]; 

	(*Return*)
	result
]


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


ByteArrayPosition[byteArray_ByteArray, subByteArray_ByteArray, n: _Integer?Positive: 1] /; 
Length[byteArray] >= 1024 := 
cfPosition[byteArray, subByteArray, {n}]; 


ByteArrayPosition[byteArray_ByteArray, subByteArray_ByteArray, n: _Integer?Positive: 1] /; 
Length[byteArray] < 1024 := 
fPosition[byteArray, subByteArray, n]; 


ByteArraySplit[byteArray_ByteArray, separator_ByteArray -> n_Integer?Positive] := 
Module[{position}, 
	position = ByteArrayPosition[byteArray, separator, n]; 
	If[Length[position] > 0, 
		{byteArray[[ ;; position[[1, 1]] - 1]], byteArray[[position[[1, 2]] + 1 ;; ]]}, 
	(*Else*)
		{byteArray}
	]
]; 


AssocMatchQ[assoc_Association, pattern_Association] := 
Apply[And, KeyValueMap[AssocMatchQ[assoc, #1, #2]&, pattern]]


AssocMatchQ[pattern_Association] := 
Function[assoc, AssocMatchQ[assoc, pattern]]


AssocMatchQ[request_Association, key__String, test: _String | _StringExpression] := 
StringMatchQ[request[key], test, IgnoreCase -> True]


AssocMatchQ[request_Association, key__String, test: _Association] := 
AssocMatchQ[request[key], test]


AssocMatchQ[request_Association, key: _String | {__String}, test: _Function | _Symbol | _[___]] := 
test[request[key]]


SetAttributes[PreCompile, HoldRest]


PreCompile[name_String, func_FunctionCompile] := 
Module[{lib = FileNameJoin[{$lLibraryResources, name <> "." <> Internal`DynamicLibraryExtension[]}]}, 
	If[
		FileExistsQ[lib], 
			LibraryFunctionLoad[lib], 
		(*Else*)
			LibraryFunctionLoad[FunctionCompileExportLibrary[lib, func]]
	]
]


(*Internal*)


$indent = ""; 


$lLibraryResources = FileNameJoin[{
	DirectoryName[$InputFileName, 2], 
	"LibraryResources", 
	$SystemID
}]


fPosition[byteArray_ByteArray, subByteArray_ByteArray, n_Integer] := 
StringPosition[ByteArrayToString[byteArray], ByteArrayToString[subByteArray], n]; 


cfPosition := cfPosition = PreCompile["cfPosition", 
	FunctionCompile[Function[{
		Typed[byteArray, "NumericArray"::["UnsignedInteger8", 1]], 
		Typed[subByteArray, "NumericArray"::["UnsignedInteger8", 1]], 
		Typed[n, "PackedArray"::["MachineInteger", 1]]
	}, 
		Module[{m = 0, j = 1, len = Length[subByteArray], positions = {}},
			Do[
				If[
					byteArray[[i ;; i + len - 1]] === subByteArray, 
						m++; 
						If[m === n[[j]], 
							j++; 
							positions = Append[positions, {i, i + len - 1}];  
							If[j == Length[n], Break[]]
						]
				], 
				{i, 1, Length[byteArray] - len + 1}
			]; 

			(*Return: {{_Integer, _Integer}.., }*)
			positions
		]
	]]
]; 


(*End private*)


End[]; 


(*End package*)


EndPackage[];
