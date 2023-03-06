(*:Package:*)

BeginPackage["JerryI`Tinyweb`Internal`"];


ClearAll["`*"];


ConditionApply::usage = 
"ConditionApply[assoc][args] select rule if key[args] is True and resturn value[args]";


Cache::usage = 
"Cache[expr] cache expr for one minute
Cache[expr, period] cache expr for specific period";


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


End[];


EndPackage[];
