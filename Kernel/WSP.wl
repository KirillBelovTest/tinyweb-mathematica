BeginPackage["JerryI`Tinyweb`WSP`"]


ImportWSP::usage = 
"ImportWSP[path] import Wolfram Server Page"


Begin["`Private`"]


ImportWSP[path_String] := 
Module[{text}, 
	text = Import[path, "String"]; 
	StringReplace[text, Shortest["<?wsp" ~~ code__ ~~ "?>"] :> toHTML[ToExpression[code]]]
]


ImportWSP[request_Association, path_String] := 
Module[{text}, 
	text = Import[path, "String"]; 
	StringReplace[text, Shortest["<?wsp" ~~ code__ ~~ "?>"] :> toHTML[ToExpression[code]]]
]


toHTML[image_Graphics] := 
ExportString[image, "SVG"]


toHTML[expr_] := 
ExportString[expr, "Text"]


End[]


EndPackage[]
