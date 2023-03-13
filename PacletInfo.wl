(* ::Package:: *)

PacletObject[
  <|
    "Name" -> "JerryI/Tinyweb",
    "Description" -> "The power of symbolic computation meets web design",
    "Creator" -> "Kirill Vasin",
    "License" -> "MIT",
    "PublisherID" -> "JerryI",
    "Version" -> "1.0.0",
    "WolframVersion" -> "13+",
    "Extensions" -> {
      {
        "Kernel",
        "Root" -> "Kernel",
        "Context" -> {
          {"JerryI`Tinyweb`", "Init.wl"}, 
          {"JerryI`Tinyweb`Internal`", "Internal.wl"}, 
          {"JerryI`Tinyweb`TCP`", "TCP.wl"}, 
          {"JerryI`Tinyweb`HTTP`", "HTTP.wl"}, 
          {"JerryI`Tinyweb`WS`", "WS.wl"}, 
          {"JerryI`Tinyweb`LTP`", "LTP.wl"}, 
          {"JerryI`Tinyweb`WSP`", "WSP.wl"}
        }
      },
      {"Documentation", "Language" -> "English"},
      {
        "Asset",
        "Assets" -> {
          {"ReadMe", "README.md"}
        }
      }
    }
  |>
]
