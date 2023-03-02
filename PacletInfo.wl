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
          {"JerryI`Tinyweb`TCPServer`", "TCPServer.wl"}, 
          {"JerryI`Tinyweb`HTTPHandler`", "HTTPHandler.wl"}, 
          {"JerryI`Tinyweb`WSHandler`", "WSHandler.wl"}, 
          {"JerryI`Tinyweb`LTPHandler`", "LTPHandler.wl"}, 
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
