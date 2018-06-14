port module Analyser.ContextLoader exposing (Context, emptyContext, loadContext, onLoadedContext)

import Elm.Dependency exposing (Version)
import Json.Encode exposing (Value)


type alias Context =
    { interfaceFiles : List ( String, Version )
    , sourceFiles : List String
    , configuration : String
    }


port loadContext : Value -> Cmd msg


port onLoadedContext : (Context -> msg) -> Sub msg


emptyContext : Context
emptyContext =
    { interfaceFiles = [], sourceFiles = [], configuration = "" }
