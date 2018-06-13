port module Util.Logger exposing (error, info, warning)


port log : ( String, String ) -> Cmd msg


info : String -> Cmd msg
info =
    Tuple.pair "INFO" >> log


warning : String -> Cmd msg
warning =
    Tuple.pair "WARN" >> log


error : String -> Cmd msg
error =
    Tuple.pair "ERROR" >> log
