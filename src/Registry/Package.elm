module Registry.Package exposing (Package, decode, encode, newestVersion)

import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as JE exposing (Value)
import List.Extra
import Registry.Version as Version exposing (Version)


type alias Package =
    { name : String
    , summary : String
    , versions : List Version
    }


decode : Decoder Package
decode =
    JD.succeed Package
        |> required "name" JD.string
        |> required "summary" JD.string
        |> required "versions" (JD.list Version.decode)


encode : Package -> Value
encode package =
    JE.object
        [ ( "name", JE.string package.name )
        , ( "summary", JE.string package.summary )
        , ( "versions", JE.list Version.encode package.versions )
        ]


newestVersion : Package -> Maybe Version
newestVersion p =
    List.sortWith Version.order p.versions |> List.Extra.last
