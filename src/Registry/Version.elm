module Registry.Version exposing (Version, asString, decode, encode, fromString, isMajorUpgrade, order)

import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)


type Version
    = Version Int Int Int


isMajorUpgrade : Version -> Version -> Bool
isMajorUpgrade (Version a _ _) (Version b _ _) =
    a < b


order : Version -> Version -> Order
order (Version a b c) (Version x y z) =
    if a /= x then
        compare a x
    else if b /= y then
        compare b y
    else
        compare c z


fromString : String -> Result String Version
fromString input =
    case String.split "." input of
        [ x, y, z ] ->
            fromStrings ( x, y, z )
                |> Result.fromMaybe "Version does not consist of three numbers"

        _ ->
            Err "Version does not consist of three numbers"


fromStrings : ( String, String, String ) -> Maybe Version
fromStrings ( x, y, z ) =
    Maybe.map3 Version (String.toInt x) (String.toInt y) (String.toInt z)


fromResult : Result String a -> Decoder a
fromResult r =
    case r of
        Ok a ->
            JD.succeed a

        Err string ->
            JD.fail string


decode : Decoder Version
decode =
    JD.string |> JD.andThen (fromString >> fromResult)


asString : Version -> String
asString (Version a b c) =
    String.join "." [ String.fromInt a, String.fromInt b, String.fromInt c ]


encode : Version -> Value
encode =
    JE.string << asString
