module Client exposing (main)

import Browser
import Client.App.App as App
import Html exposing (div)
import Json.Encode exposing (Value)


main : Program Value App.Model App.Msg
main =
    Browser.fullscreen
        { init = App.init
        , onNavigation = Just App.OnLocation
        , view = view
        , update = App.update
        , subscriptions = App.subscriptions
        }


view : App.Model -> Browser.Page App.Msg
view model =
    { title = "Elm Analyse"
    , body =
        [ App.view model
        ]
    }
