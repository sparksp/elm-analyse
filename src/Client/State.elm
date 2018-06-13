module Client.State exposing (State, listen, tick, toMaybe, view)

import Analyser.Checks
import Analyser.State as AS
import Browser.Navigation
import Client.LoadingScreen as LoadingScreen
import Client.Socket exposing (dashboardAddress)
import Html exposing (Html)
import Json.Decode as JD
import RemoteData exposing (RemoteData)
import Url exposing (Url)
import WebSocket as WS


type alias State =
    RemoteData String AS.State


listen : Url -> Sub State
listen location =
    --TODO
    WS.listen (dashboardAddress location) (JD.decodeString (AS.decodeState Analyser.Checks.schemas) >> Result.mapError Debug.toString >> RemoteData.fromResult)


tick : Url -> Cmd msg
tick location =
    WS.send (dashboardAddress location) "ping"


toMaybe : State -> Maybe AS.State
toMaybe =
    RemoteData.toMaybe


view : State -> (AS.State -> Html msg) -> Html msg
view s f =
    LoadingScreen.viewStateFromRemoteData s f
