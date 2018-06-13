module Client.Socket exposing (controlAddress, dashboardAddress)

import Browser.Navigation
import Url exposing (Url)


controlAddress : Url -> String
controlAddress { host } =
    "ws://" ++ host ++ "/control"


dashboardAddress : Url -> String
dashboardAddress { host } =
    "ws://" ++ host ++ "/state"
