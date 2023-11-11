port module Effect exposing
    ( Effect, none, map, batch, replaceUrl
    , fromSharedMsg
    , pushRoute, replaceRoute, loadExternalUrl
    , pushUrlPath
    , save
    , toCmd
    , sendCmd
    )

{-|

@docs Effect, none, map, batch
@docs fromCmd, fromSharedMsg
@docs Msg, fromAction
@docs pushRoute, replaceRoute, loadExternalUrl
@docs pushUrlPath
@docs save
@docs toCmd

-}

import Browser.Navigation
import Dict exposing (Dict)
import Json.Encode
import Route
import Route.Path
import Shared.Model
import Shared.Msg
import Task
import Url exposing (Url)


type Effect msg
    = None
    | Batch (List (Effect msg))
    | SendCmd (Cmd msg)
    | PushUrl String
    | ReplaceUrl String
    | LoadExternalUrl String
    | SaveToLocalStorage { key : String, value : Json.Encode.Value }
    | SendSharedMsg Shared.Msg.Msg


none : Effect msg
none =
    None


replaceUrl : String -> Effect msg
replaceUrl s =   
    ReplaceUrl s


batch : List (Effect msg) -> Effect msg
batch =
    Batch


fromSharedMsg : Shared.Msg.Msg -> Effect msg
fromSharedMsg =
    SendSharedMsg


sendCmd : Cmd msg -> Effect msg
sendCmd =
    SendCmd


-- ROUTING


pushUrlPath : String -> Effect msg
pushUrlPath str =
    PushUrl str


pushRoute :
    { path : Route.Path.Path
    , query : Dict String String
    , hash : Maybe String
    }
    -> Effect msg
pushRoute route =
    PushUrl (Route.toString route)


replaceRoute :
    { path : Route.Path.Path
    , query : Dict String String
    , hash : Maybe String
    }
    -> Effect msg
replaceRoute route =
    ReplaceUrl (Route.toString route)


loadExternalUrl : String -> Effect msg
loadExternalUrl =
    LoadExternalUrl


sendMsg : msg -> Effect msg
sendMsg msg =
    Task.succeed msg
        |> Task.perform identity
        |> SendCmd



-- LOCAL STORAGE


port saveToLocalStorage : { key : String, value : Json.Encode.Value } -> Cmd msg


save : { key : String, value : Json.Encode.Value } -> Effect msg
save keyValueRecord =
    SaveToLocalStorage keyValueRecord


-- Define a port to send the element ID to JavaScript
port scrollTo : String -> Cmd msg

-- MAP


map : (msg1 -> msg2) -> Effect msg1 -> Effect msg2
map fn effect =
    case effect of
        None ->
            None

        Batch list ->
            Batch (List.map (map fn) list)

        PushUrl url ->
            PushUrl url

        ReplaceUrl url ->
            ReplaceUrl url

        LoadExternalUrl url ->
            LoadExternalUrl url

        SaveToLocalStorage options ->
            SaveToLocalStorage options

        SendCmd cmd ->
            SendCmd (Cmd.map fn cmd)

        SendSharedMsg sharedMsg ->
            SendSharedMsg sharedMsg



-- Used by Main.elm


toCmd :
    { key : Browser.Navigation.Key
    , url : Url
    , shared : Shared.Model.Model
    , fromSharedMsg : Shared.Msg.Msg -> msg
    , fromCmd : Cmd msg -> msg
    , toCmd : msg -> Cmd msg
    }
    -> Effect msg
    -> Cmd msg
toCmd options effect =
    case effect of
        None ->
            Cmd.none

        Batch list ->
            Cmd.batch (List.map (toCmd options) list)

        PushUrl url ->
            Browser.Navigation.pushUrl options.key url

        ReplaceUrl url ->
            -- Browser.Navigation.replaceUrl options.key url
            let
                -- Extract the hash from the URL and remove the leading '#'
                maybeHash =
                    String.dropLeft 1 <| Maybe.withDefault "" (Just url)
            in
            -- If there is a hash, tell JavaScript to scroll to that element
                case maybeHash of
                    ""   -> Browser.Navigation.replaceUrl options.key url
                    hash -> scrollTo hash        

        LoadExternalUrl url ->
            Browser.Navigation.load url

        SaveToLocalStorage keyValueRecord ->
            saveToLocalStorage keyValueRecord

        SendCmd cmd ->
            cmd

        SendSharedMsg sharedMsg ->
            Task.succeed sharedMsg
                |> Task.perform options.fromSharedMsg
