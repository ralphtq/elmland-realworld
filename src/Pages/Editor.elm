module Pages.Editor exposing (Model, Msg, page)

import Auth
import Effect exposing (Effect)
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events
import Http
import Json.Decode exposing (list, string)
import Json.Decode.Pipeline exposing (required)
import Json.Encode
import Layout exposing (Layout)
import Page exposing (Page)
import Route exposing (Route)
import Shared
import View exposing (View)


layout : Layout
layout =
    Layout.HeaderAndFooter


page : Auth.User -> Shared.Model -> Route () -> Page Model Msg
page user shared route =
    Page.new
        { init = init
        , update = update user
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


type alias Model =
    { title : String
    , body : String
    , description : String
    , tagList : List String
    , errors : List FormError
    , isSubmittingForm : Bool
    }


init : () -> ( Model, Effect Msg )
init () =
    ( { title = ""
      , body = ""
      , description = ""
      , tagList = []
      , errors = []
      , isSubmittingForm = False
      }
    , Effect.none
    )



-- UPDATE


type Msg
    = UserUpdatedInput Field String
    | UserSubmittedForm
    | ArticleCreateApiResponded (Result (List FormError) CreateArticlePayload)


update : Auth.User -> Msg -> Model -> ( Model, Effect Msg )
update user msg model =
    case msg of
        UserUpdatedInput Title value ->
            ( { model
                | title = value
                , errors = clearErrorsFor Title model.errors
              }
            , Effect.none
            )

        UserUpdatedInput Description value ->
            ( { model
                | description = value
                , errors = clearErrorsFor Description model.errors
              }
            , Effect.none
            )

        UserUpdatedInput Body value ->
            ( { model
                | body = value
                , errors = clearErrorsFor Body model.errors
              }
            , Effect.none
            )

        UserUpdatedInput TagList value ->
            ( { model
                | tagList = [ value ]
                , errors = clearErrorsFor TagList model.errors
              }
            , Effect.none
            )

        UserSubmittedForm ->
            ( { model
                | isSubmittingForm = True
                , errors = []
              }
            , Effect.fromCmd
                (callCreateArticleApi
                    { title = model.title
                    , body = model.body
                    , description = model.description
                    , tagList = model.tagList
                    , token = user.token
                    }
                )
            )

        ArticleCreateApiResponded (Err formErrors) ->
            ( { model | errors = formErrors, isSubmittingForm = False }
            , Effect.none
            )

        ArticleCreateApiResponded (Ok _) ->
            ( model
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Pages.Editor"
    , body = [ viewBody ]
    }


viewBody : Html Msg
viewBody =
    div
        [ Attr.class "editor-page"
        ]
        [ div
            [ Attr.class "container page"
            ]
            [ div
                [ Attr.class "row"
                ]
                [ div
                    [ Attr.class "col-md-10 offset-md-1 col-xs-12"
                    ]
                    [ form [ Html.Events.onSubmit UserSubmittedForm ]
                        [ fieldset []
                            [ fieldset
                                [ Attr.class "form-group"
                                ]
                                [ input
                                    [ Attr.type_ "text"
                                    , Attr.class "form-control form-control-lg"
                                    , Attr.placeholder "Article Title"
                                    , Html.Events.onInput (UserUpdatedInput Title)
                                    ]
                                    []
                                ]
                            , fieldset
                                [ Attr.class "form-group"
                                ]
                                [ input
                                    [ Attr.type_ "text"
                                    , Attr.class "form-control"
                                    , Attr.placeholder "What's this article about?"
                                    , Html.Events.onInput (UserUpdatedInput Description)
                                    ]
                                    []
                                ]
                            , fieldset
                                [ Attr.class "form-group"
                                ]
                                [ textarea
                                    [ Attr.class "form-control"
                                    , Attr.rows 8
                                    , Attr.placeholder "Write your article (in markdown)"
                                    , Html.Events.onInput (UserUpdatedInput Body)
                                    ]
                                    []
                                ]
                            , fieldset
                                -- TODO: Fix multiple tags
                                [ Attr.class "form-group"
                                ]
                                [ input
                                    [ Attr.type_ "text"
                                    , Attr.class "form-control"
                                    , Attr.placeholder "Enter tags"
                                    , Html.Events.onInput (UserUpdatedInput TagList)
                                    ]
                                    []
                                , div
                                    [ Attr.class "tag-list"
                                    ]
                                    []
                                ]
                            , button
                                [ Attr.class "btn btn-lg pull-xs-right btn-primary" ]
                                [ text "Publish Article" ]
                            ]
                        ]
                    ]
                ]
            ]
        ]



-- Form


type alias CreateArticlePayload =
    { title : String
    , body : String
    , description : String
    , tagList : List String
    }


type Field
    = Title
    | Description
    | Body
    | TagList


type alias FormError =
    { field : Maybe Field
    , message : String
    }


callCreateArticleApi :
    { title : String
    , body : String
    , description : String
    , tagList : List String
    , token : String
    }
    -> Cmd Msg
callCreateArticleApi payload =
    let
        json : Json.Encode.Value
        json =
            Json.Encode.object
                [ ( "article"
                  , Json.Encode.object
                        [ ( "title", Json.Encode.string payload.title )
                        , ( "description", Json.Encode.string payload.description )
                        , ( "body", Json.Encode.string payload.body )
                        , ( "tagList", Json.Encode.list Json.Encode.string payload.tagList )
                        ]
                  )
                ]
    in
    Http.request
        { method = "post"
        , url = "https://api.realworld.io/api/articles"
        , body = Http.jsonBody json
        , expect = expectApiResponse ArticleCreateApiResponded articleDecoder
        , headers = [ Http.header "Authorization" ("Bearer " ++ payload.token) ]
        , timeout = Nothing
        , tracker = Nothing
        }


expectApiResponse :
    (Result (List FormError) value -> msg)
    -> Json.Decode.Decoder value
    -> Http.Expect msg
expectApiResponse toMsg decoder =
    Http.expectStringResponse toMsg (toFormApiResult decoder)


toFormApiResult : Json.Decode.Decoder value -> Http.Response String -> Result (List FormError) value
toFormApiResult decoder response =
    case response of
        Http.BadUrl_ _ ->
            Err [ { field = Nothing, message = "Unexpected URL format" } ]

        Http.Timeout_ ->
            Err [ { field = Nothing, message = "Server did not respond" } ]

        Http.NetworkError_ ->
            Err [ { field = Nothing, message = "Could not connect to server" } ]

        Http.BadStatus_ { statusCode } rawJson ->
            case Json.Decode.decodeString formErrorsDecoder rawJson of
                Ok errors ->
                    Err errors

                Err _ ->
                    Err [ { field = Nothing, message = "Received status code " ++ String.fromInt statusCode } ]

        Http.GoodStatus_ _ rawJson ->
            case Json.Decode.decodeString decoder rawJson of
                Ok value ->
                    Ok value

                Err _ ->
                    Err [ { field = Nothing, message = "Received unexpected API response" } ]


formErrorsDecoder : Json.Decode.Decoder (List FormError)
formErrorsDecoder =
    let
        formErrorDecoder : Json.Decode.Decoder FormError
        formErrorDecoder =
            Json.Decode.map2 FormError
                (Json.Decode.field "field" Json.Decode.string
                    |> Json.Decode.map fromStringToMaybeField
                )
                (Json.Decode.field "message" Json.Decode.string)

        fromStringToMaybeField : String -> Maybe Field
        fromStringToMaybeField field =
            case field of
                "title" ->
                    Just Title

                "description" ->
                    Just Description

                "body" ->
                    Just Body

                "tagList" ->
                    Just TagList

                _ ->
                    Nothing
    in
    Json.Decode.field "errors" (Json.Decode.list formErrorDecoder)


clearErrorsFor : Field -> List FormError -> List FormError
clearErrorsFor field errors =
    errors
        |> List.filter (\error -> error.field /= Just field)


articleDecoder : Json.Decode.Decoder CreateArticlePayload
articleDecoder =
    Json.Decode.field "article"
        (Json.Decode.succeed CreateArticlePayload
            |> required "title" string
            |> required "body" string
            |> required "description" string
            |> required "tagList" (list string)
        )
