module Pages.Login exposing (Model, Msg, page)

import Effect exposing (Effect)
import Html exposing (..)
import Html.Attributes as Attr
import Layout exposing (Layout)
import Page exposing (Page)
import Route exposing (Route)
import Shared
import View exposing (View)


layout : Layout
layout =
    Layout.NavBar


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


type alias Model =
    {}


init : () -> ( Model, Effect Msg )
init () =
    ( {}
    , Effect.none
    )



-- UPDATE


type Msg
    = ExampleMsgReplaceMe


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ExampleMsgReplaceMe ->
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
    { title = "login"
    , body = [ viewBody ]
    }


viewBody : Html Msg
viewBody =
    div
        [ Attr.class "auth-page"
        ]
        [ div
            [ Attr.class "container page"
            ]
            [ div
                [ Attr.class "row"
                ]
                [ div
                    [ Attr.class "col-md-6 offset-md-3 col-xs-12"
                    ]
                    [ h1
                        [ Attr.class "text-xs-center"
                        ]
                        [ text "Sign up" ]
                    , p
                        [ Attr.class "text-xs-center"
                        ]
                        [ a
                            [ Attr.href ""
                            ]
                            [ text "Have an account?" ]
                        ]
                    , ul
                        [ Attr.class "error-messages"
                        ]
                        [ li []
                            [ text "That email is already taken" ]
                        ]
                    , form []
                        [ fieldset
                            [ Attr.class "form-group"
                            ]
                            [ input
                                [ Attr.class "form-control form-control-lg"
                                , Attr.type_ "text"
                                , Attr.placeholder "Your Name"
                                ]
                                []
                            ]
                        , fieldset
                            [ Attr.class "form-group"
                            ]
                            [ input
                                [ Attr.class "form-control form-control-lg"
                                , Attr.type_ "text"
                                , Attr.placeholder "Email"
                                ]
                                []
                            ]
                        , fieldset
                            [ Attr.class "form-group"
                            ]
                            [ input
                                [ Attr.class "form-control form-control-lg"
                                , Attr.type_ "password"
                                , Attr.placeholder "Password"
                                ]
                                []
                            ]
                        , button
                            [ Attr.class "btn btn-lg btn-primary pull-xs-right"
                            ]
                            [ text "Sign up" ]
                        ]
                    ]
                ]
            ]
        ]