module Layouts.HeaderAndFooter exposing (Model, Msg, Settings, layout)

import Auth
import Effect exposing (Effect)
import Html exposing (..)
import Html.Attributes as Attr
import Layout exposing (Layout)
import Route exposing (Route)
import Route.Path
import Shared
import Shared.Model exposing (SignInStatus(..))
import View exposing (View)


type alias Settings =
    { title : String
    , user : Auth.User
    }


layout : Settings -> Shared.Model -> Route () -> Layout Model Msg mainMsg
layout settings shared route =
    Layout.new
        { init = init
        , update = update
        , view = view settings route
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    {}


init : () -> ( Model, Effect Msg )
init _ =
    ( {}
    , Effect.none
    )



-- UPDATE


type Msg
    = UserClickedSignOut


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        UserClickedSignOut ->
            ( model
            , Effect.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view :
    Settings
    -> Route ()
    ->
        { fromMsg : Msg -> mainMsg
        , content : View mainMsg
        , model : Model
        }
    -> View mainMsg
view settings route { fromMsg, model, content } =
    { title = settings.title
    , body =
        [ Html.div [ Attr.class "layout" ]
            [ --  navbar settings.user route
              Html.div [ Attr.class "page" ] content.body
            , footerView
            ]
        ]
    }


navbar : Auth.User -> Route () -> Html msg
navbar maybeUser route =
    nav
        [ Attr.class "navbar navbar-light"
        ]
        [ div
            [ Attr.class "container"
            ]
            [ a
                [ Attr.class "navbar-brand"
                , Attr.href "/"
                ]
                [ text "interoptx" ]
            , ul
                [ Attr.class "nav navbar-nav pull-xs-right"
                ]
                (navBarLinksView maybeUser route)
            ]
        ]


navBarLinksView : Auth.User -> Route () -> List (Html msg)
navBarLinksView maybeUser route =
    let
        links =
            case maybeUser of
                Nothing ->
                    signedOutNavbar route

                Just user ->
                    signedInNavbar route ++ [ profileLi user.username route ]
    in
    viewNavBarLinks [ ( "Home", Route.Path.Home_ ) ] route ++ links


signedInNavbar : Route () -> List (Html msg)
signedInNavbar routes =
    viewNavBarLinks
        [ ( "New Article", Route.Path.Editor )
        , ( "Settings", Route.Path.Settings )
        ]
        routes


viewNavBarLinks : List ( String, Route.Path.Path ) -> Route () -> List (Html msg)
viewNavBarLinks list route =
    let
        viewSidebarLink : ( String, Route.Path.Path ) -> Html msg
        viewSidebarLink ( label, path ) =
            Html.li [ Attr.class "nav-item" ]
                [ Html.a
                    [ Route.Path.href path
                    , Attr.classList
                        [ ( "active", route.path == path )
                        , ( "nav-link", True )
                        ]
                    ]
                    [ Html.text label ]
                ]
    in
    List.map viewSidebarLink list


profileLi : String -> Route () -> Html msg
profileLi username route =
    li
        [ Attr.class "nav-item"
        ]
        [ a
            [ Attr.classList
                [ ( "active", route.path == Route.Path.Profile_Username_ { username = username } )
                , ( "nav-link", True )
                ]
            , Route.Path.href (Route.Path.Profile_Username_ { username = username })
            ]
            [ img
                [ Attr.class "user-pic"
                , Attr.src "https://api.realworld.io/images/smiley-cyrus.jpeg"
                ]
                []
            , text username
            ]
        ]


signedOutNavbar : Route () -> List (Html msg)
signedOutNavbar routes =
    viewNavBarLinks
        [ ( "Sign in", Route.Path.Login )
        , ( "Sign up", Route.Path.Register )
        ]
        routes


linkedIn : Html msg
linkedIn =
    a
        [ Attr.class ""
        , Attr.style "hover" "text-purple-600 no-underline"
        , Attr.href "https://www.linkedin.com/interoptx"
        ]
        [ text "LinkedIn" ]


mailUs =
    div []
        [ img
            [ Attr.src "https://assets-global.website-files.com/62335a6993c23176d6fab483/62e2a69ab0a3485f3b1c66a3_mail.svg"
            , Attr.style "width" "24px"
            , Attr.style "loading" "lazy"
            ]
            []
        , a [ Attr.href "mailto:info@interoptx.com" ]
            [ text "info@interoptx.com" ]
        ]


socialLinksContent : Html msg
socialLinksContent =
    div [ Attr.class "row" ]
        [ div
            [ Attr.class "col text-center"
            , Attr.style "padding-right" "40px"
            ]
            [ linkedIn ]
        , div [ Attr.class "col text-center" ]
            [ mailUs ]
        ]


attributionContent : Html msg
attributionContent =
    div [ Attr.class "row" ]
        [ div
            [ Attr.class "col text-center"
            ]
            [ text "Website generated using "
            , a
                [ Attr.href "https://elm.land/"
                ]
                [ text "Elm-land" ]
            ]
        , div
            [ Attr.class "col text-center"
            , Attr.style "padding-left" "20px"
            ]
            [ text "Built from code at "
            , a
                [ Attr.href "https://github.com/elm-land/realworld-app"
                ]
                [ text "elm-land RealWorld example App" ]

            -- , text ". Code & design licensed under MIT."
            ]

        -- ,  p [] [text "© Interoptx"]
        ]


footerView : Html msg
footerView =
    footer []
        [ div
            [ Attr.class "container"
            ]
            [ socialLinksContent
            , attributionContent
            ]
        ]
