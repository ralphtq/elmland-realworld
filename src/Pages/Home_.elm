module Pages.Home_ exposing (Model, Msg, page)

import Api
import Api.Article exposing (Article)
import Api.PopularTagsList
import Auth
import Css exposing (..)
import Date
import Effect exposing (Effect, replaceUrl)
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Http
import Iso8601 exposing (toTime)
import Layouts
import Markdown.Block as Markdown
import Markdown.HomePageContent exposing (..)
import Markdown.Parser as Markdown
import Markdown.Renderer as Markdown
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Shared.Model exposing (SignInStatus(..))
import Shared.Msg
import Shared.Types exposing (..)
import Time exposing (Month(..), utc)
import View exposing (View)


markdownToHTML : MarkdownString -> Html msg
markdownToHTML mdText =
    mdText
        |> Markdown.parse
        |> Result.mapError (List.map Markdown.deadEndToString >> String.join "\n")
        |> Result.andThen (Markdown.render Markdown.defaultHtmlRenderer)
        |> Result.map (Html.main_ [])
        |> Result.withDefault (Html.text "Error occurred. This shouldn't happen.")


logo : String -> Html msg
logo logoFilePath =
    img
        [ Attr.src logoFilePath
        , Attr.style "display" "inlineBlock"
        , Attr.style "padding" "20px"
        ]
        []



--     , hover
--         [ borderColor theme.primary
--         , borderRadius (px 10)
--         ]


theme : { secondary : Color, primary : Color }
theme =
    { primary = hex "55af6a"
    , secondary = rgb 250 240 230
    }


bannerCSS : List (Attribute msg)
bannerCSS =
    [ Attr.style "background-color" "rgb(250,250,255)"
    , Attr.style "padding" "4px"
    ]


bannerLinkCSS : List (Attribute msg)
bannerLinkCSS =
    [ Attr.style "fontFamily" "sansSerif"
    , Attr.style "background-color" "rgb(250,250,255)"
    , Attr.style "border-color" "rgb(250,250,255)"
    , Attr.style "font-size" "32px"
    , Attr.style "font-weight" "bold"
    ]


subBannerCSS : List (Attribute msg)
subBannerCSS =
    [ Attr.style "fontFamily" "sansSerif"
    , Attr.style "text-align" "center"
    , Attr.style "font-size" "48px"
    , Attr.style "font-weight" "bold"
    , Attr.style "font-style" "oblique"
    , Attr.style "padding-top" "-100px"
    , Attr.style "padding-bottom" "30px"
    ]


websiteNameCSS : List (Attribute msg)
websiteNameCSS =
    [ Attr.style "padding-bottom" "60px"
    , Attr.style "font-size" "48px"
    , Attr.style "padding-top" "40px"
    , Attr.style "margin-left" "-100px"
    ]


renderButton : String -> String -> String -> Html Msg
renderButton styleClass label idRef =
    button
        [ Attr.class <| "btn " ++ styleClass
        , Attr.style "display" "flex"
        , Attr.style "color" "black"
        , Attr.style "background-color" "rgb(250,250,255)"
        , Attr.style "border-color" "rgb(250,250,255)"
        , Attr.style "font-size" "24px"
        , Attr.style "height" "32px"
        , onClick (SamePageNavigation idRef)
        ]
        [ text label ]


mainBanner : Html Msg
mainBanner =
    div [ Attr.class "row" ]
        [ div [ Attr.class "col text-center" ]
            [ logo "images/interoptx-icon.png" ]
        , renderColumn [ h3 websiteNameCSS [ text "InteroptX" ] ]
        , renderButton "btn-primary m-1" "What we provide" "whatWeProvide"
        , renderButton "btn-primary m-1" "Technologies" "technologies"
        , renderButton "btn-primary m-1" "Knowledge Graphs" "knowledgeGraphs"
        , renderButton "btn-primary m-1" "Who Are We" "whoAreWe"
        , renderButton "btn-primary m-1" "Why Us" "whyUsButton"
        , renderButton "btn-primary m-1" "Contact Us" "contactUs"
        ]


subBanner : Html msg
subBanner =
    h2
        subBannerCSS
        [ text "Making and Putting Knowledge Graphs to Work" ]


renderColumn : List (Html msg) -> Html msg
renderColumn content =
    div
        [ Attr.class "col-sm", Attr.style "padding-left" "100px" ]
        content


renderImage : ImageFilePath -> Html msg
renderImage image =
    img
        [ Attr.src image
        , Attr.style "display" "inlineBlock"
        , Attr.style "width" "1000px"
        , Attr.style "padding" "20px"
        ]
        []


renderTopic : TopicRowType -> ImageFilePath -> String -> List (Html msg) -> Html msg
renderTopic topicRowType idRef heading content =
    let
        contentPart =
            [ h2 [ Attr.style "font-weight" "bold" ] [ text heading ]
            , div
                [ Attr.style "padding-right" "200px"
                , Attr.style "font-size" "1.5rem"
                ]
                content
            ]
    in
    case topicRowType of
        ImageOnLeft image ->
            div [ Attr.id idRef, Attr.class "row" ]
                [ renderColumn [ renderImage image ], renderColumn contentPart ]

        ImageOnRight image ->
            div [ Attr.id idRef, Attr.class "row" ]
                [ renderColumn contentPart, renderColumn [ renderImage image ] ]

        NoImage ->
            div [ Attr.id idRef, Attr.class "row" ] [ renderColumn contentPart ]


homePageContent : Html msg
homePageContent =
    div [ Attr.class "row" ]
        [ renderTopic (ImageOnLeft interoperability_image1)
            "aboutUs"
            "About Us"
            [ markdownToHTML whyInteroptx ]
        , renderTopic (ImageOnRight knowledgeGraphApplication_image1)
            "whyThisIsImportant"
            "Why this is important"
            [ markdownToHTML importanceContent ]
        , renderTopic (ImageOnLeft interoperability_image3)
            "knowledgeGraphs"
            "Knowledge Graphs and Semantic Interoperability"
            [ markdownToHTML semanticInteroperability ]
        , renderTopic (ImageOnRight clusteredWebServices_image1)
            "technologies"
            "Technologies"
            [ markdownToHTML technologies ]
        , renderTopic (ImageOnLeft systemViewpoints_image1)
            "whatWeProvide"
            "What we provide"
            [ markdownToHTML whatWeProvide ]
        , renderTopic (ImageOnRight medicine_image1)
            "whoAreWe"
            "Who We Are"
            [ markdownToHTML whoWeAre ]
        , renderTopic NoImage
            "contactUs"
            "For more information"
            [ markdownToHTML contactUs ]

        -- , div [ Attr.class "row" ] [ renderColumn [ renderImage systemViewpoints_image1 ] ]
        ]


layout : Auth.User -> Model -> Layouts.Layout
layout user model =
    Layouts.HeaderAndFooter
        { headerAndFooter =
            { title = "Home - InteroptX"
            , user = user
            }
        }


page : Auth.User -> Shared.Model -> Route () -> Page Model Msg
page user smodel _ =
    Page.new
        { init = init user
        , update = update user smodel
        , subscriptions = subscriptions
        , view = view
        }
        |> Page.withLayout (layout user)



-- INIT


type SelectedFeed
    = GlobalFeed
    | YourFeed
    | TagFeed


type alias Model =
    { articleData : Api.Data (List Article)
    , popularTagData : Api.Data (List String)
    , selectedFeedTab : SelectedFeed
    , userSignIn : Bool
    , token : Maybe String
    , isFavoriteButtonClicked : Bool
    , clickedTag : String
    , anchorId : String
    }


init : Auth.User -> () -> ( Model, Effect Msg )
init maybeUser () =
    let
        userSignIn =
            case maybeUser of
                Nothing ->
                    False

                Just _ ->
                    True

        token =
            Maybe.map (\u -> u.token) maybeUser
    in
    ( { articleData = Api.Loading
      , popularTagData = Api.Loading
      , selectedFeedTab = GlobalFeed
      , userSignIn = userSignIn
      , token = token
      , isFavoriteButtonClicked = False
      , clickedTag = ""
      , anchorId = ""
      }
    , Effect.batch
        [ Api.Article.getFirst20ArticleBy
            { onResponse = ArticleApiResponded
            , author = Nothing
            , favorited = Nothing
            , tag = Nothing
            , token = token
            }
        , Api.PopularTagsList.getTags
            { onResponse = PopularTagsApiResponded
            }
        ]
    )



-- UPDATE


type Msg
    = ArticleApiResponded (Result Http.Error (List Article))
    | PopularTagsApiResponded (Result Http.Error (List String))
    | UserClickedSignOut
    | UserClickedFeeds
    | UserClickedTagFeeds String
    | UserClickedGLobalArticle
    | UserClickedOnFavoriteArticle String
    | UserClickedOnUnFavoriteArticle String
    | ArticleFavoriteApiResponded (Result Http.Error Article)
    | ArticleUnFavoriteApiResponded (Result Http.Error Article)
    | SamePageNavigation String


update : Auth.User -> Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update maybeUser _ msg model =
    case msg of
        SamePageNavigation anchorId ->
            ( { model | anchorId = anchorId }
            , replaceUrl ("#" ++ anchorId)
            )

        ArticleApiResponded (Ok listOfArticle) ->
            ( { model | articleData = Api.Success listOfArticle }
            , Effect.none
            )

        ArticleApiResponded (Err httpError) ->
            ( { model | articleData = Api.Failure httpError }
            , Effect.none
            )

        PopularTagsApiResponded (Ok listOfPopularTag) ->
            ( { model | popularTagData = Api.Success listOfPopularTag }
            , Effect.none
            )

        PopularTagsApiResponded (Err httpError) ->
            ( { model | articleData = Api.Failure httpError }
            , Effect.none
            )

        UserClickedSignOut ->
            ( { model | userSignIn = False }
            , Effect.fromSharedMsg Shared.Msg.PageSignedOutUser
            )

        UserClickedFeeds ->
            case maybeUser of
                Nothing ->
                    ( { model | selectedFeedTab = YourFeed }
                    , Effect.none
                    )

                Just user ->
                    ( { model | selectedFeedTab = YourFeed }
                    , Api.Article.getFirst20Feeds
                        { onResponse = ArticleApiResponded
                        , token = user.token
                        }
                    )

        UserClickedGLobalArticle ->
            ( { model | selectedFeedTab = GlobalFeed }
            , Api.Article.getFirst20ArticleBy
                { onResponse = ArticleApiResponded
                , author = Nothing
                , favorited = Nothing
                , tag = Nothing
                , token = model.token
                }
            )

        UserClickedTagFeeds tag ->
            ( { model | selectedFeedTab = TagFeed, clickedTag = tag }
            , Api.Article.getFirst20ArticleBy
                { onResponse = ArticleApiResponded
                , author = Nothing
                , favorited = Nothing
                , tag = Just tag
                , token = model.token
                }
            )

        UserClickedOnFavoriteArticle slug ->
            ( { model | isFavoriteButtonClicked = True }
            , Api.Article.favoriteArticleCommets
                { onResponse = ArticleFavoriteApiResponded
                , token = model.token
                , slug = slug
                }
            )

        UserClickedOnUnFavoriteArticle slug ->
            ( { model | isFavoriteButtonClicked = True }
            , Api.Article.unfavoriteArticleCommets
                { onResponse = ArticleUnFavoriteApiResponded
                , token = model.token
                , slug = slug
                }
            )

        ArticleFavoriteApiResponded (Ok _) ->
            ( { model | isFavoriteButtonClicked = False }
            , favoriteApiCallBack model.token model.selectedFeedTab
            )

        ArticleFavoriteApiResponded (Err _) ->
            ( { model | isFavoriteButtonClicked = False }
            , Effect.none
            )

        ArticleUnFavoriteApiResponded (Ok _) ->
            ( { model | isFavoriteButtonClicked = False }
            , favoriteApiCallBack model.token model.selectedFeedTab
              -- Change to in app state change
            )

        ArticleUnFavoriteApiResponded (Err _) ->
            ( { model | isFavoriteButtonClicked = False }
            , Effect.none
            )


favoriteApiCallBack : Maybe String -> SelectedFeed -> Effect Msg
favoriteApiCallBack token selector =
    case selector of
        GlobalFeed ->
            Api.Article.getFirst20ArticleBy
                { onResponse = ArticleApiResponded
                , author = Nothing
                , favorited = Nothing
                , tag = Nothing
                , token = token
                }

        YourFeed ->
            Api.Article.getFirst20Feeds
                { onResponse = ArticleApiResponded
                , token = Maybe.withDefault "" token
                }

        TagFeed ->
            Api.Article.getFirst20ArticleBy
                { onResponse = ArticleApiResponded
                , author = Nothing
                , favorited = Nothing
                , tag = Just "tag"
                , token = token
                }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Home -InteroptX"
    , body = [ viewBody model ]
    }


viewBody : Model -> Html Msg
viewBody model =
    div
        [ Attr.class "home-page"
        ]
        [ div
            [-- Attr.class "banner"
            ]
            [ div
                [ Attr.class "container"
                , Attr.style "max-width" "1600px"
                ]
                [ div
                    bannerCSS
                    [ mainBanner
                    , subBanner
                    ]
                ]
            ]
        , homePageContent

        -- , articleView model
        ]


articleView : Model -> Html Msg
articleView model =
    div
        [ Attr.class "container page" ]
        [ div
            [ Attr.class "row" ]
            [ div
                [ Attr.class "row" ]
                [ div
                    [ Attr.class "col-md-9"
                    ]
                    (feedView model :: articleListView model)
                , popularTagView model
                ]
            ]
        ]


feedView : Model -> Html Msg
feedView model =
    let
        yourFeedLiView =
            if model.userSignIn then
                [ li
                    [ Attr.class "nav-item"
                    ]
                    [ a
                        [ Attr.classList [ ( "nav-link", True ), ( "active", model.selectedFeedTab == YourFeed ) ]
                        , Attr.href ""
                        , onClick UserClickedFeeds
                        ]
                        [ text "Your Feed" ]
                    ]
                ]

            else
                []

        tagView =
            if model.selectedFeedTab == TagFeed then
                [ li
                    [ Attr.class "nav-item"
                    ]
                    [ a
                        [ Attr.classList [ ( "nav-link", True ), ( "active", model.selectedFeedTab == TagFeed ) ]
                        , Attr.href ""
                        , onClick (UserClickedTagFeeds model.clickedTag)
                        ]
                        [ text ("# " ++ model.clickedTag) ]
                    ]
                ]

            else
                []
    in
    div
        [ Attr.class "feed-toggle"
        ]
        [ ul
            [ Attr.class "nav nav-pills outline-active"
            ]
            (li
                [ Attr.class "nav-item"
                ]
                [ a
                    [ Attr.classList [ ( "nav-link", True ), ( "active", model.selectedFeedTab == GlobalFeed ) ]
                    , Attr.href ""
                    , onClick UserClickedGLobalArticle
                    ]
                    [ text "Global Feed" ]
                ]
                :: yourFeedLiView
                ++ tagView
            )
        ]


popularTagView : Model -> Html Msg
popularTagView model =
    div
        [ Attr.class "col-md-3"
        ]
        [ div
            [ Attr.class "sidebar"
            ]
            [ p []
                [ text "Popular Tags" ]
            , popularTagListView model
            ]
        ]


popularTagListView : Model -> Html Msg
popularTagListView model =
    case model.popularTagData of
        Api.Loading ->
            div []
                [ text "Loading..."
                ]

        Api.Success popularTagList ->
            div [ Attr.class "tag-list" ]
                (List.map popularTagRowView popularTagList)

        Api.Failure httpError ->
            div []
                [ text (Api.Article.toUserFriendlyMessage httpError)
                ]


popularTagRowView : String -> Html Msg
popularTagRowView tag =
    a
        [ Attr.href ""
        , onClick (UserClickedTagFeeds tag)
        , Attr.class "tag-pill tag-default"
        ]
        [ text tag ]


articleListView : Model -> List (Html Msg)
articleListView model =
    case model.articleData of
        Api.Loading ->
            [ div []
                [ text "Loading..."
                ]
            ]

        Api.Success articleList ->
            List.map (articleCardView model.isFavoriteButtonClicked) articleList

        Api.Failure httpError ->
            [ div []
                [ text (Api.Article.toUserFriendlyMessage httpError)
                ]
            ]


articleCardView : Bool -> Article -> Html Msg
articleCardView isFavoriteButtonClicked article =
    div
        [ Attr.class "article-preview"
        ]
        [ div
            [ Attr.class "article-meta"
            ]
            [ a
                [ Attr.href ("/profile/" ++ article.author.username)
                ]
                [ img
                    [ Attr.src article.author.image
                    ]
                    []
                ]
            , div
                [ Attr.class "info"
                ]
                [ a
                    [ Attr.href ("/profile/" ++ article.author.username)
                    , Attr.class "author"
                    ]
                    [ text article.author.username ]
                , span
                    [ Attr.class "date"
                    ]
                    [ text (mydateFormat article.updatedAt) ]
                ]
            , button
                [ Attr.classList
                    [ ( "btn btn-sm pull-xs-right", True )
                    , ( "btn-outline-primary", not article.favorited )
                    , ( "btn-primary", article.favorited )
                    , ( "disabled", isFavoriteButtonClicked )
                    ]
                , onClick
                    (if article.favorited then
                        UserClickedOnUnFavoriteArticle article.slug

                     else
                        UserClickedOnFavoriteArticle article.slug
                    )
                ]
                [ i
                    [ Attr.class "ion-heart"
                    ]
                    []
                , text (" " ++ String.fromInt article.favoritesCount)
                ]
            ]
        , a
            [ Attr.href ("/article/" ++ article.slug)
            , Attr.class "preview-link"
            ]
            [ h1 []
                [ text article.title ]
            , p []
                [ text article.description ]
            , span []
                [ text "Read more..." ]
            , ul [ Attr.class "tag-list" ]
                (List.map feedTagsView article.tagList)
            ]
        ]


feedTagsView : String -> Html msg
feedTagsView tags =
    li
        [ Attr.class "tag-default tag-pill tag-outline"
        ]
        [ text tags ]


mydateFormat : String -> String
mydateFormat d =
    let
        date =
            toTime d
    in
    case date of
        Ok pdate ->
            Date.format "MMMM d, y" (Date.fromPosix utc pdate)

        Err _ ->
            "err"
