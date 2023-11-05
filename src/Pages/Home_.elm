module Pages.Home_ exposing (Model, Msg, page)

import Api
import Api.Article exposing (Article)
import Api.PopularTagsList
import Auth
import Date
import Effect exposing (Effect, replaceUrl)
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Http
import Iso8601 exposing (toTime)
import Layouts
import Page exposing (Page)
import Route exposing (Route)
import Shared 
import Shared.Model exposing (SignInStatus(..))
import Shared.Msg
import Time exposing (Month(..), utc)
import View exposing (View)
-- import Html.Styled.Events exposing (..)
-- import Html.Styled exposing (..)
-- import Html.Styled.Attributes as Attr exposing (..)
import Css exposing (..)
-- import Browser.Navigation exposing (replaceUrl)


layout : Auth.User -> Model -> Layouts.Layout
layout user model =
    Layouts.HeaderAndFooter
        { headerAndFooter =
            { title = "Home -InteroptX"
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
            -- , Browser.Navigation.pushUrl Browser.Navigation.key (pageToUrl anchorId)
            -- , Effect.none
            , replaceUrl ( "#" ++ anchorId)
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
            [ Attr.class "banner"
            ]
            [ div
                [ Attr.class "container"
                ]
                [ h1
                    [ Attr.class "logo-font"
                    ]
                    [ text "InteroptX" ]
                , p []
                    [ text "Putting Knowledge Graphs to work for Semantic Interoperability" ]
                ]
            ]
        , articleView model
        ]

para1 =  p [] [text """Et labore necessitatibus necessitatibus in quas. 
                Nostrum exercitationem sequi quas cupiditate possimus blanditiis aut aut quos, 
                aliquid quos quos vel non quia vitae aut, voluptatem consequatur. 
                Repellat dicta et neque nihil commodi, nostrum labore rerum at nemo blanditiis, 
                qui sunt occaecati hic est ducimus at blanditiis omnis hic voluptate necessitatibus 
                ullam quos qui quas omnis vitae consequatur unde ducimus error dicta est quae est est 
                voluptate at maiores commodi non labore quas dolores nostrum, aut nostrum. 
                Consectetur quaerat quas fugit blanditiis ipsum quos quos vitae sequi. 
                Deserunt sapiente aliquid magnam blanditiis, ipsum, occaecati labore occaecati magnam 
                facilis quos cupiditate exercitationem unde nihil deserunt ipsum nihil voluptate maiores, 
                est voluptate sit quasi omnis excepturi, laborum in non enim reiciendis magnam aut."""]
para2 =  p [] [text """Quos unde hic id doloribus, labore, sit sed aut fugiat aliquid dicta fugiat, labore doloribus, 
                laborum sed magnam quaerat occaecati hic, possimus voluptate labore magnam possimus sunt maiores 
                ducimus neque sequi, blanditiis sunt quos occaecati excepturi facilis blanditiis. 
                Unde at tenetur quas hic esse, ducimus doloribus tenetur facilis repellat doloribus 
                excepturi excepturi aliquid voluptatibus sunt necessitatibus consequuntur laborum neque 
                nostrum necessitatibus nihil et nemo enim quasi labore beatae ullam id est excepturi 
                consequuntur nostrum nulla in, hic asperiores. 
                Aut maiores quas consequuntur voluptatem enim est, exercitationem quasi sed quaerat 
                reiciendis consectetur nostrum rerum ducimus id nemo magnam cupiditate blanditiis 
                magnam voluptatibus reiciendis quaerat voluptate excepturi esse unde hic. 
                Vitae quia id esse voluptatibus nihil nihil eos vitae dolores quos. 
                Cupiditate fugiat occaecati ipsum necessitatibus consequuntur."""]

para3 = p [] [text """At quia eos consectetur reiciendis unde rerum ducimus vitae, et exercitationem 
                quas labore unde reiciendis quas in exercitationem cupiditate at reiciendis 
                qui nulla, at cupiditate reiciendis voluptatibus facilis hic fugit ullam rerum 
                rerum fugit aliquid, consequuntur, quae, reiciendis sapiente tenetur quos, 
                reiciendis neque error asperiores sequi, hic enim tenetur error. 
                Blanditiis beatae quia tenetur esse. 
                Reiciendis voluptatibus numquam beatae id repellat possimus sunt. 
                At dicta repellat dicta nihil nostrum qui consectetur. 
                Voluptatibus esse voluptatem aut fugit numquam eos hic dolores, nihil maiores 
                esse, dicta cupiditate possimus sunt nihil, voluptate omnis, enim at quae 
                dicta necessitatibus."""]
articleView : Model -> Html Msg
articleView model =
    div
        [ Attr.class "container page"]
        [ div 
            [Attr.class "row"] 
            [ div 
              [Attr.class "col-xs-12 col-md-8 offset-md-2"]
              [ button [onClick (SamePageNavigation "section1") ] [ text "Go to Section 1" ]
              , button [onClick (SamePageNavigation "section2") ] [ text "Go to Section 2" ]
              ]
           , div [ Attr.id "section1", Attr.class "col-xs-12 col-md-8 offset-md-2" ] 
              [ div [] [
                h2 [] [text "Section 1"]
                , para1
                , para2
                , para3
                ]
              ]
           , div [ Attr.id "section2", Attr.class "col-xs-12 col-md-8 offset-md-2" ]
             [ div [] [ 
                h2 [] [text "Section 2" ]
                , para3
               ]
             ]
          , div
            [ Attr.class "row"]
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
