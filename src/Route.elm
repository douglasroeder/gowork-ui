module Route
    exposing
        ( Route(..)
        , goto
        , linkTo
        , parse
        , toHref
        , routeToHash
        , authRedirect
        )

import Data.Category exposing (CategoryId)
import Html exposing (Html, a)
import Html.Attributes
import Navigation
import UrlParser exposing ((</>), s, int, top)


type Route
    = HomeRoute
    | LoginRoute
    | CategoriesRoute
    | CategoryAddRoute
    | CategoryEditRoute CategoryId
    | NotFoundRoute


authRoutes : List Route
authRoutes =
    [ CategoriesRoute
    , CategoryAddRoute
    ]


authForRoute : Route -> Bool -> Bool
authForRoute route loggedIn =
    loggedIn || not (List.member route authRoutes)


authRedirect : Route -> Bool -> ( Route, Cmd msg )
authRedirect route loggedIn =
    if authForRoute route loggedIn then
        ( route, Cmd.none )
    else
        ( LoginRoute, Navigation.modifyUrl <| routeToHash LoginRoute )


goto : Route -> Cmd msg
goto route =
    Navigation.newUrl (routeToHash route)


linkTo : Route -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
linkTo route attributes content =
    let
        attrs =
            toHref route :: attributes
    in
        a attrs content


matchers : UrlParser.Parser (Route -> a) a
matchers =
    UrlParser.oneOf
        [ UrlParser.map HomeRoute top
        , UrlParser.map LoginRoute (s "login")
        , UrlParser.map CategoryAddRoute (s "categories" </> s "new")
        , UrlParser.map CategoryEditRoute (s "categories" </> int </> s "edit")
        , UrlParser.map CategoriesRoute (s "categories")
        ]


parse : Navigation.Location -> Route
parse location =
    case (UrlParser.parseHash matchers location) of
        Just route ->
            route

        Nothing ->
            NotFoundRoute


toHref : Route -> Html.Attribute msg
toHref =
    Html.Attributes.href << routeToHash


routeToHash : Route -> String
routeToHash route =
    case route of
        HomeRoute ->
            "#"

        LoginRoute ->
            "#login"

        CategoriesRoute ->
            "#categories"

        CategoryAddRoute ->
            "#categories/new"

        CategoryEditRoute id ->
            "#categories/" ++ (toString id) ++ "/edit"

        NotFoundRoute ->
            "#notfound"
