port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Navigation
import Page.Categories
import Page.Category
import Page.Contacts
import Login
import Route exposing (Route(..))


-- model


type alias Model =
    { route : Route
    , location : Navigation.Location
    , categoriesModel : Page.Categories.Model
    , categoryModel : Page.Category.Model
    , contactsModel : Page.Contacts.Model
    , login : Login.Model
    , token : Maybe String
    , loggedIn : Bool
    }


init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init flags location =
    let
        loggedIn =
            flags.token /= Nothing

        route =
            Route.parse location

        ( updatedRoute, routeCmd ) =
            Route.authRedirect route loggedIn

        ( loginInitModel, loginCmd ) =
            Login.init

        ( initModel, mountCmd ) =
            mount
                { route = updatedRoute
                , location = location
                , categoriesModel = Page.Categories.initModel
                , categoryModel = Page.Category.initModel
                , contactsModel = Page.Contacts.initModel
                , login = loginInitModel
                , token = flags.token
                , loggedIn = loggedIn
                }

        cmds =
            Cmd.batch
                [ Cmd.map LoginMsg loginCmd
                , routeCmd
                , mountCmd
                ]
    in
        ( initModel, cmds )



-- update


mount : Model -> ( Model, Cmd Msg )
mount model =
    case model.route of
        HomeRoute ->
            model ! []

        LoginRoute ->
            model ! []

        CategoriesRoute ->
            let
                ( subModel, subCmd ) =
                    Page.Categories.mount model.categoriesModel
            in
                ( { model | categoriesModel = subModel }
                , Cmd.map CategoriesMsg subCmd
                )

        CategoryAddRoute ->
            let
                ( subModel, subCmd ) =
                    Page.Category.mount model.categoryModel Nothing
            in
                ( { model | categoryModel = subModel }
                , Cmd.map CategoryMsg subCmd
                )

        CategoryEditRoute id ->
            let
                ( subModel, subCmd ) =
                    Page.Category.mount model.categoryModel (Just id)
            in
                ( { model | categoryModel = subModel }
                , Cmd.map CategoryMsg subCmd
                )

        ContactsRoute ->
            let
                ( subModel, subCmd ) =
                    Page.Contacts.mount model.contactsModel
            in
                ( { model | contactsModel = subModel }
                , Cmd.map ContactsMsg subCmd
                )

        NotFoundRoute ->
            model ! []


type Msg
    = OnLocationChange Navigation.Location
    | CategoriesMsg Page.Categories.Msg
    | CategoryMsg Page.Category.Msg
    | ContactsMsg Page.Contacts.Msg
    | LoginMsg Login.Msg
    | Logout


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnLocationChange location ->
            let
                route =
                    Route.parse location

                ( updatedRoute, cmd ) =
                    Route.authRedirect route model.loggedIn
            in
                mount { model | route = updatedRoute, location = location }

        CategoriesMsg msg ->
            let
                ( categoriesModel, cmd ) =
                    Page.Categories.update msg model.categoriesModel
            in
                ( { model | categoriesModel = categoriesModel }
                , Cmd.batch
                    [ Cmd.map CategoriesMsg cmd
                    ]
                )

        CategoryMsg msg ->
            let
                ( categoryModel, cmd ) =
                    Page.Category.update msg model.categoryModel
            in
                ( { model | categoryModel = categoryModel }
                , Cmd.batch
                    [ Cmd.map CategoryMsg cmd
                    ]
                )

        ContactsMsg msg ->
            let
                ( contactsModel, cmd ) =
                    Page.Contacts.update msg model.contactsModel
            in
                ( { model
                    | contactsModel = contactsModel
                  }
                , Cmd.map ContactsMsg cmd
                )

        LoginMsg msg ->
            let
                ( loginModel, cmd, token ) =
                    Login.update msg model.login

                loggedIn =
                    token /= Nothing

                saveTokenCmd =
                    case token of
                        Just jwt ->
                            saveToken jwt

                        Nothing ->
                            Cmd.none
            in
                ( { model
                    | login = loginModel
                    , token = token
                    , loggedIn = loggedIn
                  }
                , Cmd.batch
                    [ Cmd.map LoginMsg cmd
                    , saveTokenCmd
                    ]
                )

        Logout ->
            ( { model
                | token = Nothing
                , loggedIn = False
              }
            , Cmd.batch
                [ deleteToken ()
                , Route.goto HomeRoute
                ]
            )



-- view


view : Model -> Html Msg
view model =
    div []
        [ sidebar model
        , pageContainer model
        ]


pageContainer : Model -> Html Msg
pageContainer model =
    let
        page =
            case model.route of
                HomeRoute ->
                    div [ class "main" ]
                        [ h1 []
                            [ text "Home Page" ]
                        ]

                CategoriesRoute ->
                    Html.map CategoriesMsg
                        (Page.Categories.view model.categoriesModel)

                CategoryAddRoute ->
                    Html.map CategoryMsg
                        (Page.Category.view model.categoryModel)

                CategoryEditRoute id ->
                    Html.map CategoryMsg
                        (Page.Category.view model.categoryModel)

                ContactsRoute ->
                    Html.map ContactsMsg
                        (Page.Contacts.view model.contactsModel)

                LoginRoute ->
                    Html.map LoginMsg
                        (Login.view model.login)

                NotFoundRoute ->
                    div [ class "main" ]
                        [ h1 []
                            [ text "Page Not Found!" ]
                        ]
    in
        div [ class "page-container" ]
            [ div [ class "main-content bgc-grey-100" ]
                [ div [ class "mainContent" ]
                    [ div [ class "row gap-20 masonry pos-r" ]
                        [ page ]
                    ]
                ]
            , footer
            ]


footer : Html Msg
footer =
    div [ class "bdT ta-c p-30 lh-0 fsz-sm c-grey-600" ] []


sidebar : Model -> Html Msg
sidebar model =
    div [ class "sidebar" ]
        [ div [ class "sidebar-inner" ]
            [ div [ class "sidebar-logo" ] []
            , ul [ class "sidebar-menu" ]
                [ li [ class "nav-item mT-30 active" ]
                    [ homeLinkView
                    , categoriesLinkView model
                    , contactsLinkView model
                    , authLinkView model
                    ]
                ]
            ]
        ]


categoriesLinkView : Model -> Html Msg
categoriesLinkView { loggedIn } =
    if loggedIn then
        Route.linkTo
            CategoriesRoute
            [ class "sidebar-link" ]
            [ span [ class "title" ] [ text "Categories" ] ]
    else
        text ""


contactsLinkView : Model -> Html Msg
contactsLinkView { loggedIn } =
    if loggedIn then
        Route.linkTo
            ContactsRoute
            [ class "sidebar-link" ]
            [ span [ class "title" ] [ text "Contacts" ] ]
    else
        text ""


homeLinkView : Html Msg
homeLinkView =
    Route.linkTo
        HomeRoute
        [ class "sidebar-link" ]
        [ span [ class "title" ] [ text "Dashboard" ] ]


authLinkView : Model -> Html Msg
authLinkView model =
    if model.loggedIn then
        a [ class "nav-item nav-link", onClick Logout ]
            [ text "Logout" ]
    else
        Route.linkTo
            LoginRoute
            [ class "nav-item nav-link" ]
            [ text "Login" ]



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        categoriesSub =
            Page.Categories.subscriptions model.categoriesModel
    in
        Sub.batch
            [ Sub.map CategoriesMsg categoriesSub
            ]


type alias Flags =
    { token : Maybe String }


main : Program Flags Model Msg
main =
    Navigation.programWithFlags OnLocationChange
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


port saveToken : String -> Cmd msg


port deleteToken : () -> Cmd msg
