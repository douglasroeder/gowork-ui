port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Navigation
import Page.Categories
import Page.Category
import Login


-- model


type alias Model =
    { page : Page
    , categoriesModel : Page.Categories.Model
    , categoryModel : Page.Category.Model
    , login : Login.Model
    , token : Maybe String
    , loggedIn : Bool
    }


type Page
    = NotFound
    | HomePage
    | LoginPage
    | CategoriesPage
    | CategoryPage


authPages : List Page
authPages =
    [ CategoriesPage
    , CategoryPage
    ]


init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init flags location =
    let
        page =
            hashToPage location.hash

        loggedIn =
            flags.token /= Nothing

        ( updatedPage, cmd ) =
            authRedirect page loggedIn

        ( categoriesInitModel, categoriesCmd ) =
            Page.Categories.init

        ( categoryInitModel, categoryCmd ) =
            Page.Category.init

        ( loginInitModel, loginCmd ) =
            Login.init

        initModel =
            { page = updatedPage
            , categoriesModel = categoriesInitModel
            , categoryModel = categoryInitModel
            , login = loginInitModel
            , token = flags.token
            , loggedIn = loggedIn
            }

        cmds =
            Cmd.batch
                [ Cmd.map CategoriesMsg categoriesCmd
                , Cmd.map CategoryMsg categoryCmd
                , Cmd.map LoginMsg loginCmd
                , cmd
                ]
    in
        ( initModel, cmds )



-- update


type Msg
    = Navigate Page
    | ChangePage Page
    | CategoriesMsg Page.Categories.Msg
    | CategoryMsg Page.Category.Msg
    | LoginMsg Login.Msg
    | Logout


authForPage : Page -> Bool -> Bool
authForPage page loggedIn =
    loggedIn || not (List.member page authPages)


authRedirect : Page -> Bool -> ( Page, Cmd Msg )
authRedirect page loggedIn =
    if authForPage page loggedIn then
        ( page, Cmd.none )
    else
        ( LoginPage, Navigation.modifyUrl <| pageToHash LoginPage )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Navigate page ->
            ( { model | page = page }, Navigation.newUrl <| pageToHash page )

        ChangePage page ->
            let
                ( updatedPage, cmd ) =
                    authRedirect page model.loggedIn
            in
                ( { model | page = updatedPage }, cmd )

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
                , Navigation.modifyUrl <| pageToHash HomePage
                ]
            )



-- view


view : Model -> Html Msg
view model =
    let
        page =
            case model.page of
                HomePage ->
                    div [ class "main" ]
                        [ h1 []
                            [ text "Home Page" ]
                        ]

                CategoriesPage ->
                    Html.map CategoriesMsg
                        (Page.Categories.view model.categoriesModel)

                CategoryPage ->
                    Html.map CategoryMsg
                        (Page.Category.view model.categoryModel)

                LoginPage ->
                    Html.map LoginMsg
                        (Login.view model.login)

                NotFound ->
                    div [ class "main" ]
                        [ h1 []
                            [ text "Page Not Found!" ]
                        ]
    in
        div []
            [ navBar model
            , page
            ]


categoriesLinkView : Model -> Html Msg
categoriesLinkView { loggedIn } =
    if loggedIn then
        a [ class "nav-item nav-link", onClick (Navigate CategoriesPage) ]
            [ text "Categories" ]
    else
        text ""


categoryLinkView : Model -> Html Msg
categoryLinkView { loggedIn } =
    if loggedIn then
        a [ class "nav-item nav-link", onClick (Navigate CategoryPage) ]
            [ text "New Category" ]
    else
        text ""


navBar : Model -> Html Msg
navBar model =
    nav [ class "navbar navbar-expand-lg navbar-light bg-light" ]
        [ a [ class "navbar-brand", href "#" ]
            [ text "OS app" ]
        , div [ class "navbar-nav" ]
            [ a [ class "nav-item nav-link", onClick (Navigate HomePage) ] [ text "Home" ]
            , categoriesLinkView model
            , categoryLinkView model
            , authHeaderView model
            ]
        ]


authHeaderView : Model -> Html Msg
authHeaderView model =
    if model.loggedIn then
        a [ class "nav-item nav-link", onClick Logout ] [ text "Logout" ]
    else
        a [ class "nav-item nav-link", onClick (Navigate LoginPage) ] [ text "Login" ]



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


hashToPage : String -> Page
hashToPage hash =
    case hash of
        "#/" ->
            HomePage

        "" ->
            HomePage

        "#/categories" ->
            CategoriesPage

        "#/categories/new" ->
            CategoryPage

        "#/login" ->
            LoginPage

        _ ->
            NotFound


pageToHash : Page -> String
pageToHash page =
    case page of
        HomePage ->
            "#/"

        CategoriesPage ->
            "#/categories"

        CategoryPage ->
            "#/categories/new"

        LoginPage ->
            "#/login"

        NotFound ->
            "#notfound"


locationToMsg : Navigation.Location -> Msg
locationToMsg location =
    location.hash
        |> hashToPage
        |> ChangePage


type alias Flags =
    { token : Maybe String }


main : Program Flags Model Msg
main =
    Navigation.programWithFlags locationToMsg
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


port saveToken : String -> Cmd msg


port deleteToken : () -> Cmd msg
