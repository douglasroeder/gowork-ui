port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Navigation
import Contacts.Board
import Contacts.New
import Login


-- model


type alias Model =
    { page : Page
    , contactsBoardModel : Contacts.Board.Model
    , contactsNewModel : Contacts.New.Model
    , login : Login.Model
    , token : Maybe String
    , loggedIn : Bool
    }


type Page
    = NotFound
    | HomePage
    | LoginPage
    | ContactsBoardPage
    | ContactsNewPage


authPages : List Page
authPages =
    [ ContactsBoardPage
    , ContactsNewPage
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

        ( contactsboardInitModel, contactsboardCmd ) =
            Contacts.Board.init

        ( contactsNewInitModel, contactsNewCmd ) =
            Contacts.New.init

        ( loginInitModel, loginCmd ) =
            Login.init

        initModel =
            { page = updatedPage
            , contactsBoardModel = contactsboardInitModel
            , contactsNewModel = contactsNewInitModel
            , login = loginInitModel
            , token = flags.token
            , loggedIn = loggedIn
            }

        cmds =
            Cmd.batch
                [ Cmd.map ContactsBoardMsg contactsboardCmd
                , Cmd.map ContactsNewMsg contactsNewCmd
                , Cmd.map LoginMsg loginCmd
                , cmd
                ]
    in
        ( initModel, cmds )



-- update


type Msg
    = Navigate Page
    | ChangePage Page
    | ContactsBoardMsg Contacts.Board.Msg
    | ContactsNewMsg Contacts.New.Msg
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

        ContactsBoardMsg msg ->
            let
                ( contactsBoardModel, cmd ) =
                    Contacts.Board.update msg model.contactsBoardModel
            in
                ( { model | contactsBoardModel = contactsBoardModel }
                , Cmd.map ContactsBoardMsg cmd
                )

        ContactsNewMsg msg ->
            let
                ( contactsNewModel, cmd ) =
                    Contacts.New.update msg model.contactsNewModel
            in
                ( { model | contactsNewModel = contactsNewModel }
                , Cmd.map ContactsNewMsg cmd
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

                ContactsBoardPage ->
                    Html.map ContactsBoardMsg
                        (Contacts.Board.view model.contactsBoardModel)

                ContactsNewPage ->
                    Html.map ContactsNewMsg
                        (Contacts.New.view model.contactsNewModel)

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
            [ pageHeader model
            , page
            ]


contactsBoardLinkView : Model -> Html Msg
contactsBoardLinkView { loggedIn } =
    if loggedIn then
        a [ onClick (Navigate ContactsBoardPage) ]
            [ text "Contacts" ]
    else
        text ""


contactsNewLinkView : Model -> Html Msg
contactsNewLinkView { loggedIn } =
    if loggedIn then
        a [ onClick (Navigate ContactsNewPage) ]
            [ text "New Contact" ]
    else
        text ""


pageHeader : Model -> Html Msg
pageHeader model =
    header []
        [ a [ onClick (Navigate HomePage) ] [ text "Home" ]
        , ul []
            [ li []
                [ contactsBoardLinkView model
                ]
            , li []
                [ contactsNewLinkView model
                ]
            ]
        , ul []
            [ li []
                [ authHeaderView model
                ]
            ]
        ]


authHeaderView : Model -> Html Msg
authHeaderView model =
    if model.loggedIn then
        a [ href "#", onClick Logout ] [ text "Logout" ]
    else
        a [ href "#/login" ]
            [ text "Login" ]



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        contactsBoardSub =
            Contacts.Board.subscriptions model.contactsBoardModel
    in
        Sub.batch
            [ Sub.map ContactsBoardMsg contactsBoardSub
            ]


hashToPage : String -> Page
hashToPage hash =
    case hash of
        "#/" ->
            HomePage

        "" ->
            HomePage

        "#/contacts" ->
            ContactsBoardPage

        "#/contacts/new" ->
            ContactsNewPage

        "#/login" ->
            LoginPage

        _ ->
            NotFound


pageToHash : Page -> String
pageToHash page =
    case page of
        HomePage ->
            "#/"

        ContactsBoardPage ->
            "#/contacts"

        ContactsNewPage ->
            "#/contacts/new"

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
