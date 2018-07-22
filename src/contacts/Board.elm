module Contacts.Board exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Model =
    { contacts : List Contact
    , query : String
    , error : Maybe String
    }


type alias Contact =
    { id : Int
    , name : String
    , city : String
    }


tempContacts : List Contact
tempContacts =
    [ Contact 1 "Douglas Rafael Roeder" "Sydney"
    , Contact 2 "Vanessa Roeder" "Sydney"
    ]


initModel : Model
initModel =
    { contacts = tempContacts
    , query = ""
    , error = Nothing
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )


type Msg
    = SearchInput String
    | Search


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SearchInput query ->
            ( { model | query = query }, Cmd.none )

        Search ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "main" ]
        [ errorPanel model.error
        , searchForm model.query
        , contacts model
        ]


errorPanel : Maybe String -> Html a
errorPanel error =
    case error of
        Nothing ->
            text ""

        Just msg ->
            div [ class "error" ]
                [ text msg
                , button [ type_ "button" ] [ text "×" ]
                ]


searchForm : String -> Html Msg
searchForm query =
    Html.form [ onSubmit Search ]
        [ input
            [ type_ "text"
            , placeholder "Search for contact..."
            , value query
            , onInput SearchInput
            ]
            []
        , button [ type_ "submit" ] [ text "Search" ]
        ]


contacts : Model -> Html Msg
contacts { contacts } =
    contacts
        |> List.map contact
        |> tbody []
        |> (\r -> contactsHeader :: [ r ])
        |> table []


contact : Contact -> Html Msg
contact { name, city } =
    tr []
        [ td [] [ text name ]
        , td [] [ text city ]
        ]


contactsHeader : Html Msg
contactsHeader =
    thead []
        [ tr []
            [ th [] [ text "Name" ]
            , th [] [ text "City" ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
