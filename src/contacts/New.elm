module Contacts.New exposing (..)

import Html exposing (..)


type alias Model =
    { name : String
    , city : String
    }


type Msg
    = NoOp


initModel : Model
initModel =
    { name = ""
    , city = ""
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    text "New Contact"
