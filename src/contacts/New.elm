module Contacts.New exposing (..)

import Html exposing (..)


type alias Model =
    { name : String
    , city : String
    }


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    text "New Contact"
