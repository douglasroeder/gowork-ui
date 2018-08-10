module Page.Categories exposing (Model, Msg, init, update, subscriptions, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Http exposing (..)
import Route exposing (Route(..))
import Data.Category exposing (Category)


type alias Model =
    { categories : List Category
    , error : Maybe String
    }


initModel : Model
initModel =
    { categories = []
    , error = Nothing
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, fetchCategories )


type Msg
    = FetchCategories
    | CategoriesResponse (Result Http.Error (List Category))


apiUrl : String
apiUrl =
    "http://localhost:8080/v1/categories"


fetchCategories : Cmd Msg
fetchCategories =
    let
        req =
            Http.get apiUrl Data.Category.listDecoder
    in
        Http.send CategoriesResponse <| req


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchCategories ->
            model ! [ fetchCategories ]

        CategoriesResponse res ->
            case res of
                Ok categories ->
                    ( { model
                        | categories = categories
                        , error = Nothing
                      }
                    , Cmd.none
                    )

                Err error ->
                    ( { model
                        | error = Just "Error calling API"
                      }
                    , Cmd.none
                    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Categories" ]
        , renderCategories model.categories
        ]


renderCategories : List Category -> Html Msg
renderCategories categories =
    categories
        |> List.map category
        |> tbody []
        |> (\r -> categoriesHeader :: [ r ])
        |> table []


category : Category -> Html Msg
category { id, name } =
    tr []
        [ td [] [ text (toString id) ]
        , td [] [ text name ]
        , td []
            [ Route.linkTo
                (CategoryEditRoute id)
                [ class "btn btn-default" ]
                [ text "edit" ]
            ]
        ]


categoriesHeader : Html Msg
categoriesHeader =
    thead []
        [ tr []
            [ th [] [ text "ID" ]
            , th [] [ text "Name" ]
            , th [] []
            ]
        ]
