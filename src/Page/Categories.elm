module Page.Categories
    exposing
        ( Model
        , Msg
        , initModel
        , mount
        , update
        , subscriptions
        , view
        )

import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import Route exposing (Route(..))
import Data.Category exposing (Category, CategoryList, APIResult)
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http


type alias Model =
    { categories : WebData (APIResult CategoryList)
    , error : Maybe String
    }


initModel : Model
initModel =
    { categories = Loading
    , error = Nothing
    }


mount : Model -> ( Model, Cmd Msg )
mount model =
    ( initModel, fetchCategories )


type Msg
    = ClickAddCategory
    | FetchCategories
    | HandleCategoriesResponse (WebData (APIResult CategoryList))


apiUrl : String
apiUrl =
    "http://localhost:8080/v1/categories"


fetchCategories : Cmd Msg
fetchCategories =
    RemoteData.Http.get apiUrl HandleCategoriesResponse Data.Category.apiResultDecoder


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickAddCategory ->
            model ! [ Route.goto CategoryAddRoute ]

        FetchCategories ->
            ( { model | categories = Loading }, fetchCategories )

        HandleCategoriesResponse data ->
            ( { model | categories = data }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div [ class "col-md-12" ]
        [ renderApiError model.error
        , div [ class "bgc-white bd bdrs-3 p-20" ]
            [ div [ class "peers ai-c" ]
                [ div [ class "peer peer-greed" ]
                    [ h4 [ class "c-grey-900 mB-20" ] [ text "Categories" ] ]
                , div [ class "peer" ]
                    [ button [ class "btn btn-primary", onClick ClickAddCategory ] [ text "Add" ] ]
                ]
            , div [ class "mT-30" ]
                [ renderCategories model.categories ]
            ]
        ]


renderApiError : Maybe String -> Html Msg
renderApiError maybeError =
    case maybeError of
        Just error ->
            div [ class "alert alert-danger" ] [ text error ]

        Nothing ->
            text ""


renderCategories : WebData (APIResult CategoryList) -> Html Msg
renderCategories data =
    case data of
        Loading ->
            text "Fetching categories..."

        Success result ->
            result.payload
                |> List.map category
                |> tbody []
                |> (\r -> categoriesHeader :: [ r ])
                |> table [ class "table table-hover" ]

        Failure error ->
            text "Failed to fetch data"

        NotAsked ->
            text ""


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
