module Data.ApiResult exposing (ApiResult)


type alias ApiResult a =
    { statusCode : Int
    , errors : List String
    , payload : a
    }
