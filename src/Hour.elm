module Hour exposing
    ( Hour
    , millis
    )

{-| Hour


# Model

@docs Hour


# Query

@docs millis

-}


type alias Hour =
    Int


{-| millis

Returns the number of milliseconds

-}
millis : Int
millis =
    3600000
