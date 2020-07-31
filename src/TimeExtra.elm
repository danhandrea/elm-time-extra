module TimeExtra exposing
    ( epoch
    , isLeapYear
    )

{-| TimeExtra

Extra functionality


# Creatuon

@docs epoch


# Query

@docs isLeapYear

-}

import Time exposing (Posix)



-- CREATION


{-| epoch
-}
epoch : Posix
epoch =
    Time.millisToPosix 0



-- QUERY


{-| isLeapYear
-}
isLeapYear : Int -> Bool
isLeapYear year =
    modBy 400 year == 0 || modBy 100 year /= 0 && modBy 4 year == 0
