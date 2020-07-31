module TimeExtra exposing
    ( epoch, fromYear
    , isLeapYear
    )

{-| TimeExtra

Extra functionality


# Creation

@docs epoch, fromYear


# Query

@docs isLeapYear

-}

import DateFormat
import Time exposing (Month(..), Posix, millisToPosix)
import Year exposing (Year)



-- CREATION


{-| epoch
-}
epoch : Posix
epoch =
    Time.millisToPosix 0


{-| fromYear

Will create a Posix date with day 1 and month 1

-}
fromYear : Year -> Posix
fromYear year =
    let
        ( years, negate ) =
            if year >= 1970 then
                ( List.range 1970 (year - 1), 1 )

            else
                ( List.range year 1969, -1 )

        millis =
            years
                |> List.map Year.millis
                |> List.sum
                |> (*) negate
    in
    Time.millisToPosix millis



-- QUERY


{-| isLeapYear
-}
isLeapYear : Int -> Bool
isLeapYear year =
    modBy 400 year == 0 || modBy 100 year /= 0 && modBy 4 year == 0



-- UTIL
