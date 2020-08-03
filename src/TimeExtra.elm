module TimeExtra exposing
    ( epoch, fromYear, fromYearMonth, fromYearMonthDay
    , isLeapYear
    )

{-| TimeExtra

Extra functionality for Posix

All computations use `Time.utc`


# Creation

@docs epoch, fromYear, fromYearMonth, fromYearMonthDay


# Query

@docs isLeapYear

-}

import DateFormat
import Day
import Month
import Time exposing (Month(..), Posix)
import Util
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
    yearMillis_ year
        |> Time.millisToPosix


{-| fromYearMonth

Will create a Posix date with day 1

-}
fromYearMonth : Year -> Month -> Posix
fromYearMonth year month =
    yearMonthMillis_ year month
        |> Time.millisToPosix


{-| fromYearMonthDay

Will crate a Posix date

-}
fromYearMonthDay : Year -> Month -> Int -> Posix
fromYearMonthDay year month day =
    yearMonthDayMillis_ year month day
        |> Time.millisToPosix



-- TRANSOFORM


setMonth : Posix -> Month -> Posix
setMonth time month =
    Debug.todo "setMonth"



-- QUERY


{-| isLeapYear
-}
isLeapYear : Int -> Bool
isLeapYear =
    Util.isLeapYear



-- PRIVATE


yearMillis : Posix -> Int
yearMillis time =
    let
        year =
            Time.toYear Time.utc time

        millis =
            yearMillis_ year
    in
    millis


yearMillis_ : Year -> Int
yearMillis_ year =
    let
        ( years, negate ) =
            if year >= 1970 then
                ( List.range 1970 (year - 1), 1 )

            else
                ( List.range year 1969, -1 )
    in
    years
        |> List.map Year.millis
        |> List.sum
        |> (*) negate


yearMonthMillis_ : Year -> Month -> Int
yearMonthMillis_ year month =
    let
        yearM =
            yearMillis_ year

        monthM =
            Month.daysToMonth year month
                |> (*) Day.millis
    in
    yearM + monthM


yearMonthDayMillis_ : Year -> Month -> Int -> Int
yearMonthDayMillis_ year month day =
    let
        ymM =
            yearMonthMillis_ year month

        clamped =
            clamp 1 (Month.days year month) day
    in
    ymM + (clamped - 1) * Day.millis
