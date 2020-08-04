module TimeExtra exposing
    ( epoch, fromY, fromYM, fromYMD
    , fromYMDH, fromYMDHM, fromYMDHMS, fromYMDHMSM
    , isLeapYear
    )

{-| TimeExtra

Extra functionality for Posix

All computations use `Time.utc`


# Creation

@docs epoch, fromY, fromYM, fromYMD
@docs fromYMDH, fromYMDHM, fromYMDHMS, fromYMDHMSM


# Query

@docs isLeapYear

-}

import Day exposing (Day)
import Hour exposing (Hour)
import Millis exposing (Millis)
import Minute exposing (Minute)
import Month
import Second exposing (Second)
import Time exposing (Month(..), Posix)
import Util
import Year exposing (Year)



-- CREATION


{-| epoch
-}
epoch : Posix
epoch =
    Time.millisToPosix 0


{-| fromY

Will create a Posix date with day 1 and month 1

-}
fromY : Year -> Posix
fromY year =
    y_ year
        |> Time.millisToPosix


{-| fromYM

Will create a Posix date with day 1

-}
fromYM : Year -> Month -> Posix
fromYM year month =
    ym_ year month
        |> Time.millisToPosix


{-| fromYMD

Will crate a Posix date

-}
fromYMD : Year -> Month -> Day -> Posix
fromYMD year month day =
    ymd_ year month day
        |> Time.millisToPosix


{-| fromYMDH

Will create a Posix date

-}
fromYMDH : Year -> Month -> Day -> Hour -> Posix
fromYMDH year month day hour =
    ymdh_ year month day hour
        |> Time.millisToPosix


{-| fromYMDHM

Will create a Posix date

-}
fromYMDHM : Year -> Month -> Day -> Hour -> Minute -> Posix
fromYMDHM year month day hour minute =
    ymdhm_ year month day hour minute
        |> Time.millisToPosix


{-| fromYMDHMS

Will create a Posix date

-}
fromYMDHMS : Year -> Month -> Day -> Hour -> Minute -> Second -> Posix
fromYMDHMS year month day hour minute second =
    ymdhms_ year month day hour minute second
        |> Time.millisToPosix


{-| fromYMDHMSM

Will create a Posix date

-}
fromYMDHMSM : Year -> Month -> Day -> Hour -> Minute -> Second -> Millis -> Posix
fromYMDHMSM year month day hour minute second millis =
    ymdhmsm_ year month day hour minute second millis
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


y_ : Year -> Millis
y_ year =
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


ym_ : Year -> Month -> Millis
ym_ year month =
    let
        yearM =
            y_ year

        monthM =
            Month.daysToMonth year month
                |> (*) Day.millis
    in
    yearM + monthM


ymd_ : Year -> Month -> Day -> Millis
ymd_ year month day =
    let
        ymM =
            ym_ year month

        clamped =
            clamp 1 (Month.days year month) day
    in
    ymM + (clamped - 1) * Day.millis


ymdh_ : Year -> Month -> Day -> Hour -> Millis
ymdh_ year month day hour =
    let
        ymdM =
            ymd_ year month day

        cHour =
            clamp 0 23 hour

        hM =
            cHour * Hour.millis
    in
    ymdM + hM


ymdhm_ : Year -> Month -> Day -> Hour -> Minute -> Millis
ymdhm_ year month day hour minute =
    let
        ymdhM =
            ymdh_ year month day hour

        cMinute =
            clamp 0 59 minute

        mM =
            cMinute * Minute.millis
    in
    ymdhM + mM


ymdhms_ : Year -> Month -> Day -> Hour -> Minute -> Second -> Millis
ymdhms_ year month day hour minute second =
    let
        ymdhmM =
            ymdhm_ year month day hour minute

        cSecond =
            clamp 0 59 second

        sM =
            cSecond * Second.millis
    in
    ymdhmM + sM


ymdhmsm_ : Year -> Month -> Day -> Hour -> Minute -> Second -> Millis -> Millis
ymdhmsm_ year month day hour minute second millis =
    let
        ymdhmsM =
            ymdhms_ year month day hour minute second

        cMillis =
            clamp 0 999 millis
    in
    ymdhmsM + cMillis
