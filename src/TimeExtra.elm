module TimeExtra exposing
    ( epoch, fromY, fromYM, fromYMD
    , fromYMDH, fromYMDHM, fromYMDHMS, fromYMDHMSM
    , isLeapYear, daysInMonth
    )

{-| TimeExtra

Extra functionality for Posix

All computations use `Time.utc`


# Creation

@docs epoch, fromY, fromYM, fromYMD
@docs fromYMDH, fromYMDHM, fromYMDHMS, fromYMDHMSM


# Query

@docs isLeapYear, daysInMonth

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


setYear : Year -> Posix -> Posix
setYear year time =
    let
        tyM =
            Time.toYear Time.utc time
                |> y_

        tM =
            p_ time

        nyM =
            y_ year
    in
    Time.millisToPosix <| tM - tyM + nyM


setMonth : Month -> Posix -> Posix
setMonth month time =
    let
        year =
            Time.toYear Time.utc time

        oldMonth =
            Time.toMonth Time.utc time

        tM =
            p_ time

        cmM =
            m_ year oldMonth

        nmM =
            m_ year month
    in
    Time.millisToPosix <| tM - cmM + nmM


setDay : Day -> Posix -> Posix
setDay day time =
    let
        year =
            Time.toYear Time.utc time

        month =
            Time.toMonth Time.utc time

        oldDay =
            Time.toDay Time.utc time

        newDay =
            clamp 1 (Month.days year month) day

        tM =
            p_ time

        cdM =
            d_ year month oldDay

        ndM =
            d_ year month newDay
    in
    Time.millisToPosix <| tM - cdM + ndM



-- QUERY


{-| isLeapYear
-}
isLeapYear : Int -> Bool
isLeapYear =
    Util.isLeapYear


{-| daysInMonth

    returns the number of days in a month

-}
daysInMonth : Year -> Month -> Int
daysInMonth =
    Month.days



-- PRIVATE


p_ : Posix -> Millis
p_ time =
    let
        year =
            Time.toYear Time.utc time

        month =
            Time.toMonth Time.utc time

        day =
            Time.toDay Time.utc time

        hour =
            Time.toHour Time.utc time

        minute =
            Time.toMinute Time.utc time

        second =
            Time.toSecond Time.utc time

        millis =
            Time.toMillis Time.utc time
    in
    ymdhmsm_ year month day hour minute second millis


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
    y_ year + m_ year month


m_ : Year -> Month -> Millis
m_ year month =
    Month.daysToMonth year month
        |> (*) Day.millis


ymd_ : Year -> Month -> Day -> Millis
ymd_ year month day =
    ym_ year month + d_ year month day


d_ : Year -> Month -> Day -> Millis
d_ year month day =
    let
        clamped =
            clamp 1 (Month.days year month) day
    in
    (clamped - 1) * Day.millis


ymdh_ : Year -> Month -> Day -> Hour -> Millis
ymdh_ year month day hour =
    ymd_ year month day + h_ year month day hour


h_ : Year -> Month -> Day -> Hour -> Millis
h_ year month day hour =
    let
        cHour =
            clamp 0 23 hour
    in
    cHour * Hour.millis


ymdhm_ : Year -> Month -> Day -> Hour -> Minute -> Millis
ymdhm_ year month day hour minute =
    ymdh_ year month day hour + min_ year month day hour minute


min_ : Year -> Month -> Day -> Hour -> Minute -> Millis
min_ year month day hour minute =
    let
        cMinute =
            clamp 0 59 minute
    in
    cMinute * Minute.millis


ymdhms_ : Year -> Month -> Day -> Hour -> Minute -> Second -> Millis
ymdhms_ year month day hour minute second =
    ymdhm_ year month day hour minute + s_ year month day hour minute second


s_ : Year -> Month -> Day -> Hour -> Minute -> Second -> Millis
s_ year month day hour minute second =
    let
        cSecond =
            clamp 0 59 second
    in
    cSecond * Second.millis


ymdhmsm_ : Year -> Month -> Day -> Hour -> Minute -> Second -> Millis -> Millis
ymdhmsm_ year month day hour minute second millis =
    ymdhms_ year month day hour minute second + ms_ year month day hour minute second millis


ms_ : Year -> Month -> Day -> Hour -> Minute -> Second -> Millis -> Millis
ms_ year month day hour minute second millis =
    clamp 0 999 millis
