module Year exposing
    ( Year
    , days, millis
    )

{-| Year


# Model

@docs Year


# Query

@docs days, millis

-}

import Day
import Util


{-| Year
-}
type alias Year =
    Int


{-| days

Number of days in year

-}
days : Year -> Int
days year =
    if Util.isLeapYear year then
        366

    else
        365


{-| millis

Number of milliseconds in year

-}
millis : Year -> Int
millis year =
    days year * Day.millis
