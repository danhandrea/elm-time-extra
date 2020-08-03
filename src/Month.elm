module Month exposing
    ( list
    , before, days, daysToMonth
    )

{-| Month


# Const

@docs list


# Query

@docs before, days, daysToMonth

-}

import Time exposing (Month(..))
import Util
import Year exposing (Year)


{-| list

List of months

-}
list : List Month
list =
    [ Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec ]


{-| before

Returns the list of months before

-}
before : Month -> List Month
before month =
    let
        index =
            list
                |> List.indexedMap Tuple.pair
                |> List.filterMap
                    (\( i, m ) ->
                        if m == month then
                            Just i

                        else
                            Nothing
                    )
                |> List.head
                |> Maybe.withDefault 0
    in
    list
        |> List.take index


{-| days

Returns the number of days in a month

-}
days : Year -> Month -> Int
days year month =
    case month of
        Jan ->
            31

        Feb ->
            if Util.isLeapYear year then
                29

            else
                28

        Mar ->
            31

        Apr ->
            30

        May ->
            31

        Jun ->
            30

        Jul ->
            31

        Aug ->
            31

        Sep ->
            30

        Oct ->
            31

        Nov ->
            30

        Dec ->
            31


{-| daysToMonth

Returns number of days since January 1

-}
daysToMonth : Year -> Month -> Int
daysToMonth year month =
    before month
        |> List.map (days year)
        |> List.sum
