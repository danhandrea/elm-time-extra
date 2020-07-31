module Month exposing
    ( list
    , before
    )

{-| Month


# Const

@docs list


# Query

@docs before

-}

import Time exposing (Month(..))


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
