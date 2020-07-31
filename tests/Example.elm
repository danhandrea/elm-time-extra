module Example exposing (..)

import Day
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Month
import Test exposing (..)
import Time
import Util


suite : Test
suite =
    describe "Time Extra tests"
        [ describe "Day tests"
            [ test "Day has 86400000 milliseconds" <|
                \_ ->
                    Day.millis
                        |> Expect.equal 86400000
            ]
        , describe "Month tests"
            [ test "There are 12 months" <|
                \_ ->
                    List.length Month.list
                        |> Expect.equal 12
            , test "No month before January" <|
                \_ ->
                    Month.before Time.Jan
                        |> List.length
                        |> Expect.equal 0
            , test "Eleven months before December" <|
                \_ ->
                    Month.before Time.Dec
                        |> List.length
                        |> Expect.equal 11
            ]
        , describe "Util tests"
            [ describe "Leap year tests"
                [ test "2020" <|
                    \_ ->
                        Util.isLeapYear 2020
                            |> Expect.true "Expected 2020 is leap year"
                , test "1900" <|
                    \_ ->
                        Util.isLeapYear 1900
                            |> Expect.false "Expected 1900 not to be a leap year"
                ]
            ]
        ]
