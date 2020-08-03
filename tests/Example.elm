module Example exposing (..)

import DateFormat
import Day
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Month
import Test exposing (..)
import Time exposing (Month(..))
import TimeExtra
import Util


defaultFormat =
    DateFormat.format "dd/MM/yyyy" Time.utc


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
            , test "31 days in January" <|
                \_ ->
                    Month.days 2020 Jan
                        |> Expect.equal 31
            , test "29 days in February leap year" <|
                \_ ->
                    Month.days 2020 Feb
                        |> Expect.equal 29
            , test "28 days in February non leap year" <|
                \_ ->
                    Month.days 2019 Feb
                        |> Expect.equal 28
            , test "0 days to January" <|
                \_ ->
                    Month.daysToMonth 2020 Jan
                        |> Expect.equal 0
            , test "31 days to Febryary" <|
                \_ ->
                    Month.daysToMonth 2020 Feb
                        |> Expect.equal 31
            , test "60 days to March leap year" <|
                \_ ->
                    Month.daysToMonth 2020 Mar
                        |> Expect.equal 60
            , test "335 days to December leap year" <|
                \_ ->
                    Month.daysToMonth 2020 Dec
                        |> Expect.equal 335
            , test "334 days to December non leap year" <|
                \_ ->
                    Month.daysToMonth 2019 Dec
                        |> Expect.equal 334
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
        , describe "Main library"
            [ describe "epoch"
                [ test "is correct" <|
                    \_ ->
                        TimeExtra.epoch
                            |> defaultFormat
                            |> Expect.equal "01/01/1970"
                ]
            , describe "fromYear"
                [ test "2020" <|
                    \_ ->
                        TimeExtra.fromYear 2020
                            |> defaultFormat
                            |> Expect.equal "01/01/2020"
                , test "1965" <|
                    \_ ->
                        TimeExtra.fromYear 1965
                            |> defaultFormat
                            |> Expect.equal "01/01/1965"
                , test "214" <|
                    \_ ->
                        TimeExtra.fromYear 214
                            |> defaultFormat
                            |> Expect.equal "01/01/214"
                ]
            , describe "fromYearMonth"
                [ test "2020 Apr" <|
                    \_ ->
                        TimeExtra.fromYearMonth 2020 Apr
                            |> defaultFormat
                            |> Expect.equal "01/04/2020"
                , test "1965 Apr" <|
                    \_ ->
                        TimeExtra.fromYearMonth 1965 Apr
                            |> defaultFormat
                            |> Expect.equal "01/04/1965"
                , test "1965 Dec" <|
                    \_ ->
                        TimeExtra.fromYearMonth 1965 Dec
                            |> defaultFormat
                            |> Expect.equal "01/12/1965"
                ]
            , describe "fromYearMonthDay"
                [ test "2020 Apr 1" <|
                    \_ ->
                        TimeExtra.fromYearMonthDay 2020 Apr 1
                            |> defaultFormat
                            |> Expect.equal "01/04/2020"
                , test "2020 Apr 30" <|
                    \_ ->
                        TimeExtra.fromYearMonthDay 2020 Apr 30
                            |> defaultFormat
                            |> Expect.equal "30/04/2020"
                , test "2020 Apr 31 should resolve to 2020 Apr 30" <|
                    \_ ->
                        TimeExtra.fromYearMonthDay 2020 Apr 30
                            |> defaultFormat
                            |> Expect.equal "30/04/2020"
                , test "1955 Dec 31" <|
                    \_ ->
                        TimeExtra.fromYearMonthDay 1955 Dec 31
                            |> defaultFormat
                            |> Expect.equal "31/12/1955"
                ]
            ]
        ]
