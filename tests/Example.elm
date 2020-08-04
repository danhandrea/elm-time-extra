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


timeFormat =
    DateFormat.format "dd/MM/yyyy HH:mm:ss fff" Time.utc


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
            , describe "fromY"
                [ test "2020" <|
                    \_ ->
                        TimeExtra.fromY 2020
                            |> defaultFormat
                            |> Expect.equal "01/01/2020"
                , test "1965" <|
                    \_ ->
                        TimeExtra.fromY 1965
                            |> defaultFormat
                            |> Expect.equal "01/01/1965"
                , test "214" <|
                    \_ ->
                        TimeExtra.fromY 214
                            |> defaultFormat
                            |> Expect.equal "01/01/214"
                , test "2020 year is same as Time.getYear" <|
                    \_ ->
                        TimeExtra.fromY 2020
                            |> Time.toYear Time.utc
                            |> Expect.equal 2020
                , test "1065  is same as Time.getYear" <|
                    \_ ->
                        TimeExtra.fromY 1065
                            |> Time.toYear Time.utc
                            |> Expect.equal 1065
                ]
            , describe "fromYM"
                [ test "2020 Apr" <|
                    \_ ->
                        TimeExtra.fromYM 2020 Apr
                            |> defaultFormat
                            |> Expect.equal "01/04/2020"
                , test "1965 Apr" <|
                    \_ ->
                        TimeExtra.fromYM 1965 Apr
                            |> defaultFormat
                            |> Expect.equal "01/04/1965"
                , test "1965 Dec" <|
                    \_ ->
                        TimeExtra.fromYM 1965 Dec
                            |> defaultFormat
                            |> Expect.equal "01/12/1965"
                , test "Month same as Time.toMonth Dec" <|
                    \_ ->
                        TimeExtra.fromYM 1965 Dec
                            |> Time.toMonth Time.utc
                            |> Expect.equal Dec
                , test "Month same as Time.toMonth Jan" <|
                    \_ ->
                        TimeExtra.fromYM 2020 Jan
                            |> Time.toMonth Time.utc
                            |> Expect.equal Jan
                ]
            , describe "fromYMD"
                [ test "2020 Apr 1" <|
                    \_ ->
                        TimeExtra.fromYMD 2020 Apr 1
                            |> defaultFormat
                            |> Expect.equal "01/04/2020"
                , test "2020 Apr 30" <|
                    \_ ->
                        TimeExtra.fromYMD 2020 Apr 30
                            |> defaultFormat
                            |> Expect.equal "30/04/2020"
                , test "2020 Apr 31 should resolve to 2020 Apr 30" <|
                    \_ ->
                        TimeExtra.fromYMD 2020 Apr 30
                            |> defaultFormat
                            |> Expect.equal "30/04/2020"
                , test "1955 Dec 31" <|
                    \_ ->
                        TimeExtra.fromYMD 1955 Dec 31
                            |> defaultFormat
                            |> Expect.equal "31/12/1955"
                , test "Day same as Time.toDay 1955 Dec 31" <|
                    \_ ->
                        TimeExtra.fromYMD 1955 Dec 31
                            |> Time.toDay Time.utc
                            |> Expect.equal 31
                ]
            , describe "fromYMDH"
                [ test "1955 Dec 31 5:00" <|
                    \_ ->
                        TimeExtra.fromYMDH 1955 Dec 31 5
                            |> timeFormat
                            |> Expect.equal "31/12/1955 05:00:00 0"
                , test "Hour same as Time.toHour 1955 Dec 31 5:00" <|
                    \_ ->
                        TimeExtra.fromYMDH 1955 Dec 31 5
                            |> Time.toHour Time.utc
                            |> Expect.equal 5
                ]
            , describe "fromYMDHM"
                [ test "1955 Dec 31 7:39" <|
                    \_ ->
                        TimeExtra.fromYMDHM 1955 Dec 31 7 39
                            |> timeFormat
                            |> Expect.equal "31/12/1955 07:39:00 0"
                , test "Minute same as Time.toMinute 1955 Dec 31 7:39" <|
                    \_ ->
                        TimeExtra.fromYMDHM 1955 Dec 31 7 39
                            |> Time.toMinute Time.utc
                            |> Expect.equal 39
                ]
            , describe "fromYMDHMS"
                [ test "1955 Dec 31 7:39:59" <|
                    \_ ->
                        TimeExtra.fromYMDHMS 1955 Dec 31 7 39 59
                            |> timeFormat
                            |> Expect.equal "31/12/1955 07:39:59 0"
                , test "Second same as Time.toSecond 1955 Dec 31 7:39:59" <|
                    \_ ->
                        TimeExtra.fromYMDHMS 1955 Dec 31 7 39 59
                            |> Time.toSecond Time.utc
                            |> Expect.equal 59
                ]
            , describe "fromYMDHMSM"
                [ test "1955 Dec 31 7:39:59 597" <|
                    \_ ->
                        TimeExtra.fromYMDHMSM 1955 Dec 31 7 39 59 597
                            |> timeFormat
                            |> Expect.equal "31/12/1955 07:39:59 597"
                , test "Millis same as Time.toMillis 1955 Dec 31 7:39:59 597" <|
                    \_ ->
                        TimeExtra.fromYMDHMSM 1955 Dec 31 7 39 59 597
                            |> Time.toMillis Time.utc
                            |> Expect.equal 597
                ]
            ]
        ]
