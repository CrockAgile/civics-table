module Tests exposing (..)

import Test exposing (..)
import Fuzz exposing (string)
import Expect
import GoogleCivic exposing (..)
import Json.Decode exposing (decodeString)


decoders : Test
decoders =
    describe "Google civic data decoders"
        [ describe "stringToOpenCivicBoundary"
            [ test "Oregon boundary" <|
                \_ ->
                    stringToOpenCivicBoundary "state:or"
                        |> Expect.equal (Ok (OpenCivicBoundary "state" "or"))
            , test "4th circuit court" <|
                \_ ->
                    stringToOpenCivicBoundary "circuit_court:4"
                        |> Expect.equal (Ok (OpenCivicBoundary "circuit_court" "4"))
            , test "boundary type without identifier" <|
                \_ ->
                    stringToOpenCivicBoundary "circuit_court:"
                        |> Expect.err
            , test "identifier without boundary type" <|
                \_ ->
                    stringToOpenCivicBoundary ":12"
                        |> Expect.err
            , test "empty string" <|
                \_ ->
                    stringToOpenCivicBoundary ""
                        |> Expect.err
            ]
        , describe "OpenCivic identifier decoder"
            [ test "minimal identifier" <|
                \_ ->
                    decodeString decodeOpenCivicDataId "\"ocd-division/country:us\""
                        |> Expect.equal
                            (Ok (OpenCivicDataId "us" []))
            , test "empty identifier" <|
                \_ ->
                    decodeString decodeOpenCivicDataId "\"\""
                        |> Expect.err
            , test "Oregon 4th circuit court identifier" <|
                \_ ->
                    let
                        boundariesString =
                            "state:or/circuit_court:4"

                        isMaybeOk result =
                            case result of
                                Ok value ->
                                    Just value

                                Err _ ->
                                    Nothing

                        boundaries =
                            boundariesString
                                |> String.split "/"
                                |> List.map stringToOpenCivicBoundary
                                |> List.filterMap isMaybeOk

                        idString =
                            "\"ocd-division/country:us/"
                                ++ boundariesString
                                ++ "\""
                    in
                        decodeString decodeOpenCivicDataId idString
                            |> Expect.equal
                                (Ok (OpenCivicDataId "us" boundaries))
            , test "identifier missing country code" <|
                \_ ->
                    let
                        boundariesString =
                            "state:or/circuit_court:4"

                        isMaybeOk result =
                            case result of
                                Ok value ->
                                    Just value

                                Err _ ->
                                    Nothing

                        boundaries =
                            boundariesString
                                |> String.split "/"
                                |> List.map stringToOpenCivicBoundary
                                |> List.filterMap isMaybeOk

                        idString =
                            "\"ocd-division/country:/"
                                ++ boundariesString
                                ++ "\""
                    in
                        decodeString decodeOpenCivicDataId idString
                            |> Expect.err
            ]
        ]
