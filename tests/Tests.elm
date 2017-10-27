module Tests exposing (..)

import Test exposing (..)
import Fuzz exposing (string)
import Expect
import GoogleCivic exposing (..)
import Json.Decode exposing (decodeString)


decoders : Test
decoders =
    describe "Google civic data decoders"
        [ describe "OpenCivic boundary decoder"
            [ test "Oregon boundary" <|
                \_ ->
                    decodeString decodeOpenCivicBoundary "\"state:or\""
                        |> Expect.equal (Ok (OpenCivicBoundary "state" "or"))
            , test "4th circuit court" <|
                \_ ->
                    decodeString decodeOpenCivicBoundary "\"circuit_court:4\""
                        |> Expect.equal (Ok (OpenCivicBoundary "circuit_court" "4"))
            , test "boundary type without identifier" <|
                \_ ->
                    decodeString decodeOpenCivicBoundary "\"circuit_court:\""
                        |> Expect.err
            , test "identifier without boundary type" <|
                \_ ->
                    decodeString decodeOpenCivicBoundary "\":12\""
                        |> Expect.err
            , test "empty string" <|
                \_ ->
                    decodeString decodeOpenCivicBoundary "\"\""
                        |> Expect.err
            ]
        ]
