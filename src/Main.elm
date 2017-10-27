module Main exposing (main)

import Color
import Html exposing (Html)
import Element exposing (..)
import Element.Attributes exposing (..)
import Style exposing (..)
import Style.Color as Color
import GoogleCivic


type alias InputColumn =
    { text : String
    }


type alias ComputedColumn =
    { text : String
    }


type Column
    = Input InputColumn
    | Computed ComputedColumn


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


type Styles
    = None
    | Page
    | Header
    | Table
    | CsvButton


stylesheet : Style.StyleSheet Styles variation
stylesheet =
    Style.styleSheet
        [ style None
            []
        , style Page
            []
        , style Header
            []
        , style Table
            []
        , style CsvButton
            []
        ]


view : Model -> Html Msg
view model =
    layout stylesheet <|
        column Page
            []
            [ header model
            , table model
            ]


header : Model -> Element Styles variation msg
header model =
    row Header
        [ spread, paddingXY 20 5 ]
        [ el None [] (text "Civics Table")
        , el None [] (text "Buttons")
        ]


table : Model -> Element Styles variation msg
table model =
    el Table [] (text "table here")


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
