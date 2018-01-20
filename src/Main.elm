module Main exposing (..)

import FormatNumber
import FormatNumber.Locales
import Html exposing (..)
import Html.Attributes exposing (class, type_, value)
import Html.Events


-- Solar system


type alias Object =
    { name : String

    -- radius, in meters
    , radius : Maybe Float

    -- distance from the sun, in meters
    , distance : Float
    }


realSunRadius : Float
realSunRadius =
    6.95e8


objects : List Object
objects =
    [ { name = "Sun", radius = Just realSunRadius, distance = 0 }
    , { name = "Mercury", radius = Just 2.44e6, distance = 5.79e10 }
    , { name = "Venus", radius = Just 6.052e6, distance = 1.08e11 }
    , { name = "Earth", radius = Just 6.378e6, distance = 1.5e11 }
    , { name = "Mars", radius = Just 3.397e6, distance = 2.28e11 }
    , { name = "Jupiter", radius = Just 7.1492e7, distance = 7.78e11 }
    , { name = "Saturn", radius = Just 6.0268e7, distance = 1.43e12 }
    , { name = "Uranus", radius = Just 2.5559e7, distance = 2.87e12 }
    , { name = "Neptune", radius = Just 2.4766e7, distance = 4.5e12 }
    , { name = "Pluto", radius = Just 1.15e6, distance = 5.91e12 }
    , { name = "Kuiper Belt (inner edge)", radius = Nothing, distance = 4.5e12 }
    , { name = "Kuiper Belt (outer edge)", radius = Nothing, distance = 7.5e12 }
    , { name = "Alpha Centauri", radius = Nothing, distance = 4.07e16 }
    ]



-- App


type alias Model =
    { selectedSunDiameterAsString : String
    , unit : String
    }


type Msg
    = ChangeUnit String
    | ChangeSize String



-- init


init =
    { selectedSunDiameterAsString = ""
    , unit = "cm"
    }



-- update


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeUnit unit ->
            { model | unit = unit }

        ChangeSize s ->
            { model | selectedSunDiameterAsString = s }



-- view


formatNumber =
    FormatNumber.format FormatNumber.Locales.usLocale


type Status
    = NoInput
    | BadNumber
    | Radius Float


viewRow : Float -> String -> Object -> Html Msg
viewRow selectedSunRadius unit obj =
    let
        scale n =
            n * selectedSunRadius / realSunRadius

        diameterText =
            case obj.radius of
                Nothing ->
                    ""

                Just r ->
                    formatNumber (scale r * 2) ++ unit

        distanceText =
            formatNumber (scale obj.distance) ++ unit
    in
    tr
        []
        [ td [ class "name" ] [ text obj.name ]
        , td [ class "range" ] [ text diameterText ]
        , td [ class "range" ] [ text distanceText ]
        ]


viewObject : Status -> String -> Object -> Html Msg
viewObject status unit obj =
    case status of
        BadNumber ->
            text ""

        NoInput ->
            viewRow (realSunRadius / 1000) "km" obj

        Radius radius ->
            viewRow radius unit obj


view : Model -> Html Msg
view model =
    let
        status =
            if model.selectedSunDiameterAsString == "" then
                NoInput
            else
                case String.toFloat model.selectedSunDiameterAsString of
                    Err msg ->
                        BadNumber

                    Ok n ->
                        Radius (n / 2)
    in
    div
        [ class "root" ]
        [ div
            []
            [ div
                [ class "mt" ]
                [ text "If the Sun had a diameter of "
                , span [] []
                , input
                    [ type_ "number"
                    , Html.Events.onInput ChangeSize
                    ]
                    []
                , select
                    [ Html.Events.onInput ChangeUnit
                    , Html.Attributes.disabled (status == NoInput || status == BadNumber)
                    ]
                    [ option [ value "cm" ] [ text "cm" ]
                    , option [ value "m" ] [ text "m" ]
                    , option [ value "in" ] [ text "in" ]
                    ]
                ]
            , div
                [ class "mt" ]
                [ text "...then the objects in the Solar System would have:" ]
            , table
                [ class "mt" ]
                [ thead
                    []
                    [ tr
                        []
                        [ th [] [ text "Name" ]
                        , th [] [ text "Diameter" ]
                        , th [] [ text "Distance from the Sun" ]
                        ]
                    ]
                , tbody
                    []
                    (List.map (viewObject status model.unit) objects)
                ]
            ]
        ]



-- Main


main =
    Html.beginnerProgram
        { model = init
        , update = update
        , view = view
        }
