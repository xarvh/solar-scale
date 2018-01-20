module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (type_, value)
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
    , { name = "Mercury", radius = Just 2440, distance = 5.79e10 }
    , { name = "Venus", radius = Just 6052, distance = 1.08e11 }
    , { name = "Earth", radius = Just 6378, distance = 1.5e11 }
    , { name = "Mars", radius = Just 3397, distance = 2.28e11 }
    , { name = "Jupiter", radius = Just 71492, distance = 7.78e11 }
    , { name = "Saturn", radius = Just 60268, distance = 1.43e12 }
    , { name = "Uranus", radius = Just 25559, distance = 2.87e12 }
    , { name = "Neptune", radius = Just 24766, distance = 4.5e12 }
    , { name = "Pluto", radius = Just 1150, distance = 5.91e12 }
    , { name = "Kuiper Belt (inner edge)", radius = Nothing, distance = 4.5e12 }
    , { name = "Kuiper Belt (outer edge)", radius = Nothing, distance = 7.5e12 }
    , { name = "Alpha Centauri", radius = Nothing, distance = 4.07e16 }
    ]



-- App


type alias Model =
    { selectedSunRadiusAsString : String
    , unit : String
    }


type Msg
    = ChangeUnit String
    | ChangeSize String



-- init


init =
    { selectedSunRadiusAsString = ""
    , unit = "cm"
    }



-- update


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeUnit unit ->
            { model | unit = unit }

        ChangeSize s ->
            { model | selectedSunRadiusAsString = s }



-- view


type Status
    = NoInput
    | BadNumber
    | Number Float


viewRow : Float -> String -> Object -> Html Msg
viewRow selectedSunRadius unit obj =
    let
        scale n =
            n * selectedSunRadius / realSunRadius

        radiusText =
            case obj.radius of
                Nothing ->
                    ""

                Just r ->
                    toString (scale r) ++ unit

        distanceText =
            toString (scale obj.distance) ++ unit
    in
    tr
        []
        [ td [] [ text obj.name ]
        , td [] [ text radiusText ]
        , td [] [ text distanceText ]
        ]


viewObject : Status -> String -> Object -> Html Msg
viewObject status unit obj =
    case status of
        BadNumber ->
            text ""

        NoInput ->
            viewRow realSunRadius "m" obj

        Number radius ->
            viewRow radius unit obj


view : Model -> Html Msg
view model =
    let
        status =
            if model.selectedSunRadiusAsString == "" then
                NoInput
            else
                case String.toFloat model.selectedSunRadiusAsString of
                    Err msg ->
                        BadNumber

                    Ok n ->
                        Number n
    in
    div
        []
        [ div
            []
            [ text "If the Sun had a radius of "
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
            []
            [ text "then the objects in the Solar System would have:" ]
        , table
            []
            [ thead
                []
                [ tr
                    []
                    [ th [] [ text "Name" ]
                    , th [] [ text "Radius" ]
                    , th [] [ text "Distance from the Sun" ]
                    ]
                ]
            , tbody
                []
                (List.map (viewObject status model.unit) objects)
            ]
        ]



-- Main


main =
    Html.beginnerProgram
        { model = init
        , update = update
        , view = view
        }
