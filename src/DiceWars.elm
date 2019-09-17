module Main exposing (Model, Msg(..), init, main, update, view)

import Array exposing (Array)
import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type Valid
    = OK
    | WrongSum
    | WrongFormat
    | WrongNever


type alias Dot =
    { number : Int
    , text : String
    , valid : Valid
    }


type alias Die =
    { dots : Array Dot
    , name : String
    , valid : Valid
    }


type alias Model =
    { numPlayers : Int
    , numSides : Int
    , sumSides : Int
    , dice : Array Die
    , valid : Valid
    }


init : Model
init =
    { numPlayers = 0
    , numSides = 6
    , sumSides = 21
    , dice = Array.fromList []
    , valid = OK
    }



-- UPDATE


mergeValidity : Valid -> Valid -> Valid
mergeValidity v1 v2 =
    case ( v1, v2 ) of
        ( WrongNever, _ ) ->
            WrongNever

        ( _, WrongNever ) ->
            WrongNever

        ( WrongFormat, _ ) ->
            WrongFormat

        ( _, WrongFormat ) ->
            WrongFormat

        ( WrongSum, _ ) ->
            WrongSum

        ( _, WrongSum ) ->
            WrongSum

        ( OK, OK ) ->
            OK


updateValidityDice : Model -> Model
updateValidityDice model =
    { model
        | dice =
            Array.map
                (\die ->
                    { die
                        | valid =
                            mergeValidity
                                (Array.foldl
                                    mergeValidity
                                    OK
                                    (Array.map (\dot -> dot.valid) die.dots)
                                )
                                (if model.sumSides == Array.foldl (+) 0 (Array.map (\dot -> dot.number) die.dots) then
                                    OK

                                 else
                                    WrongSum
                                )
                    }
                )
                model.dice
    }


updateValidityModel : Model -> Model
updateValidityModel model =
    { model
        | valid = Array.foldl mergeValidity OK (Array.map (\die -> die.valid) model.dice)
    }


updateValidity : Model -> Model
updateValidity model =
    model
        |> updateValidityDice
        |> updateValidityModel


type Msg
    = IncNumSides
    | DecNumSides
    | IncSumSides
    | DecSumSides
    | AddPlayer
    | DeletePlayer Int
    | SetName Int String
    | SetDots Int Int String
    | Battle
    | NoAction


update : Msg -> Model -> Model
update msg model =
    case msg of
        IncNumSides ->
            { model
                | numSides = model.numSides + 1
                , dice =
                    Array.map
                        (\die ->
                            { die
                                | dots = Array.fromList [ Dot 0 "0" OK ] |> Array.append die.dots
                            }
                        )
                        model.dice
            }

        DecNumSides ->
            { model
                | numSides = Basics.max 0 <| model.numSides - 1
            }
                |> (\m ->
                        { m
                            | dice =
                                Array.map
                                    (\die ->
                                        { die
                                            | dots = Array.slice 0 m.numSides die.dots
                                        }
                                    )
                                    m.dice
                        }
                   )
                |> updateValidity

        IncSumSides ->
            { model | sumSides = model.sumSides + 1 }
                |> updateValidity

        DecSumSides ->
            { model | sumSides = model.sumSides - 1 }
                |> updateValidity

        AddPlayer ->
            { model
                | numPlayers = model.numPlayers + 1
                , dice =
                    [ { name = "Player " ++ String.fromInt (model.numPlayers + 1)
                      , dots =
                            Array.repeat model.numSides <|
                                Dot 0
                                    "0"
                                    OK
                      , valid =
                            if model.sumSides == 0 then
                                OK

                            else
                                WrongSum
                      }
                    ]
                        |> Array.fromList
                        |> Array.append model.dice
            }
                |> updateValidity

        DeletePlayer idPlayer ->
            { model
                | numPlayers = model.numPlayers - 1
                , dice =
                    Array.append
                        (model.dice |> Array.slice 0 idPlayer)
                        (model.dice |> Array.slice (idPlayer + 1) model.numPlayers)
            }
                |> updateValidityModel

        SetName idPlayer newName ->
            { model
                | dice =
                    Array.set idPlayer
                        (case Array.get idPlayer model.dice of
                            Just die ->
                                { die | name = newName }

                            Nothing ->
                                Die (Array.fromList []) "" WrongNever
                        )
                        model.dice
            }

        SetDots idPlayer idDot newDot ->
            { model
                | dice =
                    case Array.get idPlayer model.dice of
                        Just die ->
                            Array.set idPlayer
                                { die
                                    | dots =
                                        Array.set idDot
                                            (case String.toInt newDot of
                                                Just num ->
                                                    Dot num newDot OK

                                                Nothing ->
                                                    Dot 0 newDot WrongFormat
                                            )
                                            die.dots
                                }
                                model.dice

                        Nothing ->
                            model.dice
            }
                |> updateValidity

        Battle ->
            model

        NoAction ->
            model



-- VIEW


white =
    Element.rgb 1 1 1


grey =
    Element.rgb 0.9 0.9 0.9


blue =
    Element.rgb 0 0 0.8


red =
    Element.rgb 0.8 0 0


darkBlue =
    Element.rgb 0 0 0.9


green =
    Element.rgb 0 0.8 0


purple =
    Element.rgb 0.8 0 0.8


colorWrongFormat =
    Element.rgb 0.9 0 0


colorWrongSum =
    Element.rgb 1 0.5 0


buttonStyle =
    [ Background.color blue
    , Font.color white
    , Border.color darkBlue
    , paddingXY 32 16
    , Border.rounded 3
    ]


inactiveButtonStyle =
    [ Background.color grey
    , Font.color white
    , Border.color darkBlue
    , paddingXY 32 16
    , Border.rounded 3
    ]


view model =
    Element.layout
        []
    <|
        Element.column [ padding 30, spacing 10, width fill ]
            ([ Element.row [ spacing 20, width fill ]
                [ if model.numSides > 1 then
                    Input.button buttonStyle { onPress = Just DecNumSides, label = text "-" }

                  else
                    Input.button inactiveButtonStyle { onPress = Just NoAction, label = text "-" }
                , Input.button buttonStyle
                    { onPress = Just IncNumSides, label = text "+" }
                , text
                    (String.fromInt model.numSides
                        ++ " sides"
                    )
                , el [ alignRight ]
                    (text
                        (String.fromInt model.sumSides
                            ++ " sum of dots"
                        )
                    )
                , if model.sumSides > 1 then
                    Input.button buttonStyle { onPress = Just DecSumSides, label = text "-" }

                  else
                    Input.button inactiveButtonStyle { onPress = Just NoAction, label = text "-" }
                , Input.button buttonStyle
                    { onPress = Just IncSumSides, label = text "+" }
                ]
             ]
                ++ [ Array.indexedMap
                        (\idPlayer die ->
                            [ Input.button
                                [ Background.color grey
                                , Font.color red
                                , Border.color red
                                , Border.width 2
                                , paddingXY 10 10
                                , Border.rounded 10
                                ]
                                { onPress = Just (DeletePlayer idPlayer), label = text "X" }
                            ]
                                ++ [ Input.text [] { onChange = SetName <| idPlayer, text = die.name, placeholder = Nothing, label = Input.labelAbove [] (text "") } ]
                                ++ (Array.indexedMap
                                        (\idDot dot ->
                                            Input.text
                                                (if dot.valid == OK then
                                                    []

                                                 else
                                                    [ Border.color colorWrongFormat, Border.width 1 ]
                                                )
                                                { text = dot.text
                                                , onChange = \t -> SetDots idPlayer idDot t
                                                , placeholder = Nothing
                                                , label = Input.labelAbove [] (text "")
                                                }
                                        )
                                        die.dots
                                        |> Array.toList
                                   )
                                |> Element.row
                                    ([ spacing 20 ]
                                        ++ (if die.valid == WrongSum then
                                                [ Border.color colorWrongSum, Border.width 2, Border.rounded 3 ]

                                            else
                                                []
                                           )
                                    )
                        )
                        model.dice
                        |> Array.toList
                        |> Element.column [ spacing 10 ]
                   ]
                ++ [ Element.row [ width fill, spacing 20 ]
                        [ Input.button
                            [ Background.color white
                            , Font.color green
                            , Border.color green
                            , Border.width 2
                            , paddingXY 10 10
                            , Border.rounded 10
                            ]
                            { onPress = Just AddPlayer, label = text "+" }
                        , case model.valid of
                            OK ->
                                el [ alignRight, Font.color green ] none

                            WrongFormat ->
                                el [ alignRight, Font.color colorWrongFormat ] (text "There is a player with a wrong input!")

                            WrongSum ->
                                el [ alignRight, Font.color colorWrongSum ] (text "There is a player with a wrong sum!")

                            WrongNever ->
                                el [ alignRight, Font.color purple ] (text "ALERT!")
                        , if model.valid == OK then
                            Input.button buttonStyle { onPress = Just Battle, label = text "Battle!" }

                          else
                            Input.button inactiveButtonStyle { onPress = Just NoAction, label = text "Battle!" }
                        ]
                   ]
            )



-- Computation stuff


type alias BattleResult =
    { win : Float
    , loss : Float
    , draw : Float
    }


dotBattle : Dot -> Dot -> BattleResult
dotBattle dot1 dot2 =
    if dot1.number == dot2.number then
        BattleResult 0 0 1

    else if dot1.number > dot2.number then
        BattleResult 1 0 0

    else
        BattleResult 0 1 0


addBattleResults : BattleResult -> BattleResult -> BattleResult
addBattleResults br1 br2 =
    { win = br1.win + br2.win, loss = br1.loss + br2.loss, draw = br1.draw + br2.draw }


battleDie : Die -> Die -> BattleResult
battleDie die1 die2 =
    Array.foldl addBattleResults
        (BattleResult 0 0 0)
        (Array.map
            (\dot1 ->
                Array.foldl addBattleResults
                    (BattleResult 0 0 0)
                    (Array.map
                        (\dot2 -> dotBattle dot1 dot2)
                        die2.dots
                    )
            )
            die1.dots
        )
        |> (\br ->
                let
                    possibilities =
                        Array.length die1.dots * Array.length die2.dots |> toFloat
                in
                { win = br.win / possibilities, loss = br.loss / possibilities, draw = br.draw / possibilities }
           )


battleAll : Array Die -> Array (Array BattleResult)
battleAll dice =
    Array.map (\die1 -> Array.map (\die2 -> battleDie die1 die2) dice) dice


battle : Model -> Array (Array BattleResult)
battle model =
    battleAll model.dice


absoluteWins : Array (Array BattleResult) -> Array (Array Float)
absoluteWins battleResults =
    Array.map
        (\row ->
            Array.map
                (\res ->
                    if res.win > res.loss then
                        1

                    else if res.win == res.loss then
                        0

                    else
                        -1
                )
                row
        )
        battleResults


relativeWins : Array (Array BattleResult) -> Array (Array Float)
relativeWins battleResults =
    Array.map
        (\row ->
            Array.map
                (\res -> res.win - res.loss)
                row
        )
        battleResults
