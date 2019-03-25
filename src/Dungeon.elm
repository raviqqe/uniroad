module Dungeon exposing (Dungeon, Msg(..), init, update, view)

import Css exposing (..)
import Floor exposing (Floor)
import Hero exposing (Hero)
import Html.Styled exposing (Html, div, styled, text)
import List exposing (map, range)
import Position
import Random exposing (Generator)
import Stairs exposing (Stairs)


type alias Dungeon =
    Maybe State


type alias State =
    { floor : Floor
    , hero : Hero
    , stairs : Stairs
    }


init : ( Dungeon, Cmd Msg )
init =
    ( Nothing
    , Random.generate Renew generate
    )


type Msg
    = HeroMsg Hero.Msg
    | Renew (Result String State)
    | None


update : Msg -> Dungeon -> ( Dungeon, Cmd Msg )
update msg dungeon =
    case msg of
        HeroMsg heroMsg ->
            case dungeon of
                Just state ->
                    let
                        newHero =
                            Hero.update heroMsg state.hero
                    in
                    if newHero.position == state.stairs.position then
                        ( Nothing, Random.generate Renew generate )

                    else
                        ( Just
                            { state
                                | hero =
                                    if Floor.inside state.floor newHero.position then
                                        newHero

                                    else
                                        state.hero
                            }
                        , Cmd.none
                        )

                Nothing ->
                    ( Nothing, Cmd.none )

        Renew result ->
            case result of
                Ok state ->
                    ( Just state
                    , Cmd.none
                    )

                Err message ->
                    -- unreachable
                    ( dungeon, Cmd.none )

        None ->
            ( dungeon, Cmd.none )


generate : Generator (Result String State)
generate =
    Random.andThen
        (\floor ->
            case ( Floor.generatePosition floor, Stairs.generate floor ) of
                ( Ok randomPosition, Ok randomStairs ) ->
                    Random.map2
                        (\position stairs ->
                            Ok
                                { floor = floor
                                , hero = Hero.init position
                                , stairs = stairs
                                }
                        )
                        randomPosition
                        randomStairs

                ( Err message, _ ) ->
                    Random.constant (Err message)

                ( _, Err message ) ->
                    Random.constant (Err message)
        )
        Floor.generate


view : Dungeon -> Html Msg
view dungeon =
    case dungeon of
        Just { floor, hero, stairs } ->
            styled div
                [ displayFlex
                , flexDirection column
                , flexWrap noWrap
                , fontSize (vmin (100 / toFloat floor.size))
                ]
                []
                (map
                    (\y ->
                        styled div
                            [ displayFlex
                            , flexDirection row
                            , flexWrap noWrap
                            ]
                            []
                            (map
                                (\x ->
                                    Floor.view floor
                                        (Position.init x y)
                                        (if hero.position == Position.init x y then
                                            [ Html.Styled.map HeroMsg (Hero.view hero) ]

                                         else if stairs.position == Position.init x y then
                                            [ Html.Styled.map (\_ -> None) (Stairs.view stairs) ]

                                         else
                                            []
                                        )
                                )
                                (range 1 floor.size)
                            )
                    )
                    (range 1 floor.size)
                )

        Nothing ->
            text "loading"
