module Dungeon exposing (Dungeon, Msg(..), init, update, view)

import Css exposing (..)
import Floor exposing (Floor)
import Hero exposing (Hero)
import Html.Styled exposing (Html, div, styled, text)
import List exposing (map, range)
import Position exposing (Position)
import Random exposing (Generator)
import Set exposing (Set)
import Stairs exposing (Stairs)


type alias Dungeon =
    Maybe State


type alias State =
    { floor : Floor
    , hero : Hero
    , stairs : Stairs
    , level : Int
    }


init : ( Dungeon, Cmd Msg )
init =
    ( Nothing
    , Random.generate Renew (generate Nothing)
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
                        ( Nothing, Random.generate Renew (generate dungeon) )

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


generate : Dungeon -> Generator (Result String State)
generate dungeon =
    Random.andThen
        (\floor ->
            case ( Floor.generatePosition floor, Stairs.generate floor ) of
                ( Ok randomPosition, Ok randomStairs ) ->
                    Random.map2
                        (\position stairs ->
                            Ok
                                { floor = floor
                                , hero = Hero.init position floor
                                , stairs = stairs
                                , level =
                                    Maybe.withDefault
                                        1
                                        (Maybe.map (.level >> (\level -> level + 1)) dungeon)
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
        Just { floor, hero, stairs, level } ->
            styled div
                [ displayFlex
                , flexDirection column
                , flexWrap noWrap
                , fontSize (vmin (100 / toFloat floor.size))
                ]
                []
                ([ styled
                    div
                    [ position absolute
                    , left (px 0)
                    , top (px 0)
                    , width (vw 100)
                    , height (vh 100)
                    , displayFlex
                    , justifyContent center
                    , alignItems center
                    , color (hex "#ffffff11")
                    , fontSize (vmin 100)
                    ]
                    []
                    [ text (String.fromInt level) ]
                 ]
                    ++ map
                        (\y ->
                            styled div
                                [ displayFlex
                                , flexDirection row
                                , flexWrap noWrap
                                ]
                                []
                                (map
                                    (\x ->
                                        let
                                            position =
                                                ( x, y )
                                        in
                                        Floor.view floor
                                            position
                                            (if hero.position == position then
                                                [ Html.Styled.map HeroMsg (Hero.view hero) ]

                                             else if
                                                Set.member position hero.sight
                                                    && stairs.position
                                                    == position
                                             then
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
