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
    { floor : Maybe Floor
    , generating : Bool
    , hero : Maybe Hero
    , stairs : Maybe Stairs
    }


init : ( Dungeon, Cmd Msg )
init =
    ( { floor = Nothing
      , generating = True
      , hero = Nothing
      , stairs = Nothing
      }
    , Random.generate Renew generate
    )


type Msg
    = HeroMsg Hero.Msg
    | Renew (Result String ( Floor, Hero, Stairs ))
    | None


update : Msg -> Dungeon -> ( Dungeon, Cmd Msg )
update msg dungeon =
    case msg of
        HeroMsg heroMsg ->
            case ( dungeon.floor, dungeon.hero ) of
                ( Just floor, Just hero ) ->
                    ( { dungeon
                        | hero =
                            Just
                                (let
                                    newHero =
                                        Hero.update heroMsg hero
                                 in
                                 if Floor.inside floor newHero.position then
                                    newHero

                                 else
                                    hero
                                )
                      }
                    , Cmd.none
                    )

                _ ->
                    if dungeon.generating then
                        ( dungeon, Cmd.none )

                    else
                        ( { dungeon | generating = True }
                        , Random.generate Renew generate
                        )

        Renew result ->
            case result of
                Ok ( floor, hero, stairs ) ->
                    ( { dungeon
                        | floor = Just floor
                        , generating = False
                        , hero = Just hero
                        , stairs = Just stairs
                      }
                    , Cmd.none
                    )

                Err message ->
                    -- unreachable
                    ( dungeon, Cmd.none )

        None ->
            ( dungeon, Cmd.none )


generate : Generator (Result String ( Floor, Hero, Stairs ))
generate =
    Random.andThen
        (\floor ->
            case ( Floor.generatePosition floor, Stairs.generate floor ) of
                ( Ok randomPosition, Ok randomStairs ) ->
                    Random.map2
                        (\position stairs -> Ok ( floor, Hero.init position, stairs ))
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
    case ( dungeon.floor, dungeon.hero, dungeon.stairs ) of
        ( Just floor, Just hero, Just stairs ) ->
            styled div
                [ displayFlex
                , flexDirection column
                , flexWrap noWrap
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
                                    styled
                                        div
                                        [ displayFlex
                                        , justifyContent center
                                        , alignItems center
                                        , width (em 1.5)
                                        , height (em 1.5)
                                        , (backgroundColor << hex)
                                            (if Floor.inside floor (Position.init x y) then
                                                "#000000"

                                             else
                                                "#444444"
                                            )
                                        ]
                                        []
                                        (if hero.position == Position.init x y then
                                            [ Html.Styled.map HeroMsg (Hero.view hero) ]

                                         else if stairs.position == Position.init x y then
                                            [ Html.Styled.map (\_ -> None) (Stairs.view stairs) ]

                                         else
                                            []
                                        )
                                )
                                (range 1 Floor.width)
                            )
                    )
                    (range 1 Floor.height)
                )

        _ ->
            text "loading"
