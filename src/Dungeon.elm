module Dungeon exposing (Dungeon, Msg(..), init, update, view)

import Css exposing (..)
import Floor exposing (Floor)
import Hero exposing (Hero)
import Html.Styled exposing (Html, styled, table, td, tr)
import List exposing (map, range, repeat)
import Position exposing (Position)
import Random


type alias Dungeon =
    { floor : Maybe Floor
    , generatingFloor : Bool
    , hero : Hero
    }


init : ( Dungeon, Cmd Msg )
init =
    ( { floor = Nothing
      , generatingFloor = True
      , hero = Hero.init (Position.init 16 16)
      }
    , Random.generate GenerateFloor Floor.generate
    )


type Msg
    = HeroMsg Hero.Msg
    | GenerateFloor Floor


update : Msg -> Dungeon -> ( Dungeon, Cmd Msg )
update msg dungeon =
    case msg of
        HeroMsg heroMsg ->
            case dungeon.floor of
                Nothing ->
                    if dungeon.generatingFloor then
                        ( dungeon, Cmd.none )

                    else
                        ( { dungeon | generatingFloor = True }
                        , Random.generate GenerateFloor Floor.generate
                        )

                Just floor ->
                    ( { dungeon
                        | hero =
                            let
                                newHero =
                                    Hero.update heroMsg dungeon.hero
                            in
                            if Floor.inside floor newHero.position then
                                newHero

                            else
                                dungeon.hero
                      }
                    , Cmd.none
                    )

        GenerateFloor newFloor ->
            ( { dungeon
                | floor = Just newFloor
                , generatingFloor = False
              }
            , Cmd.none
            )


view : Dungeon -> Html Msg
view { floor, hero } =
    styled table
        [ borderSpacing (px 0) ]
        []
        (map
            (\y ->
                styled tr
                    [ display block, whiteSpace noWrap, backgroundColor (hex "#000000") ]
                    []
                    (map
                        (\x ->
                            styled
                                td
                                [ display inlineBlock, width (em 1.5), height (em 1.5) ]
                                []
                                (if hero.position == Position.init x y then
                                    [ Html.Styled.map HeroMsg (Hero.view hero) ]

                                 else
                                    []
                                )
                        )
                        (range 1 Floor.width)
                    )
            )
            (range 1 Floor.height)
        )
