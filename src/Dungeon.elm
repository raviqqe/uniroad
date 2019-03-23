module Dungeon exposing (Dungeon, Msg(..), init, update, view)

import Css exposing (..)
import Hero exposing (Hero)
import Html.Styled exposing (Html, styled, table, td, tr)
import List exposing (map, range, repeat)
import Position exposing (Position)


type alias Dungeon =
    { width : Int
    , height : Int
    , hero : Hero
    }


init : Dungeon
init =
    { width = 32, height = 32, hero = Hero.init (Position.init 16 16) }


type Msg
    = HeroMsg Hero.Msg


update : Msg -> Dungeon -> Dungeon
update msg dungeon =
    case msg of
        HeroMsg heroMsg ->
            { dungeon
                | hero =
                    let
                        newHero =
                            Hero.update heroMsg dungeon.hero
                    in
                    if isValidPosition dungeon newHero.position then
                        newHero

                    else
                        dungeon.hero
            }


isValidPosition : Dungeon -> Position -> Bool
isValidPosition dungeon { x, y } =
    1 <= x && x <= dungeon.width && 1 <= y && y <= dungeon.height


view : Dungeon -> Html Msg
view dungeon =
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
                                (if dungeon.hero.position == Position.init x y then
                                    [ Html.Styled.map
                                        (\msg -> HeroMsg msg)
                                        (Hero.view dungeon.hero)
                                    ]

                                 else
                                    []
                                )
                        )
                        (range 1 dungeon.width)
                    )
            )
            (range 1 dungeon.height)
        )
