module Dungeon exposing (Dungeon, Msg(..), init, update, view)

import Css exposing (..)
import Floor exposing (Floor)
import Hero exposing (Hero)
import Html.Styled exposing (Html, div, styled, text)
import List exposing (map, range)
import Position
import Random exposing (Generator)


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
    , Random.generate Renew generate
    )


type Msg
    = HeroMsg Hero.Msg
    | Renew (Result String ( Floor, Hero ))


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
                        , Random.generate Renew generate
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

        Renew result ->
            case result of
                Ok ( floor, hero ) ->
                    ( { dungeon
                        | floor = Just floor
                        , generatingFloor = False
                        , hero = hero
                      }
                    , Cmd.none
                    )

                Err message ->
                    Debug.todo message


generate : Generator (Result String ( Floor, Hero ))
generate =
    Random.andThen
        (\floor ->
            case Floor.generatePosition floor of
                Ok generator ->
                    Random.map
                        (\position -> Ok ( floor, Hero.init position ))
                        generator

                Err message ->
                    Random.constant (Err message)
        )
        Floor.generate


view : Dungeon -> Html Msg
view dungeon =
    let
        hero =
            dungeon.hero
    in
    case dungeon.floor of
        Just floor ->
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

                                         else
                                            []
                                        )
                                )
                                (range 1 Floor.width)
                            )
                    )
                    (range 1 Floor.height)
                )

        Nothing ->
            text "loading"
