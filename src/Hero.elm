module Hero exposing (Hero, Msg(..), init, update, view)

import Css exposing (..)
import Html.Styled exposing (Html, div, styled, text)
import List exposing (repeat)
import Position exposing (Position)


type alias Hero =
    { position : Position }


init : Position -> Hero
init position =
    { position = position }


type Msg
    = MoveLeft
    | MoveRight
    | MoveUp
    | MoveDown


update : Msg -> Hero -> Hero
update msg hero =
    case msg of
        MoveLeft ->
            { hero
                | position =
                    Position.init
                        (Position.x hero.position - 1)
                        (Position.y hero.position)
            }

        MoveRight ->
            { hero
                | position =
                    Position.init
                        (Position.x hero.position + 1)
                        (Position.y hero.position)
            }

        MoveUp ->
            { hero
                | position =
                    Position.init
                        (Position.x hero.position)
                        (Position.y hero.position - 1)
            }

        MoveDown ->
            { hero
                | position =
                    Position.init
                        (Position.x hero.position)
                        (Position.y hero.position + 1)
            }


view : Hero -> Html Msg
view dungeon =
    styled div [ color (hex "#ff0000") ] [] [ text "@" ]
