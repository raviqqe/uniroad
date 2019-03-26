module Hero exposing (Hero, Msg(..), init, update, view)

import Css exposing (..)
import Direction exposing (Direction)
import Html.Styled exposing (Html, div, styled, text)
import Position exposing (Position)


type alias Hero =
    { position : Position }


init : Position -> Hero
init position =
    { position = position }


type Msg
    = Move Direction


update : Msg -> Hero -> Hero
update msg hero =
    case msg of
        Move direction ->
            { hero
                | position = Position.move hero.position direction
            }


view : Hero -> Html Msg
view dungeon =
    styled div [ color (hex "#ff0000") ] [] [ text "@" ]
