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
    = None


update : Msg -> Hero -> Hero
update _ model =
    model


view : Hero -> Html Msg
view dungeon =
    styled div [ color (hex "#ff0000") ] [] [ text "@" ]
