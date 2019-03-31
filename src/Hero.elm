module Hero exposing (Hero, Msg(..), init, update, view)

import Css exposing (..)
import Direction exposing (..)
import Floor exposing (Floor)
import Html.Styled exposing (Html, div, styled, text)
import Position exposing (Position)
import Set exposing (Set)


type alias Hero =
    { position : Position, sight : Set Position }


init : Position -> Floor -> Hero
init position floor =
    let
        nextToSight : Set Position -> Position -> Bool
        nextToSight sight currentPosition =
            let
                inSight direction =
                    Set.member (Position.move currentPosition direction) sight
            in
            inSight Left
                && (inSight LeftUp || inSight LeftDown)
                || inSight Right
                && (inSight RightUp || inSight RightDown)
                || inSight Up
                && (inSight LeftUp || inSight RightUp)
                || inSight Down
                && (inSight LeftDown || inSight RightDown)

        partOfSight : Set Position -> Position -> Bool
        partOfSight sight currentPosition =
            Floor.inside floor currentPosition && nextToSight sight currentPosition

        expandSight : Set Position -> Set Position
        expandSight sight =
            floor.positions
    in
    { position = position
    , sight =
        position
            :: List.map
                (\direction -> Position.move position direction)
                [ Left, Right, Up, Down, LeftUp, LeftDown, RightUp, RightDown ]
            |> List.filter (Floor.inside floor)
            |> Set.fromList
            |> expandSight
    }


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
