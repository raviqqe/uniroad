module Floor exposing (Floor, generate, generatePosition, inside, view)

import Css exposing (..)
import Direction exposing (..)
import Html.Styled exposing (Html, div, styled, text)
import Maybe
import Position exposing (Position)
import Random exposing (Generator)
import Result
import Room
import Set exposing (Set)


type alias Floor =
    { size : Int
    , validPositions : Set Position
    }


generate : Generator Floor
generate =
    let
        size =
            32
    in
    Random.map
        (\room -> { size = size, validPositions = Room.toPositions room })
        (Room.generate ( 2, 31 ) ( 2, 31 ))


inside : Floor -> Position -> Bool
inside floor position =
    Set.member position floor.validPositions


generatePosition : Floor -> Result String (Generator Position)
generatePosition floor =
    let
        positions =
            Set.toList floor.validPositions
    in
    Result.fromMaybe
        "no valid positions in a floor"
        (Maybe.map2 Random.uniform (List.head positions) (List.tail positions))


view : Floor -> Position -> List (Html msg) -> Html msg
view floor position children =
    styled
        div
        [ displayFlex
        , justifyContent center
        , alignItems center
        , width (em 1)
        , height (em 1)
        ]
        []
        (if inside floor position then
            children

         else
            let
                isWall =
                    isNextWall floor position

                isFloor =
                    not << isWall
            in
            if isWall Left && isWall Up && isFloor LeftUp then
                [ text "'" ]

            else if isWall Left && isWall Down && isFloor LeftDown then
                [ text "." ]

            else if isWall Right && isWall Up && isFloor RightUp then
                [ text "`" ]

            else if isWall Right && isWall Down && isFloor RightDown then
                [ text "," ]

            else if isWall Left && isWall Right && xor (isWall Up) (isWall Down) then
                [ text "-" ]

            else if isWall Up && isWall Down && xor (isWall Right) (isWall Left) then
                [ text "|" ]

            else
                []
        )


isNextWall : Floor -> Position -> Direction -> Bool
isNextWall floor position direction =
    not (inside floor (Position.move position direction))
