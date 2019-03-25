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
                wallLeft =
                    isNextWall floor position Left

                wallRight =
                    isNextWall floor position Right

                wallUp =
                    isNextWall floor position Up

                wallDown =
                    isNextWall floor position Down
            in
            if wallLeft && wallUp && isDiagonalFloor floor position Left Up then
                [ text "'" ]

            else if wallUp && wallRight && isDiagonalFloor floor position Up Right then
                [ text "`" ]

            else if wallRight && wallDown && isDiagonalFloor floor position Right Down then
                [ text "," ]

            else if wallDown && wallLeft && isDiagonalFloor floor position Down Left then
                [ text "." ]

            else if wallLeft && wallRight && xor wallUp wallDown then
                [ text "-" ]

            else if wallUp && wallDown && xor wallRight wallLeft then
                [ text "|" ]

            else
                -- unreachable
                []
        )


isNextWall : Floor -> Position -> Direction -> Bool
isNextWall floor position direction =
    not (inside floor (Position.move position direction))


isDiagonalFloor : Floor -> Position -> Direction -> Direction -> Bool
isDiagonalFloor floor position direction1 direction2 =
    inside floor (Position.move (Position.move position direction1) direction2)
