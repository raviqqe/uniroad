module Floor exposing (Floor, generate, generatePosition, inside)

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
