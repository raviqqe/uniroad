module Floor exposing (Floor, generate, generatePosition, height, inside, width)

import Maybe
import Position exposing (Position)
import Random exposing (Generator)
import Room
import Set exposing (Set)


type alias Floor =
    { validPositions : Set Position
    }


generate : Generator Floor
generate =
    Random.map
        (\room -> { validPositions = Room.toPositions room })
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
    case Maybe.map2 Random.uniform (List.head positions) (List.tail positions) of
        Just position ->
            Ok position

        Nothing ->
            Err "no valid postions in a floor"


width : Int
width =
    32


height : Int
height =
    32
