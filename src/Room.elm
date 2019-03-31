module Room exposing (Room, generate, minimumSize, toConnectionPositions, toPositions)

import Array exposing (Array)
import Division exposing (Division)
import List
import Position exposing (Position)
import Random exposing (Generator)
import Set exposing (Set)


type alias Room =
    { leftTop : Position, rightBottom : Position }


init : Position -> Position -> Room
init leftTop rightBottom =
    { leftTop = leftTop, rightBottom = rightBottom }


generate : Position -> Position -> Maybe (Generator Room)
generate ( minX, minY ) ( maxX, maxY ) =
    if maxX - minX - 1 < minimumSize || maxY - minY - 1 < minimumSize then
        Nothing

    else
        Position.generate ( minX + 1, maxX - minimumSize ) ( minY + 1, maxY - minimumSize )
            |> Random.andThen
                (\leftTop ->
                    let
                        ( x, y ) =
                            leftTop
                    in
                    Position.generate
                        ( x + minimumSize - 1, maxX - 1 )
                        ( y + minimumSize - 1, maxY - 1 )
                        |> Random.map (\rightBottom -> init leftTop rightBottom)
                )
            |> Just


toPositions : Room -> Set ( Int, Int )
toPositions room =
    List.range (Position.x room.leftTop) (Position.x room.rightBottom)
        |> List.map
            (\x ->
                List.range (Position.y room.leftTop) (Position.y room.rightBottom)
                    |> List.map (\y -> ( x, y ))
            )
        |> List.concat
        |> Set.fromList


toConnectionPositions : Room -> Division -> Division -> Generator (Maybe (Set Position))
toConnectionPositions room division globalDivision =
    let
        horizontalLineToPositions : Int -> Int -> Int -> Set Position
        horizontalLineToPositions startX endX y =
            Set.fromList (List.map (\x -> ( x, y )) (List.range startX endX))

        verticalLineToPositions : Int -> Int -> Int -> Set Position
        verticalLineToPositions startY endY x =
            Set.fromList (List.map (\y -> ( x, y )) (List.range startY endY))

        positionsCandidates : Array (Generator (Set Position))
        positionsCandidates =
            [ if Division.left division == Division.left globalDivision then
                Nothing

              else
                Random.int (top room) (bottom room)
                    |> Random.map
                        (horizontalLineToPositions (Division.left division) (left room))
                    |> Just
            , if Division.right division == Division.right globalDivision then
                Nothing

              else
                Random.int (top room) (bottom room)
                    |> Random.map
                        (horizontalLineToPositions (right room) (Division.right division))
                    |> Just
            , if Division.top division == Division.top globalDivision then
                Nothing

              else
                Random.int (left room) (right room)
                    |> Random.map
                        (verticalLineToPositions (Division.top division) (top room))
                    |> Just
            , if Division.bottom division == Division.bottom globalDivision then
                Nothing

              else
                Random.int (left room) (right room)
                    |> Random.map
                        (verticalLineToPositions (bottom room) (Division.bottom division))
                    |> Just
            ]
                |> List.map (Maybe.map List.singleton >> Maybe.withDefault [])
                |> List.concat
                |> Array.fromList
    in
    if Array.length positionsCandidates == 0 then
        Random.constant Nothing

    else
        Random.int 0 (Array.length positionsCandidates - 1)
            |> Random.andThen
                (\index ->
                    case Array.get index positionsCandidates of
                        Nothing ->
                            Random.constant Nothing

                        Just generator ->
                            generator |> Random.map Just
                )


minimumSize : Int
minimumSize =
    4


left : Room -> Int
left room =
    Position.x room.leftTop


right : Room -> Int
right room =
    Position.x room.rightBottom


top : Room -> Int
top room =
    Position.y room.leftTop


bottom : Room -> Int
bottom room =
    Position.y room.rightBottom
