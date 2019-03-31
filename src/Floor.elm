module Floor exposing (Floor, generate, generatePosition, inside, view)

import Css exposing (..)
import Direction exposing (..)
import Division exposing (Division)
import Html.Styled exposing (Html, div, styled, text)
import Maybe
import Position exposing (Position)
import Random exposing (Generator)
import Result
import Room
import Set exposing (Set)


type alias Floor =
    { size : Int
    , positions : Set Position
    }


generate : Generator Floor
generate =
    let
        size : Int
        size =
            32

        splitProbability : Float
        splitProbability =
            0.2

        splitDivisionHorizontally : Division -> Generator (List Division)
        splitDivisionHorizontally division =
            Random.float 0 1
                |> Random.andThen
                    (\probability ->
                        if probability < splitProbability then
                            Random.constant [ division ]

                        else
                            let
                                ( left, top ) =
                                    division.leftTop

                                ( right, bottom ) =
                                    division.rightBottom

                                minSeparator =
                                    left + 2 + Room.minimumSize

                                maxSeparator =
                                    right - 2 - Room.minimumSize
                            in
                            if minSeparator > maxSeparator then
                                Random.constant [ division ]

                            else
                                Random.int minSeparator maxSeparator
                                    |> Random.andThen
                                        (\separator ->
                                            Random.map2
                                                (++)
                                                (splitDivisionVertically
                                                    (Division.init division.leftTop ( separator - 1, bottom ))
                                                )
                                                (splitDivisionVertically
                                                    (Division.init ( separator + 1, top ) division.rightBottom)
                                                )
                                        )
                    )

        splitDivisionVertically : Division -> Generator (List Division)
        splitDivisionVertically division =
            Random.float 0 1
                |> Random.andThen
                    (\probability ->
                        if probability < splitProbability then
                            Random.constant [ division ]

                        else
                            let
                                ( left, top ) =
                                    division.leftTop

                                ( right, bottom ) =
                                    division.rightBottom

                                minSeparator =
                                    top + 2 + Room.minimumSize

                                maxSeparator =
                                    bottom - 2 - Room.minimumSize
                            in
                            if minSeparator > maxSeparator then
                                Random.constant [ division ]

                            else
                                Random.int minSeparator maxSeparator
                                    |> Random.andThen
                                        (\separator ->
                                            Random.map2
                                                (++)
                                                (splitDivisionHorizontally
                                                    (Division.init division.leftTop ( right, separator - 1 ))
                                                )
                                                (splitDivisionHorizontally
                                                    (Division.init ( left, separator + 1 ) division.rightBottom)
                                                )
                                        )
                    )
    in
    Random.int 0 1
        |> Random.map
            (\int ->
                if int == 0 then
                    splitDivisionHorizontally

                else
                    splitDivisionVertically
            )
        |> Random.andThen (\split -> split (Division.init ( 1, 1 ) ( size, size )))
        |> Random.andThen
            (List.map
                (\division ->
                    Room.generate division.leftTop division.rightBottom
                        |> (Random.andThen >> Maybe.map)
                            (\room ->
                                let
                                    roomPositions : Set Position
                                    roomPositions =
                                        Set.union
                                            (Room.toPositions room)
                                            (Division.toCorridorPositions division)
                                in
                                Room.toConnectionPositions room
                                    division
                                    (Division.init ( 1, 1 ) ( size, size ))
                                    |> Random.map
                                        (\maybeConnectionPositions ->
                                            case maybeConnectionPositions of
                                                Just connectionPositions ->
                                                    Set.union roomPositions connectionPositions

                                                Nothing ->
                                                    roomPositions
                                        )
                            )
                )
                >> List.map (Maybe.map List.singleton >> Maybe.withDefault [])
                >> List.concat
                >> List.foldr
                    (Random.map2 (\positions set -> Set.union positions set))
                    (Random.constant Set.empty)
            )
        |> Random.map (Set.filter (\( x, y ) -> 1 <= x && x <= size && 1 <= y && y <= size))
        |> Random.map (\positions -> { size = size, positions = positions })


inside : Floor -> Position -> Bool
inside floor position =
    Set.member position floor.positions


generatePosition : Floor -> Result String (Generator Position)
generatePosition floor =
    let
        positions =
            Set.toList floor.positions
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
