module Position exposing (Position, init)


type alias Position =
    { x : Int, y : Int }


init : Int -> Int -> Position
init x y =
    { x = x, y = y }
