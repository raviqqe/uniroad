module Division exposing (Division, init)

import Position exposing (Position)


type alias Division =
    { leftTop : Position, rightBottom : Position }


init : Position -> Position -> Division
init leftTop rightBottom =
    { leftTop = leftTop, rightBottom = rightBottom }
