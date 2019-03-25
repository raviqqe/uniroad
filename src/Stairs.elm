module Stairs exposing (Stairs, generate, view)

import Css exposing (color, hex)
import Floor exposing (Floor)
import Html.Styled exposing (Html, div, styled, text)
import Position exposing (Position)
import Random exposing (Generator)
import Result


type alias Stairs =
    { position : Position }


init : Position -> Stairs
init position =
    { position = position }


generate : Floor -> Result String (Generator Stairs)
generate floor =
    (Result.map << Random.map) init (Floor.generatePosition floor)


view : Stairs -> Html ()
view stairs =
    styled div
        [ color (hex "#ffffff") ]
        []
        [ text "#" ]
