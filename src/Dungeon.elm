module Dungeon exposing (Dungeon, Msg(..), init, update, view)

import Css exposing (..)
import Html.Styled exposing (Html, styled, table, td, tr)
import List exposing (repeat)


type alias Dungeon =
    { width : Int, height : Int }


init : Dungeon
init =
    { width = 32, height = 32 }


type Msg
    = None


update : Msg -> Dungeon -> Dungeon
update msg model =
    case msg of
        None ->
            model


view : Dungeon -> Html Msg
view dungeon =
    styled table
        [ borderSpacing (px 0) ]
        []
        (repeat dungeon.height
            (styled tr
                [ display block, whiteSpace noWrap, backgroundColor (hex "#000000") ]
                []
                (repeat
                    dungeon.width
                    (styled td [ display inlineBlock, width (em 1.2), height (em 1.2) ] [] [])
                )
            )
        )
