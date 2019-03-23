module Main exposing (App, init, main, update)

import Browser
import Css exposing (..)
import Css.Global exposing (..)
import Dungeon exposing (Dungeon)
import Hero
import Html.Styled exposing (Html, div, toUnstyled)


main =
    Browser.sandbox { init = init, update = update, view = view >> toUnstyled }


type alias App =
    { dungeon : Dungeon }


init : App
init =
    { dungeon = Dungeon.init }


type Msg
    = DungeonMsg Dungeon.Msg


update : Msg -> App -> App
update msg app =
    case msg of
        DungeonMsg dungeonMsg ->
            { app | dungeon = Dungeon.update dungeonMsg app.dungeon }


view : App -> Html Msg
view app =
    div []
        [ global
            [ selector "body"
                [ width (vw 100)
                , height (vh 100)
                , margin zero
                , padding zero
                , fontSize (px 8)
                , displayFlex
                , alignItems center
                , justifyContent center
                ]
            ]
        , Html.Styled.map (\msg -> DungeonMsg msg) (Dungeon.view app.dungeon)
        ]
