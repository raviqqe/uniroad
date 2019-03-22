module Main exposing (Model, init, main, update)

import Browser
import Css exposing (..)
import Css.Global exposing (..)
import Dungeon exposing (Dungeon)
import Html.Styled exposing (Html, div, toUnstyled)


main =
    Browser.sandbox { init = init, update = update, view = view >> toUnstyled }


type alias Model =
    Dungeon


init : Model
init =
    Dungeon.init


type alias Msg =
    Dungeon.Msg


update : Msg -> Model -> Model
update msg model =
    case msg of
        Dungeon.None ->
            model


view : Model -> Html Msg
view model =
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
        , Dungeon.view model
        ]
