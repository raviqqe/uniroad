module Main exposing (main)

import Browser exposing (Document)
import Browser.Events exposing (onKeyDown)
import Css exposing (..)
import Css.Global exposing (..)
import Direction exposing (..)
import Dungeon exposing (Dungeon, Msg(..))
import Hero
import Html.Styled exposing (div, node, text, toUnstyled)
import Json.Decode exposing (Decoder)


main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> onKeyDown keyDecoder
        }


type alias App =
    { dungeon : Dungeon }


init : () -> ( App, Cmd Msg )
init flags =
    let
        ( dungeon, cmd ) =
            Dungeon.init
    in
    ( { dungeon = dungeon }, Cmd.map DungeonMsg cmd )


type Msg
    = DungeonMsg Dungeon.Msg
    | None


update : Msg -> App -> ( App, Cmd Msg )
update msg app =
    case msg of
        DungeonMsg dungeonMsg ->
            let
                ( dungeon, cmd ) =
                    Dungeon.update dungeonMsg app.dungeon
            in
            ( { app | dungeon = dungeon }, Cmd.map DungeonMsg cmd )

        None ->
            ( app, Cmd.none )


view : App -> Document Msg
view app =
    { title = "uniroad"
    , body =
        [ toUnstyled
            (div
                []
                [ node "style"
                    []
                    [ text "@import url('https://fonts.googleapis.com/css?family=Ubuntu+Mono');" ]
                , global
                    [ selector "body"
                        [ width (vw 100)
                        , height (vh 100)
                        , margin zero
                        , padding zero
                        , displayFlex
                        , alignItems center
                        , justifyContent center
                        , fontFamilies [ "Ubuntu Mono", "monospace" ]
                        , color (hex "#ffffff")
                        , backgroundColor (hex "#000000")
                        ]
                    ]
                , Html.Styled.map DungeonMsg (Dungeon.view app.dungeon)
                ]
            )
        ]
    }


keyDecoder : Decoder Msg
keyDecoder =
    let
        toMsg : String -> Msg
        toMsg key =
            let
                move =
                    Hero.Move >> HeroMsg >> DungeonMsg
            in
            case key of
                "h" ->
                    move Left

                "l" ->
                    move Right

                "k" ->
                    move Up

                "j" ->
                    move Down

                "u" ->
                    move LeftUp

                "i" ->
                    move RightUp

                "n" ->
                    move LeftDown

                "m" ->
                    move RightDown

                _ ->
                    None
    in
    Json.Decode.map toMsg (Json.Decode.field "key" Json.Decode.string)
