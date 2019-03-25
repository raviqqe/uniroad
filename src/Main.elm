module Main exposing (main)

import Browser exposing (Document)
import Browser.Events exposing (onKeyDown)
import Css exposing (..)
import Css.Global exposing (..)
import Dungeon exposing (Dungeon)
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
    { title = "Uniroad"
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
                        , fontFamilies [ "Ubuntu Mono", "monospace" ]
                        , displayFlex
                        , alignItems center
                        , justifyContent center
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
            case key of
                "h" ->
                    DungeonMsg (Dungeon.HeroMsg Hero.MoveLeft)

                "l" ->
                    DungeonMsg (Dungeon.HeroMsg Hero.MoveRight)

                "k" ->
                    DungeonMsg (Dungeon.HeroMsg Hero.MoveUp)

                "j" ->
                    DungeonMsg (Dungeon.HeroMsg Hero.MoveDown)

                _ ->
                    None
    in
    Json.Decode.map toMsg (Json.Decode.field "key" Json.Decode.string)
