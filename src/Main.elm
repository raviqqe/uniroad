module Main exposing (main)

import Browser exposing (Document)
import Browser.Events exposing (onKeyDown)
import Css exposing (..)
import Css.Global exposing (..)
import Dungeon exposing (Dungeon)
import Hero
import Html.Styled exposing (Html, div, toUnstyled)
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
    ( { dungeon = Dungeon.init }, Cmd.none )


type Msg
    = DungeonMsg Dungeon.Msg
    | None


update : Msg -> App -> ( App, Cmd Msg )
update msg app =
    case msg of
        DungeonMsg dungeonMsg ->
            ( { app | dungeon = Dungeon.update dungeonMsg app.dungeon }, Cmd.none )

        None ->
            ( app, Cmd.none )


view : App -> Document Msg
view app =
    { title = "Uniroad"
    , body =
        [ toUnstyled
            (div
                []
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
