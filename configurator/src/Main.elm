port module Main exposing (init, main)

import Browser
import Browser.Dom as BD
import Browser.Events as BE
import Element as E
import Html
import Json.Decode as JD
import Json.Encode as JE
import LayoutEdit
import SvgControl.SvgCommand as SvgCommand exposing (Command(..))
import SvgControl.SvgControl as SvgControl
import SvgControl.SvgLabel as SvgLabel
import SvgControl.SvgTextSize as SvgTextSize exposing (TextSizeReply, decodeTextSizeReply, encodeTextSizeRequest)
import SvgControl.SvgThings as SvgThings
import SvgControl.Util exposing (RectSize)
import SvgControlPage
import Task
import WebSocket



-- port receiveSocketMsg : (JD.Value -> msg) -> Sub msg
-- port sendSocketCommand : JE.Value -> Cmd msg
-- wssend =
--     WebSocket.send sendSocketCommand
-- wsreceive =
--     receiveSocketMsg <| WebSocket.receive WsMsg


port requestTextSize : JE.Value -> Cmd msg


port receiveTextMetrics : (JD.Value -> msg) -> Sub msg


type Msg
    = WsMsg (Result JD.Error WebSocket.WebSocketMsg)
    | TextSize (Result JD.Error TextSizeReply)
    | CoordMsg (BD.Element -> LayoutEdit.Msg) (Result BD.Error BD.Element)
    | LeMsg LayoutEdit.Msg


type alias Flags =
    { location : String
    , wsport : Int
    , width : Int
    , height : Int
    }


type State
    = LayoutEdit LayoutEdit.Model
    | BDError BD.Error


type alias Model =
    { state : State
    , size : RectSize
    }


commandToCmd : SvgCommand.Command SvgControl.UpdateMessage SvgControlPage.Msg -> Cmd Msg
commandToCmd scmd =
    case scmd of
        Send dta ->
            Cmd.none

        -- wssend <|
        --     WebSocket.Send
        --         { name = "touchpage"
        --         , content = dta
        --         }
        RequestTextWidth rtw ->
            requestTextSize <|
                encodeTextSizeRequest <|
                    rtw

        GetElement id fn ->
            BD.getElement id
                |> Task.attempt (CoordMsg (\elt -> LayoutEdit.ScpMsg (fn elt)))

        None ->
            Cmd.none

        Batch cmds ->
            Cmd.batch (List.map commandToCmd cmds)


processLeMsg lemsg lemodel mod =
    let
        ( umod, cmd ) =
            LayoutEdit.update
                lemsg
                lemodel
    in
    ( { mod | state = LayoutEdit umod }
    , commandToCmd cmd
    )


main : Program Flags Model Msg
main =
    Browser.document
        { init =
            \flags ->
                let
                    ( mod, cmd ) =
                        init flags
                in
                ( mod
                , commandToCmd cmd
                  -- , Cmd.none
                  -- , Cmd.batch
                  --     [ -- wssend <|
                  --       --     WebSocket.Connect
                  --       --         { name = "touchpage"
                  --       --         , address = mod.wsUrl
                  --       --         , protocol = "rust-websocket"
                  --       --         }
                  --       commandToCmd cmd
                  --     ]
                )
        , subscriptions =
            \_ ->
                Sub.batch
                    [ -- Sub.map ScpMsg <|
                      --     BE.onResize
                      --         (\a b ->
                      --             SvgControlPage.Resize <|
                      --                 RectSize (toFloat a) (toFloat b)
                      --         )
                      -- , wsreceive
                      receiveTextMetrics (TextSize << JD.decodeValue decodeTextSizeReply)
                    ]
        , update =
            \msg mod ->
                case ( msg, mod.state ) of
                    ( LeMsg sm, LayoutEdit lemodel ) ->
                        processLeMsg sm lemodel mod

                    ( TextSize (Ok tsr), LayoutEdit lemodel ) ->
                        ( { mod | state = LayoutEdit (LayoutEdit.onTextSize tsr lemodel) }
                        , Cmd.none
                        )

                    ( CoordMsg fn res, LayoutEdit lemodel ) ->
                        case res of
                            Ok elt ->
                                processLeMsg (fn elt) lemodel mod

                            Err e ->
                                ( { mod | state = BDError e }, Cmd.none )

                    _ ->
                        ( mod, Cmd.none )
        , view =
            \model ->
                case model.state of
                    LayoutEdit lm ->
                        Browser.Document "svg control edit"
                            [ E.layout [] (E.map LeMsg <| LayoutEdit.view model.size lm) ]

                    BDError (BD.NotFound s) ->
                        Browser.Document "svg control edit"
                            [ E.layout [] (E.text <| "not found: " ++ s) ]
        }


init : Flags -> ( Model, Command SvgControl.UpdateMessage SvgControlPage.Msg )
init flags =
    let
        rmargin =
            4

        ( sm, cmd ) =
            SvgControlPage.init
                -- (SvgThings.Rect 0 0 (flags.width - rmargin) (flags.height - rmargin))
                "layout"
                (SvgThings.Rect 0 0 500 500)
                (SvgControlPage.Spec
                    ""
                    (SvgControl.CsLabel (SvgLabel.Spec "empty" "except for this label!"))
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                )

        size =
            { width = 500, height = 500 }

        -- { width = toFloat flags.width, height = toFloat flags.height }
    in
    ( { state = LayoutEdit <| LayoutEdit.init sm size Nothing
      , size = size
      }
    , cmd
    )
