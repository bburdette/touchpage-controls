module SvgControlPage exposing
    ( ID
    , JsMessage(..)
    , Model
    , Msg(..)
    , Spec
    , init
    , jsMessage
    , jsSpec
    , onTextSize
    , resize
    , update
    , view
    , viewSvgControl
    )

import Dict exposing (..)
import Html
import Html.Attributes exposing (style)
import Json.Decode as JD
import List exposing (..)
import Svg exposing (Attribute, Svg, g, rect, svg, text)
import Svg.Attributes as SA exposing (height, viewBox, width, x, y)
import SvgControl.SvgCommand exposing (Command(..))
import SvgControl.SvgControl as SvgControl
import SvgControl.SvgTextSize exposing (TextSizeReply)
import SvgControl.SvgThings exposing (Orientation(..), Rect, SRect, UiColor(..), UiTheme, colorFun, defaultColors, toSRect)
import SvgControl.Util exposing (RectSize, andMap)
import VirtualDom as VD


type alias Spec =
    { title : String
    , rootControl : SvgControl.Spec
    , state : Maybe (List SvgControl.Msg)
    , controlsColor : Maybe String
    , labelsColor : Maybe String
    , textColor : Maybe String
    , pressedColor : Maybe String
    , unpressedColor : Maybe String
    , backgroundColor : Maybe String
    }


jsSpec : JD.Decoder Spec
jsSpec =
    JD.succeed Spec
        |> andMap
            (JD.field "title" JD.string)
        |> andMap
            (JD.field "rootControl" SvgControl.jsSpec)
        |> andMap
            (JD.maybe (JD.field "state" (JD.list SvgControl.jsUpdateMessage)))
        |> andMap
            (JD.maybe (JD.field "controlsColor" JD.string))
        |> andMap
            (JD.maybe (JD.field "labelsColor" JD.string))
        |> andMap
            (JD.maybe (JD.field "textColor" JD.string))
        |> andMap
            (JD.maybe (JD.field "pressedColor" JD.string))
        |> andMap
            (JD.maybe (JD.field "unpressedColor" JD.string))
        |> andMap
            (JD.maybe (JD.field "backgroundColor" JD.string))


type alias Model =
    { title : String
    , mahrect : Rect
    , srect : SRect
    , control : SvgControl.Model
    , windowSize : RectSize
    , uiTheme : UiTheme
    }


type alias ID =
    Int


type Msg
    = JsonMsg String
    | CMsg SvgControl.Msg
    | Resize RectSize
    | NoOp


type JsMessage
    = JmSpec Spec
    | JmUpdate Msg


jsMessage : JD.Decoder JsMessage
jsMessage =
    JD.oneOf
        [ jsSpec |> JD.andThen (\x -> JD.succeed (JmSpec x))
        , SvgControl.jsUpdateMessage
            |> JD.andThen
                (\x -> JD.succeed (JmUpdate (CMsg x)))
        ]


update : Msg -> Model -> ( Model, Command SvgControl.UpdateMessage )
update msg model =
    case msg of
        JsonMsg s ->
            case JD.decodeString jsMessage s of
                Ok (JmSpec spec) ->
                    init model.mahrect spec

                Ok (JmUpdate jmact) ->
                    update jmact model

                Err e ->
                    ( { model | title = JD.errorToString e }, None )

        CMsg act ->
            let
                wha =
                    SvgControl.update act model.control

                newmod =
                    { model | control = Tuple.first wha }
            in
            ( newmod, Tuple.second wha )

        Resize newSize ->
            resize newSize model

        NoOp ->
            ( model, None )


resize : RectSize -> Model -> ( Model, Command SvgControl.UpdateMessage )
resize newSize model =
    let
        nr =
            Rect 0 0 (round (newSize.width - 1)) (round (newSize.height - 4))

        ( ctrl, cmd ) =
            SvgControl.resize model.control nr
    in
    ( { model
        | mahrect = nr
        , srect = toSRect nr
        , windowSize = newSize
        , control = ctrl
      }
    , cmd
    )


onTextSize : TextSizeReply -> Model -> Model
onTextSize tsr model =
    { model | control = SvgControl.onTextSize model.uiTheme tsr model.control }


init :
    Rect
    -> Spec
    -> ( Model, Command SvgControl.UpdateMessage )
init rect spec =
    let
        ( conmod, cmd ) =
            SvgControl.init rect [] spec.rootControl

        ( updmod, cmds ) =
            SvgControl.update_list (Maybe.withDefault [] spec.state) conmod

        colors =
            colorFun
                (spec.controlsColor |> Maybe.withDefault (defaultColors Controls))
                (spec.labelsColor |> Maybe.withDefault (defaultColors Labels))
                (spec.textColor |> Maybe.withDefault (defaultColors Text))
                (spec.pressedColor |> Maybe.withDefault (defaultColors Pressed))
                (spec.unpressedColor |> Maybe.withDefault (defaultColors Unpressed))
                (spec.backgroundColor |> Maybe.withDefault (defaultColors Background))
    in
    ( Model spec.title
        rect
        (toSRect rect)
        updmod
        (RectSize 0 0)
        { colorString = colors }
    , Batch (cmd :: cmds)
    )


view : Model -> Html.Html Msg
view model =
    Html.div
        [ style "margin" "0"
        , style "touch-action" "none"
        ]
        [ Svg.svg
            [ width model.srect.w
            , height model.srect.h
            , viewBox
                (model.srect.x
                    ++ " "
                    ++ model.srect.y
                    ++ " "
                    ++ model.srect.w
                    ++ " "
                    ++ model.srect.h
                )
            ]
            [ rect
                [ x model.srect.x
                , y model.srect.y
                , width model.srect.w
                , height model.srect.h
                , SA.style ("fill: #" ++ model.uiTheme.colorString Background ++ ";")
                ]
                []
            , VD.map CMsg (viewSvgControl model.uiTheme model.control)
            ]
        ]


viewSvgControl : UiTheme -> SvgControl.Model -> Svg.Svg SvgControl.Msg
viewSvgControl theme conmodel =
    SvgControl.view theme conmodel
