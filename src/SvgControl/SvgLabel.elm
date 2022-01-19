module SvgControl.SvgLabel exposing (Model, Msg(..), Spec, UpdateMessage, encodeUpdateMessage, init, jsSpec, jsUpdateMessage, resize, update, view)

import Html exposing (Html)
import Html.Events exposing (onClick, onMouseDown, onMouseOut, onMouseUp)
import Json.Decode as JD
import Json.Encode as JE
import String
import Svg exposing (Attribute, Svg, g, rect, svg, text)
import Svg.Attributes exposing (..)
import SvgControl.SvgCommand exposing (Command(..))
import SvgControl.SvgTextSize exposing (..)
import SvgControl.SvgThings exposing (ControlId, Rect, SRect, UiColor(..), UiTheme, decodeControlId, encodeControlId)
import Task
import Template exposing (render, template)
import Time exposing (..)
import VirtualDom as VD


type alias Spec =
    { name : String
    , label : String
    }


jsSpec : JD.Decoder Spec
jsSpec =
    JD.map2 Spec
        (JD.field "name" JD.string)
        (JD.field "label" JD.string)


type alias Model =
    { name : String
    , label : String
    , stringWidth : Maybe Float -- not used??
    , cid : ControlId
    , rect : Rect
    , srect : SRect
    , textSvg : List (Svg ())
    }


type Msg
    = SvgUpdate UpdateMessage
    | NoOp


type alias UpdateMessage =
    { controlId : ControlId
    , label : String
    }


jsUpdateMessage : JD.Decoder UpdateMessage
jsUpdateMessage =
    JD.map2 UpdateMessage
        (JD.field "controlId" decodeControlId)
        (JD.field "label" JD.string)


encodeUpdateMessage : UpdateMessage -> JD.Value
encodeUpdateMessage um =
    JE.object
        [ ( "controlType", JE.string "label" )
        , ( "controlId", encodeControlId um.controlId )
        , ( "label", JE.string um.label )
        ]


init :
    Rect
    -> ControlId
    -> Spec
    -> ( Model, Command UpdateMessage )
init rect cid spec =
    let
        model =
            Model spec.name
                spec.label
                Nothing
                cid
                rect
                (SRect (String.fromInt rect.x)
                    (String.fromInt rect.y)
                    (String.fromInt rect.w)
                    (String.fromInt rect.h)
                )
                []
    in
    ( model, resizeCommand model )


update : Msg -> Model -> ( Model, Command UpdateMessage )
update msg model =
    case msg of
        SvgUpdate um ->
            let
                newmodel =
                    { model | label = um.label, textSvg = [], stringWidth = Nothing }
            in
            ( newmodel, resizeCommand newmodel )

        NoOp ->
            ( model, None )


resize : Model -> Rect -> ( Model, Command UpdateMessage )
resize model rect =
    let
        -- ts = calcTextSvgM theme model
        newmodel =
            { model
                | rect = rect
                , srect =
                    SRect (String.fromInt rect.x)
                        (String.fromInt rect.y)
                        (String.fromInt rect.w)
                        (String.fromInt rect.h)
                , textSvg = []
                , stringWidth = Nothing
            }
    in
    ( newmodel, resizeCommand newmodel )


view : UiTheme -> Model -> Svg Msg
view theme model =
    let
        _ =
            Debug.log "svglabel model" model

        lbrect =
            rect
                [ x model.srect.x
                , y model.srect.y
                , width model.srect.w
                , height model.srect.h
                , style ("fill: #" ++ theme.colorString Labels ++ ";")
                ]
                []

        svgl =
            lbrect :: model.textSvg
    in
    VD.map (\_ -> NoOp) (g [] svgl)
