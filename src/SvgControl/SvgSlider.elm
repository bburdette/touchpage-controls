module SvgControl.SvgSlider exposing (Model, Msg(..), Spec, UpdateMessage, UpdateType(..), buildEvtHandlerList, encodeSpec, encodeUpdateMessage, encodeUpdateType, getLocation, getX, getY, init, jsSpec, jsUpdateMessage, jsUpdateType, onMouseDown, onMouseLeave, onMouseMove, onMouseUp, onTouchCancel, onTouchEnd, onTouchLeave, onTouchMove, onTouchStart, resize, sliderEvt, toSpec, update, updsend, view)

-- import NoDragEvents exposing (onClick, onMouseUp, onMouseMove, onMouseDown, onMouseOut)

import Browser.Dom as BD
import Json.Decode as JD
import Json.Encode as JE
import List
import Svg exposing (Attribute, Svg, g, rect, svg, text)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick, onMouseDown, onMouseOut, onMouseUp)
import SvgControl.SvgCommand exposing (Command(..))
import SvgControl.SvgTextSize exposing (calcTextSvg, calcTextSvgM, resizeCommand)
import SvgControl.SvgThings exposing (ControlId, Orientation(..), Rect, SRect, UiColor(..), UiTheme, decodeControlId, encodeControlId, encodeOrientation, jsOrientation)
import SvgControl.SvgTouch as ST
import Toop
import VirtualDom as VD


type alias Spec =
    { name : String
    , label : Maybe String
    , orientation : Orientation
    }


jsSpec : JD.Decoder Spec
jsSpec =
    JD.map3 Spec
        (JD.field "name" JD.string)
        (JD.maybe (JD.field "label" JD.string))
        (JD.field "orientation" JD.string |> JD.andThen jsOrientation)


encodeSpec : Spec -> JE.Value
encodeSpec spec =
    JE.object
        (( "name"
         , JE.string spec.name
         )
            :: ( "orientation", encodeOrientation spec.orientation )
            :: (case spec.label of
                    Nothing ->
                        []

                    Just l ->
                        [ ( "label", JE.string l ) ]
               )
        )


type alias Model =
    { name : String
    , label : String
    , stringWidth : Maybe Float
    , cid : ControlId
    , rect : Rect
    , srect : SRect
    , orientation : Orientation
    , pressed : Bool
    , location : Float
    , textSvg : List (Svg ())
    , touchonly : Bool
    }


toSpec : Model -> Spec
toSpec model =
    { name = model.name
    , label =
        case model.label of
            "" ->
                Nothing

            _ ->
                Just model.label
    , orientation = model.orientation
    }


type Msg
    = SvgPress JE.Value
    | SvgUnpress JE.Value
    | NoOp
    | Reply String
    | SvgMoved JE.Value
    | SvgTouch ST.Msg
    | SvgUpdate UpdateMessage


type UpdateType
    = Press
    | Unpress


type alias UpdateMessage =
    { controlId : ControlId
    , updateType : Maybe UpdateType
    , location : Maybe Float
    , label : Maybe String
    }


init :
    Rect
    -> ControlId
    -> Spec
    -> ( Model, Command UpdateMessage a )
init rect cid spec =
    let
        model =
            Model spec.name
                (Maybe.withDefault "" spec.label)
                Nothing
                cid
                rect
                (SRect (String.fromInt rect.x)
                    (String.fromInt rect.y)
                    (String.fromInt rect.w)
                    (String.fromInt rect.h)
                )
                spec.orientation
                False
                0.5
                []
                False
    in
    ( model, resizeCommand model )


getX : JD.Decoder Int
getX =
    JD.field "offsetX" JD.int


getY : JD.Decoder Int
getY =
    JD.field "offsetY" JD.int


encodeUpdateMessage : UpdateMessage -> JD.Value
encodeUpdateMessage um =
    let
        outlist1 =
            [ ( "controlType", JE.string "slider" )
            , ( "controlId", encodeControlId um.controlId )
            ]

        outlist2 =
            case um.updateType of
                Just ut ->
                    List.append outlist1 [ ( "state", encodeUpdateType ut ) ]

                Nothing ->
                    outlist1

        outlist3 =
            case um.location of
                Just loc ->
                    List.append outlist2 [ ( "location", JE.float loc ) ]

                Nothing ->
                    outlist2

        outlist4 =
            case um.label of
                Just txt ->
                    List.append outlist3 [ ( "label", JE.string txt ) ]

                Nothing ->
                    outlist3
    in
    JE.object outlist4


encodeUpdateType : UpdateType -> JD.Value
encodeUpdateType ut =
    case ut of
        Press ->
            JE.string "Press"

        Unpress ->
            JE.string "Unpress"


jsUpdateMessage : JD.Decoder UpdateMessage
jsUpdateMessage =
    JD.map4 UpdateMessage
        (JD.field "controlId" decodeControlId)
        (JD.maybe (JD.field "state" JD.string |> JD.andThen jsUpdateType))
        (JD.maybe (JD.field "location" JD.float))
        (JD.maybe (JD.field "label" JD.string))


jsUpdateType : String -> JD.Decoder UpdateType
jsUpdateType ut =
    case ut of
        "Press" ->
            JD.succeed Press

        "Unpress" ->
            JD.succeed Unpress

        _ ->
            JD.succeed Unpress



-- get mouse/whatever location from the json message,
-- compute slider location from that.


getLocation : Model -> JD.Value -> Result String Float
getLocation model v =
    case model.orientation of
        Horizontal ->
            case JD.decodeValue getX v of
                Ok i ->
                    Ok
                        (toFloat (i - model.rect.x)
                            / toFloat model.rect.w
                        )

                Err e ->
                    Err (JD.errorToString e)

        Vertical ->
            case JD.decodeValue getY v of
                Ok i ->
                    Ok
                        (toFloat (i - model.rect.y)
                            / toFloat model.rect.h
                        )

                Err e ->
                    Err (JD.errorToString e)


update : BD.Element -> Msg -> Model -> ( Model, Command UpdateMessage a )
update elt msg model =
    case msg of
        SvgPress v ->
            case getLocation model v of
                Ok l ->
                    updsend model (Just Press) l

                _ ->
                    ( model, None )

        SvgUnpress v ->
            case model.pressed of
                True ->
                    updsend model (Just Unpress) model.location

                False ->
                    ( model, None )

        NoOp ->
            ( model, None )

        Reply s ->
            ( { model | name = s }, None )

        SvgMoved v ->
            case model.pressed of
                True ->
                    case getLocation model v of
                        Ok l ->
                            updsend model Nothing l

                        _ ->
                            ( model, None )

                False ->
                    ( model, None )

        SvgUpdate um ->
            -- sanity check for ids?  or don't.
            let
                mod =
                    { model
                        | pressed =
                            case um.updateType of
                                Just Press ->
                                    True

                                Just Unpress ->
                                    False

                                _ ->
                                    model.pressed
                        , location =
                            case um.location of
                                Just loc ->
                                    loc

                                Nothing ->
                                    model.location
                    }

                mod2 =
                    case um.label of
                        Just txt ->
                            { mod
                                | label = txt
                                , textSvg = []
                                , stringWidth = Nothing
                            }

                        Nothing ->
                            mod
            in
            ( mod2, resizeCommand mod2 )

        SvgTouch stm ->
            let
                rect =
                    model.rect |> (\r -> { r | x = r.x + round elt.element.x, y = r.y + round elt.element.y })
            in
            case ST.extractFirstRectTouchSE stm rect of
                Nothing ->
                    if model.pressed then
                        updsend model (Just Unpress) model.location

                    else
                        ( model, None )

                Just touch ->
                    case model.orientation of
                        Horizontal ->
                            let
                                loc =
                                    (touch.x - toFloat rect.x)
                                        / toFloat rect.w
                            in
                            if model.pressed then
                                updsend model (Just Press) loc

                            else
                                updsend model Nothing loc

                        Vertical ->
                            let
                                loc =
                                    (touch.y - toFloat rect.y)
                                        / toFloat rect.h
                            in
                            if model.pressed then
                                updsend model (Just Press) loc

                            else
                                updsend model Nothing loc


updsend : Model -> Maybe UpdateType -> Float -> ( Model, Command UpdateMessage a )
updsend model mbut loc =
    let
        bLoc =
            if loc > 1.0 then
                1.0

            else if loc < 0.0 then
                0.0

            else
                loc

        prest =
            mbut /= Just Unpress
    in
    -- if nothing changed, no message.
    if model.location == bLoc && model.pressed == prest then
        ( model, None )

    else
        -- let
        --     um =
        --         JE.encode 0
        --             (encodeUpdateMessage
        --                 (UpdateMessage model.cid mbut (Just bLoc) Nothing)
        --             )
        -- in
        ( { model | location = bLoc, pressed = prest }
        , Send (UpdateMessage model.cid mbut (Just bLoc) Nothing)
        )


resize : Model -> Rect -> ( Model, Command UpdateMessage a )
resize model rect =
    let
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



-- VIEW
-- try VD.onWithOptions for preventing scrolling on touchscreens and
-- etc. See virtualdom docs.


sliderEvt : String -> (JD.Value -> Msg) -> VD.Attribute Msg
sliderEvt evtname mkmsg =
    -- VD.onWithOptions evtname (VD.Options True True) (JD.map (\v -> mkmsg v) JD.value)
    VD.on evtname <|
        VD.Custom
            (JD.map
                (\v ->
                    { stopPropagation = True, preventDefault = True, message = mkmsg v }
                )
                JD.value
            )


onMouseDown =
    sliderEvt "mousedown" SvgPress


onMouseMove =
    sliderEvt "mousemove" SvgMoved


onMouseLeave =
    sliderEvt "mouseleave" SvgUnpress


onMouseUp =
    sliderEvt "mouseup" SvgUnpress


onTouchStart =
    sliderEvt "touchstart" (\e -> SvgTouch (ST.SvgTouchStart e))


onTouchEnd =
    sliderEvt "touchend" (\e -> SvgTouch (ST.SvgTouchEnd e))


onTouchCancel =
    sliderEvt "touchcancel" (\e -> SvgTouch (ST.SvgTouchCancel e))


onTouchLeave =
    sliderEvt "touchleave" (\e -> SvgTouch (ST.SvgTouchLeave e))


onTouchMove =
    sliderEvt "touchmove" (\e -> SvgTouch (ST.SvgTouchMove e))


buildEvtHandlerList : Bool -> List (VD.Attribute Msg)
buildEvtHandlerList touchonly =
    let
        te =
            [ onTouchStart
            , onTouchEnd
            , onTouchCancel
            , onTouchLeave
            , onTouchMove
            ]

        me =
            [ onMouseDown
            , onMouseUp
            , onMouseLeave
            , onMouseMove
            ]
    in
    if touchonly then
        te

    else
        List.append me te


view : UiTheme -> Model -> Svg Msg
view theme model =
    let
        (Toop.T4 sx sy sw sh) =
            case model.orientation of
                Vertical ->
                    Toop.T4 model.srect.x
                        (String.fromInt (round (model.location * toFloat model.rect.h) + model.rect.y))
                        model.srect.w
                        "3"

                Horizontal ->
                    Toop.T4 (String.fromInt (round (model.location * toFloat model.rect.w) + model.rect.x))
                        model.srect.y
                        "3"
                        model.srect.h

        evtlist =
            buildEvtHandlerList model.touchonly
    in
    g evtlist
        [ rect
            [ x model.srect.x
            , y model.srect.y
            , width model.srect.w
            , height model.srect.h
            , rx "2"
            , ry "2"
            , style ("fill: #" ++ theme.colorString Controls ++ ";")
            ]
            []
        , rect
            [ x sx
            , y sy
            , width sw
            , height sh
            , rx "2"
            , ry "2"
            , style
                ("fill: #"
                    ++ theme.colorString
                        (if model.pressed then
                            Pressed

                         else
                            Unpressed
                        )
                    ++ ";"
                )
            ]
            []
        , VD.map (\_ -> NoOp) (g [] model.textSvg)
        ]
