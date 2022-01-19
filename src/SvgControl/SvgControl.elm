module SvgControl.SvgControl exposing
    ( ID
    , Model(..)
    , Msg(..)
    , Spec(..)
    , SzModel
    , SzMsg(..)
    , SzSpec
    , UpdateMessage
    , border
    , controlId
    , controlName
    , findControl
    , firstJust
    , init
    , jsCs
    , jsSpec
    , jsSzSpec
    , jsUmType
    , jsUpdateMessage
    , mkRlist
    , myTail
    , onTextSize
    , processProps
    , resize
    , szFindControl
    , szOnTextSize
    , szinit
    , szresize
    , szupdate
    , szview
    , toCtrlMsg
    , tupMap2
    , update
    , update_list
    , view
    , viewSvgControls
    , zip
    )

import Dict exposing (..)
import Html
import Json.Decode as JD
import List exposing (..)
import Svg exposing (Svg)
import Svg.Attributes as SA
import SvgControl.SvgButton as SvgButton
import SvgControl.SvgCommand exposing (Command(..), cmdMap)
import SvgControl.SvgLabel as SvgLabel
import SvgControl.SvgSlider as SvgSlider
import SvgControl.SvgTextSize exposing (TextSizeReply, calcTextSvg, onTextSizeReply)
import SvgControl.SvgThings exposing (ControlId, Orientation(..), Rect, UiColor(..), UiTheme, containsXY, hrects, hrectsp, jsOrientation, shrinkRect, vrects, vrectsp)
import SvgControl.SvgXY as SvgXY
import Task
import VirtualDom as VD



----------------------------------------------------------
-- Both control container and sizer ard in this file.
-- Normally I'd break them out into separate files, but
-- they are mutually recursive so they have to
-- both be in a single file.
-------------------- control container -------------------


type Spec
    = CsButton SvgButton.Spec
    | CsSlider SvgSlider.Spec
    | CsXY SvgXY.Spec
    | CsLabel SvgLabel.Spec
    | CsSizer SzSpec


type Model
    = CmButton SvgButton.Model
    | CmSlider SvgSlider.Model
    | CmXY SvgXY.Model
    | CmLabel SvgLabel.Model
    | CmSizer SzModel


type Msg
    = CaButton SvgButton.Msg
    | CaSlider SvgSlider.Msg
    | CaXY SvgXY.Msg
    | CaLabel SvgLabel.Msg
    | CaSizer SzMsg


type UpdateMessage
    = UmButton SvgButton.UpdateMessage
    | UmSlider SvgSlider.UpdateMessage
    | UmXY SvgXY.UpdateMessage
    | UmLabel SvgLabel.UpdateMessage
    | UmSizer UpdateMessage


encodeUpdateMessage : UpdateMessage -> JD.Value
encodeUpdateMessage um =
    case um of
        UmButton u ->
            SvgButton.encodeUpdateMessage u

        UmSlider u ->
            SvgSlider.encodeUpdateMessage u

        UmXY u ->
            SvgXY.encodeUpdateMessage u

        UmLabel u ->
            SvgLabel.encodeUpdateMessage u

        UmSizer u ->
            encodeUpdateMessage u


findControl : Int -> Int -> Model -> Maybe Model
findControl x y mod =
    case mod of
        CmButton bmod ->
            if containsXY bmod.rect x y then
                Just mod

            else
                Nothing

        CmSlider smod ->
            if containsXY smod.rect x y then
                Just mod

            else
                Nothing

        CmXY smod ->
            if containsXY smod.rect x y then
                Just mod

            else
                Nothing

        CmLabel smod ->
            Nothing

        CmSizer szmod ->
            szFindControl szmod x y


controlId : Model -> ControlId
controlId mod =
    case mod of
        CmButton bmod ->
            bmod.cid

        CmSlider smod ->
            smod.cid

        CmXY smod ->
            smod.cid

        CmLabel smod ->
            smod.cid

        CmSizer szmod ->
            szmod.cid


controlName : Model -> Maybe String
controlName mod =
    case mod of
        CmButton bmod ->
            Just bmod.name

        CmSlider smod ->
            Just smod.name

        CmXY smod ->
            Just smod.name

        CmLabel smod ->
            Just smod.name

        CmSizer szmod ->
            Nothing


tupMap2 : (a -> c) -> ( a, b ) -> ( c, b )
tupMap2 fa ab =
    ( fa (Tuple.first ab), Tuple.second ab )


resize : Model -> Rect -> ( Model, Command UpdateMessage )
resize model rect =
    let
        aptg =
            \f g ( m, c ) -> ( f m, g c )
    in
    case model of
        CmButton mod ->
            aptg CmButton (cmdMap UmButton) <| SvgButton.resize mod (shrinkRect border rect)

        CmSlider mod ->
            aptg CmSlider (cmdMap UmSlider) <| SvgSlider.resize mod (shrinkRect border rect)

        CmXY mod ->
            aptg CmXY (cmdMap UmXY) <| SvgXY.resize mod (shrinkRect border rect)

        CmLabel mod ->
            aptg CmLabel (cmdMap UmLabel) <| SvgLabel.resize mod (shrinkRect border rect)

        CmSizer mod ->
            aptg CmSizer (cmdMap UmSizer) <| szresize mod rect


jsSpec : JD.Decoder Spec
jsSpec =
    JD.field "type" JD.string |> JD.andThen jsCs


jsCs : String -> JD.Decoder Spec
jsCs t =
    case t of
        "button" ->
            SvgButton.jsSpec |> JD.andThen (\a -> JD.succeed (CsButton a))

        "slider" ->
            SvgSlider.jsSpec |> JD.andThen (\a -> JD.succeed (CsSlider a))

        "xy" ->
            SvgXY.jsSpec |> JD.andThen (\a -> JD.succeed (CsXY a))

        "label" ->
            SvgLabel.jsSpec |> JD.andThen (\a -> JD.succeed (CsLabel a))

        "sizer" ->
            jsSzSpec |> JD.andThen (\a -> JD.succeed (CsSizer a))

        _ ->
            JD.fail ("unkown type: " ++ t)


jsUpdateMessage : JD.Decoder Msg
jsUpdateMessage =
    JD.field "controlType" JD.string |> JD.andThen jsUmType


jsUmType : String -> JD.Decoder Msg
jsUmType wat =
    case wat of
        "button" ->
            SvgButton.jsUpdateMessage
                |> JD.andThen
                    (\x -> JD.succeed (toCtrlMsg x.controlId (CaButton (SvgButton.SvgUpdate x))))

        "slider" ->
            SvgSlider.jsUpdateMessage
                |> JD.andThen
                    (\x -> JD.succeed (toCtrlMsg x.controlId (CaSlider (SvgSlider.SvgUpdate x))))

        "xy" ->
            SvgXY.jsUpdateMessage
                |> JD.andThen
                    (\x -> JD.succeed (toCtrlMsg x.controlId (CaXY (SvgXY.SvgUpdate x))))

        "label" ->
            SvgLabel.jsUpdateMessage
                |> JD.andThen
                    (\x -> JD.succeed (toCtrlMsg x.controlId (CaLabel (SvgLabel.SvgUpdate x))))

        _ ->
            JD.fail ("unknown update type" ++ wat)


myTail : List a -> List a
myTail lst =
    let
        tl =
            tail lst
    in
    case tl of
        Just l ->
            l

        Nothing ->
            []


toCtrlMsg : ControlId -> Msg -> Msg
toCtrlMsg id msg =
    case head id of
        Nothing ->
            msg

        Just x ->
            CaSizer (SzCMsg x (toCtrlMsg (myTail id) msg))


onTextSize : UiTheme -> TextSizeReply -> Model -> Model
onTextSize theme tsr model =
    case model of
        CmButton m ->
            CmButton <|
                onTextSizeReply theme tsr m

        CmSlider m ->
            CmSlider <|
                onTextSizeReply theme tsr m

        CmXY m ->
            CmXY <|
                onTextSizeReply theme tsr m

        CmLabel m ->
            CmLabel <| onTextSizeReply theme tsr m

        CmSizer m ->
            CmSizer <| szOnTextSize theme tsr m


update : Msg -> Model -> ( Model, Command UpdateMessage )
update msg model =
    case ( msg, model ) of
        ( CaButton ms, CmButton m ) ->
            let
                ( a, b ) =
                    SvgButton.update ms m
            in
            ( CmButton a, cmdMap UmButton b )

        ( CaSlider ms, CmSlider m ) ->
            let
                ( a, b ) =
                    SvgSlider.update ms m
            in
            ( CmSlider a, cmdMap UmSlider b )

        ( CaXY ms, CmXY m ) ->
            let
                ( a, b ) =
                    SvgXY.update ms m
            in
            ( CmXY a, cmdMap UmXY b )

        ( CaLabel ms, CmLabel m ) ->
            let
                ( md, c ) =
                    SvgLabel.update ms m
            in
            ( CmLabel md, cmdMap UmLabel c )

        ( CaSizer ms, CmSizer m ) ->
            let
                ( a, b ) =
                    szupdate ms m
            in
            ( CmSizer a, cmdMap UmSizer b )

        _ ->
            ( model, None )



-- should probably produce an error.  to the user??


update_list : List Msg -> Model -> ( Model, List (Command UpdateMessage) )
update_list msgs model =
    List.foldl
        (\msg ( mod, cmds ) ->
            let
                ( modnew, cmd ) =
                    update msg mod
            in
            ( modnew, cmd :: cmds )
        )
        ( model, [] )
        msgs


init :
    Rect
    -> ControlId
    -> Spec
    -> ( Model, Command UpdateMessage )
init rect cid spec =
    let
        aptg =
            \f g ( m, c ) -> ( f m, g c )
    in
    case spec of
        CsButton s ->
            aptg CmButton (cmdMap UmButton) <| SvgButton.init (shrinkRect border rect) cid s

        CsSlider s ->
            aptg CmSlider (cmdMap UmSlider) <| SvgSlider.init (shrinkRect border rect) cid s

        CsXY s ->
            aptg CmXY (cmdMap UmXY) <| SvgXY.init (shrinkRect border rect) cid s

        CsLabel s ->
            aptg CmLabel (cmdMap UmLabel) <| SvgLabel.init (shrinkRect border rect) cid s

        CsSizer s ->
            aptg CmSizer (cmdMap UmSizer) <| szinit rect cid s


view : UiTheme -> Model -> Svg Msg
view theme model =
    case model of
        CmButton m ->
            VD.map CaButton (SvgButton.view theme m)

        CmSlider m ->
            VD.map CaSlider (SvgSlider.view theme m)

        CmXY m ->
            VD.map CaXY (SvgXY.view theme m)

        CmLabel m ->
            VD.map CaLabel (SvgLabel.view theme m)

        CmSizer m ->
            VD.map CaSizer (szview m theme)



-------------------- sizer -------------------


{-| json spec
-}
type alias SzSpec =
    { orientation : Orientation
    , proportions : Maybe (List Float)
    , controls : List Spec
    }



-- proportions should all add up to 1.0


processProps : List Float -> List Float
processProps lst =
    let
        s =
            sum lst
    in
    List.map (\x -> x / s) lst


jsSzSpec : JD.Decoder SzSpec
jsSzSpec =
    JD.map3 SzSpec
        (JD.field "orientation" JD.string |> JD.andThen jsOrientation)
        (JD.maybe (JD.field "proportions" (JD.list JD.float))
            |> JD.andThen
                (\x -> JD.succeed (Maybe.map processProps x))
        )
        (JD.field "controls" (JD.list (JD.lazy (\_ -> jsSpec))))


type alias SzModel =
    { cid : ControlId
    , rect : Rect
    , controls : Dict ID Model
    , orientation : Orientation
    , proportions : Maybe (List Float)
    }


type alias ID =
    Int


szFindControl : SzModel -> Int -> Int -> Maybe Model
szFindControl mod x y =
    if containsXY mod.rect x y then
        firstJust (findControl x y) (values mod.controls)

    else
        Nothing


firstJust : (a -> Maybe b) -> List a -> Maybe b
firstJust f xs =
    case head xs of
        Nothing ->
            Nothing

        Just x ->
            case f x of
                Just v ->
                    Just v

                Nothing ->
                    Maybe.andThen (firstJust f) (tail xs)



-- UPDATE


type SzMsg
    = SzCMsg ID Msg


zip =
    List.map2 Tuple.pair


szupdate : SzMsg -> SzModel -> ( SzModel, Command UpdateMessage )
szupdate msg model =
    case msg of
        SzCMsg id act ->
            let
                bb =
                    get id model.controls
            in
            case bb of
                Just bm ->
                    let
                        wha =
                            update act bm

                        updcontrols =
                            insert id (Tuple.first wha) model.controls

                        newmod =
                            { model | controls = updcontrols }
                    in
                    ( newmod, Tuple.second wha )

                Nothing ->
                    ( model, None )


szOnTextSize : UiTheme -> TextSizeReply -> SzModel -> SzModel
szOnTextSize theme tsr model =
    case tsr.controlId of
        idx :: rst ->
            case Dict.get idx model.controls of
                Just control ->
                    let
                        nc =
                            onTextSize theme { tsr | controlId = rst } control
                    in
                    { model | controls = Dict.insert idx nc model.controls }

                Nothing ->
                    model

        [] ->
            model


szresize : SzModel -> Rect -> ( SzModel, Command UpdateMessage )
szresize model rect =
    let
        clist =
            Dict.toList model.controls

        rlist =
            mkRlist model.orientation rect (List.length clist) model.proportions

        rlist2 =
            List.map (\( ( i, c ), r ) -> ( i, resize c r )) (zip clist rlist)

        controls =
            List.map (\( i, ( m, c ) ) -> ( i, m )) rlist2

        cmds =
            List.map (\( i, ( m, c ) ) -> c) rlist2

        cdict =
            Dict.fromList controls

        nm =
            { model | rect = rect, controls = cdict }
    in
    ( nm, Batch cmds )


mkRlist : Orientation -> Rect -> Int -> Maybe (List Float) -> List Rect
mkRlist orientation rect count mbproportions =
    case orientation of
        Horizontal ->
            case mbproportions of
                Nothing ->
                    hrects rect count

                Just p ->
                    hrectsp rect count p

        Vertical ->
            case mbproportions of
                Nothing ->
                    vrects rect count

                Just p ->
                    vrectsp rect count p


szinit :
    Rect
    -> ControlId
    -> SzSpec
    -> ( SzModel, Command UpdateMessage )
szinit rect cid szspec =
    let
        rlist =
            mkRlist szspec.orientation rect (List.length szspec.controls) szspec.proportions

        blist =
            List.map
                (\( spec, rect_, idx ) -> init rect_ (cid ++ [ idx ]) spec)
                (map3 (\a b c -> ( a, b, c )) szspec.controls rlist idxs)

        mods =
            List.map Tuple.first blist

        cmds =
            List.map Tuple.second blist

        idxs =
            List.range 0 (length szspec.controls)

        controlz =
            zip idxs mods

        model =
            SzModel cid rect (Dict.fromList controlz) szspec.orientation szspec.proportions
    in
    ( model, Batch cmds )



-- VIEW


szview : SzModel -> UiTheme -> Svg SzMsg
szview model theme =
    let
        controllst =
            Dict.toList model.controls
    in
    Svg.g [] (List.map (viewSvgControls theme) controllst)


viewSvgControls : UiTheme -> ( ID, Model ) -> Svg.Svg SzMsg
viewSvgControls theme ( id, model ) =
    VD.map (SzCMsg id) (view theme model)


border : Int
border =
    1
