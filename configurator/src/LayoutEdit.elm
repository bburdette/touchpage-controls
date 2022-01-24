module LayoutEdit exposing (Model, Msg(..), buttonStyle, onTextSize, update, view)

-- import Dialog as D
-- import TangoColors as TC
-- exposing (SvgControlPage)

import Dict
import Element as E exposing (Element)
import Element.Background as EBk
import Element.Border as EBd
import Element.Events as EE
import Element.Font as EF
import Element.Input as EI
import Element.Region
import FlatColors.BritishPalette as Color
import SvgControl.SvgButton as SvgButton
import SvgControl.SvgCommand as SvgCommand exposing (Command(..))
import SvgControl.SvgControl as SvgControl
import SvgControl.SvgLabel as SvgLabel
import SvgControl.SvgSlider as SvgSlider
import SvgControl.SvgTextSize exposing (TextSizeReply)
import SvgControl.SvgThings as SvgThings exposing (ControlId)
import SvgControl.SvgXY as SvgXY
import SvgControl.Util as Util
import SvgControlPage
import Toop



-- import Util
-- import WindowKeys as WK


buttonStyle =
    [ EBk.color Color.blueberrySoda
    , EF.color Color.hintOfPensive
    , EBd.color Color.blueNights
    , E.paddingXY 10 5
    , EBd.rounded 3
    ]


type Msg
    = AddButtonPress
    | AddHSliderPress
    | AddVSliderPress
    | AddHSizerPress
    | AddVSizerPress
    | AddXYPress
    | AddLabelPress
    | TreeRowClicked ControlId
    | ScpMsg SvgControlPage.Msg


type alias Model =
    { scpModel : SvgControlPage.Model
    , size : Util.RectSize
    , selected : Maybe ControlId
    }


onTextSize : TextSizeReply -> Model -> Model
onTextSize tsr model =
    { model | scpModel = SvgControlPage.onTextSize tsr model.scpModel }


controlTree : Maybe ControlId -> SvgControlPage.Model -> Element Msg
controlTree mbselected svgmod =
    let
        l =
            controlTreeH 0 mbselected svgmod.control
    in
    E.column [] l


controlTreeH : Int -> Maybe ControlId -> SvgControl.Model -> List (Element Msg)
controlTreeH indent mbselected spec =
    let
        rowattribs controlid =
            [ E.spacing 10
            , E.paddingEach
                { top = 0
                , right = 0
                , bottom = 0
                , left = indent * 20
                }
            , EE.onClick (TreeRowClicked controlid)
            ]
                ++ (if Just controlid == mbselected then
                        [ EBk.color Color.blueNights ]

                    else
                        []
                   )
    in
    case spec of
        SvgControl.CmButton cmod ->
            [ E.row (rowattribs cmod.cid) [ E.text "Button", E.text cmod.name, E.text cmod.label ] ]

        SvgControl.CmSlider cmod ->
            [ E.row (rowattribs cmod.cid) [ E.text "Slider", E.text cmod.name, E.text cmod.label ] ]

        SvgControl.CmXY cmod ->
            [ E.row (rowattribs cmod.cid) [ E.text "XY", E.text cmod.name, E.text cmod.label ] ]

        SvgControl.CmLabel cmod ->
            [ E.row (rowattribs cmod.cid) [ E.text "Label", E.text cmod.name, E.text cmod.label ] ]

        SvgControl.CmSizer cmod ->
            let
                moar =
                    cmod.controls
                        |> Dict.values
                        |> List.map (controlTreeH (indent + 1) mbselected)
                        |> List.concat
            in
            E.row (rowattribs cmod.cid) [ E.text "Sizer" ] :: moar



-- onWkKeyPress : WK.Key -> Model -> ( Model, Command )
-- onWkKeyPress key model =
--     case Toop.T4 key.key key.ctrl key.alt key.shift of
--         Toop.T4 "Enter" False False False ->
--             handleSPUpdate model (SP.onEnter model.spmodel)
--         _ ->
--             ( model, None )


view : Util.RectSize -> Model -> Element Msg
view size model =
    let
        maxwidth =
            700

        titlemaxconst =
            85
    in
    E.row [ E.width E.fill, E.spacing 8 ]
        [ E.column [ E.spacing 8 ]
            [ EI.button buttonStyle
                { onPress = Just AddHSizerPress
                , label = E.text "Add HSizer"
                }
            , EI.button buttonStyle
                { onPress = Just AddVSizerPress
                , label = E.text "Add VSizer"
                }
            , EI.button buttonStyle
                { onPress = Just AddButtonPress
                , label = E.text "Add Button"
                }
            , EI.button buttonStyle
                { onPress = Just AddHSliderPress
                , label = E.text "Add HSlider"
                }
            , EI.button buttonStyle
                { onPress = Just AddVSliderPress
                , label = E.text "Add VSlider"
                }
            , EI.button buttonStyle
                { onPress = Just AddXYPress
                , label = E.text "Add XY"
                }
            , EI.button buttonStyle
                { onPress = Just AddLabelPress
                , label = E.text "Add Label"
                }
            ]
        , controlTree model.selected model.scpModel
        , E.el [ E.centerX, E.centerY ] <| E.map ScpMsg <| E.html (SvgControlPage.view model.scpModel)
        ]


initScp : SvgThings.Rect -> SvgControl.Spec -> ( SvgControlPage.Model, Command SvgControl.UpdateMessage )
initScp size spec =
    SvgControlPage.init
        size
        (SvgControlPage.Spec
            ""
            spec
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
        )


update : Msg -> Model -> ( Model, Command SvgControl.UpdateMessage )
update msg model =
    let
        addcontrol =
            \spec ->
                case model.selected of
                    Just cid ->
                        let
                            ( nc, c ) =
                                SvgControl.modControl cid
                                    (SvgControl.addControl cid
                                        spec
                                    )
                                    model.scpModel.control

                            sm =
                                model.scpModel
                        in
                        ( { model
                            | scpModel = { sm | control = nc }
                          }
                        , c
                        )

                    Nothing ->
                        ( model, None )
    in
    case msg of
        TreeRowClicked controlid ->
            ( { model
                | selected =
                    if model.selected == Just controlid then
                        Nothing

                    else
                        Just controlid
              }
            , None
            )

        AddHSizerPress ->
            addcontrol
                (SvgControl.CsSizer
                    { orientation = SvgThings.Horizontal
                    , proportions = Nothing
                    , controls = []
                    }
                )

        AddVSizerPress ->
            addcontrol
                (SvgControl.CsSizer
                    { orientation = SvgThings.Vertical
                    , proportions = Nothing
                    , controls = []
                    }
                )

        AddButtonPress ->
            addcontrol
                (SvgControl.CsButton (SvgButton.Spec "test" (Just "test button")))

        AddHSliderPress ->
            addcontrol
                (SvgControl.CsSlider (SvgSlider.Spec "" Nothing SvgThings.Horizontal))

        AddVSliderPress ->
            addcontrol
                (SvgControl.CsSlider (SvgSlider.Spec "" Nothing SvgThings.Vertical))

        AddXYPress ->
            addcontrol
                (SvgControl.CsXY (SvgXY.Spec "test" (Just "test button")))

        AddLabelPress ->
            addcontrol
                (SvgControl.CsLabel (SvgLabel.Spec "name" "label"))

        ScpMsg scpmsg ->
            ( model, None )



-- ScpMsg sm ->
--     let
--         ( umod, cmd ) =
--             SvgControlPage.update sm mod.scpModel
--     in
--     ( { mod | scpModel = umod }
--     , commandToCmd cmd
--     )
-- handleSPUpdate : Model -> ( SP.Model, SP.Command ) -> ( Model, Command )
-- handleSPUpdate model ( nm, cm ) =
--     let
--         mod =
--             { model | spmodel = nm }
--     in
--     case cm of
--         SP.None ->
--             ( mod, None )
--         SP.Save ->
--             ( mod, None )
--         SP.Copy _ ->
--             ( mod, None )
--         SP.Search ts ->
--             ( mod, Search ts )
