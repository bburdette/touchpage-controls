module LayoutEdit exposing (Model, Msg(..), buttonStyle, init, onTextSize, update, view)

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
import SvgControl.SvgControl as SvgControl exposing (getControl)
import SvgControl.SvgLabel as SvgLabel
import SvgControl.SvgSlider as SvgSlider
import SvgControl.SvgTextSize exposing (TextSizeReply)
import SvgControl.SvgThings as SvgThings exposing (ControlId, Orientation(..))
import SvgControl.SvgXY as SvgXY
import SvgControl.Util as Util
import SvgControlPage
import Toop


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
    | DeletePress
    | TreeRowClicked ControlId
    | EdNameChanged String
    | EdLabelChanged String
    | EdOrientationChanged Orientation
    | ScpMsg SvgControlPage.Msg


type alias Model =
    { scpModel : SvgControlPage.Model
    , size : Util.RectSize
    , selected : Maybe ControlId
    , edname : Maybe String
    , edlabel : Maybe String
    , edorientation : Maybe Orientation
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
controlTreeH indent mbselected model =
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
                        [ EBk.color Color.mattPurple ]

                    else
                        []
                   )

        showId =
            \cid ->
                cid
                    |> List.map String.fromInt
                    |> List.intersperse ","
                    |> String.concat
    in
    case model of
        SvgControl.CmButton cmod ->
            [ E.row (rowattribs cmod.cid)
                [ E.text "Button"
                , E.text cmod.name
                , E.text cmod.label
                , E.text <| showId cmod.cid
                ]
            ]

        SvgControl.CmSlider cmod ->
            [ E.row (rowattribs cmod.cid)
                [ E.text "Slider"
                , E.text cmod.name
                , E.text cmod.label
                , E.text <| showId cmod.cid
                ]
            ]

        SvgControl.CmXY cmod ->
            [ E.row (rowattribs cmod.cid)
                [ E.text "XY"
                , E.text cmod.name
                , E.text cmod.label
                , E.text <| showId cmod.cid
                ]
            ]

        SvgControl.CmLabel cmod ->
            [ E.row (rowattribs cmod.cid)
                [ E.text "Label"
                , E.text cmod.name
                , E.text cmod.label
                , E.text <| showId cmod.cid
                ]
            ]

        SvgControl.CmSizer cmod ->
            let
                moar =
                    cmod.controls
                        |> Dict.values
                        |> List.map (controlTreeH (indent + 1) mbselected)
                        |> List.concat
            in
            E.row (rowattribs cmod.cid)
                [ if cmod.orientation == SvgThings.Horizontal then
                    E.text "HSizer"

                  else
                    E.text "VSizer"
                , E.text <| showId cmod.cid
                ]
                :: moar



-- onWkKeyPress : WK.Key -> Model -> ( Model, Command )
-- onWkKeyPress key model =
--     case Toop.T4 key.key key.ctrl key.alt key.shift of
--         Toop.T4 "Enter" False False False ->
--             handleSPUpdate model (SP.onEnter model.spmodel)
--         _ ->
--             ( model, None )


editPanel : Model -> Element Msg
editPanel model =
    E.column
        [ EBk.color Color.mattPurple
        , E.padding 10
        ]
    <|
        List.filterMap identity
            [ model.edname
                |> Maybe.map
                    (\name ->
                        EI.text []
                            { onChange = EdNameChanged
                            , text = name
                            , placeholder = Nothing
                            , label = EI.labelLeft [] (E.text "name")
                            }
                    )
            , model.edlabel
                |> Maybe.map
                    (\label ->
                        EI.text []
                            { onChange = EdLabelChanged
                            , text = label
                            , placeholder = Nothing
                            , label = EI.labelLeft [] (E.text "label")
                            }
                    )
            , model.edorientation
                |> Maybe.map
                    (\o ->
                        EI.radio []
                            { onChange = EdOrientationChanged
                            , options =
                                [ EI.option Horizontal <| E.text "horizontal"
                                , EI.option Vertical <| E.text "vertical"
                                ]
                            , selected = Just o
                            , label = EI.labelLeft [] (E.text "orientation")
                            }
                    )
            ]


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
            , EI.button buttonStyle
                { onPress = Just DeletePress
                , label = E.text "Delete"
                }
            ]
        , E.column [ E.spacing 15 ]
            [ controlTree model.selected model.scpModel
            , editPanel model
            ]
        , E.el [ E.centerX, E.centerY ] <| E.map ScpMsg <| E.html (SvgControlPage.view model.scpModel)
        ]


init : SvgControlPage.Model -> Util.RectSize -> Maybe ControlId -> Model
init scpModel size selected =
    { scpModel = scpModel
    , size = size
    , selected = selected
    , edname = Nothing
    , edlabel = Nothing
    , edorientation = Nothing
    }


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


focusFields : SvgControl.Model -> Model -> Model
focusFields control model =
    case control of
        SvgControl.CmButton mod ->
            { model
                | edname = Just mod.name
                , edlabel = Just mod.label
                , edorientation = Nothing
            }

        SvgControl.CmSlider mod ->
            { model
                | edname = Just mod.name
                , edlabel = Just mod.label
                , edorientation = Nothing
            }

        SvgControl.CmXY mod ->
            { model
                | edname = Just mod.name
                , edlabel = Just mod.label
                , edorientation = Nothing
            }

        SvgControl.CmLabel mod ->
            { model
                | edname = Just mod.name
                , edlabel = Just mod.label
                , edorientation = Nothing
            }

        SvgControl.CmSizer mod ->
            { model
                | edname = Nothing
                , edlabel = Nothing
                , edorientation = Just mod.orientation
            }


focusFieldsMod : SvgControl.Model -> Model -> SvgControl.Model
focusFieldsMod control model =
    case control of
        SvgControl.CmButton mod ->
            SvgControl.CmButton
                { mod
                    | name = model.edname |> Maybe.withDefault mod.name
                    , label = model.edlabel |> Maybe.withDefault mod.label
                }

        SvgControl.CmSlider mod ->
            SvgControl.CmSlider
                { mod
                    | name = model.edname |> Maybe.withDefault mod.name
                    , label = model.edlabel |> Maybe.withDefault mod.label
                }

        SvgControl.CmXY mod ->
            SvgControl.CmXY
                { mod
                    | name = model.edname |> Maybe.withDefault mod.name
                    , label = model.edlabel |> Maybe.withDefault mod.label
                }

        SvgControl.CmLabel mod ->
            SvgControl.CmLabel
                { mod
                    | name = model.edname |> Maybe.withDefault mod.name
                    , label = model.edlabel |> Maybe.withDefault mod.label
                }

        SvgControl.CmSizer mod ->
            SvgControl.CmSizer { mod | orientation = model.edorientation |> Maybe.withDefault mod.orientation }


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
                        ( doFocusFields
                            { model
                                | scpModel = { sm | control = nc }
                            }
                        , c
                        )

                    Nothing ->
                        ( model, None )

        doFocusFields =
            \mdl ->
                mdl.selected
                    |> Maybe.andThen (\sid -> getControl sid mdl.scpModel.control)
                    |> Maybe.map (\control -> focusFields control mdl)
                    |> Maybe.withDefault { mdl | edname = Nothing, edlabel = Nothing, edorientation = Nothing }

        modSelected =
            \mdl ->
                mdl.selected
                    |> Maybe.andThen
                        (\sid ->
                            getControl sid mdl.scpModel.control
                                |> Maybe.map
                                    (\control ->
                                        let
                                            nc =
                                                Debug.log "ffm" <|
                                                    focusFieldsMod
                                                        control
                                                        mdl

                                            sm =
                                                mdl.scpModel

                                            ( nc2, _ ) =
                                                SvgControl.modControl
                                                    sid
                                                    (\_ ->
                                                        ( nc, None )
                                                    )
                                                    sm.control

                                            ( nc3, upd ) =
                                                SvgControl.resize nc2 mdl.scpModel.mahrect
                                        in
                                        ( { mdl | scpModel = { sm | control = nc3 } }, upd )
                                    )
                        )
                    |> Maybe.withDefault ( mdl, None )
    in
    case msg of
        TreeRowClicked controlid ->
            let
                selected =
                    if model.selected == Just controlid then
                        Nothing

                    else
                        Just controlid
            in
            ( doFocusFields
                { model
                    | selected = selected
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
                (SvgControl.CsButton (SvgButton.Spec "test" Nothing))

        AddHSliderPress ->
            addcontrol
                (SvgControl.CsSlider (SvgSlider.Spec "hslider" Nothing SvgThings.Horizontal))

        AddVSliderPress ->
            addcontrol
                (SvgControl.CsSlider (SvgSlider.Spec "vslider" Nothing SvgThings.Vertical))

        AddXYPress ->
            addcontrol
                (SvgControl.CsXY (SvgXY.Spec "test" Nothing))

        AddLabelPress ->
            addcontrol
                (SvgControl.CsLabel (SvgLabel.Spec "name" "label"))

        DeletePress ->
            case model.selected of
                Just cid ->
                    let
                        ( sm, c ) =
                            SvgControlPage.deleteControl cid model.scpModel
                    in
                    ( doFocusFields
                        { model
                            | scpModel = sm
                        }
                    , c
                    )

                Nothing ->
                    ( model, None )

        ScpMsg scpmsg ->
            let
                ( nscpm, cmd ) =
                    SvgControlPage.update scpmsg model.scpModel
            in
            ( { model | scpModel = nscpm }, cmd )

        EdNameChanged string ->
            modSelected { model | edname = Just string }

        EdLabelChanged string ->
            modSelected { model | edlabel = Just string }

        EdOrientationChanged orientation ->
            modSelected { model | edorientation = Just orientation }



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
