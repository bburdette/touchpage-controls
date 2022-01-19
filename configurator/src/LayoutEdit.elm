module LayoutEdit exposing (Command(..), Model, Msg(..), buttonStyle, update, view)

-- import Dialog as D
-- import TangoColors as TC
-- exposing (SvgControlPage)

import Element as E exposing (Element)
import Element.Background as EBk
import Element.Border as EBd
import Element.Font as EF
import Element.Input as EI
import Element.Region
import FlatColors.BritishPalette as Color
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
    | ScpMsg SvgControlPage.Msg


type alias Model =
    { scpModel : SvgControlPage.Model
    }


type Command
    = None



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
    E.row [ E.width E.fill ]
        [ E.column [ E.spacing 8, E.width E.fill ]
            [ EI.button buttonStyle
                { onPress = Just AddHSizerPress
                , label = E.text "AddHSizer"
                }
            , EI.button buttonStyle
                { onPress = Just AddVSizerPress
                , label = E.text "AddVSizer"
                }
            , EI.button buttonStyle
                { onPress = Just AddButtonPress
                , label = E.text "AddButton"
                }
            , EI.button buttonStyle
                { onPress = Just AddHSliderPress
                , label = E.text "AddHSlider"
                }
            , EI.button buttonStyle
                { onPress = Just AddVSliderPress
                , label = E.text "AddVSlider"
                }
            , EI.button buttonStyle
                { onPress = Just AddXYPress
                , label = E.text "AddXY"
                }
            ]
        , E.map ScpMsg <| E.html (SvgControlPage.view model.scpModel)
        ]


update : Msg -> Model -> ( Model, Command )
update msg model =
    case msg of
        AddHSizerPress ->
            ( model, None )

        AddVSizerPress ->
            ( model, None )

        AddButtonPress ->
            ( model, None )

        AddHSliderPress ->
            ( model, None )

        AddVSliderPress ->
            ( model, None )

        AddXYPress ->
            ( model, None )

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
