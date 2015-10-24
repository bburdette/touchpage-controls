module SvgButton where

import Effects exposing (Effects, Never)
import Html exposing (Html)
-- import Html.Attributes exposing (style)
-- import Html.Events exposing (onClick)
import Http
import Json.Decode as Json exposing ((:=))
import Task
import Svg exposing (svg, rect, g, text, text', Attribute)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)

-- how to specify a button in json.
type alias Spec = 
  { name: String
  }

{-
  , x: Int
  , y: Int
  , w: Int
  , h: Int  
-}

jsSpec : Json.Decoder Spec
jsSpec = Json.object1 Spec ("name" := Json.string)

-- MODEL

type alias Model =
  { name : String
  , pressed: Bool
  , color: String
  , sendf : (String -> Task.Task Never ())
  }


init : (String -> Task.Task Never ()) -> Spec ->  
  (Model, Effects Action)
init sendf spec =
  ( Model (spec.name) False "#60B5CC" sendf
  , Effects.none
  )


-- UPDATE

type Action
    = SvgClick | UselessCrap | Reply String


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    SvgClick -> ({ model | color <- "#f000f0"}, Effects.task 
      ((model.sendf model.name) `Task.andThen` (\_ -> Task.succeed UselessCrap)))
    UselessCrap -> (model, Effects.none)
    Reply s -> ({model | name <- s}, Effects.none)

-- VIEW

(=>) = (,)

view : Signal.Address Action -> Model -> Html
view address model =
  svg
    [ width "200", height "200", viewBox "0 0 200 200" ]
    [ g [ transform ("translate(100, 100)")
        , onClick (Signal.message address SvgClick)
        ]
        [ rect
            [ x "-50"
            , y "-50"
            , width "100"
            , height "100"
            , rx "15"
            , ry "15"
            , style ("fill: " ++ model.color ++ ";")
            ]
            []
        , text' [ fill "white", textAnchor "middle" ] [ text model.name ]
        ]
    ] 


-- EFFECTS

