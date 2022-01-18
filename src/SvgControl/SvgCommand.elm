module SvgControl.SvgCommand exposing (Command(..), TextSizeRequest)

import SvgControl.SvgThings exposing (ControlId)


type alias TextSizeRequest =
    { string : String
    , font : String
    , controlId : ControlId
    }


type Command
    = Send String
    | RequestTextWidth TextSizeRequest
    | None
    | Batch (List Command)
