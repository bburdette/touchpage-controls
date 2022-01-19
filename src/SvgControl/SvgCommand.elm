module SvgControl.SvgCommand exposing (Command(..), TextSizeRequest, cmdMap)

import SvgControl.SvgThings exposing (ControlId)


type alias TextSizeRequest =
    { string : String
    , font : String
    , controlId : ControlId
    }


type Command update
    = Send update
    | RequestTextWidth TextSizeRequest
    | None
    | Batch (List (Command update))


cmdMap : (upd1 -> upd2) -> Command upd1 -> Command upd2
cmdMap f cmd =
    case cmd of
        Send upd1 ->
            Send (f upd1)

        RequestTextWidth t ->
            RequestTextWidth t

        None ->
            None

        Batch cmds ->
            Batch (List.map (cmdMap f) cmds)
