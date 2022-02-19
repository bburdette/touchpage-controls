module SvgControl.SvgCommand exposing (Command(..), TextSizeRequest, cmdMap)

import Browser.Dom as BD
import SvgControl.SvgThings exposing (ControlId)


type alias TextSizeRequest =
    { string : String
    , font : String
    , controlId : ControlId
    }


type Command update msg
    = Send update
    | RequestTextWidth TextSizeRequest
    | GetElement String (BD.Element -> msg)
    | None
    | Batch (List (Command update msg))


cmdMap : (upd1 -> upd2) -> Command upd1 msg -> Command upd2 msg
cmdMap f cmd =
    case cmd of
        Send upd1 ->
            Send (f upd1)

        RequestTextWidth t ->
            RequestTextWidth t

        GetElement t fn ->
            GetElement t fn

        None ->
            None

        Batch cmds ->
            Batch (List.map (cmdMap f) cmds)
