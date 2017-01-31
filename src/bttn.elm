module Bttn exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onMouseEnter, onMouseLeave)


type alias BttnModel = { color : String }


-- model: String -> BttnModel
-- model s = 



type BttnMsg 
    = MouseIn
    | MouseOut

-- update: Msg -> BttnModel -> BttnModel
update bm model = 
    case bm of
        MouseIn -> { model | color = "red" }
        MouseOut -> { model | color = "green" }
        -- _ -> model

-- bttnView: BttnModel -> Html BttnMsg
bttnView model =
    let
        color = model.color
    in
        div 
            [   class "button"
            ,   style [ ( "background-color", color) 
                    ,   ( "width", "400 px" ) 
                    ,   ( "height", "400 px" ) 
                    ]
            ,   onMouseEnter MouseIn
            ,   onMouseLeave MouseOut ] [ text "AAAAA"]
