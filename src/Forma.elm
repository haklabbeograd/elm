module Forma exposing (..)

import Dugme
import Color
import Html exposing (..)


submitDugme : Dugme.Dugme
submitDugme =
    Dugme.init Color.lightPurple Color.yellow "Hajmo"


type Msg
    = SubmitDugme


view : Html Msg
view =
    form []
        [ label []
            [ text "Prva"
            , input [] []
            ]
        , label []
            [ text "Druga"
            , input [] []
            ]
        , (Html.map (\_ -> SubmitDugme) (Dugme.view submitDugme))
        ]
