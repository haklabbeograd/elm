module Klok exposing (..)

import Html exposing (..)
import Html.Events exposing (on, onInput, onClick)



type alias Klok = { i: Int }


type Msg
  = Dole
  | Gore


init: Int -> Klok
init i = Klok i


update: Msg -> Klok -> Klok
update msg m =
  case msg of
    Dole -> { m | i = m.i - 1}
    Gore -> { m | i = m.i + 1}


view: Klok -> Html Msg
view m =
  div []
    [ button [ onClick Dole ] [ text "Dole" ]
    , text (toString m.i)
    , button [ onClick Gore ] [ text "Gore" ]
    ]