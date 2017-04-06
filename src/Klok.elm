module Klok exposing (..)

import Html exposing (..)
import Html.Events exposing (on, onInput, onClick)
import Json.Decode
import Json.Encode
import Util exposing (..)



type alias Klok =
  { id: String
  , i: Int
  }


type Msg
  = Dole Klok
  | Gore Klok
  | Update Int
  | Izmena Klok

init: Int -> Int -> Klok
init i index = Klok (makeId "klok" index) i


update: Msg -> Klok -> ( Klok, Cmd Msg )
update msg m =
  case msg of
    Dole k -> m ! []
    Gore k-> m ! []
    Update i -> { m | i = i } ! []
    Izmena k  -> m  ! []

-- izmeni: Msg -> String -> Int -> Cmd Msg
-- izmeni msg id i =


postaviVrednost: Int -> Klok -> Klok
postaviVrednost i k = 
  { k | i = i}


updatePayload: String -> Int -> List ( String, Json.Encode.Value )
updatePayload id i =
  [   ( "id", Json.Encode.string id )
  ,   ( "vrednost", Json.Encode.int i )
  ]


decoder: Json.Decode.Decoder Klok
decoder =
  Json.Decode.map2 Klok
    ( Json.Decode.field "id" Json.Decode.string)
    ( Json.Decode.field "vrednost" Json.Decode.int)

view: Klok -> Html Msg
view m =
  div []
    [ button [ onClick ( Dole { m | i = m.i - 1 } ) ] [ text "Dole" ]
    , text (toString m.i)
    , button [ onClick ( Gore { m | i = m.i + 1 } ) ] [ text "Gore" ]
    , text (" - " ++ m.id)
    ]
