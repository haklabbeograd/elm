
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onInput, onClick)
import Random exposing (..)
import Http

main =
  beginnerProgram { model = { i = 0 } , view = view, update = update }

type alias Model = { i: Int }

type Msg
  = Ova
  | Ona
  | NekiTreci Treci
  
type Treci 
  = Cetvrti
  | Peti
  

update: Msg -> Model -> Model
update msg m =
  case msg of
    Ova -> m
    Ona -> m
    NekiTreci treci -> { m | i = g treci m.i }
    
    
    
g: Treci -> Int -> Int
g nt i =
  case nt of
    Cetvrti -> i + 1
    Peti -> i - 1
    
    
    
view: Model -> Html Msg
view m =
  div [] 
    [ button [ onClick (NekiTreci Cetvrti) ] [ text "klik" ]
    , text (toString m.i) 
    , button [ onClick (NekiTreci Peti) ] [ text "klok" ]
    ]