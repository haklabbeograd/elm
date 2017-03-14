module GejmOfLajf exposing (..)

import Matrix exposing (..)
import Matrix.Extra exposing (neighbours)
import Time exposing (Time)


boardSize : Int
boardSize =
    1000


type Celija
    = Ziva
    | Mrtva


type Msg
    = Tick Time


type alias Tabla =
    Matrix Celija


type alias Model =
    { matrica : Tabla
    , clock : Time
    }


init : Model
init =
    Model (repeat boardSize boardSize Mrtva) 0


numbOfZive : Int -> Int -> Tabla -> Int
numbOfZive x y t =
    (neighbours x y t) |> List.filter isZiva |> List.length


isZiva : Celija -> Bool
isZiva c =
    case c of
        Ziva ->
            True

        Mrtva ->
            False


update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick time ->
            model
