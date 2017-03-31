module GejmOfLajf exposing (..)

import Matrix exposing (..)
import Matrix.Extra exposing (neighbours)
import Time exposing (Time)
import Html exposing (..)
import Html.Attributes exposing (class, style)
import Array


boardSize : Int
boardSize =
    85


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


init : List ( Int, Int ) -> Model
init zivi =
    let
        matrica =
            (repeat boardSize boardSize Mrtva)
                |> indexedMap
                    (\x y c ->
                        if List.member ( x, y ) zivi then
                            Ziva
                        else
                            Mrtva
                    )
    in
        Model matrica 0


numbOfZive : Int -> Int -> Tabla -> Int
numbOfZive x y t =
    (neighbours x y t) |> List.filter isZiva |> List.length


novoStanje : Tabla -> Tabla
novoStanje t =
    t
        |> indexedMap
            (\x y c ->
                let
                    komsije =
                        numbOfZive x y t
                in
                    survival komsije c
            )


survival : Int -> Celija -> Celija
survival komsije celija =
    if komsije < 2 then
        Mrtva
    else if komsije == 2 then
        celija
    else if komsije == 3 then
        Ziva
        -- else if komsije == 4 then
        --     Ziva
    else
        Mrtva


isZiva : Celija -> Bool
isZiva c =
    case c of
        Ziva ->
            True

        Mrtva ->
            False


update : Time -> Model -> Model
update t model =
    { model | matrica = novoStanje model.matrica }


celToEle : Celija -> List ( String, String ) -> Html msg
celToEle c pos =
    div
        [ class ((toString c) ++ " celija")
        , style pos
        ]
        []


cellSize : Int
cellSize =
    (500 / toFloat boardSize) |> floor


matrixToBoard : Int -> Int -> Celija -> Html msg
matrixToBoard x y c =
    let
        x_ =
            x * cellSize

        y_ =
            y * cellSize

        pos =
            [ ( "top", (toString y_) ++ "px" ), ( "left", (toString x_) ++ "px" ) ]

        size =
            [ ( "width", (toString cellSize) ++ "px" )
            , ( "height", (toString cellSize) ++ "px" )
            ]
    in
        celToEle c (pos ++ size)


view : Model -> Html msg
view model =
    div [ class "board" ]
        ((indexedMap
            matrixToBoard
            model.matrica
         )
            |> toIndexedArray
            |> Array.map Tuple.second
            |> Array.toList
        )
