import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onInput)

main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


-- MODEL
type alias Model =
        { kveri : String
        , res : Int
        }

init : ( Model, Cmd Msg )
init =
    ( Model "" 0, Cmd.none )


-- UPDATE
type Msg
        = Inputed String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( updateHelp msg model, Cmd.none )


updateHelp : Msg -> Model -> Model
updateHelp msg ({kveri} as model) =
    case msg of
        Inputed s ->
            Model s (djole kveri)


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model = Sub.none


-- " PAMET "
djole : String -> Int
djole s =
    String.split "," s
    |> List.map (\s -> String.trim s |> String.toInt |> Result.withDefault 0)
    |> List.foldl (+) 0 


-- VIEW
view : Model -> Html Msg
view model =
    let
        txt =
            djole model.kveri |> toString
        txt2 =
            toString model.res
    in
        div []
        [
            input [ onInput Inputed ] [],
            p [] [
                span [] [ text ("post update: " ++ txt) ]
            ],
            p [] [
                span [] [ text ("in update: " ++ txt2) ]
            ]

         ]