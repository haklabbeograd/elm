import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random exposing (..)
import Http

import DB as DB
-- import Bttn exposing (..)


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
        , zbir : String
        , random : Int
        , button : BttnModel
        }

init : ( Model, Cmd Msg )
init =
    ( Model "" 0 "" 1 (BttnModel "green"), Cmd.none )


-- UPDATE
type Msg
        = Inputed String
        | Procitaj
        | Osvezi (Result Http.Error DB.DbMeta)
        | UpisiAlbum (Result Http.Error ())
        | DajRandom
        | NoviRandom Int
        | ButtonMsg BttnMsg


send: Cmd Msg
send =
    Http.send UpisiAlbum DB.addAlbum

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DajRandom ->
            ( model, Random.generate NoviRandom (Random.int 1 6))
        NoviRandom nr ->
            ( { model | random = nr }, Cmd.none )
        Procitaj ->
            ( model, send )
        Osvezi ( Ok msg ) ->
            ( { model | zbir = msg.version ++ " " ++ msg.couchdb ++ " " ++ Maybe.withDefault "AAA" msg.pogresna }, Cmd.none )
        Osvezi ( Err e ) ->
            ( { model | zbir = toString e }, Cmd.none )
        UpisiAlbum ( Ok () ) ->
            ( { model | zbir = "Upisano" }, Cmd.none )
        UpisiAlbum ( Err e ) ->
            ( { model | zbir = toString e }, Cmd.none )
        ButtonMsg bttnmsg-> 
            ( { model | button = (bttnUpdate bttnmsg model.button) } , Cmd.none )
        _ ->
            ( updateHelp msg model, Cmd.none )


updateHelp : Msg -> Model -> Model
updateHelp msg ({ kveri, res } as model) =
    case msg of
        Inputed s ->
            { model | kveri = s, res = djole s}
        _ -> model




-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model = Sub.none




-- " PAMET "
djole : String -> Int
djole =
    String.split ","
    >> List.map (String.trim >> String.toInt >> Result.withDefault 0)
    >> List.foldl (+) 0

-- VIEW
view : Model -> Html Msg
view model =
    let
        post_update =
            djole model.kveri |> toString
        in_update =
            toString model.res
        zbir =  
            model.zbir
        random =
            toString model.random
        randomInt =
            model.random
        equals smth =
            " = " ++ smth

    in
        div []
        [
            input [ onInput Inputed ] [],
            p [] [
                span [] [ text ("post update: " ++ post_update) ]
            ],
            p [] [
                span [] [ text ("in update: " ++ in_update) ]
            ],
            div [] [
                button [ onClick Procitaj ] [ text "Upisi album" ],
                span [] [ text ( equals zbir ) ]
            ],
            div [] [
                button [ onClick DajRandom ] [ text "Daj random" ],
                span [] [ text ( equals "" ) ],
                span
                    [   class "popin"
                    ,   style
                            [ ( "font-size", (toString (randomInt * 10)) ++ "px" ) ]
                    ]
                    [ text random  ]
            ]
            , (bttnView model.button)

         ]





type alias BttnModel = { color : String }


-- model: String -> BttnModel
-- model s = 
type BttnMsg 
    = MouseIn
    | MouseOut

-- update: Msg -> BttnModel -> BttnModel
bttnUpdate bm model = 
    case bm of
        MouseIn -> { model | color = "red" }
        MouseOut -> { model | color = "green" }
        -- _ -> model

bttnView: BttnModel -> Html Msg
bttnView model =
    let
        color = model.color
    in
        div [] [
            div 
                [   class "button"
                ,   style [ ("background-color", color) ]
                ,   onMouseEnter (ButtonMsg MouseIn)
                ,   onMouseLeave (ButtonMsg MouseOut) 
                ]   
                [
                    text "AAAAa"
                ]

        ]
