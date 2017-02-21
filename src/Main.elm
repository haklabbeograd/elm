module Main exposing (main)

import Html exposing (Html)
import Dugme exposing (Dugme)
import Color
import Forma


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { dugmici : List Dugme
    , naziv : Maybe String
    }


init : ( Model, Cmd Msg )
init =
    ( { dugmici =
            [ Dugme.init Color.darkGray Color.orange "Kara"
            , Dugme.init Color.green Color.red "Klasik"
            ]
      , naziv = Nothing
      }
    , Cmd.none
    )


type Msg
    = AMsg
    | Dugmici Int Dugme.Msg
    | NasaForma Forma.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AMsg ->
            model ! []

        Dugmici i dmsg ->
            let
                ( ndugmici, maybstr ) =
                    model.dugmici
                        |> List.indexedMap
                            (\index dug ->
                                if index == i then
                                    case dmsg of
                                        Dugme.Klick ->
                                            ( dug, Just dug.text )

                                        _ ->
                                            let
                                                nd =
                                                    Dugme.update dmsg dug
                                            in
                                                ( nd, model.naziv )
                                else
                                    ( dug, Nothing )
                            )
                        |> List.unzip

                noviNaziv =
                    maybstr
                        |> List.filterMap identity
                        |> List.head
            in
                { model | dugmici = ndugmici, naziv = noviNaziv } ! []

        NasaForma fmsg ->
            model ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    let
        naziv =
            case model.naziv of
                Just t ->
                    Html.h1 [] [ Html.text t ]

                Nothing ->
                    Html.div [] []

        levo =
            naziv :: (dugmici model.dugmici)

        desno =
            Html.map NasaForma Forma.view
    in
        Html.div []
            [ Html.div [] levo
            , desno
            ]


dugmici : List Dugme -> List (Html Msg)
dugmici =
    List.indexedMap
        (\i dug ->
            Html.map (Dugmici i) (Dugme.view dug)
        )
