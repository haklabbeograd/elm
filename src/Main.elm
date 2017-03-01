module Main exposing (main)

import Html exposing (Html)
import Dugme exposing (Dugme)
import Color
import Forma
import Klok
import Klokotalo


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
    , formica : Forma.Model
    , klok: Klok.Klok
    , klokotalo: Klokotalo.Model
    }


init : ( Model, Cmd Msg )
init =
    ( { dugmici =
            [ Dugme.init Color.darkGray Color.orange "Kara"
            , Dugme.init Color.green Color.red "Klasik"
            ]
      , naziv = Nothing
      , formica = Forma.init 0
      , klok = Klok.init 0 0
      , klokotalo = Klokotalo.init
      }
    , Cmd.none
    )


type Msg
    = AMsg
    | Dugmici Int Dugme.Msg
    | NasaForma Forma.Msg
    | Klok Klok.Msg
    | Klokotalo Klokotalo.Msg


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
            let
                ( nf, cmd ) =
                    Forma.update fmsg model.formica

                dugmici =
                    case fmsg of
                        Forma.Submit (Just { p, d, t }) ->
                            let
                                nd =
                                    Dugme.init p d t
                            in
                                nd :: model.dugmici

                        _ ->
                            model.dugmici
            in
                { model | formica = nf, dugmici = dugmici } ! [ Cmd.map NasaForma cmd ]
        Klok kmsg ->
            { model | klok = Klok.update kmsg model.klok } ! []
        Klokotalo kt ->
            { model | klokotalo = Klokotalo.update kt model.klokotalo } ! []


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
            Forma.view model.formica
                |> Html.map NasaForma
    in
        Html.div []
            [ Html.div [] levo
            , desno
            , Klok.view model.klok |> Html.map Klok
            , Klokotalo.view model.klokotalo |> Html.map Klokotalo
            ]


dugmici : List Dugme -> List (Html Msg)
dugmici =
    List.indexedMap
        (\i dug ->
            Html.map (Dugmici i) (Dugme.view dug)
        )
