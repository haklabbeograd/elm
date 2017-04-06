port module Gql exposing (..)

import Http
import Klokotalo
import Klok
import Json.Decode
import Json.Encode

gqlEndpoint: String
gqlEndpoint = "https://eu-west-1.api.scaphold.io/graphql/ffelm"


type Msg
    = StigoK (Result Http.Error Klokotalo.Klokovi)
    | IzmenjenKlok (Result Http.Error Klok.Klok)

type GqlOp
    = Mutation
    | Query


payload: Json.Encode.Value
payload =
    Json.Encode.object
        [ ("query", Json.Encode.string """
                query GetKlokotalo($id: ID!) {
                    getKlokotalo(id: $id) {
                        id,
                        klokovi {
                            edges {
                                node {
                                    id,
                                    vrednost
                                }
                            }
                        }
                    }
                }""")
        , ("variables", Json.Encode.string """{
                "id": "S2xvazo0" 
            }""")
        ]

setQuery: Json.Encode.Value -> (String, Json.Encode.Value)
setQuery q =
    ( "query", q )

setMutation: Json.Encode.Value -> (String, Json.Encode.Value)
setMutation m =
    ( "mutation", m )

setVariables: List (String , Json.Encode.Value) -> (String, Json.Encode.Value)
setVariables vars =
    ( "variables", Json.Encode.string (Json.Encode.encode 0 (Json.Encode.object vars)) )

mutationKlok: Json.Encode.Value
mutationKlok = Json.Encode.string """
    mutation MutKlok($input: UpdateKlokInput!) {
        updateKlok(input: $input) {
            changedKlok {
                id,
                vrednost
            }
        }
    }
"""

makeBody: List (String, Json.Encode.Value) -> Json.Encode.Value
makeBody vars =
    Json.Encode.object 
        [   setQuery mutationKlok
        ,   setVariables vars
        ]

decodeKlokotalo : Json.Decode.Decoder Klokotalo.Klokovi
decodeKlokotalo =
    Json.Decode.map Klokotalo.Klokovi
        ( Json.Decode.field "data" 
            ( Json.Decode.field "getKlokotalo" 
                ( Json.Decode.field "klokovi" 
                    (decodeGqlList Klok.decoder) ) ) )

decodeGqlList : Json.Decode.Decoder a -> Json.Decode.Decoder (List a)
decodeGqlList dec =
    Json.Decode.field "edges" 
        ( Json.Decode.list 
            ( Json.Decode.field "node" dec) )


uzmiKlokotalo : Cmd Msg
uzmiKlokotalo =
    let
        post =
            Http.post gqlEndpoint
                (Http.jsonBody payload)
                decodeKlokotalo
    in
        Http.send (\a -> StigoK a) post



updejtujKlok : String -> Int -> Cmd Msg
updejtujKlok kid i =
    Http.send (\a -> StigoK a)
        <| Http.post gqlEndpoint
            (Http.jsonBody (makeBody
                [   ( "input", Json.Encode.object
                        [   ( "id", Json.Encode.string kid )
                        ,   ( "vrednost", Json.Encode.int i )
                        ]
                    )
                ]
            ))
            decodeKlokotalo

-- updateKlokPlayload: String -> Int -> Json.Encode.Value
-- updateKlokPlayload id i =


