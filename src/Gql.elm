port module Gql exposing (..)

import Http
import Klokotalo
import Klok
import Json.Decode
import Json.Encode



type Msg
    = StigoK (Result Http.Error Klokotalo.Klokovi)

q : String
q =
    """
'{"query":"
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
    }",
    "variables": "{
        "id": "S2xvazo0" 
    }"
}'
"""

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


decodeKlokotalo : Json.Decode.Decoder Klokotalo.Klokovi
decodeKlokotalo =
    Json.Decode.map Klokotalo.Klokovi
        ( Json.Decode.field "data" 
            ( Json.Decode.field "getKlokotalo" 
                ( Json.Decode.field "klokovi" 
                    ( Json.Decode.field "edges" 
                        ( Json.Decode.list 
                            ( Json.Decode.field "node"
                            ( Json.Decode.map2 Klok.Klok
                                ( Json.Decode.field "id" Json.Decode.string)
                                ( Json.Decode.field "vrednost" Json.Decode.int)
                            ) ) ) ) ) ) )



uzmiKlokotaloRq : Cmd Msg
uzmiKlokotaloRq =
    let
        post =
            Http.post "https://eu-west-1.api.scaphold.io/graphql/ffelm"
                (Http.jsonBody payload)
                decodeKlokotalo
    in
        Http.send (\a -> StigoK a) post


