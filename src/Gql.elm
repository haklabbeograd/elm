port module Gql exposing (..)

import Http
import Klokotalo
import Klok
import Json.Decode



type Msg
    = StigkoK Klokotalo.Klokovi
    | Stagod

q : String
q =
    """
{"query":"
    query getKlokotalo($id: ID!) {
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
    }",
    "operationName": "getKlokotalo"
}
"""


decodeKlokotalo : Json.Decode.Decoder Klokotalo.Klokovi
decodeKlokotalo =
    Json.Decode.map Klokotalo.Klokovi
        ( Json.Decode.field "data" 
            ( Json.Decode.field "getKlokotalo" 
                ( Json.Decode.field "klokovi" 
                    ( Json.Decode.field "edges" 
                        ( Json.Decode.list 
                            (Json.Decode.map2 Klok.Klok
                                ( Json.Decode.field "id" Json.Decode.string)
                                ( Json.Decode.field "vrednost" Json.Decode.int)
                            )  ) ) ) ) )


-- post : String -> Http.Body -> Http.Request Msg
-- post url body =
--     Http.request
--         { method = "POST"
--         , headers = []
--         , url = url
--         , body = body
--         , expect = Http.expectStringResponse (\_ -> Ok ())
--         , timeout = Nothing
--         , withCredentials = False
--         } 



uzmiKlokotaloRq : Http.Request Klokotalo.Klokovi
uzmiKlokotaloRq =
    Http.post "https://eu-west-1.api.scaphold.io/graphql/ffelm"
        (Http.stringBody "application/json" q)
        decodeKlokotalo

        
