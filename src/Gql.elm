port module Gql exposing (..)

import Http
import Klokotalo exposing (..)
import Json.Decode exposing (Decoder, succeed, bool, int, string, field)



type alias Query ="
{\"query\":\"
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
}\",\"variables\":\"{
  \"id\": \"S2xvazo0\"
}\",\"operationName\":\"getKlokotalo\"}
"

decodeKlokotalo: Decoder Klokotalo
decodeKlokotalo =
    succeed Klokotalo
        ("id"   string)
        ("klokovi" (list Klok.Klok))


post: String -> Http.Body -> Http.Request
post url body =
  Http.request
    { method = "POST"
    , headers = []
    , url = url
    , body = body
    , expect = Http.expectStringResponse (\_ -> Ok ())
    , timeout = Nothing
    , withCredentials = False
    }
    decodeKlokotalo



uzmiKlokotaloRq : Request -> Klokotalo
uzmiKlokotaloRq =
    post "https://eu-west-1.api.scaphold.io/graphql/ffelm"
        Query
        decodeKlokotalo
