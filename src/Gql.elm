port module Gql exposing (..)

import Http exposing (..)
import Klokotalo exposing (..)


-- type Query
--     = String

type alias Query = "
  getKlokotalo('S2xva290YWxvOjQ') {
    id,
    klokovi {
      edges {
        node {
          vrednost
        }
      }
    }
  }
"

uzmiKlokotalo : Request -> Klokotalo
uzmiKlokotalo =
    post "https://eu-west-1.api.scaphold.io/graphql/ffelm"
        Query
