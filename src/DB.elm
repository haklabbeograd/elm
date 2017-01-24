module DB exposing (DbMeta, getVersion)

import Http
import Json.Decode as Decode


type alias DbMeta =
    { version : String
    , couchdb : String
    , pogresna : Maybe String
    }


decodeDbMeta: Decode.Decoder DbMeta
decodeDbMeta =
    Decode.map3 DbMeta
    (Decode.field "version" Decode.string)
    (Decode.field "couchdb" Decode.string)
    (Decode.maybe (Decode.field "pogresna" Decode.string))

getVersion : Http.Request DbMeta
getVersion =
      Http.get "http://localhost:5984/" decodeDbMeta