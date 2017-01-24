module DB exposing (DbMeta, getVersion, addAlbum)

import Http
import Json.Decode as Decode
import Json.Encode as Encode


type alias DbMeta =
    { version : String
    , couchdb : String
    , pogresna : Maybe String
    }

type alias Album =
    { ime: String
    , autor: String
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

alb1: Album
alb1 =
    { ime = "RATM"
    , autor = "RATM"
    }

encodeAlbum: Album -> Encode.Value
encodeAlbum al =
    Encode.object
    [ ("ime", Encode.string al.ime)
    , ("autor", Encode.string al.autor)
    ]

put : String -> Http.Body -> Http.Request ()
put url body =
  Http.request
    { method = "POST"
    , headers = []
    , url = url
    , body = body
    , expect = Http.expectStringResponse (\_ -> Ok ())
    , timeout = Nothing
    , withCredentials = False
    }

addAlbum: Http.Request ()
addAlbum =
    put "http://localhost:5984/ec/" (Http.jsonBody (encodeAlbum alb1))
