module DB exposing (Album, DbInfo, getDbInfo, initialDbInfo)

import Http
import Json.Decode exposing (Decoder, succeed, bool, int, string, field)
import Json.Decode.Extra exposing ((|:))
import Json.Encode as Encode


-- CONSTANTS
serverUrl: String
serverUrl =
    "http://localhost:5984/"

dbName: String
dbName =
    "ec"

dbUrl: String
dbUrl =
    (serverUrl ++ dbName)


-- HTTP requests
put: String -> Http.Body -> Http.Request ()
put url body =
  Http.request
    { method = "PUT"
    , headers = []
    , url = url
    , body = body
    , expect = Http.expectStringResponse (\_ -> Ok ())
    , timeout = Nothing
    , withCredentials = False
    }

post: String -> Http.Body -> Http.Request ()
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

-- Database information
type alias DbInfo =
    { compact_running     : Bool
    , data_size           : Int
    , db_name             : String
    , disk_format_version : Int
    , disk_size           : Int
    , doc_count           : Int
    , doc_del_count       : Int
    , instance_start_time : String
    , purge_seq           : Int
    , update_seq          : String
    }

initialDbInfo: DbInfo
initialDbInfo =
    { compact_running     = False
    , data_size           = 0
    , db_name             = ""
    , disk_format_version = 0
    , disk_size           = 0
    , doc_count           = 0
    , doc_del_count       = 0
    , instance_start_time = ""
    , purge_seq           = 0
    , update_seq          = ""
    }

decodeDbInfo: Decoder DbInfo
decodeDbInfo =
    succeed DbInfo
        |: (field "compact_running"      bool)
        |: (field "data_size"            int)
        |: (field "db_name"              string)
        |: (field "disk_format_version"  int)
        |: (field "disk_size"            int)
        |: (field "doc_count"            int)
        |: (field "doc_del_count"        int)
        |: (field "instance_start_time"  string)
        |: (field "purge_seq"            int)
        |: (field "update_seq"           string)

getDbInfo: Http.Request DbInfo
getDbInfo =
    Http.get dbUrl decodeDbInfo


type alias WithId       x = { x | id  : String }
type alias WithRevision x = { x | rev : String }
type alias Document     x = WithId(WithRevision(x))


type alias Album = Document
    { ime: String
    , autor: String
    }

decodeAlbum: Decoder Album
decodeAlbum =
    succeed Album
        |: (field "ime"   string)
        |: (field "autor" string)
        |: (field "_id"   string)
        |: (field "_rev"  string)

getAlbum: Http.Request Album
getAlbum =
    Http.get (dbUrl ++ "/" ++ "03d6a2de4e812de44b1fd6d4f500135a") decodeAlbum
