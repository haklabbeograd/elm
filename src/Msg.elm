type Msg
        = Inputed String
        | Procitaj
        | Osvezi (Result Http.Error DB.DbMeta)
        | UpisiAlbum (Result Http.Error ())
        | DajRandom
        | NoviRandom Int
        | ButtonMsg BttnMsg