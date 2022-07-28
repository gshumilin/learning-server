type QString = [(String, Maybe String)]

data ClientInfo = ClientInfo 
    {   isAdmin :: Bool,
        isAbleToCreateNews :: Bool,
        userID :: Int
    }

clientAuth :: String -> ClientInfo
clientAuth x = ClientInfo 
    {   isAdmin = True,
        isAbleToCreateNews = True,
        userID = 1
    }


"SELECT * FROM news WHERE "
ЕСЛИ клиент авторизовался
    ТО выполняем поиск client_ID
        ЕСЛИ нашли айдишник, 
            ТО поверяем, не указан ли айдишник в фильтрах
                ЕСЛИ в фильрах есть айдишник
                    ТО initQ = SELECT * FROM news WHERE
                    ИНАЧЕ do 
                        initQ = SELECT * FROM news WHERE user_id =
            ИНАЧЕ initQ = SELECT * FROM news WHERE
    ИНАЧЕ initQ = SELECT * FROM news WHERE