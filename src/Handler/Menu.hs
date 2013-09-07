{-# LANGUAGE OverloadedStrings #-}

module Handler.Menu ( handleMenu ) where

import Snap
import Snap.Snaplet.SqliteSimple
import qualified Data.ByteString.Char8 as B

import Application

data Item = Item
    { title    :: String
    , iid      :: Int
    , children :: [Item]
    }

data DbItem = DbItem
    { dbtitle :: String
    , dbiid   :: Int
    }

instance FromRow DbItem where
    fromRow = DbItem <$> field
                     <*> field

instance FromRow Int where
    fromRow = field

testItems :: [Item]
testItems = [ Item "jeden" 1 [ Item "subjeden" 2 []
                             , Item "subjeden" 3 []
                             , Item "subjeden" 4 []
                             ]
            , Item "dwa" 5 [ Item "subdwa" 7 []
                           , Item "subdwa" 8 []
                           , Item "subdwa" 9 [ Item "subsubdwa" 10 []
                                             , Item "subsubdwa" 11 []
                                             , Item "subsubdwa" 12 []
                                             ]
                           ]
            , Item "trzy" 6 []
            ]

getParentId :: Int -> Handler App Sqlite Int
getParentId id =
    liftM head $ query "SELECT parent_id FROM dokumenty WHERE id = ?" [id]

getItems :: Int -> Handler App Sqlite [DbItem]
getItems pid =
    query "SELECT title,id FROM dokumenty WHERE parent_id = ?" [pid]

-- Mozna to pewnie lepiej napisac :)
genItems :: Int -> [Item] -> Handler App Sqlite [Item]
genItems 0  pits = return pits
genItems id pits = do
    pid    <- getParentId id
    dits   <- getItems pid
    chdits <- getItems id
    let its = case pits of
            [] -> itss dits $ itss chdits []
            _  -> itss dits pits
    genItems pid its
  where
    itss dits pits = map (\dit -> Item (dbtitle dit)
                                       (dbiid dit)
                                       (if id == dbiid dit then pits else [])) dits

handleMenu :: Handler App App ()
handleMenu = do
    --items <- with db getItems
    writeBS "<html>"
    mid <- getParam "id"
    let _id = case mid of
            Just id -> read $ B.unpack id
            Nothing -> 1
    its <- with db $ flip genItems [] _id
    writeBS $ B.pack $ genMenu its
    writeBS "</html>"

genMenu :: [Item] -> String
genMenu [] = []
genMenu is =
    "<ul>\n" ++
    concatMap (\i -> "<li>" ++
                     "<a href='/menu/" ++ show (iid i) ++ "'>" ++ title i ++ "</a>" ++
                     "</li>\n" ++
                     genMenu (children i)
              ) is
    ++ "</ul>\n" 
