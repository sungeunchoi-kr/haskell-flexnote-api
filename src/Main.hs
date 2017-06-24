{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass, ScopedTypeVariables #-}
module Main where

import qualified Data.ByteString.Lazy.Char8 as C
import qualified Web.Scotty as Scotty
import qualified Data.Aeson as Json
import qualified Data.Map as Map
import Data.List
import Network.Wai.Middleware.Static
import Data.Monoid (mconcat)
import Data.Aeson.Types
import GHC.Generics
import Control.Monad.State.Lazy
import System.IO
import System.Directory
import Data.UID
import Data.Time.Format
import Network.HTTP.Types
import Network.Wai.Middleware.Cors

data Memo = Memo {
        title :: String,
        content :: String
    } deriving (Generic, ToJSON, FromJSON)

data MemoM = MemoM {
        uid :: String,
        memo :: Memo
    } deriving (Generic, ToJSON, FromJSON)

f :: String -> Memo -> MemoM
f s m = MemoM {uid=s, memo=m}

get_data_dir :: IO (String)
get_data_dir = fmap (++"/data/") getCurrentDirectory

make_memo_fpath :: String -> IO (String)
make_memo_fpath uid = (++uid) <$> get_data_dir

memo_gen_uid :: IO (String)
memo_gen_uid = ("memo-"++) <$> newUIDString

-- reads a memo file from disk.
memo_read :: String -> IO (Memo)
memo_read uid = do
    fpath <- make_memo_fpath uid
    s <- memo_parse <$> lines <$> readFile fpath
    return Memo {title=(fst s), content=(snd s)}

memo_read_abstract :: String -> IO (Memo)
memo_read_abstract uid = do
    fpath <- make_memo_fpath uid
    s <- memo_parse_abstract <$> lines <$> readFile fpath
    return Memo {title=(fst s), content=(snd s)}

memo_parse_abstract :: [String] -> (String,String)
memo_parse_abstract xs = memo_parse_abstract_h 0 xs ("","")

memo_parse_abstract_h :: Int -> [String] -> (String,String) -> (String,String)
memo_parse_abstract_h 0 (x:xs) _ = memo_parse_abstract_h 1 xs (x,"")
memo_parse_abstract_h 0 _ _ = ("Untitled", "")
memo_parse_abstract_h 1 (x:xs) (t,_) = (t, x)

memo_parse :: [String] -> (String,String)
memo_parse xs = memo_parse_h 0 xs ("","")

memo_parse_h :: Int -> [String] -> (String,String) -> (String,String)
memo_parse_h 0 (x:xs) _ = memo_parse_h 1 xs (x,"")
memo_parse_h 0 _ _ = ("Untitled", "")
memo_parse_h 1 xs (t,_) = (t, Data.List.intercalate "\n" xs)

memo_write :: String -> String -> String -> IO ()
memo_write uid title content = do
    fpath <- make_memo_fpath uid
    writeFile fpath (title ++ "\n" ++ content)
    
ls_memo :: IO ( ([String],[String]) )
ls_memo = do
    data_dir <- get_data_dir
    file_names::[String] <- listDirectory data_dir
    contents::[String] <- sequence $ map (\f -> readFile $ data_dir++f) file_names
    return (file_names, contents)

post_v1_memos :: Scotty.ActionM()
post_v1_memos = 
    Scotty.jsonData
        >>= \(m::Memo) -> liftIO memo_gen_uid
        >>= \uid -> liftIO (memo_write uid (title m) (content m))
        >>= \_ -> Scotty.json $ toJSON (MemoM {uid=uid, memo=m})

list_v1_memos :: Scotty.ActionM()
list_v1_memos = do
    data_dir <- liftIO get_data_dir
    uids::[String] <- liftIO $ listDirectory data_dir
    ms::[Memo] <- liftIO $ sequence $ map memo_read_abstract uids
    t <- return $ zipWith f uids ms
    Scotty.json $ toJSON t

get_v1_memos :: Scotty.ActionM()
get_v1_memos = do
    uid <- Scotty.param "uid"
    memo <- liftIO $ memo_read uid
    Scotty.json $ toJSON memo
    --Scotty.json $ object [ "ok" .= (fbody::String) ]

put_v1_memos :: Scotty.ActionM()
put_v1_memos = do 
    uid <- Scotty.param "uid"
    m::Memo <- Scotty.jsonData
    _ <- liftIO $ memo_write uid (title m) (content m)
    Scotty.json $ object [ "content" .= ((content m)::String), "title" .= ((title m)::String) ]

routes :: Scotty.ScottyM ()
routes = do
    Scotty.middleware simpleCors
    Scotty.middleware $ staticPolicy $
        (addBase "/home/sung/projs/current/angular2-starter")

    Scotty.post "/v1/memos" post_v1_memos
    Scotty.get "/v1/memos" list_v1_memos
    Scotty.get "/v1/memos/:uid" get_v1_memos
    Scotty.put "/v1/memos/:uid" put_v1_memos

main = Scotty.scotty 8081 routes

--memos_insert :: (Map.Map String Memo) -> Memo -> (Map.Map String Memo)
--memos_insert map memo = Map.insert "index" memo map

--memos_insert :: Memo -> State (Map.Map String Memo) Bool 
--memos_insert memo =
--    Control.Monad.State.Lazy.get
--        >>= (\map -> Control.Monad.State.Lazy.put (Map.insert "index" memo (map::(Map.Map String Memo)))
--        >> return True)

--Scotty.json $ toJSON t
--memos_raw <- liftIO ls_memo
--let uids = fst memos_raw
--let contents = snd memos_raw
--memo_objs::Maybe [Memo] <- return $ sequence $ map (\c -> Json.decode (C.pack c)) contents
--case memo_objs of
--    Nothing -> do
--        Scotty.status Status {statusCode=500, statusMessage="Internal Server Error."}
--    Just memo_objs -> do
--        t <- return $ zipWith f uids memo_objs
--        Scotty.json $ toJSON t

