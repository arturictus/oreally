{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( parseURI,
      main,
      buildAuth
    ) where

import qualified Network.URI as URI
import qualified Data.Text as T
import qualified System.Environment as E
import System.Exit
import qualified System.Command as C
import Data.List

type Book = (T.Text, T.Text)
type Auth = (T.Text, T.Text)

-- instance Show a => Show (C.CmdResult a) where
--     show (C.CmdResult a) = show a

main :: String -> String -> IO ()
main varName url = do
    putStrLn "setting up the project"
    auth <- lookUp
    putStrLn (show auth)
    book <- parseURI' url
    let cmdL = ["(docker run kirinnee/orly:latest login ",
                (bookId book), " ",
                (showAuth auth),
                 ") > \"",
                (booktitle book),
                ".epub\""]
        cmdStr = T.unpack (T.concat cmdL)

    putStrLn (cmdStr)
    C.Exit r <- C.cmd cmdStr
    -- putStrLn
    return ()
  where
    lookUp = buildAuth <$> E.lookupEnv varName
    parseURI' uri = case parseURI uri of
      (Just b) -> return b
      _ -> exitFailure

    showAuth :: Maybe Auth -> T.Text
    showAuth (Just (a, b)) = T.concat [a, ":", b]
    showAuth _ = ""
    bookId (_, b) = b
    booktitle (a, _) = a


buildAuth :: Maybe String -> Maybe Auth
buildAuth d = do
    l <- spl d
    auth <- build l
    return auth
  where
    spl :: Maybe String -> Maybe [T.Text]
    spl (Just var) = Just (T.splitOn ":" (T.pack var))
    spl _ = Nothing

    build :: [T.Text] -> Maybe Auth
    build (username:pass:_) = Just (username, pass)
    build _ = Nothing

parseURI :: String -> Maybe Book
parseURI url = do
    parsed <- URI.parseURI url
    path <- getPath parsed
    out <- buildBook (toList path)
    return out
  where
    toList :: T.Text -> [T.Text]
    toList = T.splitOn "/"

    buildBook :: [T.Text] -> Maybe Book
    buildBook (_:"library":"view":name:id:_) = Just (name, id)
    buildBook _ = Nothing

    getPath :: URI.URI -> Maybe T.Text
    getPath URI.URI{URI.uriPath = []} = Nothing
    getPath URI.URI{URI.uriPath = p} = Just (T.pack p)
