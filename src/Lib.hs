{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( parseURI,
      buildCmd,
      buildAuth
    ) where

import qualified Network.URI as URI
import qualified Data.Text as T
import qualified System.Environment as E
import System.Exit
import qualified System.Process as P
import Data.List

type Book = (T.Text, T.Text)
type Auth = (T.Text, T.Text)

buildCmd :: Book -> Auth -> FilePath -> String
buildCmd book auth path =
    T.unpack $ T.concat str
  where
    str = ["(docker run kirinnee/orly:latest login ",
          (bookId book), " ",
          (showAuth auth),
            ") > \"", (T.pack path), "/",
          (booktitle book),
          ".epub\""]
    showAuth :: Auth -> T.Text
    showAuth (a, b) = T.concat [a, ":", b]
    bookId (_, b) = b
    booktitle (a, _) = a


buildAuth :: String -> Maybe Auth
buildAuth d = do
    l <- spl d
    auth <- build l
    return auth
  where
    spl :: String -> Maybe [T.Text]
    spl var = Just (T.splitOn ":" (T.pack var))

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
