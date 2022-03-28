module Main where

-- import Control.Monad
-- import Data.Char
-- import Data.List
import System.Console.GetOpt
import Data.Maybe ( fromMaybe )
-- import System.Environment
-- import System.Exit
-- import System.IO
-- import Text.Printf

import qualified Lib as Lib

data Options = Options {
  optOutputPath :: FilePath
, optAuth :: Maybe String
} deriving Show

defaultOptions = Options { optOutputPath = "~/Books"
, optAuth = Nothing
}

options :: [OptDescr (Options -> Options)]
options = [
  Option ['o'] ["output"] (OptArg ((\ f opts -> opts { optOutputPath = f }) . fromMaybe "output") "PATH") "output PATH"
  , Option ['a'] ["auth"] (OptArg ((\ f opts -> opts { optAuth = Just f }) . fromMaybe "auth") "AUTH") "literal Auth"
  ]

compilerOpts :: [String] -> IO (Options, [String])
compilerOpts argv =
  case getOpt Permute options argv of
         (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
         (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: ic [OPTION...] files..."

main ::  IO ()
main = do
  putStrLn "hello"
