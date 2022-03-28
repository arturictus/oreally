module Main where
import System.Console.GetOpt
import Data.Maybe ( fromMaybe )
import System.Environment
import qualified System.Environment as E
import qualified System.Process as P
import Lib


data Options = Options
  { optOutput      :: Maybe FilePath
  , optAuth       :: Maybe String
  } deriving Show

defaultOptions    = Options
  { optOutput      = Just "~/Books"
  , optAuth       = Nothing
  }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['o']     ["output"]
      (OptArg ((\ f opts -> opts { optOutput = Just f }) . fromMaybe "output")
              "PATH")
      "output PATH"
  , Option ['a']     ["auth"]
      (OptArg ((\ f opts -> opts { optAuth = Just f }) . fromMaybe "auth")
              "AUTH")
      "literal auth"
  ]

compilerOpts :: [String] -> IO (Options, [String])
compilerOpts argv =
  case getOpt Permute options argv of
      (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
      (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: oreally [OPTION...] url"

main ::  IO ()
main = do
    args <- getArgs
    (opts, [url]) <- compilerOpts args
    putStrLn $ show opts
    putStrLn $ show url
    envAuth <- E.lookupEnv "OREALLY_AUTH"
    let auth' = do
                raw <- selectAuth opts envAuth
                authData <- buildAuth raw
                return authData

    putStrLn $ show auth'
    auth <- case auth' of
              Just a -> return a
              Nothing -> ioError (userError "invalid Auth")
    book <- case parseURI url of
              Just a -> return a
              Nothing -> ioError (userError "invalid URL")
    putStrLn $ show book
    cmd <- case fmap (buildCmd book auth) $ optOutput opts of
             Just a -> return a
             Nothing -> ioError (userError "unable to generate command")
    putStrLn cmd
    r <- P.callCommand cmd
    return ()
  where
    selectAuth :: Options -> Maybe String -> Maybe String
    selectAuth Options {optAuth = Nothing} d = d
    selectAuth Options {optAuth = Just e} _ = Just e

