module CliTest where
import System.Console.GetOpt
import Data.Maybe ( fromMaybe )
import System.Environment
import qualified System.Environment as E
import qualified System.Process as P
import Debug.Trace
import Lib



data Options = Options
  { optOutput      :: Maybe FilePath
  , optAuth       :: Maybe String
  } deriving Show

defaultOptions    = Options
  { optOutput      = Nothing
  , optAuth       = Nothing
  }

options :: [OptDescr (Options -> Options)]
options =
    [ Option ['o']     ["output"]
        (OptArg (debug "-o received: " $ ((\ f opts -> opts { optOutput = Just f }) . fromMaybe "~/books"))
                "PATH")
        "output PATH"
    , Option ['a']     ["auth"]
        (OptArg (debug "-a received: " $ ((\ f opts -> opts { optAuth = Just f }) . fromMaybe "Nothing"))
                "AUTH")
        "literal auth"
    ]
  where
    debug :: String -> (Maybe String -> b) -> Maybe String -> b
    debug txt f (Just a) = trace (txt ++ show a) (f $ Just a)
    debug txt f Nothing = trace (txt ++ "NOTHING") (f Nothing)

compilerOpts :: [String] -> IO (Options, [String])
compilerOpts argv =
  case getOpt Permute options argv of
      (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
      (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: oreally [OPTION...] url"

main ::  IO ()
main = do
    args <- getArgs
    putStrLn $ show args
    (opts, params) <- compilerOpts args
    putStrLn $ show opts
    putStrLn $ show params
    url <- case params of
            [] -> ioError (userError "argument required")
            l -> return $ last l
    putStrLn url
    envAuth <- E.lookupEnv "OREALLY_AUTH"
    let auth' = do
                raw <- selectAuth opts envAuth
                authData <- buildAuth raw
                return authData

    auth <- case auth' of
              Just a -> return a
              Nothing -> ioError (userError "invalid Auth")
    book <- case parseURI url of
              Just a -> return a
              Nothing -> ioError (userError "invalid URL")
    cmd <- case fmap (buildCmd book auth) $ optOutput opts of
             Just a -> return a
             Nothing -> ioError (userError "unable to generate command")
    r <- P.callCommand cmd
    return ()
  where
    selectAuth :: Options -> Maybe String -> Maybe String
    selectAuth Options {optAuth = Nothing} d = d
    selectAuth Options {optAuth = Just e} _ = Just e

