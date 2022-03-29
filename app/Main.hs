module Main where
import Options.Applicative
import Data.Semigroup ((<>))
import qualified System.Environment as E
import qualified System.Process as P
import Lib

data Options = Options
  { output :: FilePath
  , authVar :: String
  , url :: String
  } deriving Show

options :: Parser Options
options = Options
    <$> strOption (
        long "output"
        <> short 'o'
        <> metavar "FOLDERPATH"
        <> help "target path"
        <> value "~"
        <> showDefault)
    <*> strOption (
        long "auth-var"
        <> short 'a'
        <> value "OREALLY_AUTH"
        <> metavar "ENVVAR"
        <> showDefault
        <> help "auth ENV VAR: must be: `USERNAME:PASSWORD` format")
    <*> argument str (metavar "URL" <> help "full o'reilly book url")

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (options <**> helper)
      ( fullDesc
        <> progDesc "Download O'Reilly books to epub"
        <> header "O'Reilly ----> epub == oreally!!")



run :: Options -> IO ()
run options = do
    envAuth <- E.getEnv $ authVar options
    auth <- maybeToIO "invalid AUTH" $ buildAuth envAuth
    book <- maybeToIO "invalid URL" $ parseURI $ url options
    let cmd = buildCmd book auth $ output options
    putStrLn "Starting to download:"
    putStrLn $ "  output folder: " ++ output options
    putStrLn $ "  ENV VAR auth: " ++ authVar options
    r <- P.callCommand cmd
    return ()
  where
    maybeToIO :: String -> Maybe a -> IO a
    maybeToIO _ (Just a) = do return a
    maybeToIO txt _ = do ioError (userError txt)


