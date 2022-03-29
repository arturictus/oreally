module Main where
import Options.Applicative
import Data.Semigroup ((<>))
import Data.Maybe ( fromMaybe )
import System.Environment
import qualified System.Environment as E
import qualified System.Process as P
import Debug.Trace
import Lib



data Options = Options
  { output :: FilePath
  , auth :: String
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
        long "auth"
        <> short 'a'
        <> metavar "USER:PASS"
        <> value ""
        <> help "authentication: username and password")

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (options <**> helper)
      ( fullDesc
        <> progDesc "desc blablabla"
        <> header "header balabla")



run :: Options -> IO ()
run options = do
  putStrLn $ show options

