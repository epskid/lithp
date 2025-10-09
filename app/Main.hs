module Main (main) where

import Data.List
import Emit
import Options.Applicative
import Parse
import qualified Translators.Assembly as Assembly (translator)
import Translators.Common
import qualified Translators.Python as Python (translator)

data TranslatorType = Python | Assembly deriving (Read, Show, Enum, Bounded)

getTranslator :: TranslatorType -> Translator
getTranslator Python = Python.translator
getTranslator Assembly = Assembly.translator

data LithpOptions = Options
  { file :: FilePath,
    translator :: TranslatorType
  }

lithpOptions :: Parser LithpOptions
lithpOptions =
  Options
    <$> argument
      str
      ( metavar "FILE"
          <> help "File to translate"
      )
    <*> option
      auto
      ( long "translator"
          <> metavar "TRANSLATOR"
          <> help
            ( "Translator to use (available: "
                ++ intercalate ", " (map show [(minBound :: TranslatorType) ..])
                ++ ")"
            )
      )

options :: ParserInfo LithpOptions
options =
  info
    (lithpOptions <**> helper)
    (fullDesc <> progDesc "transpile lisp files" <> header "lithp - a crappy lisp implementation")

main :: IO ()
main = do
  opts <- execParser options
  result <- parseFile $ file opts
  case result of
    Right ast -> putStrLn <$> translate (getTranslator $ translator opts) $ emit ast
    Left err -> putStr "parse error: " *> print err
