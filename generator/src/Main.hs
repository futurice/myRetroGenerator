module Main where

import Data (MyRetro)
import Data.Aeson (Value (..), toJSON)
import Data.Text (Text, pack, unpack)
import Development.Shake (Action, ShakeOptions (..), Verbosity (Verbose), liftIO, readFile', shakeOptions, writeFile')
import Development.Shake.Forward (shakeForward)
import MarkdownParser (parseMyRetro)
import Options.Applicative ((<**>), Parser, ParserInfo, argument, execParser, fullDesc, header, help, helper, info, long, metavar, progDesc, short, str, strOption, strOption, value)
import Slick (compileTemplate', substitute)
import Slick.Pandoc (defaultHtml5Options, defaultMarkdownOptions, flattenMeta)
import Text.Pandoc (Block (..), Pandoc (..), PandocIO, readMarkdown, runIO, writeHtml5String)

buildRules :: Options -> Action ()
buildRules opts = do
  (doc, meta) <- readRetro (path opts)
  retro <- case parseMyRetro doc of
    Just r -> renderPandoc r
    Nothing -> fail "Error while parsing my retro"
  template <- compileTemplate' $ templatePath opts
  let indexHTML = unpack $ substitute template $ combine meta (toJSON retro)
  writeFile' (outputPath opts) indexHTML

data Options
  = MkOptions
      { path :: FilePath,
        templatePath :: FilePath,
        outputPath :: FilePath
      }
  deriving stock (Show)

cliParser :: Parser Options
cliParser =
  MkOptions
    <$> argument str (metavar "FILE" <> help "Specify the path of the myRetro markdown file")
    <*> strOption
      ( long "templatePath"
          <> short 't'
          <> (metavar "FILE")
          <> help "Specify the mustache template file to use"
          <> value "template/index.html"
      )
    <*> strOption
      ( long "output"
          <> short 'o'
          <> (metavar "FILE")
          <> help "Specify where the generated html file should be placed"
          <> value "build/index.html"
      )

parserOptions :: ParserInfo Options
parserOptions =
  info
    (cliParser <**> helper)
    ( fullDesc
        <> progDesc "Convert a markdown version of myRetro into a single html file"
        <> header "myRetroGenerator - Markdown to HTML"
    )

main :: IO ()
main = execParser parserOptions >>= shakeForward options . buildRules
  where
    options = shakeOptions {shakeVerbosity = Verbose, shakeLintInside = ["\\"]}

combine :: Value -> Value -> Value
combine (Object m1) (Object m2) = Object $ m1 <> m2
combine _ _ = error "Expected object"

htmlWriter :: Pandoc -> PandocIO Text
htmlWriter = writeHtml5String defaultHtml5Options

renderPandoc :: MyRetro Pandoc -> Action (MyRetro Text)
renderPandoc = traverse (unPandocM . htmlWriter)

readRetro :: FilePath -> Action ([Block], Value)
readRetro p = do
  file <- pack <$> readFile' p
  Pandoc meta blocks <- unPandocM $ readMarkdown defaultMarkdownOptions file
  meta' <- flattenMeta htmlWriter meta
  pure (blocks, meta')

unPandocM :: PandocIO a -> Action a
unPandocM p = do
  result <- liftIO $ runIO p
  either (fail . show) pure result
