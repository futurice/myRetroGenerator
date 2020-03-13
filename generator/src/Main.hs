module Main where

import Control.Monad (forM_)
import Data (MyRetro)
import Data.Aeson (Value (..), toJSON)
import Data.Text (Text, pack, unpack)
import Development.Shake (Action, ShakeOptions (..), Verbosity (Verbose), copyFileChanged, getDirectoryFiles, liftIO, readFile', shakeOptions, writeFile')
import Development.Shake.FilePath ((</>))
import Development.Shake.Forward (shakeForward)
import MarkdownParser (parseMyRetro)
import Options.Applicative ((<**>), Parser, ParserInfo, argument, execParser, fullDesc, header, help, helper, info, long, metavar, progDesc, short, str, strOption, strOption, value)
import Slick (compileTemplate', substitute)
import Slick.Pandoc (defaultHtml5Options, defaultMarkdownOptions, flattenMeta)
import System.Directory (setCurrentDirectory)
import Text.Pandoc (Block (..), Pandoc (..), PandocIO, readMarkdown, runIO, writeHtml5String)

buildRules :: Options -> Action ()
buildRules opts = do
  (doc, meta) <- readRetro (path opts)
  retro <- case parseMyRetro doc of
    Just r -> renderPandoc r
    Nothing -> fail "Error while parsing my retro"
  template <- compileTemplate' $ templatePath opts
  let indexHTML = unpack $ substitute template $ combine meta (toJSON retro)
  writeFile' (outputPath opts </> "index.html") indexHTML
  copyStatic opts

copyStatic :: Options -> Action ()
copyStatic opts = do
  files <- getDirectoryFiles "" ["static/*"]
  forM_ files $ \f -> copyFileChanged f (outputPath opts </> f)

data Options
  = MkOptions
      { path :: FilePath,
        templatePath :: FilePath,
        outputPath :: FilePath,
        cwd :: FilePath
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
          <> (metavar "DIR")
          <> help "Specify where the generated html page should be placed"
          <> value "build"
      )
    <*> strOption
      ( long "cwd"
          <> (metavar "DIR")
          <> help "Specify the current working directory"
          <> value "."
      )

parserOptions :: ParserInfo Options
parserOptions =
  info
    (cliParser <**> helper)
    ( fullDesc
        <> progDesc "Convert a markdown version of myRetro into a single html file. See --help for more info"
        <> header "myRetroGenerator - Markdown to HTML"
    )

main :: IO ()
main = do
  opts <- execParser parserOptions
  setCurrentDirectory $ cwd opts
  shakeForward options $ buildRules opts
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
