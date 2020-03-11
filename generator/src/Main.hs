module Main where

import Data (MyRetro)
import Data.Aeson (Value (..), toJSON)
import Data.Text (Text, pack, unpack)
import Development.Shake (Action, ShakeOptions (..), Verbosity (Verbose), liftIO, readFile', shakeOptions, writeFile')
import Development.Shake.Forward (shakeArgsForward)
import MarkdownParser (parseMyRetro)
import Slick (compileTemplate', substitute)
import Slick.Pandoc (defaultHtml5Options, defaultMarkdownOptions, flattenMeta)
import Text.Pandoc (Block (..), Pandoc (..), PandocIO, readMarkdown, runIO, writeHtml5String)

path :: FilePath
path = "../Example.md"

templatePath :: FilePath
templatePath = "../template/index.html"

outputPath :: FilePath
outputPath = "../build/index.html"

buildRules :: Action ()
buildRules = do
  (doc, meta) <- readRetro path
  retro <- case parseMyRetro doc of
    Just r -> renderPandoc r
    Nothing -> fail "Error while parsing my retro"
  template <- compileTemplate' templatePath
  let indexHTML = unpack $ substitute template $ combine meta (toJSON retro)
  writeFile' outputPath indexHTML

main :: IO ()
main = shakeArgsForward options buildRules
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
