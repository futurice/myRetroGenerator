module Main where

import Data (MyRetro (..), PastMonths (..), Project (..), Slider)
import Data.Aeson (Value (..), toJSON)
import Data.Char (isDigit)
import Data.Text (Text, isPrefixOf, pack, stripPrefix, toLower, unpack)
import Development.Shake (Action, ShakeOptions (..), Verbosity (Verbose), liftIO, readFile', shakeOptions, writeFile')
import Development.Shake.Forward (shakeArgsForward)
import Parser (Parser, embed, get', head', runParser, takeWhile', takeWhileM)
import Slick (compileTemplate', substitute)
import Slick.Pandoc (defaultHtml5Options, defaultMarkdownOptions, flattenMeta)
import Text.Pandoc (Block (..), Pandoc (..), PandocIO, readMarkdown, runIO, writeHtml5String)
import Text.Pandoc.Shared (stringify)
import Text.Read (readMaybe)

path :: FilePath
path = "../Example.md"

templatePath :: FilePath
templatePath = "../template/index.html"

outputPath :: FilePath
outputPath = "../build/index.html"

buildRules :: Action ()
buildRules = do
  (doc, meta) <- readRetro path
  retro <- case parseRetro doc of
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
renderPandoc (MkMyRetro past) = do
  ps <- mapM renderProject $ projects past
  pure $ MkMyRetro $ past {projects = ps}
  where
    w = unPandocM . htmlWriter
    renderProject (MkProject a b c d e) = MkProject <$> w a <*> w b <*> w c <*> w d <*> w e

readRetro :: FilePath -> Action ([Block], Value)
readRetro p = do
  file <- pack <$> readFile' p
  Pandoc meta blocks <- unPandocM $ readMarkdown defaultMarkdownOptions file
  meta' <- flattenMeta htmlWriter meta
  pure (blocks, meta')

parseRetro :: [Block] -> Maybe (MyRetro Pandoc)
parseRetro = runParser $ do
  past <- parsePastMonths
  pure $ MkMyRetro past

readNumber :: Text -> Text -> Maybe Int
readNumber prefix x = readMaybe . takeWhile isDigit . unpack =<< stripPrefix prefix (toLower x)

parsePastMonths :: Parser [Block] (PastMonths Pandoc)
parsePastMonths = do
  Header 1 _ (stringify -> h1) <- head'
  n <- embed $ readNumber "my past " h1
  projs <-
    takeWhileM
      ( \case
          Header 2 _ (toLower . stringify -> ("project" `isPrefixOf`) -> True) -> Just parseProject
          _ -> Nothing
      )
  Header 2 _ (toLower . stringify -> "retro of the current role/work") <- head'
  [a, b, c, d, e, f] <- parseSliders
  pure $ MkPastMonths n projs a b c d e f

parseSliders :: Parser [Block] [Slider]
parseSliders = do
  Table _ _ _ _ rows <- head'
  embed $ mapM (runParser parseInline . head . tail) rows
  where
    parseInline = do
      [Plain (stringify -> val)] <- get'
      n <- embed . readMaybe . unpack $ val
      pure $ toEnum n

parseProject :: Parser [Block] (Project Pandoc)
parseProject = do
  parts <-
    takeWhileM
      ( \case
          Header n _ _ | n <= 2 -> Nothing
          _ -> Just parsePart
      )
  [a, b, c, d, e] <- pure $ take 5 $ parts <> repeat (Pandoc mempty [])
  pure $ MkProject a b c d e
  where
    parsePart =
      Pandoc mempty
        <$> takeWhile'
          ( \case
              Header n _ _ | n <= 3 -> False
              _ -> True
          )

unPandocM :: PandocIO a -> Action a
unPandocM p = do
  result <- liftIO $ runIO p
  either (fail . show) pure result
