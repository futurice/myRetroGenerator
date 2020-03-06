module Main where

import Control.Monad (forM)
import Data (MyRetro (..), PastMonths (..), Project (..), Slider)
import Data.Aeson (Value)
import Data.Char (isDigit)
import Data.Text (Text, isPrefixOf, pack, stripPrefix, unpack)
import Development.Shake (Action, ShakeOptions (..), Verbosity (Verbose), liftIO, readFile', shakeOptions)
import Development.Shake.Forward (shakeArgsForward)
import Parser (Parser, embed, get', head', put', runParser, takeWhile', takeWhileM)
import Slick.Pandoc (defaultHtml5Options, defaultMarkdownOptions, flattenMeta)
import Text.Pandoc (Block (..), Inline (..), Pandoc (..), PandocIO, readMarkdown, runIO, writeHtml5String)
import Text.Pandoc.Shared (stringify)
import Text.Read (readMaybe)

path :: FilePath
path = "../Example.md"

buildRules :: Action ()
buildRules = do
  document@(doc, _) <- readRetro path
  liftIO $ print document
  liftIO $ print $ parseRetro doc

main :: IO ()
main = shakeArgsForward options buildRules
  where
    options = shakeOptions {shakeVerbosity = Verbose, shakeLintInside = ["\\"]}

readRetro :: FilePath -> Action ([Block], Value)
readRetro p = do
  file <- pack <$> readFile' p
  Pandoc meta blocks <- unPandocM $ readMarkdown defaultMarkdownOptions file
  meta' <- flattenMeta (writeHtml5String defaultHtml5Options) meta
  pure (blocks, meta')

parseRetro :: [Block] -> Maybe MyRetro
parseRetro = runParser $ do
  past <- parsePastMonths
  pure $ MkMyRetro past

readNumber :: Text -> Text -> Maybe Int
readNumber prefix x = readMaybe . takeWhile isDigit . unpack =<< stripPrefix prefix x

parsePastMonths :: Parser [Block] PastMonths
parsePastMonths = do
  Header 1 _ (stringify -> h1) <- head'
  n <- embed $ readNumber "My past " h1
  projs <-
    takeWhileM
      ( \case
          Header 2 _ (stringify -> ("Project" `isPrefixOf`) -> True) -> Just parseProject
          _ -> Nothing
      )
  Header 2 _ (stringify -> "Retro of the current role/work") <- head'
  [a, b, c, d, e, f] <- parseSliders
  pure $ MkPastMonths n projs a b c d e f

parseSliders :: Parser [Block] [Slider]
parseSliders = do
  Para inlines <- head'
  embed $ runParser parseInlines inlines
  where
    parseInlines = do
      (stringify -> x) <- takeLine
      a <- embed $ readNumber "Your projects: " x
      (stringify -> x1) <- takeLine
      b <- embed $ readNumber "Your impact: " x1
      (stringify -> x2) <- takeLine
      c <- embed $ readNumber "Your role & responsibilities: " x2
      (stringify -> x3) <- takeLine
      d <- embed $ readNumber "Your growth & career path level: " x3
      (stringify -> x4) <- takeLine
      e <- embed $ readNumber "The appreciation from your colleagues and supervisor: " x4
      (stringify -> x5) <- takeLine
      f <- embed $ readNumber "Life in general: " x5
      pure $ map toEnum [a, b, c, d, e, f]
    takeLine = do
      xs <-
        takeWhile'
          ( \case
              SoftBreak -> False
              _ -> True
          )
      s <- get'
      case s of
        SoftBreak : ys -> put' ys
        _ -> pure ()
      pure xs

parseProject :: Parser [Block] Project
parseProject = do
  [a, b, c, d, e] <- forM [1 .. 5] $ \(_ :: Int) -> do
    Header 3 _ _ <- head'
    content <-
      takeWhile'
        ( \case
            Header n _ _ | n <= 3 -> False
            _ -> True
        )
    pure $ Pandoc mempty content
  pure $ MkProject a b c d e

unPandocM :: PandocIO a -> Action a
unPandocM p = do
  result <- liftIO $ runIO p
  either (fail . show) pure result
