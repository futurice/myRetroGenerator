module MarkdownParser (parseMyRetro) where

import Data (MyRetro (..), NextMonths (..), PastMonths (..), Project (..), Retro (..), Slider)
import Data.Char (isDigit)
import Data.Text (Text, isPrefixOf, stripPrefix, toLower, unpack)
import Parser (Parser, embed, get', head', runParser, takeWhile', takeWhileM)
import Text.Pandoc (Block (..), Pandoc (..))
import Text.Pandoc.Shared (stringify)
import Text.Read (readMaybe)

parseMyRetro :: [Block] -> Maybe (MyRetro Pandoc)
parseMyRetro = runParser $ do
  past <- parsePastMonths
  months <- parseNextMonths
  pure $ MkMyRetro past months

readNumber :: Text -> Text -> Maybe Int
readNumber prefix x = readMaybe . takeWhile isDigit . unpack =<< stripPrefix prefix (toLower x)

parseNextMonths :: Parser [Block] (NextMonths Pandoc)
parseNextMonths = do
  Header 1 _ (toLower . stringify -> "my next 6-12 months") <- head'
  retro <- parseRetro
  pure $ MkNextMonths retro

parseRetro :: Parser [Block] (Retro Pandoc)
parseRetro = do
  Header 2 _ (toLower . stringify -> "retro of the current role/work") <- head'
  parts <-
    takeWhileM
      ( \case
          Header n _ _ | n <= 2 -> Nothing
          _ -> Just $ parsePart 3
      )
  [a, b, c] <- pure $ take 3 $ parts <> repeat (Pandoc mempty [])
  pure $ MkRetro a b c

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

parsePart :: Int -> Parser [Block] Pandoc
parsePart n =
  Pandoc mempty
    <$> takeWhile'
      ( \case
          Header x _ _ | x <= n -> False
          _ -> True
      )

parseProject :: Parser [Block] (Project Pandoc)
parseProject = do
  parts <-
    takeWhileM
      ( \case
          Header n _ _ | n <= 2 -> Nothing
          _ -> Just $ parsePart 3
      )
  [a, b, c, d, e] <- pure $ take 5 $ parts <> repeat (Pandoc mempty [])
  pure $ MkProject a b c d e
