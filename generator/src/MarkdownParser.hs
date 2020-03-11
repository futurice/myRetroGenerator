module MarkdownParser (parseMyRetro) where

import Data (Future (..), MyRetro (..), NextMonths (..), PastMonths (..), Project (..), Retro (..), Slider)
import Data.Char (isDigit)
import Data.Text (isPrefixOf, stripPrefix, toLower, unpack)
import Parser (Parser, embed, get', head', runParser, takeWhile', takeWhileM)
import Text.Pandoc (Block (..), Pandoc (..))
import Text.Pandoc.Shared (stringify)
import Text.Read (readMaybe)

parseMyRetro :: [Block] -> Maybe (MyRetro Pandoc)
parseMyRetro = runParser $ MkMyRetro <$> parsePastMonths <*> parseNextMonths <*> parseFuture

parseFuture :: Parser [Block] (Future Pandoc)
parseFuture = do
  Header 1 _ (toLower . stringify -> "find your future") <- head'
  retro <- parseRetro
  pure $ MkFuture retro

parseNextMonths :: Parser [Block] (NextMonths Pandoc)
parseNextMonths = do
  Header 1 _ (toLower . stringify -> "my next 6-12 months") <- head'
  retro <- parseRetro
  parts <- parseParts 2
  [a, b, c, d, e, f, g, h] <- pure $ take 8 $ parts <> repeat mempty
  pure $ MkNextMonths retro a b c d e f g h

parseRetro :: Parser [Block] (Retro Pandoc)
parseRetro = do
  Header 2 _ (toLower . stringify -> ("retro of" `isPrefixOf`) -> True) <- head'
  parts <- parseParts 3
  [a, b, c] <- pure $ take 3 $ parts <> repeat (Pandoc mempty [])
  pure $ MkRetro a b c

parseParts :: Int -> Parser [Block] [Pandoc]
parseParts n =
  takeWhileM
    ( \case
        Header x _ _ | x <= (n - 1) -> Nothing
        _ -> Just parsePart
    )
  where
    parsePart =
      Pandoc mempty
        <$> takeWhile'
          ( \case
              Header x _ _ | x <= n -> False
              _ -> True
          )

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
  Header 2 _ (toLower . stringify -> ("retro of" `isPrefixOf`) -> True) <- head'
  [a, b, c, d, e, f] <- parseSliders
  pure $ MkPastMonths n projs a b c d e f
  where
    readNumber prefix x = readMaybe . takeWhile isDigit . unpack =<< stripPrefix prefix (toLower x)

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
  parts <- parseParts 3
  [a, b, c, d, e] <- pure $ take 5 $ parts <> repeat (Pandoc mempty [])
  pure $ MkProject a b c d e
