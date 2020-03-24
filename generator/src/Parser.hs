module Parser (Parser, embed, head', runParser, takeWhile', takeWhileM, get', put') where

import Control.Monad.Fail (MonadFail (..))
import Control.Monad.State (State, evalState, get, put)

newtype Parser s a = MkParser (State s (Maybe a))

instance Functor (Parser s) where
  fmap f (MkParser x) = MkParser (fmap (fmap f) x)

instance Applicative (Parser s) where
  pure x = MkParser $ pure $ Just x
  (MkParser x) <*> (MkParser y) = MkParser $ x >>= \f -> y >>= \v -> pure $ f <*> v

instance Monad (Parser s) where
  (MkParser x) >>= f = MkParser $
    x >>= \case
      Just y -> case (f y) of MkParser r -> r
      Nothing -> pure Nothing

instance MonadFail (Parser s) where
  fail _ = MkParser (pure Nothing)

embed :: Maybe a -> Parser s a
embed = MkParser . pure

get' :: Parser a a
get' = MkParser $ Just <$> get

put' :: a -> Parser a ()
put' = MkParser . fmap Just . put

head' :: Parser [a] a
head' = do
  (x : xs) <- get'
  put' xs
  pure x

takeWhile' :: (a -> Bool) -> Parser [a] [a]
takeWhile' f = get' >>= \case
  [] -> pure []
  (x : xs) ->
    if f x
      then put' xs *> fmap ((:) x) (takeWhile' f)
      else pure []

takeWhileM :: (a -> Maybe (Parser [a] b)) -> Parser [a] [b]
takeWhileM f = get' >>= \case
  [] -> pure []
  (x : xs) -> case f x of
    Nothing -> pure []
    Just p -> put' xs *> p >>= \b -> (:) b <$> takeWhileM f

runParser :: Parser s a -> s -> Maybe a
runParser (MkParser x) = evalState x
