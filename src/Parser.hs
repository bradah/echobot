{- |
Copyright:  (c) 2021 wspbr
Maintainer: wspbr <rtrn.0@ya.ru>

Parse Haskell data structures.
-}

module Parser
    ( Parser(..)
    , (<?>)
    ) where

import           Control.Applicative (Alternative (..))

-- | Generalized type of record field selectors.
newtype Parser a b = Parser
  { runParser :: a -> Maybe b }

-- | Infix flipped version of 'runParser'.
infixl 4 <?>
(<?>) :: a -> Parser a b -> Maybe b
(<?>) = flip runParser

instance Functor (Parser a) where
  fmap f (Parser p) =
    Parser $ fmap f . p

instance Applicative (Parser a) where
  pure = Parser . pure . pure
  Parser f <*> Parser x =
    Parser $ \a -> f a <*> x a

instance Monad (Parser a) where
  return = pure
  Parser x >>= f =
    Parser $ \a -> x a >>= flip runParser a . f

instance Alternative (Parser a) where
  empty = Parser (const Nothing)
  Parser f <|> Parser g = Parser (\a -> f a <|> g a)
