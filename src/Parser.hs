{- |
Copyright:  (c) 2021 wspbr
Maintainer: wspbr <rtrn.0@ya.ru>

This module contains lens-like functions to extract fields from
Telegram API ADTs such as 'Update' or 'Message'.
-}

module Parser
    ( Parser(..)
    , (<?>)
    ) where

import           Control.Applicative
import           Control.Monad
import qualified Data.Text           as T

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
