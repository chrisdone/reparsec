-- | Parse characters from a list. Exists mostly for testing purposes.

module Data.Reparsec.List.Char
  ( digit
  , letter
  , letters
  , digits
  , NonDigitError(..)
  , NonLetterError(..)
  ) where

import Data.Char
import Data.Maybe
import Data.Reparsec
import Data.Reparsec.List

-- | A non-digit was given.
class NonDigitError e where
  nonDigitError :: e

-- | A non-letter was given.
class NonLetterError e where
  nonLetterError :: e

digit :: (NoMoreInput e, NonDigitError e, Monad m) => ParserT [Char] e m Char
digit = do
  c <- nextElement
  if isDigit c
    then pure c
    else failWith nonDigitError

letter :: (NoMoreInput e, NonLetterError e, Monad m) => ParserT [Char] e m Char
letter = do
  c <- nextElement
  if isLetter c
    then pure c
    else failWith nonLetterError

letters :: (NoMoreInput e, NonLetterError e, Semigroup e, Monad m) => ParserT [Char] e m [Char]
letters = do
  c <- letter
  d <- fmap Just letters <> pure Nothing
  pure (c : fromMaybe [] d)

digits :: (NoMoreInput e , NonDigitError e, Semigroup e, Monad m) => ParserT [Char] e m [Char]
digits = do
  c <- digit
  d <- fmap Just digits <> pure Nothing
  pure (c : fromMaybe [] d)
