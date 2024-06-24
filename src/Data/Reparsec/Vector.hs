{-# LANGUAGE BangPatterns #-}
-- | Parsing from an input list.

module Data.Reparsec.Vector
  ( nextElement
  , lookAhead
  , endOfInput
  ) where

import           Data.Reparsec
import           Data.Vector (Vector)
import qualified Data.Vector as V

-- | Try to extract the next element from the input.
nextElement :: (NoMoreInput e, Monad m) => ParserT (Vector a) e m a
nextElement =
  ParserT
    (\mi0 pos more0 done failed ->
       let go mi more =
             case V.uncons $ V.drop pos mi of
               Just (x,_) -> done mi (pos + 1) more x
               Nothing ->
                 case more of
                   Complete -> failed mi pos more noMoreInputError
                   Incomplete ->
                     pure
                       (Partial
                          (\m ->
                             case m of
                               Nothing -> go mi Complete
                               Just i -> go (mi <> i) more))
        in go mi0 more0)
{-# INLINABLE nextElement #-}

-- | Expect the end of input.
endOfInput :: (ExpectedEndOfInput e, Monad m) => ParserT (Vector a) e m ()
endOfInput =
  ParserT
    (\mi0 pos more0 done failed ->
       let go mi more =
             case V.uncons $ V.drop pos mi of
               Just{} -> failed mi (pos + 1) more expectedEndOfInputError
               Nothing ->
                 case more of
                   Complete -> done mi pos more ()
                   Incomplete ->
                     pure
                       (Partial
                          (\m ->
                             case m of
                               Nothing -> go mi Complete
                               Just i -> go (mi <> i) more))
        in go mi0 more0)
{-# INLINABLE endOfInput #-}

-- | Look ahead by one token.
lookAhead :: (NoMoreInput e, Monad m) => ParserT (Vector a) e m a
lookAhead =
  ParserT
    (\mi0 pos more0 done failed ->
       runParserT
         nextElement
         mi0
         pos
         more0
         (\mi _pos more a -> done mi pos more a)
         failed)
