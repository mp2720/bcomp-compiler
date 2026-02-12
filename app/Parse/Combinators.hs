{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Parse.Combinators where

import Control.Applicative (Alternative (empty, many, (<|>)))
import Control.Monad (liftM)
import GHC.Base (MonadPlus (mzero), ap)

class State s where
  type Term s

  -- | Accepts two states that were produced by chain of @next@ calls (zero or more) from the same input state.
  -- Returns true iff one of them advanced further that the other.
  didAdvance :: s -> s -> Bool

  -- | Returns current terminal and state advanced by one.
  -- 'Nothing' is returned iff there's no more terminals left.
  -- Let @s1@ be some state and @t, s2 = next s1@. Then @didAdvance s1 s2@ <=> @t@ is not 'Nothing'.
  next :: s -> (Maybe (Term s), s)

type ParserResult s v = (State s) => Either s (v, s)

newtype (State s) => Parser s v = Parser
  { runParser :: s -> ParserResult s v
  }

instance (State s) => Functor (Parser s) where
  fmap = liftM

instance (State s) => Applicative (Parser s) where
  pure f = Parser $ \s -> Right (f, s)
  (<*>) = ap

instance (State s) => Monad (Parser s) where
  return = pure
  (Parser p) >>= f = Parser $ \s -> do
    (pv, s') <- p s
    runParser (f pv) s'

instance (State s) => Alternative (Parser s) where
  empty = Parser $ \s -> Left s
  (Parser p) <|> (Parser q) = Parser $ \s ->
    case p s of
      Left s'
        | didAdvance s s' ->
            -- Fail to prevent backtracking.
            Left s'
        | otherwise ->
            q s'
      r -> r

instance (State s) => MonadPlus (Parser s)

instance (State s) => MonadFail (Parser s) where
  fail _ = mzero

try :: (State s) => Parser s v -> Parser s v
try p = Parser $ \s -> case runParser p s of
  Left _ -> Left s
  right -> right

lookAhead :: (State s) => Parser s v -> Parser s v
lookAhead p = Parser $ \s -> case runParser p s of
  Right (v, _) -> Right (v, s)
  left -> left

notFollowedBy :: (State s) => Parser s v -> Parser s ()
notFollowedBy p = Parser $ \s -> case runParser p s of
  Left _ -> Right ((), s)
  Right _ -> Left s

mapTerm :: (State s) => (Term s -> Maybe v) -> Parser s v
mapTerm f = Parser $ \s ->
  case next s of
    (Nothing, _) -> Left s
    (Just t, ns) -> case f t of
      Just v -> Right (v, ns)
      Nothing -> Left s

satisfy :: (State s) => (Term s -> Bool) -> Parser s (Term s)
satisfy p = mapTerm $ \t -> if p t then Just t else Nothing

failParser :: (State s) => Parser s v
failParser = Parser $ \s -> Left s

-- | Advances state on error.
eof :: (State s) => Parser s ()
eof = Parser $ \s -> case next s of
  (Nothing, _) -> Right ((), s)
  (Just _, ns) -> Left ns

join :: (State s) => Parser s a -> Parser s b -> Parser s [b]
join sep el = (:) <$> el <*> many (sep *> el) <|> pure []
