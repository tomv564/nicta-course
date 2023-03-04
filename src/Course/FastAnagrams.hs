{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.FastAnagrams where

import Course.Core
import Course.List
import Course.Functor
import qualified Data.Set as S

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
fastAnagrams ::
  Chars
  -> Filename
  -> IO (List Chars)
fastAnagrams word file =
    (flip (filter . flip S.member) (permutations word) . S.fromList . hlist . lines) <$> readFile file
    -- note: so much argument flipping to get a clean apply. would prefer do notation or let bindings
    -- let set = ( S.fromList . hlist . lines ) <$> (readFile file)
    -- in filter (\w -> S.member set w) (permutations word)
    -- (filter (S.member (permutations word) (S.fromList . hlist . lines))) <$> (readFile dict)
  --error "todo: Course.FastAnagrams#fastAnagrams"

newtype NoCaseString =
  NoCaseString {
    ncString :: Chars
  }

instance Eq NoCaseString where
  (==) = (==) `on` map toLower . ncString

instance Show NoCaseString where
  show = show . ncString
