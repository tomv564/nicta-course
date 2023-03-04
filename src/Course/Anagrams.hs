{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Course.Anagrams where

import Course.Core
import Course.List
import Course.Functor

{-

Functions you will need
--
* fmap :: (a -> b) -> IO a -> IO b
* readFile :: FilePath -> IO Str
* lines :: Str -> [Str]
* permutations :: [a] -> [[a]]
* intersectBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
* toLower :: Char -> Char

Functions that might help
-
* on :: (b -> b -> c) -> (a -> b) -> a -> a -> c

-}


-- Return all anagrams of the given string
-- that appear in the given dictionary file.
anagrams ::
  Chars
  -> Filename
  -> IO (List Chars)
anagrams s dict =
    (<$>) (intersectBy equalIgnoringCase (permutations s) . lines) (readFile dict)
  --

-- Compare two strings for equality, ignoring case
equalIgnoringCase ::
  Chars
  -> Chars
  -> Bool
equalIgnoringCase a b = (map toLower a) == (map toLower b)
  -- error "todo: Course.Anagrams#equalIgnoringCase"
