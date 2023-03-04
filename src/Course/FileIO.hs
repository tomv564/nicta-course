{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.FileIO where

import Course.Core
import Course.Applicative
import Course.Apply
import Course.Bind
import Course.Functor
import Course.List

{-

Useful Functions --

  getArgs :: IO (List Chars)
  putStrLn :: Chars -> IO ()
  readFile :: Chars -> IO Chars
  lines :: Chars -> List Chars
  void :: IO a -> IO ()

Abstractions --
  Applicative, Monad:

    <$>, <*>, >>=, =<<, pure

Problem --
  Given a single argument of a file name, read that file,
  each line of that file contains the name of another file,
  read the referenced file and print out its name and contents.

Example --
Given file files.txt, containing:
  a.txt
  b.txt
  c.txt

And a.txt, containing:
  the contents of a

And b.txt, containing:
  the contents of b

And c.txt, containing:
  the contents of c

$ runhaskell FileIO.hs "files.txt"
============ a.txt
the contents of a

============ b.txt
the contents of b

============ c.txt
the contents of c

-}

-- /Tip:/ use @getArgs@ and @run@
main ::
  IO ()
main = do
  fileName <- (headOr "") <$> getArgs
  if fileName == ""
    then putStrLn "Please provide a valid path to a file"
    else (getFile fileName) >>= \(_, contents) -> run contents

type FilePath =
  Chars

-- /Tip:/ Use @getFiles@ and @printFiles@.
run ::
  Chars
  -> IO ()
run contents = getFiles (lines contents) >>= printFiles

-- readFile is IO Chars.
 -- error "todo: Course.FileIO#run"

-- getFiles needs a List FilePath.

getFiles ::
  List FilePath
  -> IO (List (FilePath, Chars))
getFiles ps =
  sequence (map getFile ps)
  --error "todo: Course.FileIO#getFiles"

getFile ::
  FilePath
  -> IO (FilePath, Chars)
getFile path = readFile path >>= \contents -> return (path, contents)
-- stolen, below was mine:
  -- (,) <$> pure path <*> readFile path
--   -- readFile is already in IO
  -- bring path into IO as well, then apply (,) to it

printFiles ::
  List (FilePath, Chars)
  -> IO ()
printFiles fs = sequence (map (uncurry printFile) fs) >> return ()
-- todo: sequence converts List () to ()
  -- error "todo: Course.FileIO#printFiles"
-- printFiles files = map printFile files


printFile ::
  FilePath
  -> Chars
  -> IO ()
printFile path contents =
  putStrLn ("============ " ++ path) >>
  putStrLn contents

