{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Monad where

import Course.Applicative
import Course.Bind
import Course.Core
import Course.Id
import Course.List
import Course.Optional
import qualified Prelude as P

{-

The only exercise here is a thinking one. The understanding that the Monad
type-class is the coming together of its sub type-classes
(`Applicative` and `Bind`). There are no coding exercises here. The purpose of
this module is simply to provide a definition for the word "monad" and that
definition is built on previous exercises.

The monad type-class provides no additional methods to `Applicative` and `Bind`.

-}

class (Applicative f, Bind f) => Monad f where
    -- return :: a -> f a
    -- (=<<) :: f a -> (a -> f b) -> f b
    -- (<<) :: f a -> f b -> f b

instance Monad Id where
    -- return = Id
    -- (Id x) =<< f = f x -- (bindId)
    -- _ << f = f

instance Monad List where
    -- return x = pure x -- (x (:.) Nil)
    -- xs =<< f = flatMap f xs
    -- _ << f = f

instance Monad Optional where
    -- return x = Full x
    -- (Full x) =<< f = f x -- (bindOptional)
    -- Empty =<< _ = Empty
    -- _ << f = f

instance Monad ((->) t) where
    -- return x = const x
    -- g =<< h = \x -> h (g x) x -- return a lambda where value is first applied to g, result is then applied to h.
    -- _ << f = f


-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Monad IO where

instance Monad [] where

instance Monad P.Maybe where
