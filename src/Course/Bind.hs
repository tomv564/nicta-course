{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.Bind(
  Bind(..)
, (>>=)
, join
, (<=<)
) where

import Course.Core
import Course.Functor
import Course.Apply(Apply)
import Course.Id
import Course.List
import Course.Optional
import qualified Prelude as P

-- | All instances of the `Bind` type-class must satisfy one law. This law
-- is not checked by the compiler. This law is given as:
--
-- * The law of associativity
--   `∀f g x. g =<< (f =<< x) ≅ ((g =<<) . f) =<< x`
class Apply f => Bind f where
  -- Pronounced, bind.
  (=<<) ::
    (a -> f b)
    -> f a
    -> f b

infixr 1 =<<

-- | Witness that all things with (=<<) and (<$>) also have (<*>).
--
-- >>> Id (+10) <*> Id 8
-- Id 18
--
-- >>> (+1) :. (*2) :. Nil <*> 1 :. 2 :. 3 :. Nil
-- [2,3,4,2,4,6]
--
-- >>> Full (+8) <*> Full 7
-- Full 15
--
-- >>> Empty <*> Full 7
-- Empty
--
-- >>> Full (+8) <*> Empty
-- Empty
--
-- >>> ((+) <*> (+10)) 3
-- 16
--
-- >>> ((+) <*> (+5)) 3
-- 11
--
-- >>> ((+) <*> (+5)) 1
-- 7
--
-- >>> ((*) <*> (+10)) 3
-- 39
--
-- >>> ((*) <*> (+2)) 3
-- 15
(<*>) ::
  Bind f =>
  f (a -> b)
  -> f a
  -> f b
(<*>) f x = (<$> x) =<< f
  -- confused, x was not already in f ?
  -- error "todo: Course.Bind#(<*>)"

infixl 4 <*>

-- | Binds a function on the Id monad.
--
-- >>> (\x -> Id(x+1)) =<< Id 2
-- Id 3
instance Bind Id where
  (=<<) ::
    (a -> Id b)
    -> Id a
    -> Id b
  (=<<) = bindId
    -- error "todo: Course.Bind (=<<)#instance Id"

-- | Binds a function on a List.
--
-- >>> (\n -> n :. n :. Nil) =<< (1 :. 2 :. 3 :. Nil)
-- [1,1,2,2,3,3]
instance Bind List where
  (=<<) ::
    (a -> List b)
    -> List a
    -> List b
  (=<<) = flatMap -- see type parameters, same as Apply?!

-- | Binds a function on an Optional.
--
-- >>> (\n -> Full (n + n)) =<< Full 7
-- Full 14
instance Bind Optional where
  (=<<) ::
    (a -> Optional b)
    -> Optional a
    -> Optional b
  (=<<) = bindOptional

-- | Binds a function on the reader ((->) t).
--
-- >>> ((*) =<< (+10)) 7
-- 119
instance Bind ((->) t) where
  (=<<) ::
    (a -> ((->) t b))
    -> ((->) t a)
    -> ((->) t b)
  (=<<) g h = \x -> g (h x) x
    -- from the example: 119 = 70 + 49 = (10 * 7) + (7 * 7)
    -- so: apply g (*) to x and to ( h x) ??
    --error "todo: Course.Bind (=<<)#instance ((->) t)"

-- | Flattens a combined structure to a single structure.
--
-- >>> join ((1 :. 2 :. 3 :. Nil) :. (1 :. 2 :. Nil) :. Nil)
-- [1,2,3,1,2]
--
-- >>> join (Full Empty)
-- Empty
--
-- >>> join (Full (Full 7))
-- Full 7
--
-- >>> join (+) 7
-- 14
join ::
  Bind f =>
  f (f a)
  -> f a
join x = id =<< x
  -- bindOptional id
  -- this is like flatten for lists

-- | Implement a flipped version of @(=<<)@, however, use only
-- @join@ and @(<$>)@.
-- Pronounced, bind flipped.
--
-- >>> ((+10) >>= (*)) 7
-- 119
(>>=) ::
  Bind f =>
  f a
  -> (a -> f b)
  -> f b
(>>=) m f = join (f <$> m)
  -- take a value in a context and a function that creates more context,
  -- and apply function to value but extract value from double context.

infixl 1 >>=

-- | Implement composition within the @Bind@ environment.
-- Pronounced, kleisli composition.
--
-- >>> ((\n -> n :. n :. Nil) <=< (\n -> n+1 :. n+2 :. Nil)) 1
-- [2,2,3,3]
(<=<) ::
  Bind f =>
  (b -> f c)
  -> (a -> f b)
  -> a
  -> f c
(<=<) f1 f2 x = f1 =<< f2 x --join (f1 <$> (f2 x))
  -- bind is apply-and-join?
  -- error "todo: Course.Bind#(<=<)"

infixr 1 <=<

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Bind IO where
  (=<<) =
    (P.=<<)

instance Bind [] where
  (=<<) =
    (P.=<<)

instance Bind P.Maybe where
  (=<<) =
    (P.=<<)
