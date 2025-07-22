-------------------------------------------------------------

-- "Data types à la carte", Swierstra, JFP 2008

-- "a technique for assembling both data types and functions from isolated
-- individual components."

-------------------------------------------------------------


{-# OPTIONS_GHC   -Wno-deprecated-flags #-}
{-# LANGUAGE OverlappingInstances #-}

module Util where

import Base (Free(..), foldAlg)


data (f :+: g) a = Inl (f a) | Inr (g a)
infixr 5 :+:

instance (Functor f, Functor g) => Functor (f :+: g) where
   fmap f (Inl x) = Inl (fmap f x)
   fmap f (Inr y) = Inr (fmap f y)





-- a "subtyping" constraint
-- automate searching

class (Functor sub, Functor sup) => sub <: sup where
  inj :: sub a -> sup a
  prj :: sup a -> Maybe (sub a)

instance Functor sig => sig <: sig where
  inj = id
  prj = Just

instance (Functor sig1, Functor sig2) => sig1 <: (sig1 :+: sig2) where
  inj = Inl
  prj (Inl fa) = Just fa
  prj _ = Nothing

instance (Functor sig1, sig <: sig2) => sig <: (sig1 :+: sig2) where
  inj = Inr . inj
  prj (Inr ga) = prj ga
  prj _ = Nothing



-- composing

(#) :: (f a -> b) -> (g a -> b) -> (f :+: g) a -> b
(alg # fwd) (Inl op) = alg op
(alg # fwd) (Inr op) = fwd op


-- injection

inject :: (sub <: sup) => sub (Free sup a) -> Free sup a
inject = Op . inj

-- projection

-- project :: (sub <: sup) => Free sup a -> Maybe (sub (Free sup a))
-- project (Op s) = prj s
-- project _ = Nothing
