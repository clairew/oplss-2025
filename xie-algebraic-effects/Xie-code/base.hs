-------------------------------------------------------------

-- Building your own effect handlers library
-- using Free Monads.

-- Ningning Xie, University of Toronto

-- OPLSS 2025

-------------------------------------------------------------


{-# OPTIONS_GHC  -Wno-noncanonical-monad-instances -Wno-unused-matches #-}

module Base where

import Control.Monad (ap)
import Prelude hiding (fail, or)
import Data.Kind


-------------------------------------------------------------

--  1. Free Monad

-------------------------------------------------------------

data Free (f :: Type -> Type) a
  = Pure a | Op (f (Free f a))

instance Functor f => Monad (Free f) where
  return x = Pure x

  -- Free f a -> (a -> Free f b) -> Free f b
  (Pure x) >>= g = g x
  (Op y)   >>= g = Op $ fmap (>>= g) y


instance Functor f => Applicative (Free f) where
  pure = return
  (<*>) = ap

instance Functor f => Functor (Free f) where
  fmap f x = pure f <*> x


-------------------------------------------------------------

--  2. Exception

-------------------------------------------------------------

data Exc a = Throw
  deriving Functor

throw :: Free Exc a
throw = Op Throw

safeDiv :: Int -> Int -> Free Exc Int
safeDiv m n =
  if n == 0 then throw
  else return $ m `div` n






-------------------------------------------------------------

--  3. Handler for exception

-------------------------------------------------------------

hExc :: Free Exc a -> Maybe a
hExc (Pure x) = Just x
hExc (Op f) = Nothing


-- hExc $ safeDiv 42 2
-- hExc $ safeDiv 42 0



-------------------------------------------------------------

--  4. State

-------------------------------------------------------------

data State s a
  = Get (s -> a) | Put s a


instance Functor (State s) where
  fmap f (Get k) = Get (\s -> f (k s))
  fmap f (Put s k) = Put s (f k)

get :: Free (State s) s
get = Op (Get return)

put :: s -> Free (State s) ()
put s = Op (Put s (return ()))

incr :: Free (State Int) Int
incr = do x <- get
          put (x + 1)
          return x



-------------------------------------------------------------

--  5. Handler for State

-------------------------------------------------------------


hState :: Free (State s) a -> (s -> (a, s))
hState (Pure x) = \s -> (x, s)
hState (Op (Get k)) = \s ->  (hState (k s)) s
hState (Op (Put s' k)) = \_ -> (hState k) s'


runIncr :: (Int, Int)
runIncr = hState incr 42




-------------------------------------------------------------
--  6. Folding over the free monad
-------------------------------------------------------------


foldAlg :: Functor op =>
   (a -> b) -> (op b -> b) -> Free op a -> b
foldAlg gen alg (Pure x)  = gen x
foldAlg gen alg (Op op)   = alg (fmap (foldAlg gen alg) op)


hState' :: Free (State s) a -> (s -> (a, s))
hState' = foldAlg (\x y -> (x,y)) alg where
  alg (Get k)    = \s -> k s s
  alg (Put s' k) = \_ -> k s'


-- Exceptions

hExc' :: Free Exc a -> Maybe a
hExc' = foldAlg Just alg where
  alg Throw = Nothing


-- other states


hStateLog :: Free (State s) a -> (s -> (a, [s]))
hStateLog = foldAlg (\a s -> (a,[s])) alg where
  alg (Get k) = \s -> k s s
  alg (Put s' k) = \s -> let (r, ss) = k s'
                         in (r, s : ss)


hStateDiscard :: Free (State s) a -> (s -> a)
hStateDiscard = foldAlg (\a s -> a) alg where
  alg (Get k) = \s -> k s s
  alg (Put s' k) = \s -> k s'


-------------------------------------------------------------
--  7. Non-determinism
-------------------------------------------------------------


data NDet a = Fail | Or a a
  deriving Functor

fail :: Free NDet a
fail = Op Fail

or :: Free NDet a -> Free NDet a -> Free NDet a
or x y = Op $ Or x y


-- handler

hDet :: Free NDet a -> [a]
hDet = foldAlg (\x -> [x]) alg where
  alg Fail = []
  alg (Or y z) = y ++ z


choices = or (return 1)
             (or  (return 3) fail)



-- alternatively

data NDet' a = Fail' | Or' (Bool -> a)
  deriving Functor


fail' :: Free NDet' a
fail' = Op Fail'

or' ::  Free NDet' Bool
or' = Op $ Or' return


hDet' :: Free NDet' a -> [a]
hDet' = foldAlg (\x -> [x]) alg where
  alg Fail' = []
  alg (Or' k) = let y = k True
                    z = k False
                in y ++ z


choices' = do  b <- or'
               if b then return 1
               else do b2 <- or'
                       if b2 then return 3
                       else fail'
