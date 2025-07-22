{-# OPTIONS_GHC  -Wno-noncanonical-monad-instances -Wno-unused-matches -Wno-deprecated-flags #-}
{-# LANGUAGE OverlappingInstances #-}

import Prelude hiding (fail, or)
import Base (Free(..), foldAlg)
import Util


-------------------------------------------------------------

--  1. Exception

-------------------------------------------------------------


data Exc a = Throw
  deriving Functor

throw :: Exc <: sig => Free sig a
throw = inject Throw

safeDiv :: Exc <: sig => Int -> Int -> Free sig Int
safeDiv m n =
  if n == 0 then throw
  else return $ (m `div` n)

hExc :: Functor sig => Free (Exc :+: sig) a -> Free sig (Maybe a)
hExc = foldAlg (return . Just) (alg # fwd) where
  alg Throw = return Nothing
  fwd op = Op op


data Nil a
  deriving Functor


hNil :: Free Nil a -> a
hNil (Pure x) = x



-------------------------------------------------------------

--  2. State

-------------------------------------------------------------

data State s a
  = Get (s -> a) | Put s a
  deriving Functor

get :: State s <: sig => Free sig s
get = inject (Get return)

put :: State s <: sig => s -> Free sig ()
put s = inject (Put s (return ()))

incr :: State Int <: sig => Free sig Int
incr = do x <- get
          put (x + 1)
          return x

hState :: Functor sig => Free (State s :+: sig) a -> (s -> Free sig (a, s))
hState = foldAlg (\a s -> return (a, s)) (alg # fwd) where
  alg (Get k)    = \s -> k s s
  alg (Put s' k) = \_ -> k s'
  fwd op = \s -> Op $ fmap (\k -> k s) op


runIncr :: (Int, Int)
runIncr = hNil $ hState incr 42



-------------------------------------------------------------

--  3. Combine effects

-------------------------------------------------------------

pgm :: (State Int <: sig, Exc <: sig )=> Free sig Int
pgm = do
  (s::Int) <- get
  if s == 42 then throw
  else incr


hStateLog :: Functor sig => Free (State s :+: sig) a -> (s -> Free sig (a, [s]))
hStateLog = foldAlg (\a s -> return (a,[s])) (alg # fwd) where
  alg (Get k) = \s -> k s s
  alg (Put s' k) = \s -> do (r, ss) <- k s'
                            return (r, s : ss)
  fwd op = \s -> Op $ fmap (\k -> k s) op

runPgm3 :: (Maybe Int, [Int])
runPgm3 = hNil $ (hStateLog $ hExc pgm) 42

hStateDiscard :: Functor sig => Free (State s :+: sig) a -> (s -> Free sig a)
hStateDiscard = foldAlg (\a s -> return a) (alg # fwd) where
  alg (Get k) = \s -> k s s
  alg (Put s' k) = \s -> k s'
  fwd op = \s -> Op $ fmap (\k -> k s) op



-------------------------------------------------------------

--  4. Non-determinisim

-------------------------------------------------------------


data NDet a = Or (Bool -> a)
  deriving Functor

or :: NDet <: sig => Free sig Bool
or = inject $ Or return


hDet :: Functor sig => Free (NDet :+: sig) a -> Free sig [a]
hDet = foldAlg (\x -> return [x]) (alg # fwd) where
  alg (Or k) = do l1 <- k True
                  l2 <- k False
                  return $ l1 ++ l2
  fwd op = Op op


-------------------------------------------------------------

--  5. drunkToss

-------------------------------------------------------------

data Coin = Heads | Tail
  deriving Show


drunkToss :: (NDet <: sig, Exc <: sig) => Free sig Coin
drunkToss = do b <- or
               if b then do
                 b' <- or
                 if b' then return Heads else return Tail
               else throw


-- hNil $ hExc $ hDet $ drunkToss

-- hNil $ hDet $ hExc $ drunkToss
