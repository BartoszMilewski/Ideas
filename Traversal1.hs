{-# language GADTs #-}
{-# language ConstraintKinds #-}
{-# language DataKinds #-}
{-# language KindSignatures #-}
{-# language RankNTypes #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleContexts #-}
{-# language TypeFamilies #-}
{-# language PolyKinds #-}
{-# language ScopedTypeVariables #-}
{-# language AllowAmbiguousTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Traversal1 where
  
data Nat = Z | S Nat
  deriving Show
  
data Vec n a where
    VNil  :: Vec Z a
    VCons :: a -> Vec n a -> Vec (S n) a

-- a to the power of n
type family Pow (a :: *) (n :: Nat) :: * where
  Pow a Z = ()
  Pow a (S n) = (a, Pow a n)

-- ts is a stream of types
-- Sum n. (ts n, a^n)
data TrAct (ts :: Nat -> *) (a :: *) = 
  forall (n :: Nat). TrAct (ts n) (Pow a n)

data TraversalEx s t a b = 
  forall ts. TEx (s -> TrAct ts a) (TrAct ts b -> t)
  
-- to construct it, pick a stream of types ts, one per n
-- a function that takes a container and produces an instance
-- of TAct. TAct is constructed by picking a particular n,
-- using it to access the type (ts n), creating a value of that type,
-- and creating a power of a with this exponent.
  
vecTrav :: TraversalEx (Vec n a) (Vec n b) a b
vecTrav = TEx f g

f :: forall n a b. (Vec n a) -> TrAct ShapeVec a
f v = TrAct (vgrade v) (vecTuple v)
g :: forall n b. TrAct ShapeVec b -> Vec n b
g (TrAct tsn pow) = tupleVec tsn pow
    

vecTuple :: Vec n a -> Pow a n
vecTuple VNil = ()
vecTuple (VCons x xs) = (x, vecTuple xs)

tupleVec :: forall n a. ShapeVec n -> Pow a n -> Vec n a
tupleVec sn pow = undefined

    
-- Use Nat->* rather than
-- ShapeVec (n :: Nat) :: * 
-- because GCH only allows fully saturated type families
-- to be passed to other types

type family ShapeVec :: Nat-> * where
  ShapeVec = SNat
  
data SNat n where
  SZ :: SNat Z
  SS :: SNat n -> SNat (S n)

class Graded (f :: Nat -> *) where
  grade :: f n -> SNat n
  
vgrade :: Vec n a -> SNat n
vgrade VNil = SZ
vgrade (VCons _ v) = SS (vgrade v)
  
-- Here's one trick to allow partial application

type family Apply (token :: *) (n :: Nat) :: a 

data ShapeVecToken

type instance Apply ShapeVecToken Z = ()
type instance Apply ShapeVecToken (S n) = ((), Apply ShapeVecToken n)

