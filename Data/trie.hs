{-# LANGUAGE RankNTypes, GADTs, KindSignatures, ScopedTypeVariables,
             DataKinds, TypeSynonymInstances, FlexibleInstances,
             OverlappingInstances, TypeOperators, StandaloneDeriving #-}

module Data.Trie where

import Control.Arrow
import Data.Array
import Data.DiscOrder
import Unsafe.Coerce
import Text.Show.Functions()

-- Debateable whether you want this here, or indeed at all
deriving instance Show v => Show (Trie k v)

type TArray v = Array Int v

data Trie :: * -> * -> * where
   TEmpty :: Trie k v
   TUnit  :: v -> Trie () v
   TSum   :: Trie k1 v -> Trie k2 v -> Trie (Either k1 k2) v
   TProd  :: Trie k1 (Trie k2 v) -> Trie (k1,k2) v
   TMap   :: (k1 -> k2) -> Trie k2 v -> Trie k1 v
   TInt   :: TArray v -> Trie Int v

instance Functor (Trie k) where
   fmap _ TEmpty       = TEmpty
   fmap f (TUnit v)    = TUnit (f v)
   fmap f (TSum t1 t2) = TSum (fmap f t1) (fmap f t2)
   fmap f (TProd t)    = TProd (fmap (fmap f) t)
   fmap f (TMap g t)   = TMap g (fmap f t)
   fmap f (TInt t)     = TInt (fmap f t)

merge :: (v -> v -> v) -> Trie k v -> Trie k v -> Trie k v
merge _ TEmpty t                  = t
merge _ t TEmpty                  = t
merge c (TUnit v1) (TUnit v2)     = TUnit $ c v1 v2
merge c (TSum t1 t2) (TSum s1 s2) = TSum (merge c t1 s1) (merge c t2 s2)
merge c (TProd t1) (TProd t2)     = TProd $ merge (merge c) t1 t2
-- NB: If the internal structure of TMap, i.e. the function and underlying map
--     are different then we cannot merge! We can't be typesafe here unless we have
-- TODO find a scheme where we can provide proof that both TMaps have been constructed
--      identically so that we do not need the coerce.
merge c (TMap f t1) (TMap _ t2)   = TMap f (merge c t1 (unsafeCoerce t2))
merge f (TInt a1) (TInt a2)       = TInt $ mergeArr f a1 a2
merge _ t _                       = t

tlookup :: Trie k v -> k -> Maybe v
tlookup TEmpty _              = Nothing
tlookup (TUnit v) ()          = Just v
tlookup (TSum t1 _) (Left a)  = tlookup t1 a
tlookup (TSum _ t2) (Right a) = tlookup t2 a
tlookup (TProd t1) (k1,k2)    = tlookup t1 k1 >>= (`tlookup` k2)
tlookup (TMap g t1) k         = tlookup t1 $ g k
-- (!) is a totally inappropriate lookup fn here it does not preserve
-- the correct API. If a cell is empty it returns the 0 element, not Nothing
tlookup (TInt t1) k           = Just $ t1 ! k

trie :: Order k -> forall v.[(k,v)] -> Trie k [v]
trie _ [ ]             = TEmpty
trie TrivO rel         = TUnit (map snd rel)
trie (SumL o1 o2) rel  = TSum l r
   where l = trie o1 (sumlefts rel)
         r = trie o2 (sumrights rel)
trie (ProdL o1 o2) rel = TProd (fmap (trie o2) (trie o1 (map curryl rel)))
trie (MapO g o) rel    = TMap g (trie o (map (first g) rel))
trie (NatO i) rel      = TInt (bdiscNat i rel)

singleton :: Order k -> k -> v -> Trie k [v]
singleton o k v = trie o [(k,v)]

-----------------
-- Helper code --
-----------------
curryl :: ((a,b),c) -> (a,(b,c))
curryl ((a,b),c) = (a,(b,c))

sumlefts :: [(Either a b,c)] -> [(a,c)]
sumlefts xs = [ (a,c) | (Left a, c) <- xs ]

sumrights :: [(Either a b,c)] -> [(b,c)]
sumrights xs = [ (b,c) | (Right b, c) <- xs ]

bdiscNat :: Int -> [(Int, v)] -> TArray [v]
bdiscNat (n :: Int) = accumArray (flip (:)) [] (0, n-1)

-- Merging arrays, requires a scan, probably could do better
mergeArr :: (v -> v -> v) -> TArray v -> TArray v -> TArray v
mergeArr f a1 a2 = array (bounds a1) $ zipWith (\(a,b) (_,d) -> (a,f b d)) (assocs a1) (assocs a2)

