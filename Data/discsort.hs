{-# LANGUAGE RankNTypes, GADTs, ScopedTypeVariables, DataKinds,
             TypeSynonymInstances, FlexibleInstances, OverlappingInstances,
             TypeOperators #-}

module Data.DiscSort where

import Data.DiscOrder
import Data.Array

type Disc k = forall v. [(k, v)] -> [[v]]

sdisc :: Order k -> Disc k
sdisc _ [] = []
sdisc _ [(_, v)] = [[v]]
sdisc (NatO n) xs = sdiscNat n xs
sdisc TrivO xs = [[ v | (_, v) <- xs ]]
sdisc (SumL r1 r2) xs = sdisc r1 [ (k, v) | (Left k, v) <- xs ]
                        ++ sdisc r2 [ (k, v) | (Right k, v) <- xs ]
sdisc (ProdL r1 r2) xs =
   [ vs | ys <- sdisc r1 [ (k1,(k2,v)) | ((k1,k2), v) <-xs],
          vs <- sdisc r2 ys ]
sdisc (MapO f r) xs = sdisc r [ (f k, v) | (k, v) <- xs ]

dsort :: Order k -> forall v.[(k,v)] -> [v]
dsort o vs = concat $ sdisc o vs

-----------------
-- Helper code --
-----------------
sdiscNat :: Int -> Disc Int
sdiscNat n xs = filter (not . null) (srtdiscNat n update xs)
   where update vs v = v : vs
srtdiscNat :: Int -> ([v] -> v -> [v]) -> [(Int, v)] -> [[v]]
srtdiscNat (n :: Int) update xs = map reverse (elems (accumArray update [] (0, n-1) xs))
