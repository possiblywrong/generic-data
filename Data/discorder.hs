{-# LANGUAGE RankNTypes, GADTs, KindSignatures, ScopedTypeVariables,
             DataKinds, TypeSynonymInstances, FlexibleInstances,
             OverlappingInstances, TypeOperators #-}

module Data.DiscOrder where

import Data.Bits
import Data.Char
import Data.Function

data Order :: * -> * where
   NatO  :: Int -> Order Int
   TrivO :: Order ()
   SumL  :: Order t1 -> Order t2 -> Order (Either t1 t2)
   ProdL :: Order t1 -> Order t2 -> Order (t1, t2)
   MapO  :: (t1 -> t2) -> Order t2 -> Order t1

-- Unit
ordUnit :: Order ()
ordUnit = TrivO

-- Lists
listL :: Order t -> Order [t]
listL = fix (\r v -> MapO fromList (SumL TrivO (ProdL v (r v))))

-- Numbers
ordNat8 :: Order Int
ordNat8 = NatO 255

ordNat16 :: Order Int
ordNat16 = NatO 65535

ordInt32 :: Order Int
ordInt32 = MapO (splitW . (+ (-2147483648))) (ProdL ordNat16 ordNat16)

-- Strings
ordString8 :: Order String
ordString8 = listL ordChar8

ordChar8 :: Order Char
ordChar8 = MapO ord ordNat8

ordChar16 :: Order Char
ordChar16 = MapO ord ordNat16

-----------------
-- Helper code --
-----------------
fromList :: [t] -> Either () (t, [t])
fromList [] = Left ()
fromList (x : xs) = Right (x, xs)

splitW :: Int -> (Int, Int)
splitW x = (shiftR x 16 .&. 65535, x .&. 65535)
