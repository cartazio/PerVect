{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Trustworthy #-}


module Data.Vector.Persistent.Generic.Internal.Types where 

import Data.IORef 
import System.IO.Unsafe
import  Unsafe.Coerce

import GHC.Prim (RealWorld)

import qualified  Data.Vector.Generic.Mutable as GM 
import qualified  Data.Vector.Generic as G 


import qualified  Control.Monad.Primitive as Prim 

newtype PerVect (v:: * -> * -> *)  (a :: * )  where
    PerVect :: IORef (PVect v a) -> PerVect v a 


-- | we hide the IOy bits in PVect, mwwahahahaha
data PVect  (v::  * -> * -> *)  (a :: * )  where 
    TheArray ::(v RealWorld  a) -> PVect  v  a 
    ArrDiff :: Int -> a -> PerVect v a -> PVect v a


{- internally, i'll just have the machinery use IO rather than ST
this unsafe coerce is actually save -}
unsafeMVectorAsIOVector :: ( GM.MVector v a) => (v s) a ->  (v (RealWorld)) a 
unsafeMVectorAsIOVector =  unsafeCoerce


--newPerVect :: GM.MVector v a => v s  a -> PerVect (v RealWorld) a 

newPerVect :: GM.MVector v a => v s a -> PerVect v  a
newPerVect v =  PerVect $! unsafePerformIO  $!  newIORef $! TheArray $! unsafeMVectorAsIOVector v

