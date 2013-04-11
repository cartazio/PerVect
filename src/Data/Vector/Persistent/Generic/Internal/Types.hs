{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE BangPatterns #-}
{-#  LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Vector.Persistent.Generic.Internal.Types where 

import Data.IORef 
import System.IO.Unsafe
import  Unsafe.Coerce

import GHC.Prim (RealWorld)

import qualified  Data.Vector.Generic.Mutable as GM 
import qualified  Data.Vector.Generic as G 


import qualified  Control.Monad.Primitive as Prim 

newtype PerVect (v:: * -> *)  (a :: * ) (tok :: Threadiness) where
    PerVect :: IORef (PVect v a tok ) -> PerVect v a tok  



{-NOTE:  Resulttype  will be one of  (), IORef Bool, or Mvar (),   

Single -> () --- DONT USE IN THREADS
AutoRetry -> Mvar ()  --- use the GHC schedule
ManualRetry -> IORef Bool  --- you decide when retry 


-}


-- | we hide the IOy bits in PVect, mwwahahahaha
--  need to make sure that tok is the SAME everywhere
data PVect  (v::  * -> *)   (a :: * ) (tok :: Threadiness )  where 
    TheArray :: (TokType tok) -> !((G.Mutable v) RealWorld  a) -> PVect  v  a tok 
    ArrDiff :: (TokType tok ) -> !Int -> !a -> !(PerVect v a tok ) -> PVect v a tok 


data Threadiness = Single | AutoRetry | ManualRetry

class (GM.MVector (G.Mutable v ) a ) => PersistentVector (tok :: Threadiness) v a  where 
    type TokType :: Threadiness -> * 
    type ResultType :: Threadiness -> * -> * 
    init :: Int -> (Int -> a ) -> PerVect v a tok 
    get :: PerVect v a  tok -> Int -> ResultType tok a 
    set  :: PerVect v a tok -> Int -> a -> ResultType tok (PerVect v a tok )
    getAndRoot :: PerVect v a tok -> Int -> ResultType tok a 
    setAndRoot :: PerVect v a   tok -> Int -> a -> ResultType tok (PerVect v a tok )
    --- unsafe ones will be O(1) but you CANT ref the PerVect afterwards safely
    ---- either a) you'll have visible mutation in pure code,
    ---- or in the case of mvars, you'll block, or cas you'll always fail
    unsafeExtractM :: Prim.PrimMonad m =>PerVect v a tok  ->ResultType tok  ( m   (G.Mutable v (Prim.PrimState m) a))
    unsafeExtract :: PerVect v a tok  -> ResultType tok (v a) 
    -- the safe ones will have O(1) complexity for CAS, O(n) for single and MVar
    extractM :: Prim.PrimMonad m =>PerVect v a tok  ->   m (ResultType tok     (G.Mutable v (Prim.PrimState m) a))
    extract :: PerVect v a tok  -> ResultType tok (v a) 

{-
extract and extractM should have default instances
likewise not sure if the monad m should be inside or outside the resulttype constructor
-}


--instance (GM.MVector v a ) => PersistentVector Single  v a  where

-- need to sort out what the "right" return type will be.
class TokRegulator (tk :: Threadiness ) where
    --claim :: TokType tk -> IO Bool 
    --release :: TokType tk -> IO Bool

    -- | withToken is a private type class api for handling the token checking logic
    withToken :: PerVect v a tk -> ( PerVect v a tk ->IO  b ) -> IO (ResultType tok b )



{- internally, i'll just have the machinery use IO rather than ST
this unsafe coerce is actually save -}
unsafeMVectorAsIOVector :: ( GM.MVector v a) => (v s) a ->  (v (RealWorld)) a 
unsafeMVectorAsIOVector =  unsafeCoerce


--newPerVect :: GM.MVector v a => v s  a -> PerVect (v RealWorld) a 

{- tok is our auth token, could be (), IORef Bool, or MVar () -}
newPerVect :: GM.MVector (G.Mutable v) a => (G.Mutable v) s a -> TokType tok -> PerVect v  a tok 
newPerVect !v !tk  =  PerVect $! unsafePerformIO  $!  newIORef $! TheArray tk $! unsafeMVectorAsIOVector v

{-
NOTE: Thread safe version would have every constructor point at a shared 
reference 
    could use MVar ()  api  or a IORef Bool using CAS
    MVar would have more sensible blocking / scheduling,
    CAS approach would either succeed or Return a Nothing (retry)  operation,
        and would punt on handling the retry logic
                -}

