{- | Growable Mutable Vector. -}
module Data.Vector.Growable(
    GrowVector(),
    new, grow, length,
    read, write, modify,
    unsafeRead, unsafeWrite, unsafeModify,
    unsafeToMVector
) where

import Prelude hiding (read, length)

import Data.STRef 
import Control.Monad.Primitive
import qualified Data.Vector.Mutable as MV

data GrowVector' s a =
    GV { gvecBase :: MV.MVector s a
       , gvecLength :: Int }

newtype GrowVector s a = GVR (STRef s (GrowVector' s a))

new :: (PrimMonad m) => Int -> m (GrowVector (PrimState m) a)
new initialSize = stToPrim $
    do v <- MV.new initialSize
       GVR <$> newSTRef (GV v initialSize)

grow :: (PrimMonad m) => Int -> GrowVector (PrimState m) a -> m ()
grow n (GVR ref) = stToPrim $
    do GV vec len <- readSTRef ref
       let len' = len + n
           cap = MV.length vec
           delta = maximum [1, len' - cap, cap]
       vec' <-
         if len' <= cap
           then return vec
           else MV.unsafeGrow vec delta
       writeSTRef ref $ GV vec' len'

length :: (PrimMonad m) => GrowVector (PrimState m) a -> m Int
length (GVR ref) = stToPrim $ gvecLength <$> readSTRef ref

unsafeRead :: (PrimMonad m) => GrowVector (PrimState m) a -> Int -> m a
unsafeRead (GVR ref) i = stToPrim $
    do vec <- gvecBase <$> readSTRef ref
       MV.unsafeRead vec i

unsafeWrite :: (PrimMonad m) => GrowVector (PrimState m) a -> Int -> a -> m ()
unsafeWrite (GVR ref) i a = stToPrim $
    do vec <- gvecBase <$> readSTRef ref
       MV.unsafeWrite vec i a

unsafeModify :: (PrimMonad m) => GrowVector (PrimState m) a ->
    (a -> a) -> Int -> m ()
unsafeModify (GVR ref) f i = stToPrim $
    do vec <- gvecBase <$> readSTRef ref
       MV.unsafeModify vec f i

read :: (PrimMonad m) => GrowVector (PrimState m) a -> Int -> m a
read (GVR ref) i = stToPrim $
  do GV vec len <- readSTRef ref
     if i >= 0 && i < len
       then MV.unsafeRead vec i
       else error $ "GrowVector.read " ++ show (len, i)

write :: (PrimMonad m) => GrowVector (PrimState m) a -> Int -> a -> m ()
write (GVR ref) i a = stToPrim $
  do GV vec len <- readSTRef ref
     if i >= 0 && i < len
       then MV.unsafeWrite vec i a
       else error $ "GrowVector.write " ++ show (len, i)

modify :: (PrimMonad m) =>
   GrowVector (PrimState m) a -> (a -> a) -> Int -> m ()
modify (GVR ref) f i = stToPrim $
    do GV vec len <- readSTRef ref
       if i >= 0 && i < len
         then MV.unsafeModify vec f i
         else error $ "GrowVector.modify " ++ show (len, i)

unsafeToMVector :: (PrimMonad m) =>
  GrowVector (PrimState m) a -> m (MV.MVector (PrimState m) a)
unsafeToMVector (GVR ref) = stToPrim $
    do GV vec len <- readSTRef ref
       return $ MV.slice 0 len vec