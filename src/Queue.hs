{-# LANGUAGE DeriveFunctor #-}
module Queue(
    Queue(),
    getLength,
    offer,
    poll,
    peek,

    runQueue,
    traceQueue
) where

import Data.Foldable
import Control.Monad(ap)

newtype Queue c a =
    Queue { unQueue :: Int -> [c] -> [c] -> (a, Int, [c], [c]) }
    deriving (Functor)

instance Applicative (Queue c) where
  pure = return
  (<*>) = ap

instance Monad (Queue c) where
  return a = Queue $ \n is os -> (a, n, is, os)
  qa >>= k = Queue $ \n is os ->
    let (a, n',  is',  os'') = unQueue qa n is os'
        (b, n'', is'', os') = unQueue (k a) n' is' os
    in (b, n'', is'', os'')

-- | Get the current length of the queue.
getLength :: Queue c Int
getLength = Queue $ \n is os -> (n, n, is, os)

-- | Add items to the right side of the queue.
offer :: (Foldable f) => f c -> Queue c ()
offer cs = Queue $ \n is os ->
    let !n' = n + length cs
    in ((), n', is, toList cs ++ os)

-- | @poll m@ removes @m@ items from left of the queue.
--   then return removed items in order.
--   If the length of the queue is smaller than @m@, it removes
--   all remaining items.
poll :: Int -> Queue c [c]
poll m = Queue $ \n is os ->
    let d = min m n
        !n' = n - d
        (removedElems, is') = splitAt d is
    in (removedElems, n', is', os)

-- | @peek m@ gets @m@ items from left of the queue,
--   without removing them.
--   If the length of the queue is smaller than @m@, it returns
--   all remaining items.
peek :: Int -> Queue c [c]
peek m = Queue $ \n is os ->
    let d = min m n
    in (take d is, n, is, os)

-- | Run @Queue@ with initially empty queue. Return the computation
--   result and final state of the queue.
runQueue :: Queue c a -> (a, [c])
runQueue qa =
    let (a, _, is', os') = unQueue qa 0 os' []
    in (a, is')

-- | Run @Queue@ with initially empty queue. Return the computation
--   result and all items added to queue, in chronological order,
--   regardless of whether it exists in final queue.
traceQueue :: Queue c a -> (a, [c])
traceQueue qa =
    let (a, _, _, os') = unQueue qa 0 os' []
    in (a, os')
