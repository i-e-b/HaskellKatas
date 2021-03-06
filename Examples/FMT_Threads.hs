{-# LANGUAGE DeriveFunctor #-}

-- Free Monad Transformer thread model from
--   http://www.haskellforall.com/2013/06/from-zero-to-cooperative-threads-in-33.html

import Control.Monad.Trans.Free  -- from the `free` package
import Control.Monad.Trans
import Control.Monad
import Data.Sequence -- Queue with O(1) head and tail operations

-- Using: --
main = roundRobin mainThread :: IO ()

mainThread :: Thread IO ()
mainThread = do
    lift $ putStrLn "Forking thread #1"
    fork thread1
    lift $ putStrLn "Forking thread #1"
    fork thread2

thread1 :: Thread IO ()
thread1 = forM_ [1..10] $ \i -> do
    lift $ print i
    yield

thread2 :: Thread IO ()
thread2 = replicateM_ 3 $ do
    lift $ putStrLn "Hello"
    yield


-- Implementation: --

type Thread = FreeT ThreadF
data ThreadF next = Fork  next next
                  | Yield next
                  | Done
                  deriving (Functor)
--
yield :: (Monad m) => Thread m ()
yield = liftF (Yield ())

done :: (Monad m) => Thread m r
done = liftF Done

cFork :: (Monad m) => Thread m Bool
cFork = liftF (Fork False True)

fork :: (Monad m) => Thread m a -> Thread m ()
fork thread = do
    child <- cFork
    when child $ do
        thread
        done
--

roundRobin :: (Monad m) => Thread m a -> m ()
roundRobin t = go (singleton t)  -- Begin with a single thread
  where
    go ts = case (viewl ts) of
        -- The queue is empty: we're done!
        EmptyL   -> return ()

        -- The queue is non-empty: Process the first thread
        t :< ts' -> do
            x <- runFreeT t  -- Run this thread's effects
            case x of
                -- New threads go to the back of the queue
                Free (Fork t1 t2) -> go (t1 <| (ts' |> t2))

                -- Yielding threads go to the back of the queue
                Free (Yield   t') -> go (ts' |> t')

                -- Thread done: Remove the thread from the queue
                Free  Done        -> go ts'
                Pure  _           -> go ts'
--
{-data FreeF f a x = Pure a | Free (f x)

newtype FreeT f m a =
    FreeT { runFreeT :: m (FreeF f a (FreeT f m a)) }

instance (Functor f, Monad m) => Monad (FreeT f m) where
    return a = FreeT (return (Pure a))
    FreeT m >>= f = FreeT $ m >>= \v -> case v of
        Pure a -> runFreeT (f a)
        Free w -> return (Free (fmap (>>= f) w))

instance MonadTrans (FreeT f) where
    lift = FreeT . liftM Pure

liftF :: (Functor f, Monad m) => f r -> FreeT f m r
liftF x = FreeT (return (Free (fmap return x)))
-}

