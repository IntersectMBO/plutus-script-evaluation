module Data.IORef.Strict (
  StrictIORef,
  newIORef,
  readIORef,
  writeIORef,
  atomicModifyIORef,
) where

import Data.IORef qualified as Lazy
import GHC.Stack (HasCallStack)
import NoThunks.Class (NoThunks, unsafeNoThunks)

newtype StrictIORef a = StrictIORef (Lazy.IORef a)

newIORef :: (NoThunks a, HasCallStack) => a -> IO (StrictIORef a)
newIORef a = do
  checkThunksIO a
  StrictIORef <$> Lazy.newIORef a

readIORef :: StrictIORef a -> IO a
readIORef (StrictIORef ref) = Lazy.readIORef ref

writeIORef :: (NoThunks a, HasCallStack) => StrictIORef a -> a -> IO ()
writeIORef (StrictIORef ref) !x = do
  checkThunksIO x
  Lazy.writeIORef ref x

atomicModifyIORef :: StrictIORef a -> (a -> (a, b)) -> IO b
atomicModifyIORef (StrictIORef ref) = Lazy.atomicModifyIORef' ref

-- | Check for thunks and throw an error if any are found
checkThunksIO :: (NoThunks a, HasCallStack) => a -> IO ()
checkThunksIO x =
  case unsafeNoThunks x of
    Nothing -> pure ()
    Just thunkInfo ->
      error $ "Thunk detected: " <> show thunkInfo
