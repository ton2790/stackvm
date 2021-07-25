{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards #-}

module VM ( runBytecode ) where

import Opcodes

import System.Exit
import Control.Monad
import Control.Monad.State.Class
import Control.Monad.Reader
import Data.Array.IO
import Data.IORef

data VMState = VMState
  { vmLocals :: IOUArray Int Int
  , vmCode   :: IOUArray Int Int
  , vmStack  :: IOUArray Int Int
  , vmPC     :: IORef Int
  , vmSP     :: IORef Int
  , vmFP     :: IORef Int }

newtype VM a = VM { runVM :: ReaderT VMState IO a }
  deriving (Functor, Applicative, Monad, MonadReader VMState, MonadIO)

newVMState :: IO VMState
newVMState = do
  vmLocals <- newArray (0,65535) 0
  vmCode   <- newArray (0,65535) 0
  vmStack  <- newArray (0,1023) 0
  vmPC     <- newIORef 0
  vmSP     <- newIORef (-1)
  vmFP     <- newIORef 0
  return VMState{..}

pop :: VM Int
pop = do
  VMState{..} <- ask
  liftIO $ do
    sp <- readIORef vmSP
    x <- readArray vmStack sp
    writeIORef vmSP (sp - 1)
    return x

push :: Int -> VM ()
push n = do
  VMState{..} <- ask
  liftIO $ do
    sp <- readIORef vmSP
    writeIORef vmSP (sp + 1)
    writeArray vmStack (sp + 1) n

advancePC :: VM Int
advancePC = do
  VMState{..} <- ask
  liftIO $ do
    pc <- readIORef vmPC
    op <- readArray vmCode pc
    writeIORef vmPC (pc + 1)
    return op

readLocal :: Int -> VM Int
readLocal offset = do
  VMState{..} <- ask
  liftIO $ do
    fp <- readIORef vmFP
    readArray vmStack (fp + offset)

writeLocal :: Int -> Int -> VM ()
writeLocal offset val = do
  VMState{..} <- ask
  liftIO $ do
    fp <- readIORef vmFP
    writeArray vmStack (fp + offset) val

run :: VM ()
run = do
  op <- advancePC
  exOp (toEnum op)
  run

exOp :: Op -> VM ()
exOp o = do
  VMState{..} <- ask
  case o of
    Nop -> pure ()
    Add -> do
      x <- pop
      y <- pop
      push (y + x)
    Sub -> do
      x <- pop
      y <- pop
      push (y - x)
    Mul -> do
      x <- pop
      y <- pop
      push (y * x)
    Lt  -> do
      x <- pop
      y <- pop
      push (boolToInt (y < x))
    Eq  -> do
      x <- pop
      y <- pop
      push (boolToInt (y == x))
    Jmp -> do
      addr <- advancePC
      liftIO $ writeIORef vmPC addr
    Jnz -> do
      x <- pop
      addr <- advancePC
      unless (x == 0) $ do
        liftIO $ writeIORef vmPC addr
    Jz -> do
      x <- pop
      addr <- advancePC
      when (x == 0) $ do
        liftIO $ writeIORef vmPC addr
    Lit -> do
      x <- advancePC
      push x
    Load -> do
      offset <- advancePC
      x <- readLocal offset
      push x
    Store -> do
      x <- pop
      offset <- advancePC
      writeLocal offset x
    GLoad -> pure ()
    GStore -> pure ()
    Print -> do
      x <- pop
      liftIO $ print x
    Pop -> do
      void pop
    Halt -> do
      liftIO $ exitSuccess
    Call -> do
      addr <- advancePC
      argc <- advancePC
      push argc
      liftIO (readIORef vmFP) >>= push
      liftIO (readIORef vmPC) >>= push
      liftIO $ readIORef vmSP >>= writeIORef vmFP
      liftIO $ writeIORef vmPC addr
    Ret -> do
      ret <- pop
      liftIO $ readIORef vmFP >>= writeIORef vmSP
      pop >>= \x -> liftIO (writeIORef vmPC x)
      pop >>= \x -> liftIO (writeIORef vmFP x)
      argc <- pop
      liftIO $ modifyIORef vmSP (subtract argc)
      push ret
      
boolToInt :: Bool -> Int
boolToInt False = 0
boolToInt True  = -1

runBytecode :: [Int] -> IO ()
runBytecode ops = do
  s@VMState{..} <- newVMState
  mapM_ (uncurry (writeArray vmCode)) (zip [0..] ops)
  runReaderT (runVM run) s
