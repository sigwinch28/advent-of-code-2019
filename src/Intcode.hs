module Intcode where

import Control.Monad.State

type Address = Int
type Value = Int
type Memory = [Int]
type InstructionPointer = Int
type Op = (Int,Int,Int,Int)

type IntcodeState = State (Memory, InstructionPointer)

getMemory :: Address -> Memory -> Address
getMemory addr mem = mem !! addr

setMemory :: Address -> Value -> Memory -> Memory 
setMemory addr val mem = case splitAt addr mem of
                           (left, []) -> left ++ [val]
                           (left, _:right) -> left ++ (val : right)

getPtr :: IntcodeState Address
getPtr = gets snd

putPtr :: Address -> IntcodeState ()
putPtr ptr = state $ \(prog,_) -> ((), (prog, ptr))

mov :: Address -> Value -> IntcodeState ()
mov addr val = state $ \(mem,ptr) -> ((), (setMemory addr val mem, ptr))

retrieve :: Address -> IntcodeState Value
retrieve addr = state $ \(mem,ptr) -> (getMemory addr mem, (mem, ptr))

indirect :: Address -> IntcodeState Int
indirect addr = do
  newAddr <- retrieve addr
  val <- retrieve newAddr
  return val

step :: IntcodeState Bool
step = do
  ptr <- getPtr
  instr <- retrieve ptr
  case instr of
    99 -> return False
    n  -> do
      doInstr n
      putPtr (ptr+4)
      return True

doInstr :: Value -> IntcodeState ()
doInstr 1 = do
  ptr  <- getPtr
  noun <- indirect (ptr+1)
  verb <- indirect (ptr+2)
  dst  <- retrieve (ptr+3)
  mov dst (noun + verb)
doInstr 2 = do
  ptr  <- getPtr
  noun <- indirect (ptr+1)
  verb <- indirect (ptr+2)
  dst  <- retrieve (ptr+3)
  mov dst (noun * verb)

run :: Memory -> Int
run mem = evalState run' (mem,0)

run' :: IntcodeState Int
run' = do
  continue <- step
  if continue then
    run'
  else
    gets $ (\(prog, pc) -> head prog)
