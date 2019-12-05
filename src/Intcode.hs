module Intcode where

import Control.Monad.State

type Address = Int
type Value = Int
type Memory = [Int]
type InstructionPointer = Int

type IntcodeState = State (Memory, InstructionPointer, [Int],[Int])

data Mode = Immediate | Position deriving (Show)
type Param = (Int,Mode)

data Op = Add | Mult | Input | Output | JumpT | JumpF | LessThan | Equals | Halt deriving (Show)

parseOp n = (parseOpCode (n `mod` 100), parseModes (n `div` 100) [])

parseModes n acc | n < 10 = reverse ((parseMode n : acc)) ++ repeat Position
                 | otherwise = parseModes (n `div` 10) (parseMode (n `mod` 10) : acc)

parseMode 0 = Position
parseMode 1 = Immediate

parseOpCode 1 = Add
parseOpCode 2 = Mult
parseOpCode 3 = Input
parseOpCode 4 = Output
parseOpCode 5 = JumpT
parseOpCode 6 = JumpF
parseOpCode 7 = LessThan
parseOpCode 8 = Equals
parseOpCode 99 = Halt
parseOpCode n = error $ "Can't parse op " ++ show n ++ "\n"

getMemory :: Address -> Memory -> Address
getMemory addr mem = mem !! addr

setMemory :: Address -> Value -> Memory -> Memory 
setMemory addr val mem = case splitAt addr mem of
                           (left, _:right) -> left ++ (val : right)

getPtr :: IntcodeState Address
getPtr = gets (\(_,ptr,_,_) -> ptr)

putPtr :: Address -> IntcodeState ()
putPtr ptr = state $ \(prog,_,i,o) -> ((), (prog, ptr, i, o))

mov :: Address -> Value -> IntcodeState ()
mov addr val = state $ \(mem,ptr,i,o) -> ((), (setMemory addr val mem, ptr,i,o))

retrieve :: Address -> IntcodeState Value
retrieve addr = state $ \(mem,ptr,i,o) -> (getMemory addr mem, (mem, ptr,i,o))

indirect :: Address -> IntcodeState Int
indirect addr = do
  newAddr <- retrieve addr
  val <- retrieve newAddr
  return val

getInput :: IntcodeState Int
getInput = state $ \(mem,ptr,i,o) -> (head i, (mem, ptr, tail i, o))

putOutput :: Int -> IntcodeState ()
putOutput n = state $ \(mem,ptr,i,o) -> ((), (mem, ptr, i, n : o))

getArg :: Int -> [Mode] -> IntcodeState Value
getArg n modes = do
  ptr <- getPtr
  case modes !! (n-1) of
    Position -> do
      newAddr <- retrieve (ptr+n)
      retrieve newAddr
    Immediate -> retrieve (ptr+n)

step :: IntcodeState  Bool
step = do
  ptr <- getPtr
  instr <- retrieve ptr
  (uncurry doInstr) (parseOp instr)

doInstr :: Op -> [Mode] -> IntcodeState Bool
doInstr Add modes = do
  ptr <- getPtr
  noun <- getArg 1 modes
  verb <- getArg 2 modes
  dst  <- retrieve (ptr+3)
  mov dst (noun + verb)
  putPtr (ptr+4)
  return True
doInstr Mult modes = do
  ptr  <- getPtr
  noun <- getArg 1 modes
  verb <- getArg 2 modes
  dst  <- retrieve (ptr+3)
  mov dst (noun * verb)
  putPtr (ptr+4)
  return True
doInstr Input modes = do
  ptr <- getPtr
  dst <- retrieve (ptr+1)
  input <- getInput
  mov dst input
  putPtr (ptr+2)
  return True
doInstr Output modes = do
  ptr <- getPtr
  output <- indirect (ptr+1)
  putOutput output
  putPtr (ptr+2)
  return True
doInstr JumpT modes = do
  ptr <- getPtr
  test <- getArg 1 modes
  if test /= 0 then
    do
      newPtr <- getArg 2 modes
      putPtr newPtr
      return True
  else
    do
      putPtr (ptr+3)
      return True
doInstr JumpF modes = do
  ptr <- getPtr
  test <- getArg 1 modes
  if test == 0 then
    do
      newPtr <- getArg 2 modes
      putPtr newPtr
      return True
  else
    do
      putPtr (ptr+3)
      return True
doInstr LessThan modes = do
  ptr <- getPtr
  arg1 <- getArg 1 modes
  arg2 <- getArg 2 modes
  let res = if arg1 < arg2 then 1 else 0 in
    do
      dst <- retrieve (ptr+3)
      mov dst res
      putPtr (ptr+4)
      return True
doInstr Equals modes = do
  ptr <- getPtr
  arg1 <- getArg 1 modes
  arg2 <- getArg 2 modes
  let res = if arg1 == arg2 then 1 else 0 in
    do
      dst <- retrieve (ptr+3)
      mov dst res
      putPtr (ptr+4)
      return True
doInstr Halt _ = do
  return False

run :: Memory -> Int
run mem = evalState run' (mem,0,[],[])

run' :: IntcodeState Int
run' = do
  continue <- step
  if continue then
    run'
  else
    gets $ (\(prog, pc, i, o) -> head prog)

runIO :: Memory -> [Int] -> [Int]
runIO mem input = evalState runIO' (mem,0,input,[])

runIO' :: IntcodeState [Int]
runIO' = do
  continue <- step
  if continue then
    runIO'
  else
    gets $ (\(prog, pc, i, o) -> reverse o)
