module Intcode where

import Control.Monad.State

type Address = Int
type Value = Int
type Memory = [Int]
type InstructionPointer = Int

data Signal = Halted | InputWait Address | OutputWait Value deriving (Show)
type IntcodeState = State (Memory, InstructionPointer)

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
getPtr = gets $ snd

putPtr :: Address -> IntcodeState ()
putPtr ptr = state $ \(prog,_) -> ((), (prog, ptr))

mov :: Address -> Value -> IntcodeState ()
mov addr val = state $ \(mem,ptr) -> ((), (setMemory addr val mem, ptr))

retrieve :: Address -> IntcodeState Value
retrieve addr = gets $ (getMemory addr) . fst

indirect :: Address -> IntcodeState Int
indirect addr = return addr >>= retrieve >>= retrieve

getArg :: Int -> [Mode] -> IntcodeState Value
getArg n modes = do
  ptr <- getPtr
  case modes !! (n-1) of
    Position -> do
      newAddr <- retrieve (ptr+n)
      retrieve newAddr
    Immediate -> retrieve (ptr+n)


doInstr :: Op -> [Mode] -> IntcodeState (Maybe Signal)
doInstr Add modes = do
  ptr <- getPtr
  noun <- getArg 1 modes
  verb <- getArg 2 modes
  dst  <- retrieve (ptr+3)
  mov dst (noun + verb)
  putPtr (ptr+4)
  return Nothing
doInstr Mult modes = do
  ptr  <- getPtr
  noun <- getArg 1 modes
  verb <- getArg 2 modes
  dst  <- retrieve (ptr+3)
  mov dst (noun * verb)
  putPtr (ptr+4)
  return Nothing
doInstr Input modes = do
  ptr <- getPtr
  dst <- retrieve (ptr+1)
  putPtr (ptr+2)
  return $ Just (InputWait dst)
doInstr Output modes = do
  ptr <- getPtr
  output <- indirect (ptr+1)
  putPtr (ptr+2)
  return $ Just (OutputWait output)
doInstr JumpT modes = do
  ptr <- getPtr
  test <- getArg 1 modes
  if test /= 0 then
      (getArg 2 modes) >>= putPtr >> return Nothing
  else
      putPtr (ptr+3) >> return Nothing
doInstr JumpF modes = do
  ptr <- getPtr
  test <- getArg 1 modes
  if test == 0 then
    (getArg 2 modes) >>= putPtr >> return Nothing
  else
    putPtr (ptr+3) >> return Nothing
doInstr LessThan modes = do
  ptr <- getPtr
  arg1 <- getArg 1 modes
  arg2 <- getArg 2 modes
  let res = (if arg1 < arg2 then 1 else 0) in
    (retrieve (ptr+3)) >>= ((flip mov) res) >> (putPtr (ptr+4)) >> return Nothing
doInstr Equals modes = do
  ptr <- getPtr
  arg1 <- getArg 1 modes
  arg2 <- getArg 2 modes
  let res = (if arg1 == arg2 then 1 else 0) in
    (retrieve (ptr+3)) >>= ((flip mov) res) >> (putPtr (ptr+4)) >> return Nothing
doInstr Halt _ =
  return $ Just Halted


step :: IntcodeState (Maybe Signal)
step = do
  ptr <- getPtr
  instr <- retrieve ptr
  (uncurry doInstr) (parseOp instr)

stepUntilSignal :: IntcodeState Signal
stepUntilSignal = do
  sig <- step
  case sig of
    Just s -> return s
    Nothing -> stepUntilSignal

takeOutput :: Signal -> IntcodeState Value
takeOutput (OutputWait output) = return output

stepUntilInput :: IntcodeState (Signal,[Value])
stepUntilInput = stepUntilInput' []
  where stepUntilInput' acc = do
          sig <- stepUntilSignal
          case sig of
            InputWait addr -> return (InputWait addr, reverse acc)
            OutputWait out -> stepUntilInput' (out:acc)
            Halted -> return (Halted, reverse acc)

putInput :: Value -> (Signal,[Value]) -> IntcodeState [Value]
putInput value (InputWait addr,outs) = (mov addr value) >> (return outs)

stepWithInputs :: [Value] -> (Signal,[Value]) -> IntcodeState (Signal,[Value])
stepWithInputs [] (signal,outs) = return (signal,outs)
stepWithInputs (input:inputs) res = do
  outs <- putInput input res
  (sig, outs2) <- stepUntilInput
  stepWithInputs inputs (sig, outs ++ outs2)
  
-- run :: [Int] -> ( (Signal, [Value]), (Memory, InstructionPointer) )
-- run prog = runState stepUntilInput (prog,0)

-- eval :: [Int] -> (Signal, [Value])
-- eval prog = evalState stepUntilInput (prog,0)

-- runWithInput :: Value -> [Int] -> ( (Signal, [Value]), (Memory, InstructionPointer) )
-- runWithInput input prog = runState (liftM2 (\x y -> (fst y, (snd x) ++ (snd y))) (stepWithInput input) (stepUntilInput)) (prog, 0)

-- runWithInputSt :: Value -> (Memory, InstructionPointer) -> ( (Signal, [Value]), (Memory, InstructionPointer) )
-- runWithInputSt input st = runState (liftM2 (\x y -> (fst y, (snd x) ++ (snd y))) (stepWithInput input) (stepUntilInput)) st

-- evalWithInput :: Value -> [Int] -> (Signal, [Value])
-- evalWithInput input prog = fst $ runWithInput input prog

-- runWithInputs :: [Value] -> [Int] -> ( (Signal, [Value]), (Memory, InstructionPointer) )
-- runWithInputs inputs prog = runState (liftM2 (\x y -> (fst y, (snd x) ++ (snd y))) (stepWithInputs inputs) (stepUntilInput)) (prog, 0)

-- evalWithInputs :: [Value] -> [Int] -> (Signal, [Value])
-- evalWithInputs inputs prog = fst $ runWithInputs inputs prog
