module Intcode where

data State = State { mem :: [Int], ptr :: Int, base :: Int }

data Mode = Immediate | Position | Relative deriving (Show)

data Op = Add | Mult | Input | Output | JumpT | JumpF | LessThan | Equals | Base | Halt deriving (Show)

parseOp n = (parseOpCode (n `mod` 100), parseModes (n `div` 100) [])

parseModes n acc | n < 10 = reverse ((parseMode n : acc)) ++ repeat Position
                 | otherwise = parseModes (n `div` 10) (parseMode (n `mod` 10) : acc)

parseMode 0 = Position
parseMode 1 = Immediate
parseMode 2 = Relative

parseOpCode 1 = Add
parseOpCode 2 = Mult
parseOpCode 3 = Input
parseOpCode 4 = Output
parseOpCode 5 = JumpT
parseOpCode 6 = JumpF
parseOpCode 7 = LessThan
parseOpCode 8 = Equals
parseOpCode 9 = Base
parseOpCode 99 = Halt
parseOpCode n = error $ "Can't parse op " ++ show n ++ "\n"

getMem :: Int -> [Int] -> Int
getMem addr mem | addr >= 0 = mem !! addr
                | otherwise = error ("getArg too small: " ++ show addr)

setMem :: Int -> Int -> [Int] -> [Int]
setMem addr val mem = case splitAt addr mem of
                        (left, _:right) -> left ++ (val : right)


runMem :: ([Int],Int) -> ([Int],Int)
runMem (mem,ptr) =
  let (op,modes) = parseOp (getMem ptr mem)
      load addr = getMem addr mem
      getArg n = case modes !! (n-1) of
        Position -> load $ load (ptr + n)
        Immediate -> load (ptr+n)
      getDstArg n = load (ptr+n)
  in
    case op of
      Add ->
        let (arg1, arg2, dst) = (getArg 1, getArg 2, getDstArg 3)
        in runMem (setMem dst (arg1 + arg2) mem, ptr+4)
      Mult ->
        let (arg1, arg2, dst) = (getArg 1, getArg 2, getDstArg 3)
        in runMem (setMem dst (arg1 * arg2) mem, ptr+4)
      Halt ->
        (mem, ptr)

run :: [Int] -> ([Int],Int,Int) -> [Int]
run inputs (mem,ptr,base) =
  let (op,modes) = parseOp (getMem ptr mem)
      load addr = getMem addr mem
      getArg n = case modes !! (n-1) of
                   Position -> load $ load (ptr + n)
                   Immediate -> load (ptr+n)
                   Relative -> load $ base + (load (ptr+n))
      getDstArg n = case modes !! (n-1) of
        Relative -> base + (load (ptr+n))
        _ -> load (ptr+n)
  in
    case op of
      Add ->
        let (arg1, arg2, dst) = (getArg 1, getArg 2, getDstArg 3)
        in run inputs (setMem dst (arg1 + arg2) mem, ptr+4, base)
      Mult ->
        let (arg1, arg2, dst) = (getArg 1, getArg 2, getDstArg 3)
        in run inputs (setMem dst (arg1 * arg2) mem, ptr+4, base)
      Input ->
        let dst = getDstArg 1
        in run (tail inputs) (setMem dst (head inputs) mem, ptr+2, base)
      Output ->
        let output = getArg 1
        in output : (run inputs (mem, ptr+2, base))
      JumpT ->
        let (arg1, dst) = (getArg 1, getArg 2)
            ptr' = if arg1 /= 0 then dst else ptr+3
        in run inputs (mem, ptr', base)
      JumpF ->
        let (arg1, dst) = (getArg 1, getArg 2)
            ptr' = if arg1 == 0 then dst else ptr+3
        in run inputs (mem, ptr', base)
      LessThan ->
        let (arg1, arg2, dst) = (getArg 1, getArg 2, getDstArg 3)
            test = if arg1 < arg2 then 1 else 0
        in run inputs (setMem dst test mem, ptr+4, base)
      Equals ->
        let (arg1, arg2, dst) = (getArg 1, getArg 2, getDstArg 3)
            test = if arg1 == arg2 then 1 else 0
        in run inputs (setMem dst test mem, ptr+4, base)
      Base ->
        let arg1 = getArg 1
        in run inputs (mem, ptr+2, base + arg1)
      Halt ->
        []
