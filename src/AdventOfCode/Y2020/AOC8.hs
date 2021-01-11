module AdventOfCode.Y2020.AOC8(
    parseInstructions, 
    parseInstruction,
    accumulatorValue
) where

import qualified Misc.MyParser as P
import qualified Control.Monad.Trans.Maybe as M
import Control.Applicative((<|>), many)
import qualified Data.Set as Set
import Data.Maybe(fromMaybe)
import Control.Monad.Trans.State.Lazy(evalState)
import Control.Monad.State.Lazy (get, put, state, State)
import Control.Monad(guard)

data Instruction = JMP | NOP | ACC deriving(Show, Eq)
data InstructionLine = InstructionLine { instruction :: Instruction, argument :: Int } deriving(Show, Eq)

parseInstruction :: P.MyParser Instruction
parseInstruction = 
    do
        P.symbol "jmp"
        return JMP
    <|>
    do
        P.symbol "nop"
        return NOP
    <|>
    do
        P.symbol "acc"
        return ACC


parseInstructionLine :: P.MyParser InstructionLine
parseInstructionLine = do
    instruction <- parseInstruction
    sign <- P.char '+' <|> P.char '-'
    argument <- fmap (\x -> if sign == '-' then -x else x) P.nat
    return InstructionLine { instruction = instruction, argument = argument }

parseInstructions :: P.MyParser [InstructionLine]
parseInstructions = do
    do 
        i <- parseInstructionLine
        P.char '\n'
        is <- parseInstructions
        return (i:is)
    <|>
        (:[]) <$> parseInstructionLine

data ExecutionState = ExecutionState { 
        executedSteps :: Set.Set Int, 
        program :: [InstructionLine], 
        value :: Int,
        step :: Int
    }

executeInstruction :: M.MaybeT (State ExecutionState) Int
executeInstruction = do
    state <- get
    let currentValue = value state
    let currentStep =  step state
    guard $ not $ Set.member currentStep $ executedSteps state
    let InstructionLine instruction argument = program state !! currentStep
    let nextStep = currentStep + case instruction of
                                    JMP -> argument
                                    _ -> 1
    let nextValue = currentValue + case instruction of
                                    ACC -> argument
                                    _ -> 0
    put ExecutionState {
        executedSteps = Set.insert currentStep $ executedSteps state,
        program = program state,
        value = nextValue,
        step = nextStep
    }
    return nextValue

executeProgram :: M.MaybeT (State ExecutionState) Int
executeProgram = do
    many executeInstruction
    value <$> get


accumulatorValue :: [InstructionLine] -> Int
accumulatorValue program = fromMaybe 0 $ evalState (M.runMaybeT executeProgram) initialState
        where initialState = ExecutionState {
                executedSteps = Set.empty,
                program = program,
                value = 0,
                step = 1
            }