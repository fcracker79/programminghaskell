module AdventOfCode.Y2020.AOC8(
    parseInstructions, 
    parseInstruction,
    accumulatorValue
) where

-- Without this, MaybeT does not implement MonadReader
import Control.Monad.Reader(asks, runReader)
import Control.Monad.State.Class(get, put, state)
import qualified Misc.MyParser as P
import qualified Control.Monad.Trans.Maybe as M
import Control.Applicative((<|>), many)
import qualified Data.Set as Set
import Data.Maybe(fromMaybe)
import Control.Monad.Trans.State.Lazy(evalStateT, StateT)
import Control.Monad(guard)
import Control.Monad.Trans.Reader (Reader)

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
        value :: Int,
        step :: Int
    }

newtype Program = Program { instructions :: [InstructionLine] }

executeInstruction :: M.MaybeT (StateT ExecutionState (Reader Program)) Int
executeInstruction = do
    program <- asks instructions
    state <- get
    let currentValue = value state
    let currentStep =  step state
    guard $ not $ Set.member currentStep $ executedSteps state
    let InstructionLine instruction argument = program !! currentStep
    let nextStep = currentStep + case instruction of
                                    JMP -> argument
                                    _ -> 1
    let nextValue = currentValue + case instruction of
                                    ACC -> argument
                                    _ -> 0
    put ExecutionState {
        executedSteps = Set.insert currentStep $ executedSteps state,
        value = nextValue,
        step = nextStep
    }
    return nextValue

executeProgram :: M.MaybeT (StateT ExecutionState (Reader Program)) Int
executeProgram = do
    many executeInstruction
    value <$> get


-- runReader $ (runStateT (M.runMaybeT executeProgram) initialState) program
accumulatorValue :: [InstructionLine] -> Int
accumulatorValue instructions = fromMaybe 0 $ runReader (evalStateT (M.runMaybeT executeProgram) initialState) program
        where initialState = ExecutionState {
                executedSteps = Set.empty,
                value = 0,
                step = 1
              }
              program = Program { instructions = instructions }