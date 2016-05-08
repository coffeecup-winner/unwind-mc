{-# LANGUAGE TupleSections #-}
module Unwind.Analysis ( analyze
                       , findFunctions
                       ) where

import Control.Monad.State.Strict (get, modify)
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Query.DFS (reachable, xdfsWith)
import Data.List (nub)
import Data.Word (Word64)
import Hdis86 hiding (Instruction)
import qualified Hdis86 as X86

import Unwind.Types

analyze :: Unwind ()
analyze = do
    gr <- get
    let nodes = labNodes gr
        targets = (getControlFlowTargets . snd) <$> nodes
        offsets = fst <$> nodes
        offsetsAndTargets = concat $ zipWith3 (\a b vs -> (a, b, ) <$> vs) offsets (tail offsets) targets
    modify $ insEdges [(node, next, Next) | (node, next, (NextTarget, Next)) <- offsetsAndTargets]
    modify $ insEdges [(node, next + fromIntegral o, tr) | (node, next, (RelativeTarget o, tr)) <- offsetsAndTargets]
    gr <- get
    let registerTargets = concat [(node, , tr) <$> getPossibleRegisterValues gr node r | (node, _, (RegisterTarget r, tr)) <- offsetsAndTargets]
    modify $ insEdges [(node, fromIntegral next, tr) | (node, Constant next, tr) <- registerTargets]
    modify $ insNodes $ map (\n -> (fromIntegral n, fakeInstruction n "ext")) . nub $ [node | (_, Relative node, _) <- registerTargets]
    modify $ insEdges [(node, fromIntegral next, tr) | (node, Relative next, tr) <- registerTargets]
    -- memory targets require more analysis

findFunctions :: InstructionGraph -> FunctionGraph
findFunctions gr = newGraph (map (\f -> (fromIntegral $ start f, f)) functions) dependencies
    where
        dependencies = concatMap (\f -> map (fromIntegral $ start f, , ()) . getTargets . nodes $ body f) functions
        functions = map (\t -> Function (fromIntegral t) (subgraph (reachable t graphWithoutCalls) gr)) $ getTargets callSites
        getTargets = nub . map (\(_, t, _) -> t) . filter isCall . concatMap (out gr)
        callSites = [addr | (addr, Instruction _ _ _ (Inst _ Icall _)) <- labNodes gr]
        graphWithoutCalls = newGraph (labNodes gr) (filter (not . isCall) (labEdges gr))
        isCall (_, _, Call) = True
        isCall (_, _, _) = False

data PossibleValue = Constant Word64
                   | Relative Word64
                   deriving (Show)

getPossibleRegisterValues :: InstructionGraph -> Int -> Register -> [PossibleValue]
getPossibleRegisterValues gr node reg = concat $ xdfsWith traceback getAssignments [node] gr
    where traceback (ein, n, i, _) = if null $ getRegisterAssignments gr n i reg then map snd ein else []
          getAssignments (_, n, i, _) = getRegisterAssignments gr n i reg

getRegisterAssignments :: InstructionGraph -> Int -> Instruction -> Register -> [PossibleValue]
getRegisterAssignments gr node (Instruction _ _ _ instr) reg = go instr
    where
        go (Inst _ Imov [Mem _, _]) = []
        go (Inst _ Imov [Reg r, _]) | r /= reg = []
        go (Inst _ Imov [Reg r, Mem (Memory Bits32 RegNone RegNone 0 (Immediate Bits32 m))]) = [Relative $ fromIntegral m]
        go (Inst _ Imov [Reg r, Mem (Memory Bits32 b RegNone 0 (Immediate Bits0 0))]) =
            if null [v | Relative v <- baseAssignments]
            then [Relative v | Constant v <- baseAssignments]
            else (error "Cannot check 2nd relative assignment")
            where baseAssignments = getPossibleRegisterValues gr node b
        go (Inst _ Imov [Reg r, Imm (Immediate Bits32 m)]) = [Constant $ fromIntegral m]
        go (Inst _ Imov o) = error $ show o
        -- TODO: prohibit modifications of reg
        go _ = []

data ControlFlowTarget = NextTarget
                       | RelativeTarget Word64
                       | MemoryTarget Word64
                       | RegisterTarget Register
                       deriving (Show)

getControlFlowTargets :: Instruction -> [(ControlFlowTarget, Transition)]
getControlFlowTargets (Instruction _ _ _ instr) = go instr
    where
        go (Inst _ Ija    o) = [(NextTarget, Next), (getJumpTarget o, Branch)]
        go (Inst _ Ijae   o) = [(NextTarget, Next), (getJumpTarget o, Branch)]
        go (Inst _ Ijb    o) = [(NextTarget, Next), (getJumpTarget o, Branch)]
        go (Inst _ Ijbe   o) = [(NextTarget, Next), (getJumpTarget o, Branch)]
        go (Inst _ Ijg    o) = [(NextTarget, Next), (getJumpTarget o, Branch)]
        go (Inst _ Ijge   o) = [(NextTarget, Next), (getJumpTarget o, Branch)]
        go (Inst _ Ijl    o) = [(NextTarget, Next), (getJumpTarget o, Branch)]
        go (Inst _ Ijle   o) = [(NextTarget, Next), (getJumpTarget o, Branch)]
        go (Inst _ Ijp    o) = [(NextTarget, Next), (getJumpTarget o, Branch)]
        go (Inst _ Ijnp   o) = [(NextTarget, Next), (getJumpTarget o, Branch)]
        go (Inst _ Ijs    o) = [(NextTarget, Next), (getJumpTarget o, Branch)]
        go (Inst _ Ijns   o) = [(NextTarget, Next), (getJumpTarget o, Branch)]
        go (Inst _ Ijo    o) = [(NextTarget, Next), (getJumpTarget o, Branch)]
        go (Inst _ Ijno   o) = [(NextTarget, Next), (getJumpTarget o, Branch)]
        go (Inst _ Ijz    o) = [(NextTarget, Next), (getJumpTarget o, Branch)]
        go (Inst _ Ijnz   o) = [(NextTarget, Next), (getJumpTarget o, Branch)]
        go (Inst _ Ijcxz  o) = [(NextTarget, Next), (getJumpTarget o, Branch)]
        go (Inst _ Ijecxz o) = [(NextTarget, Next), (getJumpTarget o, Branch)]
        go (Inst _ Ijrcxz o) = [(NextTarget, Next), (getJumpTarget o, Branch)]
        go (Inst _ Ijmp   o) = [(getJumpTarget o, Branch)]
        go (Inst _ Icall  o) = [(NextTarget, Next), (getJumpTarget o, Call)]
        go (Inst _ Iret   _) = []
        go _                 = [(NextTarget, Next)]

getJumpTarget :: [Operand] -> ControlFlowTarget
getJumpTarget = go
    where
        go [Jump (Immediate _ value)] = RelativeTarget $ fromIntegral value
        go [Mem m] = MemoryTarget $ error (show m)
        go [Reg r] = RegisterTarget r
        go o = error $ show o
