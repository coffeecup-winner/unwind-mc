module Unwind.Types ( Instruction(..)
                    , Transition(..)
                    , InstructionGraph
                    , newGraph
                    , Function(..)
                    , Unwind
                    , fakeInstruction
                    , runUnwind
                    , showGraph
                    ) where

import Control.Monad.Identity
import Control.Monad.State.Strict
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import qualified Data.Text as T
import Data.Word (Word64)
import qualified Hdis86 as X86
import Text.Printf (printf)

data Instruction = Instruction { offset :: !Word64
                               , hex :: !T.Text
                               , assembly :: !T.Text
                               , instr :: !X86.Instruction
                               }

data Transition = Next
                | Branch
                | Call
                deriving (Show)

instance Show Instruction where
    show (Instruction _ _ _ i) = show i

type InstructionGraph = Gr Instruction Transition

newGraph :: [LNode Instruction] -> [LEdge Transition] -> InstructionGraph
newGraph = mkGraph

fakeInstruction :: Word64 -> String -> Instruction
fakeInstruction o t = Instruction o T.empty (T.pack t) (X86.Inst [] X86.Inop [])

data Function = Function { start :: !Word64
                         , body :: InstructionGraph
                         } deriving (Show)

type UnwindData = InstructionGraph
type Unwind = StateT UnwindData Identity

runUnwind :: Unwind () -> InstructionGraph -> UnwindData
runUnwind = execState

showGraph :: InstructionGraph -> String
showGraph gr = unlines . map showNode . map (context gr) . nodes $ gr
    where showNode (ein, _, (Instruction o h a _), eout) =
            printf "[%di %do] %08x %20s %s" (length ein) (length eout) o (T.unpack h) (T.unpack a)
