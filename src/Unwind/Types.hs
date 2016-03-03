module Unwind.Types ( Instruction(..)
                    , Function(..)
                    , Unwind
                    , runUnwind
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

instance Show Instruction where
    show (Instruction o h a _) = printf "%08x %20s %s" o h a

data Function = Function { start :: !Word64
                         , instrs :: !(Int, Int)
                         } deriving (Show)

type UnwindData = Gr Instruction ()
type Unwind = StateT UnwindData Identity

runUnwind :: Unwind () -> Gr Instruction () -> Gr Instruction ()
runUnwind = execState
