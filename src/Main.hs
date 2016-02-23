import qualified Data.ByteString.Lazy as B
import qualified Data.PE.Parser as PE
import qualified Data.PE.Structures as PE
import qualified Hdis86 as X86
import qualified System.Environment as System
import Text.Printf (printf)

main :: IO ()
main = do
    [filepath] <- System.getArgs
    file <- PE.buildFile filepath
    let getText = head . dropWhile ((/= ".text") . PE.sectionHeaderName . fst) . PE.sectionTables . PE.peHeader
        getInstrs = X86.disassembleMetadata X86.intel32 { X86.cfgSyntax = X86.SyntaxIntel } . B.toStrict . snd
        display = printf "%08x %20s %s" <$> X86.mdOffset <*> X86.mdHex <*> X86.mdAssembly
    mapM_ (putStrLn . display) . getInstrs . getText $ file
