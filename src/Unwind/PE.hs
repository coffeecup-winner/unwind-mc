{-# LANGUAGE RecordWildCards #-}
module Unwind.PE ( PEFile(..)
                 , loadPEFile
                 ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.PE.Parser as PE
import qualified Data.PE.Structures as PE
import Data.Word (Word32)

data PEFile = PEFile { imageBase :: !Word32
                     , textOffset :: !Word32
                     , textData :: !B.ByteString
                     } deriving (Show)

loadPEFile :: String -> IO PEFile
loadPEFile filepath = do
    file <- PE.buildFile filepath
    let header = PE.peHeader file
        imageBase = PE.imageBase . PE.windowsSpecFields $ header
        text = head . dropWhile ((/= ".text") . PE.sectionHeaderName . fst) . PE.sectionTables $ header
        textOffset = PE.virtualAddress . fst $ text
        textData = BL.toStrict . snd $ text
    return PEFile {..}
