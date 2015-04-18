import Data.Word
import Control.Monad.State
import qualified Data.ByteString.Lazy as Bs
import qualified Data.Bitstream.Lazy as Bi
import Data.Bits
import Data.Char
import Data.Binary.Get
import Data.Int
import Text.Printf

data  HuffmanTree a =    Leaf {weight:: Int, val:: a}
                        |Tree {weight:: Int, left:: HuffmanTree a, right:: HuffmanTree a}
                        deriving (Eq)

-- build a multiline string representation of a huffman tree
instance Show a => Show (HuffmanTree a) where
  show = go ""
    where
      paren x = "--" ++ show x -- ++ "+"
      go _ (Leaf _ v) = "-[" ++ show v  ++ "]\n"
      go ss (Tree w l r) =   root ++ go (ss' ++ "|") l
          ++ ss' ++ "|\n" 
          ++ ss' ++ "`" ++ go (ss' ++ " ") r 
          where 
           ss' = ss ++ replicate (length root - 1) ' '
           root = paren w 

ptrOffset :: Int64
ptrOffset = 0x38334
ptrCount :: Int
ptrCount = 0x7C
treeStructStart :: Int
treeStructStart = 0x37464 
blocksPtrTable :: Int64
blocksPtrTable = 0x736b8
msgCnt :: Int
msgCnt = 0x29E1


--build tree out of serialized bitstream
makeTree :: State ([Bool], Int) (HuffmanTree Int)
makeTree = do
  (b:bs, count) <- get
  if b 
    then do --that's a leaf, count it
      put (bs, count+1)
      return (Leaf 0 count)
    else do -- that's a node, make new tree
      put (bs, count)
      leftBranch <- makeTree
      rightBranch <- makeTree
      return (Tree 0 leftBranch rightBranch) 

--make 2 12-bit values out of 3 8-bit values 
build12BitEntries :: [Int] -> [Int]
build12BitEntries xs = [(a `shiftL` 4) .|. (b `shiftR` 4),  ((b .&. 0x0F) `shiftL` 8) .|. c] ++ build12BitEntries (drop 3 xs)
  where 
    a = head xs
    b = xs !! 1
    c = xs !! 2


toBoolStream :: Bs.ByteString -> [Bool] --convert to bytestream and then to bool via bitstream
toBoolStream inputString = Bi.unpack (Bi.fromByteString inputString :: Bi.Bitstream Bi.Left)

getLutTree :: Bs.ByteString -> Int -> (String, HuffmanTree Int) --get LutTree structure with given ROM and start offset of binary tree
getLutTree input offset = (lut, tree)
  where 
    tree = evalState makeTree (bitstream, 0)
    bitstream = toBoolStream $ Bs.drop (fromIntegral offset) input
    lut = map chr charTableVal
    charTableVal = build12BitEntries rInput --spliton 3 and make 12 bits of reversed bytelist 
    rInput =  map fromIntegral $ Bs.unpack $ Bs.reverse $ Bs.take (fromIntegral offset) input

get2Ptrs :: Get (Int, Int) --get 2 ptrs in tuple and convert them to ROM offset
get2Ptrs = do
  fs <- getWord32le
  sn <- getWord32le
  return (fromIntegral (fs .&. 0x07FFFFFF), fromIntegral  (sn .&. 0x07FFFFFF)) --convert ptr value to ROM offset

msgOffsets :: Bs.ByteString -> Int64 -> Int -> [Int] --get a list of each message offset in ROM
msgOffsets input tblOffset msgCount = concat $ getBlockMsgOffsets startOffsets lengthTables
  where 
    blockCount = ceiling ((fromIntegral msgCount :: Double) / 0x100) --get total blocks from message count
    blockPtrs = runGet (replicateM blockCount get2Ptrs) $ Bs.drop tblOffset input --get all pointers pairs
    startOffsets = map fst blockPtrs
    lengthTblOffsets = map snd blockPtrs--extract two lists
    getLengthTable size off = map fromIntegral $ take size $ drop off $ Bs.unpack input
    lengthTables = map (getLengthTable 0xFF) (init lengthTblOffsets) ++ [getLengthTable (msgCount `mod` 0x100) (last lengthTblOffsets)] --each block has FF msgs, but the last has less
    getBlockMsgOffsets = zipWith (scanl (+)) --add base offset of each block to the length accumulative

-- from a list of bits, navigate a given huffman tree and emit its decoded
-- symbol when reaching a Leaf
-- stop at NULL decoded char
decode:: [(String, HuffmanTree Int)] -> [Bool] -> [Word8]
decode trees = go (head trees) 
  where
    go _ [] = error "empty bitstream"
    -- choose path based on bit
    go (charLut, Tree _ l r) (b:bs)
      | not b = go (charLut, l) bs
      | otherwise = go (charLut, r) bs
    -- reached leaf, emit symbol
    go (charLut, Leaf _ i) bs
      | charCode == 0 = [0] 
      | otherwise     = charCode : go (trees !! fromIntegral charCode) bs
      where charCode = fromIntegral $ fromEnum $ charLut !! i 

prettyPrintBytes :: [Word8] -> String
prettyPrintBytes [] = []
prettyPrintBytes (b:bs)
  | b == 0 = hexCode ++ "\n"
  | b >= 0x20 && b < 0x80 = toEnum (fromIntegral b) : prettyPrintBytes bs
  | otherwise = hexCode ++ prettyPrintBytes bs
  where hexCode = printf "{%02X}" b

main :: IO()
main = do
  input <-  Bs.readFile "0171 - Golden Sun (UE).gba"
  let 
    treePtrs = map ((+ treeStructStart) . fromIntegral) $ runGet (replicateM ptrCount getWord16le) $ Bs.drop ptrOffset input
    lutTrees = map (getLutTree input) treePtrs
    offsets = map fromIntegral $ msgOffsets input blocksPtrTable msgCnt
    msgBitStreams = map (toBoolStream . (`Bs.drop` input)) offsets
    
    msgsBin = map (decode lutTrees) msgBitStreams 
    msgsString = concatMap prettyPrintBytes msgsBin
  --putStr msgsString
  Bs.writeFile "script.bin" $ Bs.pack $ concat msgsBin
