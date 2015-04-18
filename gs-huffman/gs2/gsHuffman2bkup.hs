import Data.Word
import Control.Monad.State
import qualified Data.ByteString.Lazy as Bs
import qualified Data.Bitstream.Lazy  as Bi
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
ptrOffset = 0x60A4C
ptrCount :: Int
ptrCount = 0xF1
treeStructStart :: Int
treeStructStart = 0x5F914


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


toBoolStream :: [Word8] -> [Bool] --convert to bytestream and then to bool via bitstream
toBoolStream wordStream = Bi.unpack (Bi.fromByteString (Bs.pack wordStream) :: Bi.Bitstream Bi.Left)

getLutTree :: Bs.ByteString -> Int -> (String, HuffmanTree Int) --get LutTree structure with given ROM and start offset of binary tree
getLutTree input offset = (lut, tree)
  where 
    inputU8 = Bs.unpack input
    tree = evalState makeTree (bitstream, 0)
    bitstream = toBoolStream $ drop offset inputU8
    lut = map chr charTableVal
    charTableVal = build12BitEntries rInput --spliton 3 and make 12 bits of reversed bytelist 
    rInput =  map fromIntegral $ Bs.unpack $ Bs.reverse $ Bs.take offset64 input
    offset64 = fromIntegral offset

get2Ptrs :: Get (Int, Int)
get2Ptrs = do
  fs <- getWord32le
  sn <- getWord32le
  return (fromIntegral (fs .&. 0x07FFFFFF), fromIntegral  (sn .&. 0x07FFFFFF)) --convert ptr value to ROM offset

msgOffsets :: Bs.ByteString -> Int64 -> Int -> [Int] --get a list of each message offset in ROM
msgOffsets input tblOffset msgCount = concat $ getBlockMsgOffsets startOffsets lengthTables
  where 
    inputU8 = Bs.unpack input
    blockCount = msgCount `div` 0x100
    blockPtrs = runGet (replicateM blockCount get2Ptrs) $ Bs.drop tblOffset input
    startOffsets = map fst blockPtrs
    lengthTblOffsets = map snd blockPtrs
    getLengthTable off = map fromIntegral $ take 0xFF $ drop off inputU8
    lengthTables = map getLengthTable lengthTblOffsets
    getBlockMsgOffsets [] _ = []
    getBlockMsgOffsets (_:_) [] = []
    getBlockMsgOffsets (b:bs) (ls: lss) = scanl (+) b ls : getBlockMsgOffsets bs lss

-- from a list of bits, navigate a given huffman tree and emit its decoded
-- symbol when reaching a Leaf
-- stop at NULL decoded char
decode:: [(String, HuffmanTree Int)] -> [Bool] -> String
decode trees = go $ head trees 
  where
    go _ [] = error "empty bitstream"
    -- choose path based on bit
    go (charLut, Tree _ l r) (b:bs)
      | not b = go (charLut, l) bs
      | otherwise = go (charLut, r) bs
    -- reached leaf, emit symbol
    go (charLut, Leaf _ i) bs
      | c == '\NUL' = "{00}\n"
      | otherwise   = prettyC ++ go (trees !! charCode) bs
      where 
        c = charLut !! i
        charCode :: Int
        charCode = fromEnum c
        prettyC = if charCode >= 0x20 && charCode < 0x80 then [c] else printf "{%02X}" charCode 

main :: IO()
main = do
  input <-  Bs.readFile "Golden Sun - The Lost Age (UE).gba"
  let 
    inputU8 = Bs.unpack input
    treePtrs = map ((+ treeStructStart) . fromIntegral) $ runGet (replicateM ptrCount getWord16le) $ Bs.drop ptrOffset input
    lutTrees = map (getLutTree input) treePtrs
    offsets = msgOffsets input 0xA9F54 0x29AE
    msgBitStreams = map (toBoolStream . (`drop` inputU8)) offsets
    msgs = concatMap (decode lutTrees) msgBitStreams
  putStrLn msgs

