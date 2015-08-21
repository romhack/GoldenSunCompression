module Codec (decodeMsg, decodeBatch, prettyPrintBytes, getEncodedFiles) where

import Data.Word
import Control.Monad.State
import qualified Data.ByteString.Lazy as Bs
import qualified Data.Bitstream.Lazy as Bi
import Data.Bits
import Data.Char
import Data.Binary.Get
import Data.Int
import Text.Printf
import qualified Data.Map         as M
import Data.List
import Data.List.Split
import Data.Ord (comparing)
import Data.Binary.Put

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


gbaRomStart :: Int64
gbaRomStart = 0x8000000

--build tree out of serialized bitstream
makeTree :: State ([Bool], Int) (HuffmanTree Int)
makeTree = do
  (b:bs, count) <- get
  if b 
    then do --that's a leaf, count it
      put (bs, count+1)
      return Leaf {weight = 0, val = count}
    else do -- that's a node, make new tree
      put (bs, count)
      leftBranch <- makeTree
      rightBranch <- makeTree
      return Tree {weight = 0, left = leftBranch, right = rightBranch}


--serialize huffman tree by pre-order traversal
serializeTree :: HuffmanTree a -> State ([Bool], [a]) () 
serializeTree (Tree _ l r)= do --that's a tree
  (treeStream, chars) <- get
  put (treeStream++[False], chars) --modify bitstream state, don't touch chars
  serializeTree l --consequently do first the all-lefts branches
  serializeTree r

serializeTree (Leaf _ char) = do --that's a leaf
  (treeStream, chars) <- get
  put (treeStream++[True], chars++[char]) --modify both bitstream and chars in state


--make 2 12-bit values out of 3 8-bit values 
build12BitEntries :: (Bits a, Num a) => [a] -> [a]
build12BitEntries [] = []
build12BitEntries [_] = error "can't build 12 bits out of 8"
build12BitEntries [a,b] = [(a `shiftL` 4) .|. (b `shiftR` 4)]
build12BitEntries (a:b:c:xs) = [(a `shiftL` 4) .|. (b `shiftR` 4),  ((b .&. 0x0F) `shiftL` 8) .|. c] ++ build12BitEntries xs
    
--make 3 8-bit values out of 2 12 bit values
serialize12BitEntries :: (Bits a, Num a) => [a] -> [a]
serialize12BitEntries [] =[]
serialize12BitEntries [a] = [a `shiftR` 4,(a .&. 0x0F) `shiftL` 4]
serialize12BitEntries (a:b:xs) = [a `shiftR` 4, (a .&. 0x0F) `shiftL` 4, b] ++ serialize12BitEntries xs



toBoolStream :: Bs.ByteString -> [Bool] --convert to bytestream and then to bool via bitstream
toBoolStream inputString = Bi.unpack (Bi.fromByteString inputString :: Bi.Bitstream Bi.Left)

toBitStream :: [Bool] -> Bi.Bitstream Bi.Left
toBitStream = Bi.pack 

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
  | b == 0 = hexCode ++ "\n" ++ prettyPrintBytes bs
  | b >= 0x20 && b < 0x80 = toEnum (fromIntegral b) : prettyPrintBytes bs
  | otherwise = hexCode ++ prettyPrintBytes bs
  where hexCode = printf "{%02X}" b

decodedStreams :: Bs.ByteString -> Int -> Int64 -> Int -> [[Word8]]
decodedStreams input treePtrsOffset textPtrTable mIndex =  map (decode lutTrees) msgBitStreams 
  where 
    (treesStartOffset, treesOffsetTable) = runGet get2Ptrs $ Bs.drop (fromIntegral treePtrsOffset) input
    treesOffsetCount = (treePtrsOffset - treesOffsetTable) `div` 2 --offsets are 16 bit goes from trees to trees pointers
    --start reading trees and calculating offsets
    treePtrs = map ((+ treesStartOffset) . fromIntegral) $ runGet (replicateM treesOffsetCount getWord16le) $ Bs.drop (fromIntegral treesOffsetTable) input
    lutTrees = map (getLutTree input) treePtrs
    offsets = map fromIntegral $ msgOffsets input textPtrTable mIndex
    msgBitStreams = map (toBoolStream . (`Bs.drop` input)) offsets



decodeMsg :: Bs.ByteString -> Int -> Int64 -> Int -> [Word8]
decodeMsg input treePtrsOffset textPtrTable mIndex = decodedStreams input treePtrsOffset textPtrTable (mIndex + 1) !! mIndex
     

decodeBatch :: Bs.ByteString -> Int -> Int64 -> Int -> [Word8]
decodeBatch input treePtrsOffset textPtrTable mCount =  concat $ decodedStreams input treePtrsOffset textPtrTable mCount



----------Encoding part-----------------------------------

-- count the number of instances each symbol occurs in a list
-- tuples are swapped, as map had fst as Key, and we should have [(weight, char)] tuples
histogram :: Ord a => [a] -> [(Int,a)]
histogram xs = swap . M.toList $ M.fromListWith (+) [(c, 1) | c <- xs]
  where swap = map (\(a,b)->(b,a))

-- build a huffman tree bototm-up from a list of symbols sorted by weight
sortedHuffman ::(Ord a) => [(Int,a)] -> HuffmanTree a
-- first, convert each tuple into a Leaf, then combine
sortedHuffman = combine . map (uncurry Leaf) . sortBy (comparing fst) . sortBy (flip (comparing snd)) --reverse is due to game's algorithm trees build
    where
    -- repeatedly combine lowest weight trees and reinsert the result into the
    -- weight ordered list
    combine [] = error "no root found\n" 
    combine [t] = t --got a root tree
    combine (ta: tb: ts) = combine $ insertByGt (comparing weight) (mergeTree ta tb) ts
     where
       mergeTree a b = Tree (weight a + weight b) a b
       -- make an internal node from two trees. the weight is the sum

-- Insert element to the largest bound when equals are encountered
-- Game uses this way of insert to build trees
insertByGt :: (a -> a -> Ordering) -> a -> [a] -> [a]
insertByGt _   x [] = [x]
insertByGt cmp x ys@(y:ys')
 = case cmp x y of
     LT  -> x : ys
     _ -> y : insertByGt cmp x ys'



treesLenTable :: M.Map Word8 (Bi.Bitstream Bi.Left, Bs.ByteString) -> Bs.ByteString --get trees table of length: offset of appropriate tree bitstream from start of trees block
treesLenTable m = runPut $ mapM_ (putWord16le . fromIntegral) $ go 0 0
  where
    go count accum
      | count > maximum (M.keys m) = [] --if we've checked all trees in map, end traverse
      | otherwise = case M.lookup count m of
          Just (bitTree, chars) -> accum + Bs.length chars : go (count+1)  (accum + Bs.length chars + ((Bi.length bitTree `div` 8) + 1)) --calc length in a normal way
          Nothing -> 0x8000 : go (count+1) accum --if char is not in the map, just put 8000 as a dummy pointer
      


treeToBytes :: (Bi.Bitstream Bi.Left, Bs.ByteString) -> Bs.ByteString
treeToBytes (bitTree, chars) = Bs.concat [chars, Bi.toByteString bitTree]

-- traverse the huffman tree generating a map from the symbol to its huffman
-- tree path (where False is left, and True is right). 
codes :: Ord a => HuffmanTree a -> M.Map a [Bool]
codes root = M.fromList (go [] root)
  where    
  go p (Leaf _ a) = [(a, reverse p)]-- leaf nodes mark the end of a path to a symbol
  go p (Tree _ l r) = go (False:p) l ++ go (True:p) r

encodeMsg :: M.Map Word8 (M.Map Word8 [Bool]) -> [Word8] -> Bs.ByteString 
encodeMsg m xs = Bi.toByteString (Bi.pack (go m xs 0) :: Bi.Bitstream Bi.Left) --first char generates from zero tree
  where 
    go _ [] _ = []
    go codess (c:cs) prev = (codess M.! prev) M.! c ++ go codess cs c



getBlockTextLen :: [Bs.ByteString] -> Bs.ByteString
getBlockTextLen msgs = Bs.pack $ map (fromIntegral.Bs.length) msgs

merge :: [a] -> [a] -> [a]
merge [] ys = ys
merge (x:xs) ys = x: merge ys xs

getTextBlockPtrs :: Int64 -> [Bs.ByteString] -> [Int64]
getTextBlockPtrs _ [] = error "getTextBlockPtrs empty list"
getTextBlockPtrs start [block] = start : [start + Bs.length block]
getTextBlockPtrs start (block:blocks) = start : (start + Bs.length block) : getTextBlockPtrs (start + Bs.length block + 0x100) blocks


alignBsTo4 :: Bs.ByteString -> Bs.ByteString --align bytestring on four by zeroes tail
alignBsTo4 input = Bs.append input $ Bs.pack dummy
  where
    modulo = Bs.length input `mod` 4
    dummy
      | modulo == 0 = []
      | otherwise =  replicate (4 - fromIntegral modulo) 0 --dummy list to append at the end




getEncodedFiles :: Bs.ByteString -> Int64 -> Int64 -> [Bs.ByteString]
getEncodedFiles input treeStartOffset treePtrsOffset = [trees, text, textPtrs]
  where
    inputU8 = Bs.unpack input
    scriptInPairs = zip xs' $ tail $ chunksOf 1 xs' --make pairs of previous and next bytes to count them
      where xs' = 0 : inputU8 --prepend 0 before first message
    treesMap = M.fromListWith (++) scriptInPairs --get list of all found companions for each key
    treesHists = M.map (sort.histogram) treesMap --get sorted histogram of each key companions
    huffmanTrees = M.map sortedHuffman treesHists --these are trees with chars encoded in them
    treesSerialized = M.map (\(bitTree, chars) -> (toBitStream bitTree, Bs.pack (reverse(serialize12BitEntries chars)))) treesBitstreams--make each tree as serialized (bitstream, bytesting)
      where treesBitstreams =  M.map (\m -> execState (serializeTree m) ([],[])) huffmanTrees --get Map ([bool],[char])
    lTable = treesLenTable treesSerialized --get length table out of map with serialized trees
    codess = M.map codes huffmanTrees --get codes for each tree at each char in map
    msgs = split (keepDelimsR $ dropFinalBlank $ whenElt (==0)) inputU8 --split script into messages null-terminated
    encodedBlocks = chunksOf 0x100 (map (encodeMsg codess) msgs) --these are serialized in bytestrings text blocks (0x100 msgs in each)
    blockTextLen = map getBlockTextLen encodedBlocks --array of msgs lengths for each block
    solidEncodedBlocks = map Bs.concat encodedBlocks --concat messages of one block into one bytestring



    treesOnly = Bs.concat $ map treeToBytes $ M.elems treesSerialized
    trees = alignBsTo4 $ Bs.append treesOnly lTable
    ptrTreesOffsets = treeStartOffset + Bs.length treesOnly --calc pointer for offsets
    treePtrSerialized = runPut $ mapM_ (putWord32le.fromIntegral ) [gbaRomStart + treeStartOffset, gbaRomStart + ptrTreesOffsets] --1st ptr is original tree start, and then calculated length table pointer

    textOnly = Bs.concat $ merge solidEncodedBlocks blockTextLen --intercalate text blocks with appropriate length tables
    text = alignBsTo4 $ Bs.append treePtrSerialized textOnly --concat tree ptrs and text in one file to position pointers in solid place in ROM

    textBlockPtrs = getTextBlockPtrs (gbaRomStart + treePtrsOffset + 8) solidEncodedBlocks --calculate pairs of pointers: one for text block, second for length table
    textPtrs = runPut $ mapM_ (putWord32le . fromIntegral) textBlockPtrs

