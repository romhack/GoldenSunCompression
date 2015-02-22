import Data.Word
import Control.Monad.State
import qualified Data.ByteString.Lazy as Bs
import qualified Data.Bitstream.Lazy  as Bi
import Data.Bits
import Data.List.Split
import Data.Char
import Data.Binary.Get
import Data.Int

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

ptrOffset = 0x38334 :: Int64
ptrCount = 0x7B :: Int
treeStructStart = 0x37464 :: Int

bitTree :: [Bool]
bitTree = [False, False, True, False, True, False, False, True, True, True, True]

toBoolStream :: [Word8] -> [Bool] --convert to bytestream and then to bool via bitstream
toBoolStream wordStream = Bi.unpack (Bi.fromByteString (Bs.pack wordStream) :: Bi.Bitstream Bi.Left)

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


build12BitEntry :: [Int] -> [Int] --make 2 12-bit values out of 3 8-bit values
build12BitEntry xs@(a:b:_)  
  | l == 2 = [x] --end of list 2 bytes left, take only first 12 bit value
  | l == 3 = [x,y] --usual case 
  | otherwise = [] --return nothing, as insufficent bits
    where 
      l = length xs
      c = xs !! 2 --should be separate, cause we don't match it on 2 byte list
      x =  (a `shiftL` 4) .|. (b `shiftR` 4) --first 12 bit value
      y =  ((b .&. 0x0F) `shiftL` 8) .|. c -- second 12 bit value

     

getLutTree :: [Word8] -> Int -> (HuffmanTree Int, [(Int, Char)]) --get LutTree structure with given ROM and start offset of binary tree
getLutTree input offset = (tree, lut)
  where 
    tree = evalState makeTree (bitstream, 0)
    bitstream = toBoolStream $ drop offset input
    lut = zip [0,1..] $ map chr charTableVal
    charTableVal = concatMap build12BitEntry $ chunksOf 3 $ map fromIntegral rInput --spliton 3 and make 12 bits of reversed bytelist 
    rInput = reverse $ take offset input


  
 


main :: IO()
main = do
  input <-  Bs.readFile "0171 - Golden Sun (UE).gba"
  let 
    inputU8 = Bs.unpack input
    pointers = map ((+ treeStructStart) . fromIntegral) $ runGet (replicateM ptrCount getWord16le) $ Bs.drop ptrOffset input
    struct = getLutTree inputU8 $ head pointers
  print struct

