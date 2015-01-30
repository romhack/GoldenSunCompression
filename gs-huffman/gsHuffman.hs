import Control.Monad.State
import qualified Data.Bitstream.Lazy      as Bi
import qualified Data.ByteString.Lazy     as Bs
import Data.Word


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

toBoolStream :: [Word8] -> [Bool] --convert to bytestream and then to bool via bitstream
toBoolStream wordStream = Bi.unpack (Bi.fromByteString (Bs.pack wordStream) :: Bi.Bitstream Bi.Left)

buildTree :: State ([Bool], Int) (HuffmanTree Int)
buildTree = do
  (bit:bits, count) <- get
  if bit 
    then do
      put (bits, count+1)
      return $ Leaf 0 count
    else do
      put (bits, count)
      leftTree <- buildTree
      rightTree <- buildTree
      return $ Tree 0 leftTree rightTree

main :: IO ()
main = do
  input <-  Bs.readFile "0171 - Golden Sun (UE).gba"
  let inputU8 = drop 0x374BB $ Bs.unpack input
      binaryTree = toBoolStream inputU8
  print $ evalState buildTree (binaryTree, 0)
