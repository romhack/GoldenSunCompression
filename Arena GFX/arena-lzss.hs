import qualified Data.ByteString.Lazy as Bs
import Text.Printf
import Data.List
import Data.Word (Word8)
import Numeric (showHex)
import qualified Data.Bitstream.Lazy as Bi
import Control.Monad.State

data LzEntry = Liter Word8 | Refer {len :: Int, dist :: Int}
data BiCount a = BiCount {str :: [a], inCount :: Int, outCount :: Int} --bitstream with statistics embedded


instance Show LzEntry where
  show (Liter b) = " Liter 0x"++ showHex b ""
  show (Refer l d) = " Refer {len = 0x" ++ showHex l "" ++ " dist = 0x" ++ showHex d "" ++ "}"

{--for debug
listToHex :: [Word8] -> String
listToHex xs = "["++ concatMap (printf "0x%02X,") xs ++ "]" 
--}

toBoolStream :: Bs.ByteString -> [Bool] --convert to bytestream and then to bool via bitstream
toBoolStream inputString = Bi.unpack (Bi.fromByteString inputString :: Bi.Bitstream Bi.Left)

bitsToNum :: (Num a) => [Bool] -> a
bitsToNum = foldl' (\byte b -> byte*2 + if b then 1 else 0) 0

deserialize :: State (BiCount Bool) [LzEntry]
deserialize = do
  s <- get
  case str s of
    [] -> error "Unexpected input stream end in next flag check"
    (True:bs) -> do  --that's Liter
      let (literBits, tailStream) = splitAt 8 bs
      put $ BiCount tailStream (inCount s + 9) (outCount s + 1) --deserialize (BiCount tailStream (inCount x + 9) (outCount x + 1)) --that's Liter
      rest <- deserialize
      return $ Liter (bitsToNum (reverse literBits)) : rest

    (False:bs) -> do --that's Refer
      let 
        l = getLength bs
        d = getDist (str l) (outCount s)
      if outCount l == 0 
        then  do --exit at zero length flag, rest of bitstream not matter 
          put $ BiCount [] (inCount s + inCount l + 1) (outCount s + outCount l)  
          return []
        else do 
          put $ BiCount (str d) (inCount s + inCount l + inCount d + 1) (outCount s + outCount l)
          rest <- deserialize
          return $ Refer {len =  outCount l, dist = outCount d} : rest

getLength :: [Bool] -> BiCount Bool --return length value as outCount, number of bits read in inCount and rest of stream. Return [] if exit comand is found
getLength xs = 
  case xs of
    (False:bs) -> BiCount {str = bs, inCount = 1, outCount = 2}
    (True:False:bs) -> BiCount {str = bs, inCount = 2, outCount = 3} 
    (True:True:False:bs) -> BiCount {str = bs, inCount = 3, outCount = 4}
    (True:True:True:False:bs) -> BiCount {str = bs, inCount = 4, outCount = 5}
    (True:True:True:True:False:False:bs) -> BiCount {str = bs, inCount = 6, outCount = 6}
    (True:True:True:True:False:True:bs) -> BiCount {str = bs, inCount = 6, outCount = 7}
    (True:True:True:True:True:x:y:bs)
        | l1 /= 0 -> BiCount {str = bs, inCount = 7, outCount = l1 + 7}
        | l2 /= 0 -> BiCount {str = tailStream, inCount = 14, outCount = l2 + 10} --if 7 failed, check for extended set of bits
        | otherwise -> BiCount {str = [], inCount = 14, outCount = 0} --exit, when extended counter shows zero length
        where l1 = bitsToNum $ reverse [x, y] :: Int
              l2 = bitsToNum $ reverse  lenBits :: Int
              (lenBits, tailStream) = splitAt 7 bs

    _ -> error "Unexpected input stream end in getLength"


getDist :: [Bool] -> Int -> BiCount Bool -- get stream and position in outStream. Return distance value in outCount, number of bits read in inCount and rest of stream
getDist [] _ = error "Unexpected input stream end in getDistance"
getDist (b:bs) outPos
  | b = BiCount {str = drop 5 bs, inCount = 6, outCount = extractFromStream 5}
  | otherwise  = BiCount {str = drop bitCount bs, inCount = bitCount + 1, outCount = extractFromStream bitCount + 0x20}
  where
    extractFromStream :: Int -> Int
    extractFromStream count =  bitsToNum . reverse $ take count bs --reverse is a game's algorithm of storing distance bits  
    outPos' = outPos - 0x20 --up to 20 will be encoded by 5 bits
    bitCount = if outPos' >= 0x800 then 12 --2k LZ window
                                   else bitSize outPos' 

    bitSize x = (+1).floor $ (logBase 2 (fromIntegral x) :: Double) --calc how much bits int value takes  

decode :: [LzEntry] -> [Word8]
decode  = foldl decode' [] 
  where decode' out (Liter b) = out ++ [b] --emit literal
        decode' out (Refer l d) = out ++ refChunk --LZ reference. add to output reference chunk from previous decoded stream
          where refChunk = take l . cycle $ drop (length out - d - 1) out -- -1 as distance is zero-based
          --infinite cycle list. If length>distance, we will read data, already decoded in this reference 


main :: IO()
main = do
  
      inputBs <- Bs.readFile "0171 - Golden Sun (UE).gba"
      let input =  toBoolStream $ Bs.drop 0x434ca9 inputBs
          initCount = BiCount input 0 0
          entries = evalState deserialize initCount
          binOut = decode entries 
          finalState = execState deserialize initCount
          inByteCnt = (inCount finalState `div` 8) + 1
      writeFile "entries.txt" $ show entries
      putStrLn (printf "Compressed data size is 0x%02X," inByteCnt)
      putStrLn (printf "Decompressed data size is 0x%02X," (outCount finalState))
      Bs.writeFile "out.bin" $ Bs.pack binOut
