import qualified Data.ByteString.Lazy as Bs
--import Text.Printf
import Data.List
import Data.Word (Word8)
import Numeric (showHex)
import qualified Data.Bitstream.Lazy as Bi

data LzEntry = Liter Word8 | Refer {len :: Int, dist :: Int}

instance Show LzEntry where
  show (Liter b) = " Liter 0x"++ showHex b ""
  show (Refer l d) = " Refer {len = 0x" ++ showHex l "" ++ " dist = 0x" ++ showHex d "" ++ "}"


toBoolStream :: Bs.ByteString -> [Bool] --convert to bytestream and then to bool via bitstream
toBoolStream inputString = Bi.unpack (Bi.fromByteString inputString :: Bi.Bitstream Bi.Left)

bitsToByte :: (Num a) => [Bool] -> a
bitsToByte = foldl' (\byte b -> byte*2 + if b then 1 else 0) 0


decode :: Int -> [Bool] -> [LzEntry] --output stream position and bitstream to process
decode _ [] = error "Unexpected input stream end in next flag check"
decode pos (True:bs) = Liter (bitsToByte (reverse literBits)) : decode (pos + 1) tailStream --that's Liter
                         where (literBits, tailStream) = splitAt 8 bs
decode pos (False:bs) = if ln == 0 --that's Refer
                        then [] --exit at zero length flag
                        else Refer {len =  ln, dist = dst} : decode (pos + ln) distBits
                        where
                          (ln, lenBits) = getLength bs
                          (dst, distBits) = getDist lenBits pos



getLength :: [Bool] -> (Int, [Bool]) --return length value and rest of stream. Retrun len = 0 as a signal to stop decoding
getLength xs = 
  case xs of
    (False:bs) -> (2, bs)
    (True:False:bs) -> (3, bs)
    (True:True:False:bs) -> (4, bs)
    (True:True:True:False:bs) -> (5, bs)
    (True:True:True:True:False:False:bs) -> (6, bs)
    (True:True:True:True:False:True:bs) -> (7, bs)
    (True:True:True:True:True:x:y:bs)
        | l1 /= 0 -> (l1 + 7, bs)
        | l2 /= 0 -> (l2 + 10, tailStream)--if 7 failed, check for extended set of bits
        | otherwise -> (0, bs) --exit, when extended counter shows zero length
        where l1 = bitsToByte $ reverse [x, y] :: Int
              l2 = bitsToByte $ reverse  lenBits :: Int
              (lenBits, tailStream) = splitAt 7 bs

    _ -> error "Unexpected input stream end in getLength"


getDist :: [Bool] -> Int -> (Int, [Bool]) -- get position in outStream and stream. Return distance value and rest of stream
getDist [] _ = error "Unexpected input stream end in getDistance"
getDist (b:bs) outPos
  | b = (extractFromStream 5, drop 5 bs)
  | otherwise  = (extractFromStream bitCount + 0x20, drop bitCount bs)
  where
    extractFromStream :: Int -> Int
    extractFromStream count =  bitsToByte . reverse $ take count bs --reverse is a game's algorithm of storing distance bits  
    outPos' = outPos - 0x20 --up to 20 will be encoded by 5 bits
    bitCount = if outPos' >= 0x800 then 12 --2k LZ window
                                   else bitSize outPos' 

    bitSize x = (+1).floor $ (logBase 2 (fromIntegral x) :: Double) --calc how much bits int value takes  

decodeEntries :: [LzEntry] -> [Word8]
decodeEntries  = foldl decodeEntries' [] 
  where decodeEntries' out (Liter b) = out ++ [b] --emit literal
        decodeEntries' out (Refer l d) = out ++ refChunk --LZ reference. add to output reference chunk from previous decoded stream
          where refChunk = take l . cycle $ drop (length out - d - 1) out -- -1 as distance is zero-based
          --infinite cycle list. If length>distance, we will read data, already decoded in this reference 

{--for debug
listToHex :: [Word8] -> String
listToHex xs = "["++ concatMap (printf "0x%02X,") xs ++ "]" 
--}

main :: IO()
main = do
  
      inputBs <- Bs.readFile "0171 - Golden Sun (UE).gba"
      let input =  toBoolStream $ Bs.drop 0x434ca9 inputBs
          entries = decode 0  input
          binOut = decodeEntries entries
    
      Bs.writeFile "out.bin" $ Bs.pack binOut
