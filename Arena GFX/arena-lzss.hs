import Data.List
import Data.Word (Word8)
import Numeric (showHex)
import Data.ByteString.Lazy.Search
import qualified Data.ByteString.Lazy as Bs
import qualified Data.Bitstream.Lazy as Bi
import Data.Bits

data LzEntry = Liter Word8 | Refer {len :: Int, dist :: Int}
data BiCount a = BiCount {str :: [a], inCount :: Int, outCount :: Int} --bitstream with statistics embedded


instance Show LzEntry where
  show (Liter b) = " Liter 0x"++ showHex b ""
  show (Refer l d) = " Refer {len = 0x" ++ showHex l "" ++ " dist = 0x" ++ showHex d "" ++ "}"

{--for debug
listToHex :: [Word8] -> String
listToHex xs = "["++ concatMap (printf "0x%02X,") xs ++ "]" 
--}

        
-----------------------------------------ENCODING-----------------------------------------------------------------------------------
windowSize :: Int
windowSize = 0xFFF --4096 as distance can be encoded only by 12 bits (for 0 and 1st LZSS schemes)
maxLength :: Int
maxLength = 0x7F + 10 --length is encoded max by 7 bits 

clampMax :: (Ord a) => a -> a -> a --clamp out by mx given value
clampMax mx x = if x > mx then mx else x

matchesList :: Bs.ByteString -> Bs.ByteString -> [(Int, Int)] --list of all matches of needle's substrings 
matchesList needle haystack = takeWhile (/=(0,0)) $ map getLastMatch ns --terminate as soon as longer needle not found
  where 
    h =  Bs.reverse haystack 
    ns = map Bs.reverse $ tail $ Bs.inits needle --generate a list of increasing needles
    getLastMatch n' --the latest match in buffer of given single full needle
      | Bs.null beforeAndMatch = (0,0)
      | otherwise = (nLen, dLen + nLen)
      where 
        needleStrict = strictify n'
        (afterMatch, beforeAndMatch) = breakOn needleStrict h
        nLen = fromIntegral $ Bs.length n'
        dLen = fromIntegral $ Bs.length afterMatch
                                    

--get latest index of  length common chunk. Returns length - distance tuple. (0, 0) means not found.
--ByteStrings are used for speed of 'indices' string search algorithm
findInBufferMatch :: Bs.ByteString -> Bs.ByteString -> (Int, Int)
findInBufferMatch needle haystack 
  | null matches = (0,0)
  | otherwise = last matches --last is the longest match, found from the end of buffer
  where 
    matches = matchesList needle haystack


-- get length longest match at the end of haystack, which begins needle
-- zero if common suffix not found
getCommonSuffixLen:: Bs.ByteString -> Bs.ByteString -> Int
getCommonSuffixLen needle haystack  
  | null suffixes = 0
  | otherwise = fst $ head suffixes --length == distance, that's needle exactly and the end of buffer
  where suffixes = filter (uncurry (==)) $ matchesList needle haystack
 


--define if needle is a circular list with period of len, then we can encode with length > distance
--find length of haystack suffix, and then check max match between cycle list
--and needle
findOutBufferMatch :: Bs.ByteString -> Bs.ByteString -> (Int, Int)
findOutBufferMatch needle haystack
  | null needleMatch = (0, 0) --needle was not found at the end of haystack
  | otherwise = (outBufferLen, d)
  where
    needle' = Bs.unpack needle
    needleMatch = take (getCommonSuffixLen needle haystack) needle'
    d = length needleMatch
    --take while needle is equal to infinite circular array and output length
    outBufferLen = length . takeWhile id  $ zipWith (==) needle' $ cycle needleMatch 


--Match is found in 2 passes: first search for match in passed buffer window (InBufferMatch), 
--then we check again if full needle is circular list with a period of found match length (OutBufferMatch)
--so we could encode with len > dist
--Then we skip 1 byte and see if we could get better result, compare that and
--emit optimal LzEntry. 
encodeLz :: [Word8] -> [LzEntry]
encodeLz x = encodeLz' x []
  where
  encodeLz' [] _ = [] -- end of stream 
  encodeLz' input@(i:is) buffer
    | l < 2  = Liter i : encodeLz' is (buffer ++ [i]) --refer takes 2 bytes, match with 1 byte len encoded as liter
    | l2 > l  = Liter i : Refer l2 (d2-1) : encodeLz' (drop (l2+1) input) (buffer ++ take (l2+1) input) --skip one literal
    | otherwise =  Refer l (d-1) : encodeLz' (drop l input) (buffer ++ take l input) -- dist minus 1 for arena compression scheme
    where
      buffer' = Bs.pack $ lastN windowSize buffer --take only last bufferSize
      lastN n xs = foldl' (const . drop 1) xs (drop n xs) --take last N elements of list
      input' = Bs.take (Bs.length buffer') $ Bs.pack input --no need to take needle larger than haystack
      (l, d)   = findLzMatch input' buffer'
      (l2, d2) = findLzMatch (Bs.tail input') (buffer' `Bs.snoc` Bs.head input') --search for a longer match at the next input byte (LZ non greedy string parse)
      findLzMatch needle haystack = clampMaxLen $ max (findInBufferMatch needle haystack) (findOutBufferMatch needle haystack)
        where clampMaxLen (a,b) = (clampMax maxLength a, b) --clamp out maximum length, which can be encoded maximum by 7 bits

toBitStream :: [Bool] -> Bi.Bitstream Bi.Left
toBitStream = Bi.pack 

--byteToBits :: Word8 -> [Bool]
--byteToBits b = map (testBit b) [7,6..0]

numToBits :: (Bits a, Num a) => Int -> a -> [Bool] --number with a given bitfield width to bits list
numToBits n b = map (testBit b) [n-1, n-2..0]

meaningBitSize :: Int -> Int
meaningBitSize x = (+1).floor $ (logBase 2 (fromIntegral x) :: Double) --calc how much bits int value takes


serialize :: [LzEntry] -> [Bool]
serialize entries = (go entries 0) ++ endStreamSign --append with end of stream sign
  where 
    endStreamSign = [False, True, True, True, True, True, False, False, False, False, False, False, False, False, False] --that's like refer with zero in extended length code
    go [] _ = []
    go ((Liter b):es) offs = True : (reverse $ numToBits 8 b) ++ go es (offs+1)
    go ((Refer l d):es) offs = False : (lenBits ++ distBits)  ++ go es (offs + l)
      where lenBits
              | l == 2 = [False]
              | l == 3 = [True, False]
              | l == 4 = [True, True, False]
              | l == 5 = [True, True, True, False]
              | l == 6 = [True, True, True, True, False, False]
              | l == 7 = [True, True, True, True, False, True]
              | l <= 10 = [True, True, True, True, True] ++ reverse (numToBits 2 (l-7))
              | l <= maxLength = [True, True, True, True, True, False, False] ++ reverse(numToBits 7 (l-10))
              | otherwise = error "Unexpexpected long length in serialize length bits"
            
            distBits = if d <= 0x1F 
                       then True : reverse (numToBits 5 d)
                       else False : reverse (numToBits bitWidth (d-0x20))
                         where bitWidth = clampMax 12 $ meaningBitSize (offs - 0x20) --maximum 12 bits for distance
            









main :: IO()
main = do
--encode
      inputBs <- Bs.readFile "uncompressedMaster.gba"
      let
        input = Bs.unpack inputBs
        entries = encodeLz input
        out = toBitStream $ serialize entries
      --print $ entries !! 194
      Bi.writeFile "serialized.bin" out 
      --writeFile "entries.txt" $ show entries

{--  
--decode 
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

--}
