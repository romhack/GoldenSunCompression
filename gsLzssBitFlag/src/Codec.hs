module Codec (decode, encode) where

import           Control.Monad.State
import           Data.Bits
import qualified Data.Bitstream.Lazy         as Bi
import qualified Data.ByteString.Lazy        as Bs
import           Data.ByteString.Lazy.Search
import           Data.List
import           Data.Word                   (Word8)
import           Numeric                     (showHex)

data LzEntry = Liter Word8 | Refer {len :: Int, dist :: Int}
data BiCount a = BiCount {str :: [a], inCount :: Int, outCount :: Int} --bitstream with statistics embedded


instance Show LzEntry where
  show (Liter b) = " Liter 0x"++ showHex b ""
  show (Refer l d) = " Refer {len = 0x" ++ showHex l "" ++ " dist = 0x" ++ showHex d "" ++ "}"

windowSize :: Int
windowSize = 0xFFF --4096 as distance can be encoded only by 12 bits (for 0 and 1st LZSS schemes)
maxLength :: Int
maxLength = 0x7F + 10 --length is encoded max by 7 bits

{--for debug
listToHex :: [Word8] -> String
listToHex xs = "["++ concatMap (printf "0x%02X,") xs ++ "]"
--}
meaningBitSize :: Int -> Int
meaningBitSize x = (+1).floor $ (logBase 2 (fromIntegral x) :: Double) --calc how much bits int value takes

-----------------------------------------DECODING-----------------------------------------------------------------------------------
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
      put $ BiCount tailStream (inCount s + 9) (outCount s + 1) --that's Liter
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
    bitCount = if outPos' >= windowSize then 12 --2k LZ window
                                   else meaningBitSize outPos'

decodeLz :: [LzEntry] -> [Word8]
decodeLz  = foldl decodeLz' []
  where decodeLz' out (Liter b) = out ++ [b] --emit literal
        decodeLz' out (Refer l d) = out ++ refChunk --LZ reference. add to output reference chunk from previous decodeLzd stream
          where refChunk = take l . cycle $ drop (length out - d - 1) out -- -1 as distance is zero-based
          --infinite cycle list. If length>distance, we will read data, already decodeLzd in this reference
decode :: Bs.ByteString -> (Bs.ByteString, Int, Int)
decode input = (plainBlock, originalSize, plainSize)
 where
  inputBits = toBoolStream input
  initCount = BiCount inputBits 0 0
  finalState = execState deserialize initCount
  originalSize = (inCount finalState `div` 8) + 1
  plainSize = outCount finalState
  plainBlock = Bs.pack $ decodeLz entries
   where entries = evalState deserialize initCount

-----------------------------------------ENCODING-----------------------------------------------------------------------------------
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

numToBits :: (Bits a, Num a) => Int -> a -> [Bool] --number with a given bitfield width to bits list
numToBits n b = map (testBit b) [n-1, n-2..0]

serialize :: [LzEntry] -> [Bool] --get bitstream serialized from entries by all rules of game's storage scheme
serialize entries = go entries 0 ++ endStreamSign --append with end of stream sign and add tracking of out stream offset for distance bits serialization
  where
    endStreamSign = [False, True, True, True, True, True, False, False, False, False, False, False, False, False, False] --that's like refer with zero in extended length code
    go [] _ = []
    go (Liter b:es) offs = True : reverse (numToBits 8 b) ++ go es (offs+1) --liter is encoded by True and 8 bits
    go (Refer l d:es) offs = False : (lenBits ++ distBits)  ++ go es (offs + l) --refer is false + len bits + dist bits
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
                       then True : reverse (numToBits 5 d) --up to 5 bits are encoded straight
                       else False : reverse (numToBits bitWidth (d-0x20)) --more than 5 bits are encoded also based on maximum distance possible (offset in out stream)
                         where bitWidth = clampMax 12 $ meaningBitSize (offs - 0x20) --maximum 12 bits for distance

encode :: Bs.ByteString -> Bs.ByteString
encode = Bi.toByteString. toBitStream. serialize. encodeLz. Bs.unpack
