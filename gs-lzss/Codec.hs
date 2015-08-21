module Codec (decode, encode) where

import qualified Data.ByteString.Lazy as Bs
import Data.ByteString.Lazy.Search
import Data.List
import Data.Word (Word8)
import Data.Bits
import Numeric (showHex)
import Data.List.Split

data LzEntry = Liter Word8 | Refer {len :: Int, dist :: Int}

instance Show LzEntry where
  show (Liter b) = " Liter 0x"++ showHex b ""
  show (Refer l d) = " Refer {len = 0x" ++ showHex l "" ++ " dist = 0x" ++ showHex d "" ++ "}"


----------------------------------------USAGE---------------------------------------------------------------
decode :: Bs.ByteString -> (Bs.ByteString, Int)
decode encodedBlock = (Bs.pack (decodeLz entries), originalEncodedSize)
  where  (entries, originalEncodedSize) = readLzEntries $ Bs.unpack encodedBlock
  
  --putStrLn $ printf "Decoded. Original encoded block size is 0x%X bytes" encodedBlockSize
  --Bs.writeFile fileName (Bs.pack . decodeLz $ entries)
encode :: Bs.ByteString -> Bs.ByteString  
encode decodedBlock = Bs.pack . writeLzEntries . encodeLz . Bs.unpack $ decodedBlock

----------------------------------------DECODE---------------------------------------------------------------
--Create list of LzEntries from list of flags and compressed stream 
--second in tuple is original compressed size for further compare before insert
--changed block in ROM
readLzEntries:: [Word8] -> ([LzEntry], Int)
readLzEntries is = readLzEntries' [] is
  where
  readLzEntries' :: [Bool] -> [Word8] -> ([LzEntry], Int)
  readLzEntries' _ [] = ([], length is) --compressed stream is read out
  readLzEntries' [] (x:xs) = readLzEntries'(byteToBits x) xs --flag byte is shifted out: read new
    where byteToBits b = map (testBit b) [7,6..0]
  readLzEntries'(False:fs) (x:xs) = (Liter x : entries, cnt) where (entries, cnt) = readLzEntries' fs xs --zero flag is plain literal                     
  readLzEntries'(True:_) [_] = ([], length is) --LZ reference has insufficent data bytes, end decode
  readLzEntries'(True:fs) (l:d:xs) 
    | l' == 0 && d' == 0 = ([], length is - length xs) --0 distance and length is 'end of compressed stream' flag
    | d' == 0 = error "Zero distance in LZ code" --no sense, should not happen
    | l' == 0 = let (entries, cnt) = readLzEntries' fs (tail xs)
                in (Refer {len = fromIntegral(head xs) + 0x11, dist = d'} : entries, cnt) --0 length is extended length flag
    | otherwise = let (entries, cnt) = readLzEntries' fs xs
                  in (Refer {len = l' + 1, dist = d'} : entries, cnt)
    --LZ reference has next 2 bytes: XY ZZ, where Y - nybble length, XZZ - 24 bit distance
      where l' = fromIntegral (l.&.0xF)
            d' = fromIntegral (l.&.0xF0) `shiftL` 4 + fromIntegral d

decodeLz :: [LzEntry] -> [Word8]
decodeLz  = foldl decodeLz' [] 
  where decodeLz' out (Liter b) = out ++ [b] --emit literal
        decodeLz' out (Refer l d) = out ++ refChunk --LZ reference. add to output reference chunk from previous decoded stream
          where refChunk = take l . cycle $ drop (length out - d) out
          --infinite cycle list. If length>distance, we will read data, already decoded in this reference 

-------------------------------------------ENCODE--------------------------------------------------------------
windowSize :: Int
windowSize = 0xFFF --4096 as distance can be encoded only by 12 bits

--get latest index of  length common chunk. Returns length - distance tuple. (0, 0) means not found.
--ByteStrings are used for speed of 'indices' string search algorithm
findInBufferMatch :: Bs.ByteString -> Bs.ByteString -> (Int, Int)
findInBufferMatch needle haystack
  | Bs.null needle = (0, 0) --match is not found at all
  | null index  = findInBufferMatch (Bs.init needle) haystack --full needle not found, cut and search again
  | d > windowSize = (0, 0) --match found but too long away
  | otherwise   = (fromIntegral (Bs.length needle), d)
  where 
    index = indices (strictify needle) haystack
    d = fromIntegral $ Bs.length haystack - last index

--define if needle is a circular list with period of len, then we can encode with length > distance
--find length of haystack suffix, and then check max match between cycle list
--and needle
findOutBufferMatch :: (Eq a) => [a] -> [a] -> (Int, Int)
findOutBufferMatch needle haystack
  | null needleMatch || (d > windowSize) = (0, 0) --needle was not found at the end of haystack or too long away
  | otherwise = (outBufferLen, d)
  where
    needleMatch = getSuffix needle
    getSuffix n = if n `isSuffixOf` haystack then n else getSuffix $ init n
    --take while needle is equal to infinite circular array and output length
    outBufferLen = length . takeWhile id  $ zipWith (==) needle $ cycle needleMatch 
    d = length needleMatch


--Match is found in 2 passes: first search for match in passed buffer window (InBufferMatch), 
--then we check again if full needle is circular list with a period of found match length (OutBufferMatch)
--so we could encode with len > dist
--Then we skip 1 byte and see if we could get better result, compare that and
--emit optimal LzEntry. 
encodeLz :: [Word8] -> [LzEntry]
encodeLz = encodeLz' []
  where
  encodeLz' _ [] = [Refer 1 0] -- end of stream code (l == 0 (zero based), d == 0)
  encodeLz' buffer input@(i:is)
    | l < 2  = emitLiteral
    | l2 > l = Liter i : Refer l2 d2 : encodeLz' (buffer ++ take (l2+1) input) (drop (l2+1) input) --skip one literal
    | otherwise =  Refer l d : encodeLz' (buffer ++ take l input) (drop l input)
    where (l, d)   = findLzMatch input buffer
          (l2, d2) = findLzMatch is (buffer ++ [i]) --search for a longer match at the next input byte (LZ non greedy string parse)
          findLzMatch needle haystack = max (findInBufferMatch (Bs.pack needle) (Bs.pack haystack)) (findOutBufferMatch needle haystack)
          emitLiteral = Liter i : encodeLz' (buffer ++ [i]) is --refer takes 2 bytes, match with 1 byte len encoded as liter



--get based on lz entries list, calculate flags list, then serialize each entry,
--and then insert flag bytes in the middle of each 8 LzEntry chunk and concat
writeLzEntries :: [LzEntry] -> [Word8]
writeLzEntries xs = concat $ mergeLists (chunksOf 1 (getLzFlags rs)) (serialLzEntries rs) 
  where
  rs = concatMap entryLengthBreakdown xs --breakdown each LzEntry on length max of 0x11
  serialLzEntries ys = map (concatMap writeLzEntry) $ chunksOf 8 ys
  writeLzEntry (Liter i) = [i] --serialization of one LzEntry
  writeLzEntry (Refer l d) 
    | l <= 0x10 = [b1, b2] --control bytes are XY ZZ, where Y is 4bit length(zero-based), XZZ - 24 bit distance.
    | otherwise = [fromIntegral ((d .&. 0xF00) `shiftR` 4), b2 , fromIntegral (l-0x11)] --extended length case
    where 
      b1 = fromIntegral $ ((d .&. 0xF00) `shiftR` 4) + (l-1).&. 0xF --l is zero based
      b2 = fromIntegral $ d .&. 0xFF

mergeLists :: [a] -> [a] -> [a]
mergeLists [] ys = ys
mergeLists (x:xs) ys = x:mergeLists ys xs --awesome

--length can be encoded only by 8 bits + immediate 0x10. zero based. Max length is 0x110:
entryLengthBreakdown :: LzEntry -> [LzEntry]
entryLengthBreakdown (Liter i) = [Liter i]
entryLengthBreakdown (Refer l d) = if l > 0x110 then Refer 0x110 d : entryLengthBreakdown (Refer (l-0x110) d) else [Refer l d]

--convert Liter/Refer to 8-aligned list of Bools, then convert each chunk to
--byte
getLzFlags :: [LzEntry] -> [Word8]
getLzFlags xs = map bitsToByte $ chunksOf 8 $ boolList ++ dummy
  where
    modulo = (length boolList) `mod` 8
    dummy
      | modulo == 0 = []
      | otherwise =  replicate (8 - modulo) False --dummy list to append at the end
    boolList = map getLzFlags' xs
    getLzFlags' (Liter _ ) = False
    getLzFlags' (Refer _ _ ) = True 
    bitsToByte = foldl (\by bi -> by*2 + (if bi then 1 else 0)) 0


{-
main :: IO()
main = do
  {-
      input <- Bs.readFile "0171 - Golden Sun (UE).gba"
      let inputU8 =  drop 0xf38bc $ Bs.unpack input
          
      --Bs.writeFile "decoded.bin" $ Bs.pack $ (take 0x5A) $ decodeLz (readLzEntries'[] inputU8) []
      --print $ decodeLz  (readLzEntries' [] inputU8) []
      --print $ readLzEntries' [] inputInt
      --print $ filter (<=0) $ logger (readLzEntries'[] inputU8) 0
      --}
      input <- Bs.readFile "plainMaster.bin"
      --input <- Bs.readFile "encoded.bin"
      let inputU8 = Bs.unpack input
      --print $ encodeLz inputU8
      Bs.writeFile "encoded.bin" . Bs.pack . writeLzEntries $ encodeLz inputU8
      --Bs.writeFile "decoded2.bin" $ Bs.pack $ decodeLz $ readLzEntries inputU8
-}
