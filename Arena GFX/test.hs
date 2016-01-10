import qualified Data.ByteString.Lazy as Bs
import Data.ByteString.Lazy.Search
windowSize = 0xFFF --4096 as distance can be encoded only by 12 bits (for 0 and 1st LZSS schemes)


getLastMatch :: Bs.ByteString -> Bs.ByteString -> (Int, Int) --haystack and needles are reversed 
getLastMatch needle haystack = let
                                 needleStrict = strictify needle
                                 (afterMatch, beforeAndMatch) = breakOn needleStrict haystack
                                 nLen = fromIntegral $ Bs.length needle :: Int
                                 dLen = fromIntegral $ Bs.length afterMatch :: Int
                                in
                                if  Bs.null beforeAndMatch then (0,0)
                                                       else (nLen, dLen + nLen)


getLongestMatch :: Bs.ByteString -> Bs.ByteString -> (Int, Int)
getLongestMatch needle haystack = 
  let 
    ns = map Bs.reverse $ tail $ Bs.inits needle
    h = Bs.take (fromIntegral windowSize) $ Bs.reverse haystack 
    refs = takeWhile (\n -> (getLastMatch n h) /= (0,0)) ns
  in
    getLastMatch (last refs) h

main :: IO()
main = do
  let
    needle = Bs.pack [10,11]
    haystack = Bs.pack [1,2,3,4,5,6,3,4,5,6,7,8,9]

  print $ getLongestMatch needle haystack
