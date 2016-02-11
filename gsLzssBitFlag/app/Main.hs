module Main where

import           Codec
import           Control.Monad.Except
import           Data.Binary.Get
import qualified Data.ByteString.Lazy  as Bs
import           Data.Maybe
import           Data.Version
import           Paths_gsLzssBitFlag   (version)
import           System.Console.GetOpt
import           System.Directory
import           System.Environment
import           System.Exit
import           Text.Printf

----------------------------------------Command line parse part----------------------------------

data Action = Decode | Batch | Encode | NoAction deriving (Show, Eq)
data Options = Options
              {optHelp    :: Bool
              ,optVersion :: Bool
              ,optAction  :: Action
               ,optOutput :: Maybe FilePath
              }
              deriving (Show)
defaultOptions :: Options
defaultOptions = Options
                  {optHelp = False
                  ,optVersion = False
                  ,optAction = NoAction
                  ,optOutput = Nothing
                  }

usage :: String
usage = usageInfo "Usage: gsLzssBitFlag [-d | -b | -e] file_name [offset]" options

options :: [OptDescr (Options -> Options)]
options =
 [ Option "d"  ["decode"]  (NoArg (\opts -> opts {optAction = Decode}))  "decode from ROM. -d <file_name offset>"
 , Option "b"  ["batch"]   (NoArg (\opts -> opts {optAction = Batch}))   "batch decode from ROM. -b <file_name table_start_offset entries_count output_folder>"
 , Option "e"  ["encode"]  (NoArg (\opts -> opts {optAction = Encode}))  "encode from raw binary. -e <file_name>"
 , Option "o"  ["output"]  (OptArg ((\f opts -> opts {optOutput = Just f}) . fromMaybe "output.bin") "FILE")
          "output to binary FILE"
 , Option "h?" ["help"]    (NoArg (\ opts -> opts { optHelp = True }))   "show help."
 , Option "v"  ["version"] (NoArg (\ opts -> opts { optVersion = True })) "show version number."
 ]


deforOpts :: [String] -> IO (Options, [String])
deforOpts argv =
  case getOpt Permute options argv of
    (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
    (_,_,errs) -> ioError (userError (concat errs ++ usage))

----------------------------------------------Main------------------------------------------------------

main :: IO()
main = do
  argv <- getArgs
  (opts, nonOpts) <- deforOpts argv
  when (optVersion opts) $ do
    putStrLn $ "gsLzssBitFlag. Utility for GBA Golden Sun LZSS scheme with flag in bitstream.\nVersion:" ++ showVersion version
    exitSuccess
  when (optHelp opts) $ do
    putStrLn usage
    exitSuccess
  let action = optAction opts
  case action of

    Decode -> do --decoding
      when (length nonOpts /= 2) $ do
        putStrLn "Supply exactly one file name and one offset for decoding"
        putStrLn usage
        exitFailure
      let [fileName, sOffset] = nonOpts
      input <-  Bs.readFile fileName
      let
        decodeBlock = Bs.drop (read sOffset) input
      if Bs.head decodeBlock /= 0 then putStrLn "Compression type byte is not 0 - decoding cancelled."
        else do
          let (plainBlock, originalSize, plainSize) = decode (Bs.tail decodeBlock)
              name = fromMaybe "decoded.gba" $ optOutput opts -- .gba extension for tile editor default codec open
          Bs.writeFile name plainBlock
          putStrLn $ printf "Decoded. Encoded block size was 0x%X bytes, decoded size is 0x%X bytes" originalSize plainSize



    Batch -> do --batch unpack
      when (length nonOpts /= 4) $ do
        putStrLn "Supply file name, start offset of table, entries count and output directory name"
        putStrLn usage
        exitFailure
      let [fileName, sStartOffset, sCount, dirName] = nonOpts
      input <- Bs.readFile fileName
      createDirectory dirName
      let base = read sStartOffset
          getCountWords = replicateM (read sCount) getWord32le
          pointers = zip [base, base+4 .. ] $ map fromIntegral (runGet getCountWords (Bs.drop base input))
          decodePointer (ptrOffset, ptr) = do
            let blockOffset = ptr - 0x08000000
                decodeBlock = Bs.drop blockOffset input
            if Bs.head decodeBlock /= 0 then putStrLn "Compression type byte is not 0 - decoding cancelled."
              else do
                let  (plainBlock, originalSize, plainSize) = decode (Bs.tail decodeBlock)
                Bs.writeFile (printf "%s/0x%07X-p0x%07X-s0x%04X.gba" dirName blockOffset ptrOffset originalSize) plainBlock
                putStrLn $ printf "Decoding block @0x%X, pointer @0x%X, original encoded size 0x%X" blockOffset ptrOffset originalSize
      mapM_ decodePointer pointers

    Encode -> do --encoding
      when (length nonOpts /= 1) $ do
        putStrLn "Supply exactly one file name to encode"
        putStrLn usage
        exitFailure
      let
        [fileName] = nonOpts
        outName = fromMaybe "encoded.bin" $ optOutput opts
      input <- Bs.readFile fileName
      Bs.writeFile outName $ Bs.cons 0 (encode input) --prepend zero as a bitFlag compression scheme
    NoAction -> do
      putStrLn "Supply action flag"
      putStrLn usage
      exitFailure
