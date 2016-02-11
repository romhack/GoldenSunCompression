module Main where

import           Codec
import           Control.Monad.Except
import           Data.Binary.Get
import qualified Data.ByteString.Lazy  as Bs
import           Data.Version
import           Paths_gsLzssByteFlags (version)
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
              }
              deriving (Show)
defaultOptions :: Options
defaultOptions = Options
                  {optHelp = False
                  ,optVersion = False
                  ,optAction = NoAction
                  }

usage :: String
usage = usageInfo "Usage: gsLzssByteFlags [-d | -b | -e] file_name [offset]" options

options :: [OptDescr (Options -> Options)]
options =
  [ Option "d"  ["decode"]  (NoArg (\opts -> opts {optAction = Decode}))  "decode from ROM. -d <file_name offset>"
  , Option "b"  ["batch"]   (NoArg (\opts -> opts {optAction = Batch}))   "batch decode from ROM. -b <file_name table_start_offset entries_count output_folder>"
  , Option "e"  ["encode"]  (NoArg (\opts -> opts {optAction = Encode}))  "encode from raw binary. -e <file_name>"
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
    putStrLn $ "gsLzssByteFlags. Utility for GBA Golden Sun LZSS scheme with 8 flags in byte.\nVersion:" ++ showVersion version
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
      let decodeBlock = Bs.drop (read sOffset) input
          (plainBlock, originalSize) = decode decodeBlock
      putStrLn $ printf "Decoded. Original encoded block size is 0x%X bytes" originalSize
      Bs.writeFile "decoded.gba" plainBlock -- .gba extension for tile editor default codec open


    Batch -> do --batch unpack
      when (length nonOpts /= 4) $ do
        putStrLn "Supply file name, start offset of table, entries count and output directory name"
        putStrLn usage
        exitFailure
      let [fileName, sStartOffset, sCount, dirName] = nonOpts
      input <- Bs.readFile fileName
      createDirectory dirName
      let base = read sStartOffset
          getCountWords = replicateM (read sCount) getWord16le
          pointers = zip [base, base+2 .. ] $ map fromIntegral (runGet getCountWords (Bs.drop base input))
          decodePointer (ptrOffset, ptr) = do
            let  blockOffset = base + ptr
                 (plainBlock, originalSize) = decode (Bs.drop blockOffset input)
            putStrLn $ printf "Decoding block @0x%X, pointer @0x%X, original encoded size 0x%X" blockOffset ptrOffset originalSize
            Bs.writeFile (printf "%s/0x%07X-p0x%07X-s0x%04X.gba" dirName blockOffset ptrOffset originalSize) plainBlock
      mapM_ decodePointer pointers

    Encode -> do --encoding
      when (length nonOpts /= 1) $ do
        putStrLn "Supply exactly one file name to encode"
        putStrLn usage
        exitFailure
      let [fileName] = nonOpts
      input <- Bs.readFile fileName
      Bs.writeFile "encoded.bin" $ encode input
    NoAction -> do
      putStrLn "Supply action flag"
      putStrLn usage
      exitFailure
