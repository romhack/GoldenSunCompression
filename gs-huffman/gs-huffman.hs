module Main where

import Codec
import System.Exit
import System.Environment
import System.Console.GetOpt
import Control.Monad.Error
import qualified Data.ByteString.Lazy as Bs
import Data.Binary.Get
import Text.Printf
import Data.Bits
import Data.Int
import Data.Maybe

----------------------------------------Command line parse part----------------------------------

data Action = Decode | Batch | Encode | NoAction deriving (Show, Eq)
data Options = Options
              {optHelp :: Bool
              ,optVersion :: Bool
              ,optAction :: Action
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
usage = usageInfo "Usage: gs-huffman [-d | -b | -e] file_name [offset]" options

options :: [OptDescr (Options -> Options)]
options =
	[ Option "d"  ["decode"]  (NoArg (\opts -> opts {optAction = Decode}))  "decode message by index from ROM. -d <file_name tree_ptrs_offset text_ptrs_offset message_index>"
        , Option "b"  ["batch"]   (NoArg (\opts -> opts {optAction = Batch}))   "batch decode messages from ROM. -b <file_name tree_ptrs_offset text_ptrs_offset message_count>"
        , Option "o"  ["output"] (OptArg ((\f opts -> opts {optOutput = Just f}) . fromMaybe "output.bin") "FILE") "output to binary FILE" 
	, Option "e"  ["encode"]  (NoArg (\opts -> opts {optAction = Encode}))  "encode from raw binary. -e <file_name trees_offset tree_prs_offset>"
        , Option "h?" ["help"]    (NoArg (\opts -> opts { optHelp = True }))   "show help."
        , Option "v"  ["version"] (NoArg (\opts -> opts { optVersion = True })) "show version number."
	]


deforOpts :: [String] -> IO (Options, [String])
deforOpts argv = 
  case getOpt Permute options argv of
    (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
    (_,_,errs) -> ioError (userError (concat errs ++ usage))

----------------------------------------------Main------------------------------------------------------

get2Ptrs :: Get (Int, Int) --get 2 ptrs in tuple and convert them to ROM offset
get2Ptrs = do
  fs <- getWord32le
  sn <- getWord32le
  return (fromIntegral (fs .&. 0x07FFFFFF), fromIntegral  (sn .&. 0x07FFFFFF)) --convert ptr value to ROM offset


main :: IO()
main = do
  argv <- getArgs
  (opts, nonOpts) <- deforOpts argv
  when (optVersion opts) $ do
    putStrLn "gs-huffman. GBA Golden Sun text utility. Version 0.1"
    exitSuccess
  when (optHelp opts) $ do
    putStrLn usage
    exitSuccess
  let action = optAction opts
  case action of 

    Decode -> do --decode one message by index
      when (length nonOpts /= 4) $ do
        putStrLn "Supply exactly one file name, two offsets for tree and text pointers and message index"
        putStrLn usage
        exitFailure
      let [fileName, sTreePtrsOffset, sTextPtrTable, sMIndex] = nonOpts
      input <-  Bs.readFile fileName
      let 
        treePtrsOffset = (read sTreePtrsOffset) :: Int
        textPtrTable = (read sTextPtrTable) :: Int64
        mIndex = (read sMIndex) :: Int
        decoded = decodeMsg input treePtrsOffset textPtrTable mIndex
      case optOutput opts of
         Just name -> Bs.writeFile name $ Bs.pack decoded
         Nothing -> putStrLn $ prettyPrintBytes decoded
      
    Batch -> do --batch decode messages
      when (length nonOpts /= 4) $ do
        putStrLn "Supply exactly one file name, two offsets for tree and text pointers and index of last message to decode"
        putStrLn usage
        exitFailure
      let [fileName, sTreePtrsOffset, sTextPtrTable, sMCount] = nonOpts
      input <-  Bs.readFile fileName
      let 
        treePtrsOffset = (read sTreePtrsOffset) :: Int
        textPtrTable = (read sTextPtrTable) :: Int64
        mCount = (read sMCount) :: Int
        decoded = decodeBatch input treePtrsOffset textPtrTable mCount
      case optOutput opts of
         Just name -> Bs.writeFile name $ Bs.pack decoded
         Nothing -> putStr $ prettyPrintBytes decoded
        
    Encode -> do --encoding
      when (length nonOpts /= 3) $ do
        putStrLn "Supply exactly one file name to encode, original trees start offset and tree pointers offset"
        putStrLn usage
        exitFailure
      let [fileName, sTreeStartOffset, sTreePtrsOffset] = nonOpts
      input <- Bs.readFile fileName
      let treePtrsOffset = read sTreePtrsOffset
          treeStartOffset = read sTreeStartOffset
          [trees, text, textPtrs] = getEncodedFiles input treeStartOffset treePtrsOffset
      Bs.writeFile "trees.bin" trees
      Bs.writeFile "text.bin" text
      Bs.writeFile "pointers.bin" textPtrs

    NoAction -> do
      putStrLn "Supply action flag"
      putStrLn usage
      exitFailure 
