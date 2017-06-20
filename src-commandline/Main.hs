module Main
  where
import           Data.Monoid          ((<>))
import qualified Data.ByteString      as B
import           JSONtoXLSX
import           Options.Applicative
import           Data.ByteString.UTF8 (toString)
import           Data.Maybe
import           System.Directory     (doesFileExist)

getJSON :: String -> IO String
getJSON json = do
  isFile <- doesFileExist json
  if isFile then
    B.readFile json >>= (return . toString)
  else return json

data Arguments = Arguments
  {
    jsonCells  :: Maybe String
  , jsonImages :: Maybe String
  , file       :: FilePath
  }

writeXLSX :: Arguments -> IO()
writeXLSX (Arguments jsonCells jsonImages file) = do
  let _json1 = fromMaybe "{}" jsonCells
      _json2 = fromMaybe "{}" jsonImages
  json1 <- getJSON _json1
  json2 <- getJSON _json2
  writeXlsx5 json1 json2 file

run :: Parser Arguments
run = Arguments
     <$> optional (strOption
           ( long "cells"
          <> short 'c'
          <> help "JSON string for cells" ))
     <*> optional (strOption
           ( long "images"
          <> short 'i'
          <> help "JSON string for images" ))
     <*> strOption
           ( long "output"
          <> short 'o'
          <> metavar "OUTPUT"
          <> help "Output file" )

main :: IO()
main = execParser opts >>= writeXLSX
 where
   opts = info (helper <*> run)
     ( fullDesc
    <> progDesc "Write a XLSX worksheet from JSON strings"
    <> header "writexlsx"
    <> footer "Author: Stéphane Laurent" )
