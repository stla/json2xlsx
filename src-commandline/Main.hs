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
    jsonCells     :: Maybe String
  , jsonImages    :: Maybe String
  , jsonPasswords :: Maybe String
  , file          :: FilePath
  }

writeXLSX :: Arguments -> IO()
writeXLSX (Arguments jsonCells jsonImages jsonPasswords file) = do
  let _json1 = fromMaybe "{}" jsonCells
      _json2 = fromMaybe "{}" jsonImages
      _json3 = fromMaybe "{}" jsonPasswords
  json1 <- getJSON _json1
  json2 <- getJSON _json2
  json3 <- getJSON _json3
  writeXlsx6 json1 json2 json3 file

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
     <*> optional (strOption
           ( long "passwords"
          <> short 'p'
          <> help "JSON string for passwords" ))
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
