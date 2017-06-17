module Main
  where
import           Data.Monoid                   ((<>))
-- import qualified Data.Text                     as T
import           Options.Applicative
import           JSONtoXLSX
import qualified Data.ByteString.Lazy       as BL
-- import qualified Data.ByteString.Lazy.Char8 as L
import           Data.ByteString.Lazy.UTF8  (toString)
import           System.Directory           (doesFileExist)
import Data.Maybe

getJSON :: String -> IO String
getJSON json = do
  isFile <- doesFileExist json
  if isFile then
    BL.readFile json >>= (return . toString)
  else return json

data Arguments = Arguments
  {
    jsonCells  :: Maybe String
  , jsonImages :: Maybe String
  , file       :: FilePath
  }

writeXLSX :: Arguments -> IO()
writeXLSX (Arguments jsonCells jsonImages file) = do
  let json1 = fromMaybe "{}" jsonCells
      json2 = fromMaybe "{}" jsonImages
  writeXlsx3 json1 json2 file

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
