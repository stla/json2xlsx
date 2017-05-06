module Main
  where
import           Data.Monoid                   ((<>))
-- import qualified Data.Text                     as T
import           Options.Applicative
import           WriteXLSX

data Arguments = Arguments
  { json :: String
  , file :: FilePath }

writeXLSX :: Arguments -> IO()
writeXLSX (Arguments json file) =
  writeXlsx json file

run :: Parser Arguments
run = Arguments
     <$> strArgument
          ( metavar "JSON"
         <> help "JSON string" )
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
    <> progDesc "Write a XLSX sheet from a JSON string"
    <> header "writexlsx"
    <> footer "Author: Stéphane Laurent" )
