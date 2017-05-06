{-# LANGUAGE OverloadedStrings #-}
module WriteXLSX
  where
import           Codec.Xlsx
import           Codec.Xlsx.Formatted
import           Control.Lens              (set)
import           Data.Aeson                (decode)
import qualified Data.ByteString.Lazy      as L
import           Data.ByteString.Lazy.UTF8 (fromString)
import qualified Data.HashMap.Lazy         as DHM
import qualified Data.Map.Lazy             as DM
import           Data.Maybe                (fromJust)
-- import           Data.Text                 (Text)
import           Data.Time.Clock.POSIX     (getPOSIXTime)
import           JSONtoCellMap             (simpleCellToFormattedCell)
import           JSONtoCellMap.Types       (SimpleCellMap)


jjj,jjj2,jjj3,jjj4 :: String
jjj = "{\"A1\":{\"value\":2,\"format\":{\"numberFormat\":\"Nf2Decimal\",\"font\":{\"bold\":true}}},\"B2\":{\"value\":1000,\"format\":{\"numberFormat\":\"yyyy-mm-dd;@\"}},\"A3\":{\"value\":\"abc\",\"format\":{\"font\":{\"family\":\"Script\",\"name\":\"Courier\"}}}}"
jjj2 = "{\"A1\":{\"value\":2,\"format\":{\"numberFormat\":\"Nf2Decimal\"}}}"
jjj3 = "{\"A1\":{\"format\":{\"numberFormat\":\"Nf2Decimal\"}}}" -- nope
jjj4 = "{\"A1\":{\"value\":null,\"format\":{\"numberFormat\":\"Nf2Decimal\"}}}"

emptyWorksheet :: Worksheet
emptyWorksheet = def
emptyXlsx :: Xlsx
emptyXlsx = def

writeXlsx :: String -> FilePath -> IO()
writeXlsx jsonString outfile = do
  ct <- getPOSIXTime
  let w = DM.map simpleCellToFormattedCell $
           DM.mapKeys (\c -> fromJust $ fromSingleCellRef CellRef {unCellRef = c}) $
             DM.fromList $ DHM.toList $
               fromJust (decode (fromString jsonString) :: Maybe SimpleCellMap)
  let frmt = formatted w minimalStyleSheet
  let ws = set wsMerges (formattedMerges frmt) $
            set wsCells (formattedCellMap frmt) emptyWorksheet
  let xlsx = set xlStyles (renderStyleSheet (formattedStyleSheet frmt)) $
               set xlSheets [("Sheet1", ws)] emptyXlsx
  L.writeFile outfile (fromXlsx ct xlsx)

test :: IO()
test = writeXlsx jjj "test.xlsx"
