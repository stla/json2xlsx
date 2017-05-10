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
import           JSONtoCellMap.Types       (SimpleCellMap, FormattedCellMap)
import Data.Text (Text)


jjj,jjj2,jjj3,jjj4 :: String
jjj = "{\"A1\":{\"value\":2,\"format\":{\"numberFormat\":\"Nf2Decimal\",\"font\":{\"bold\":true}}},\"B2\":{\"value\":1000,\"format\":{\"numberFormat\":\"yyyy-mm-dd;@\"}},\"A3\":{\"value\":\"abc\",\"format\":{\"font\":{\"family\":\"Script\",\"name\":\"Courier\"}}}}"
jjj2 = "{\"A1\":{\"value\":3,\"format\":{\"numberFormat\":\"Nf2Decimal\"}}}"
jjj3 = "{\"A1\":{\"format\":{\"numberFormat\":\"Nf2Decimal\"}}}" -- nope
jjj4 = "{\"A1\":{\"value\":null,\"format\":{\"numberFormat\":\"Nf2Decimal\"}}}"
jjj5 = "{\"Sheet1\":{\"A1\":{\"value\":null,\"format\":{\"numberFormat\":\"Nf2Decimal\"}}}}"

test5 = decode (fromString jjj5) :: Maybe (DM.Map Text SimpleCellMap)

emptyWorksheet :: Worksheet
emptyWorksheet = def
emptyXlsx :: Xlsx
emptyXlsx = def

jsonToFormattedCellMap :: String -> FormattedCellMap
jsonToFormattedCellMap jsonString =
  DM.map simpleCellToFormattedCell $
    DM.mapKeys (\c -> fromJust $ fromSingleCellRef CellRef {unCellRef = c}) $
      DM.fromList $ DHM.toList $
        fromJust (decode (fromString jsonString) :: Maybe SimpleCellMap)

jsonToWorksheetStylesheet :: String -> (Worksheet, StyleSheet)
jsonToWorksheetStylesheet jsonString =
  (ws, stylesheet)
  where ws = set wsMerges (formattedMerges frmt) $
               set wsCells (formattedCellMap frmt) emptyWorksheet
        stylesheet = formattedStyleSheet frmt
        frmt = formatted w minimalStyleSheet
        w = jsonToFormattedCellMap jsonString

writeXlsx :: String -> FilePath -> IO()
writeXlsx jsonString outfile = do
  ct <- getPOSIXTime
  let (ws, stylesheet) = jsonToWorksheetStylesheet jsonString
      xlsx = set xlStyles (renderStyleSheet stylesheet) $
               set xlSheets [("Sheet1", ws)] emptyXlsx
  L.writeFile outfile (fromXlsx ct xlsx)

-- comment faire si deux worksheet ?..
-- https://github.com/qrilka/xlsx/issues/83 : enchainer formatted

test :: IO()
test = writeXlsx jjj "test.xlsx"
