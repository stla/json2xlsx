{-# LANGUAGE OverloadedStrings #-}
module WriteXLSX
  where
import           Codec.Xlsx
import           Codec.Xlsx.Formatted
import           Control.Lens              (set)
import           Data.Aeson                (decode)
import qualified Data.ByteString.Lazy      as L
import           Data.ByteString.Lazy.UTF8 (fromString)
-- import qualified Data.HashMap.Lazy         as DHM
import qualified Data.Map.Lazy             as DM
import           Data.Maybe                (fromJust)
-- import           Data.Text                 (Text)
import           Data.Text                 (Text)
import           Data.Time.Clock.POSIX     (getPOSIXTime)
import           JSONtoCellMap             (jsonToFormattedCellMap,
                                            simpleCellMapToFormattedCellMap)
import           JSONtoCellMap.Types       (SimpleCellMap)
import           MakeWorksheets            (makeWorksheets)

jjj,jjj2,jjj3,jjj4,jjj5,jjj6 :: String
jjj = "{\"A1\":{\"value\":2,\"format\":{\"numberFormat\":\"Nf2Decimal\",\"font\":{\"bold\":true}}},\"B2\":{\"value\":1000,\"format\":{\"numberFormat\":\"yyyy-mm-dd;@\"}},\"A3\":{\"value\":\"abc\",\"format\":{\"font\":{\"family\":\"Script\",\"name\":\"Courier\"}}}}"
jjj2 = "{\"A1\":{\"value\":3,\"format\":{\"numberFormat\":\"Nf2Decimal\"}}}"
jjj3 = "{\"A1\":{\"format\":{\"numberFormat\":\"Nf2Decimal\"}}}" -- nope
jjj4 = "{\"A1\":{\"value\":null,\"format\":{\"numberFormat\":\"Nf2Decimal\"}}}"
jjj5 = "{\"Sheet1\":{\"A1\":{\"value\":9,\"format\":{\"numberFormat\":\"Nf2Decimal\"}}},\"Sheet2\":{\"A1\":{\"value\":2,\"format\":{\"numberFormat\":\"Nf2Decimal\",\"font\":{\"bold\":true,\"color\":\"FF00FF00\"}}},\"B2\":{\"value\":1000,\"format\":{\"numberFormat\":\"yyyy-mm-dd;@\"}},\"A3\":{\"value\":\"abc\",\"format\":{\"font\":{\"family\":\"Script\",\"name\":\"Courier\"}}}}}"
jjj6 = "{\"Sheet1\":{\"A1\":{\"value\":9,\"format\":{\"numberFormat\":\"Nf2Decimal\",\"font\":{\"color\":\"green\"}},\"comment\":\"hi\"}}}"

test5 = decode (fromString jjj5) :: Maybe (DM.Map Text SimpleCellMap)

emptyWorksheet :: Worksheet
emptyWorksheet = def
emptyXlsx :: Xlsx
emptyXlsx = def


jsonToWorksheetStylesheet :: String -> (Worksheet, StyleSheet)
jsonToWorksheetStylesheet jsonString =
  (ws, stylesheet)
  where ws = set wsMerges (formattedMerges frmt) $
               set wsCells (formattedCellMap frmt) emptyWorksheet
        stylesheet = formattedStyleSheet frmt
        frmt = formatted w minimalStyleSheet
        w = jsonToFormattedCellMap jsonString

writeSingleWorksheet :: String -> FilePath -> IO()
writeSingleWorksheet jsonString outfile = do
  ct <- getPOSIXTime
  let (ws, stylesheet) = jsonToWorksheetStylesheet jsonString
      xlsx = set xlStyles (renderStyleSheet stylesheet) $
               set xlSheets [("Sheet1", ws)] emptyXlsx
  L.writeFile outfile (fromXlsx ct xlsx)

test :: IO()
test = writeSingleWorksheet jjj "test.xlsx"

writeXlsx :: String -> FilePath -> IO()
writeXlsx jsonString outfile = do
  let jsonWorksheets = fromJust
           (decode (fromString jsonString) :: Maybe (DM.Map Text SimpleCellMap))
      sheetnames = DM.keys jsonWorksheets
      simplecellmaps = map snd $ DM.toList jsonWorksheets
      fcellmaps = map simpleCellMapToFormattedCellMap simplecellmaps
      (stylesheet, worksheets) = makeWorksheets fcellmaps
      namedWorksheets = zip sheetnames worksheets
      xlsx = set xlStyles (renderStyleSheet stylesheet) $
               set xlSheets namedWorksheets emptyXlsx
  ct <- getPOSIXTime
  L.writeFile outfile (fromXlsx ct xlsx)

test2 :: IO()
test2 = writeXlsx jjj6 "test3.xlsx"
