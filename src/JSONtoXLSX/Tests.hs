{-# LANGUAGE OverloadedStrings #-}
module JSONtoXLSX.Tests
  where
import           Codec.Xlsx
-- import           Codec.Xlsx.Formatted
import           Control.Lens              (set, (&), (.~))
import qualified Data.ByteString.Lazy      as L
-- import           Data.Map.Lazy             (Map)
import           Data.Time.Clock.POSIX     (getPOSIXTime)
import           JSONtoXLSX.JSONtoCellMap  (jsonToFormattedCellMap)
import           JSONtoXLSX.MakeWorksheets


jjj,jjj2 :: String
jjj = "{\"A1\":{\"value\":2,\"format\":{\"numberFormat\":\"Nf2Decimal\",\"font\":{\"bold\":true}}},\"B2\":{\"value\":1000,\"format\":{\"numberFormat\":\"yyyy-mm-dd;@\"}},\"A3\":{\"value\":\"abc\",\"format\":{\"font\":{\"family\":\"Script\",\"name\":\"Courier\"}}}}"
jjj2 = "{\"A1\":{\"value\":3,\"format\":{\"numberFormat\":\"Nf2Decimal\"}}}"

fcellmap1, fcellmap2 :: FormattedCellMap
fcellmap1 = jsonToFormattedCellMap jjj
fcellmap2 = jsonToFormattedCellMap jjj2

emptyXlsx :: Xlsx
emptyXlsx = def

test :: IO ()
test = do
  let (stylesheet, worksheets) = makeWorksheets [fcellmap1, fcellmap2]
  ct <- getPOSIXTime
  let xlsx = set xlStyles (renderStyleSheet stylesheet) $
               set xlSheets [("Sheet1", head worksheets), ("Sheet2", worksheets !! 1)] emptyXlsx
  L.writeFile "testTwoSheets.xlsx" (fromXlsx ct xlsx)
