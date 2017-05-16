{-# LANGUAGE OverloadedStrings #-}
module MakeWorksheets
  where
import Codec.Xlsx
import Codec.Xlsx.Formatted
import Data.Map.Lazy (Map)
import Control.Lens (set, (.~), (&))
import JSONtoCellMap (jsonToFormattedCellMap)
import           Data.Time.Clock.POSIX     (getPOSIXTime)
import qualified Data.ByteString.Lazy      as L

type FormattedCellMap = Map (Int, Int) FormattedCell

emptyWorksheet :: Worksheet
emptyWorksheet = def

makeWorksheets :: [FormattedCellMap] -> (StyleSheet, [Worksheet])
makeWorksheets = foldr formatSingle (minimalStyleSheet, []) 
  where
    formatSingle fcells (ssheet, wss) =
       let fmt = formatted fcells ssheet
           ws = def & wsCells .~ formattedCellMap fmt
                    & wsMerges .~ formattedMerges fmt
       in (formattedStyleSheet fmt, ws : wss)

-- makeWorksheets fcellmaps =
--   case length fcellmaps of
--     1 -> (stylesheet, [ws])
--       where ws = set wsMerges (formattedMerges frmt) $
--                    set wsCells (formattedCellMap frmt) emptyWorksheet
--             stylesheet = formattedStyleSheet frmt
--             frmt = formatted (head fcellmaps) minimalStyleSheet
--     _ -> (stylesheet, snd previous ++ [ws])
--       where ws = set wsMerges (formattedMerges frmt) $
--                    set wsCells (formattedCellMap frmt) emptyWorksheet
--             stylesheet = formattedStyleSheet frmt
--             frmt = formatted fcellmap (fst previous)
--             previous = makeWorksheets (take (l-1) fcellmaps)
--             fcellmap = last fcellmaps
--             l = length fcellmaps

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
