-- {-# LANGUAGE OverloadedStrings #-}
module JSONtoCellMap
  where
import           Codec.Xlsx
import           Codec.Xlsx.Formatted
-- import           Data.Aeson               (decode)
import           JSONtoCellMap.Internal
import           JSONtoCellMap.Types
-- import qualified Data.HashMap.Lazy         as DHM
-- import           Data.Map.Lazy             (Map)
-- import qualified Data.Map.Lazy             as DM
import           Control.Lens           (set)
-- import           Data.ByteString.Lazy.UTF8 (fromString)
import           Data.Maybe             (fromMaybe)

-- for testing
jj :: String
jj = "{\"A1\":{\"value\":2,\"format\":{\"numberFormat\":\"Nf2Decimal\",\"font\":{\"bold\":true}}},\"B2\":{\"value\":1000,\"format\":{\"numberFormat\":\"yyyy-mm-dd;@\"}},\"A3\":{\"value\":\"abc\",\"format\":{\"font\":{\"family\":\"Script\",\"name\":\"Courier\"}}}}"
-- {
--   "A1": {
--     "value": 2,
--     "format": {
--       "numberFormat": "Nf2Decimal",
--       "font": {
--         "bold": true
--       }
--     }
--   },
--   "B2": {
--     "value": 1000,
--     "format": {
--       "numberFormat": "yyyy-mm-dd;@"
--     }
--   },
--   "A3": {
--     "value": "abc",
--     "format": {
--       "font": {
--         "family": "Script",
--         "name": "Courier"
--       }
--     }
--   }
-- }
-- -- Decode
-- x :: SimpleCellMap
-- x = fromJust $ decode (fromString jj)
-- -- get a cell
-- cellA1 :: SimpleCell
-- cellA1 = fromJust $ DHM.lookup "A1" x
-- -- Convert to Map (Int, Int) SimpleCell
-- y :: Map Text SimpleCell
-- y = DM.fromList $ DHL.toList x
-- z :: Map (Int, Int) SimpleCell
-- z = DM.mapKeys (\x -> fromJust $ fromSingleCellRef CellRef {unCellRef = x}) y
-- -- Convert to FormattedCellMap
-- formattedcellmap :: Map (Int, Int) FormattedCell
-- formattedcellmap = DM.map simpleCellToFormattedCell z

simpleCellToFormattedCell :: SimpleCell -> FormattedCell
simpleCellToFormattedCell scell =
  set formattedCell cell (set formattedFormat f emptyFormattedCell)
  where cell = set cellValue (valueToCellValue (value scell)) emptyCell
        f = if formatscell == emptySimpleFormat
              then
                emptyFormat
              else
                set formatFont (simpleFontToFont $ font formatscell) $
                set formatNumberFormat
                  (textToNumberFormat (numberFormat formatscell))
                   emptyFormat
            where formatscell = fromMaybe emptySimpleFormat (format scell)

-- -- Get CellMap, [Range] and StyleSheet
-- almostDone :: Formatted
-- almostDone = formatted formattedcellmap minimalStyleSheet
-- ws :: Worksheet
-- ws = set wsMerges (formattedMerges almostDone) $
--        set wsCells (formattedCellMap almostDone) emptyWorksheet
-- xlsx :: Xlsx
-- xlsx = set xlStyles (renderStyleSheet (formattedStyleSheet almostDone)) $
--          set xlSheets [("Sheet1", ws)] emptyXlsx
