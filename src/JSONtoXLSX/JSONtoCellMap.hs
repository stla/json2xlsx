module JSONtoXLSX.JSONtoCellMap
  where
import           Codec.Xlsx
import           Codec.Xlsx.Formatted
import           JSONtoXLSX.JSONtoCellMap.Conversions
import           JSONtoXLSX.JSONtoCellMap.Types
import           Control.Lens                         (set)
import qualified Data.HashMap.Lazy                    as HM
import qualified Data.Map.Lazy                        as M
import           Data.Maybe                           (fromJust, fromMaybe)

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
-- y = M.fromList $ DHL.toList x
-- z :: Map (Int, Int) SimpleCell
-- z = M.mapKeys (\x -> fromJust $ fromSingleCellRef CellRef {unCellRef = x}) y
-- -- Convert to FormattedCellMap
-- formattedcellmap :: Map (Int, Int) FormattedCell
-- formattedcellmap = M.map simpleCellToFormattedCell z

simpleCellToFormattedCell :: SimpleCell -> FormattedCell
simpleCellToFormattedCell scell =
  set formattedColSpan cs $
  set formattedRowSpan rs $
  set formattedCell cell $
    set formattedFormat f emptyFormattedCell
  where cell = set cellComment (cellCommentToComment (comment scell)) $
                set cellValue (valueToCellValue (value scell)) emptyCell
        cs = fromMaybe 1 (colspan scell)
        rs = fromMaybe 1 (rowspan scell)
        f = if formatscell == emptySimpleFormat
              then
                emptyFormat
              else
                set formatFill (textToFill $ fill formatscell) $
                set formatFont (simpleFontToFont $ font formatscell) $
                set formatNumberFormat
                  (textToNumberFormat (numberFormat formatscell))
                   emptyFormat
            where formatscell = fromMaybe emptySimpleFormat (format scell)

simpleCellMapToFormattedCellMap :: SimpleCellMap -> FormattedCellMap
simpleCellMapToFormattedCellMap simplecellmap =
  M.map simpleCellToFormattedCell $
    M.mapKeys (\c -> fromJust $ fromSingleCellRef CellRef {unCellRef = c}) $
      M.fromList $ HM.toList simplecellmap
