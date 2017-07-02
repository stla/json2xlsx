{-# LANGUAGE OverloadedStrings #-}
module JSONtoXLSX.MakeWorksheets
  where
import           Codec.Xlsx
import           Codec.Xlsx.Formatted
import           Control.Lens               ((&), (.~))
import           Data.Map.Lazy              (Map)
import           Data.Text                  (Text)
import           JSONtoXLSX.SheetProtection

type FormattedCellMap = Map (Int, Int) FormattedCell

-- makeWorksheets :: [FormattedCellMap] -> (StyleSheet, [Worksheet])
-- makeWorksheets = foldr formatSingle (minimalStyleSheet, [])
--   where
--     formatSingle fcells (ssheet, wss) =
--        let fmt = formatted fcells ssheet
--            ws = def & wsCells .~ formattedCellMap fmt
--                     & wsMerges .~ formattedMerges fmt
--        in (formattedStyleSheet fmt, ws : wss)

-- makeWorksheets :: [(FormattedCellMap, Maybe Drawing)] -> (StyleSheet, [Worksheet])
-- makeWorksheets =
--   foldr formatSingle (minimalStyleSheet, [])
--   where
--     formatSingle (fcells, drawing) (ssheet, wss) =
--        let fmt = formatted fcells ssheet
--            ws = def & wsCells   .~ formattedCellMap fmt
--                     & wsMerges  .~ formattedMerges fmt
--                     & wsDrawing .~ drawing
--        in (formattedStyleSheet fmt, ws : wss)
--
makeWorksheets :: [((FormattedCellMap, Maybe Drawing), Maybe Text)] ->
                                                       (StyleSheet, [Worksheet])
makeWorksheets =
  foldr formatSingle (minimalStyleSheet, [])
  where
    formatSingle ((fcells, drawing), password) (ssheet, wss) =
       let fmt = formatted fcells ssheet
           ws = def & wsCells   .~ formattedCellMap fmt
                    & wsMerges  .~ formattedMerges fmt
                    & wsDrawing .~ drawing
                    & wsProtection .~ sheetProtection password
       in (formattedStyleSheet fmt, ws : wss)
