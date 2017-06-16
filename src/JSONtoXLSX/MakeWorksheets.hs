{-# LANGUAGE OverloadedStrings #-}
module JSONtoXLSX.MakeWorksheets
  where
import           Codec.Xlsx
import           Codec.Xlsx.Formatted
import           Control.Lens             ((&), (.~))
import           Data.Map.Lazy            (Map)
-- import           JSONtoXLSX.JSONtoCellMap (jsonToFormattedCellMap)

type FormattedCellMap = Map (Int, Int) FormattedCell

makeWorksheets :: [FormattedCellMap] -> (StyleSheet, [Worksheet])
makeWorksheets = foldr formatSingle (minimalStyleSheet, [])
  where
    formatSingle fcells (ssheet, wss) =
       let fmt = formatted fcells ssheet
           ws = def & wsCells .~ formattedCellMap fmt
                    & wsMerges .~ formattedMerges fmt
       in (formattedStyleSheet fmt, ws : wss)
--
makeWorksheets2 :: [(FormattedCellMap, Maybe Drawing)] -> (StyleSheet, [Worksheet])
makeWorksheets2 =
  foldr formatSingle (minimalStyleSheet, [])
  where
    formatSingle (fcells, drawing) (ssheet, wss) =
       let fmt = formatted fcells ssheet
           ws = def & wsCells   .~ formattedCellMap fmt
                    & wsMerges  .~ formattedMerges fmt
                    & wsDrawing .~ drawing
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
