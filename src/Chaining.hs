{-# LANGUAGE OverloadedStrings #-}
module Chaining
  where
import Codec.Xlsx
import Codec.Xlsx.Formatted
import Data.Map.Lazy (Map)
import Control.Lens (set)
import WriteXLSX (jsonToFormattedCellMap, jjj, jjj2)
import           Data.Time.Clock.POSIX     (getPOSIXTime)
import qualified Data.ByteString.Lazy      as L

type FormattedCellMap = Map (Int, Int) FormattedCell

emptyWorksheet :: Worksheet
emptyWorksheet = def

chaining :: [FormattedCellMap] -> (StyleSheet, [Worksheet])
chaining fcellmaps =
  case length fcellmaps of
    1 -> (stylesheet, [ws])
      where ws = set wsMerges (formattedMerges frmt) $
                   set wsCells (formattedCellMap frmt) emptyWorksheet
            stylesheet = formattedStyleSheet frmt
            frmt = formatted (head fcellmaps) minimalStyleSheet
    _ -> (stylesheet, snd previous ++ [ws])
      where ws = set wsMerges (formattedMerges frmt) $
                   set wsCells (formattedCellMap frmt) emptyWorksheet
            stylesheet = formattedStyleSheet frmt
            frmt = formatted fcellmap (fst previous)
            previous = chaining (take (l-1) fcellmaps)
            fcellmap = last fcellmaps
            l = length fcellmaps

fcellmap1, fcellmap2 :: FormattedCellMap
fcellmap1 = jsonToFormattedCellMap jjj
fcellmap2 = jsonToFormattedCellMap jjj2

emptyXlsx :: Xlsx
emptyXlsx = def

test :: IO ()
test = do
  let (stylesheet, worksheets) = chaining [fcellmap1, fcellmap2]
  ct <- getPOSIXTime
  let xlsx = set xlStyles (renderStyleSheet stylesheet) $
               set xlSheets [("Sheet1", worksheets !! 0), ("Sheet2", worksheets !! 1)] emptyXlsx
  L.writeFile "testTwoSheets.xlsx" (fromXlsx ct xlsx)
