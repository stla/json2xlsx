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
import           Data.Map.Lazy             (Map)
import qualified Data.Map.Lazy             as M
import           Data.Maybe                (fromJust)
-- import           Data.Text                 (Text)
import           Data.Text                 (Text)
import           Data.Time.Clock.POSIX     (getPOSIXTime)
import qualified Data.Traversable          as T
import           JSONtoCellMap             (jsonToFormattedCellMap,
                                            simpleCellMapToFormattedCellMap)
import           JSONtoCellMap.Types       (SimpleCellMap)
import           MakeWorksheets            (makeWorksheets, makeWorksheets2)
import           Pictures.DrawingPictures  (drawingPictures)
import           Pictures.PictureData      (PictureData (..))

jjj,jjj2,jjj3,jjj4,jjj5,jjj6 :: String
jjj = "{\"A1\":{\"value\":2,\"format\":{\"numberFormat\":\"2Decimal\",\"font\":{\"bold\":true}}},\"B2\":{\"value\":1000,\"format\":{\"numberFormat\":\"yyyy-mm-dd;@\"}},\"A3\":{\"value\":\"abc\",\"format\":{\"font\":{\"family\":\"Script\",\"name\":\"Courier\"}}}}"
jjj2 = "{\"A1\":{\"value\":3,\"format\":{\"numberFormat\":\"2Decimal\"}}}"
jjj3 = "{\"A1\":{\"format\":{\"numberFormat\":\"2Decimal\"}}}" -- nope
jjj4 = "{\"A1\":{\"value\":null,\"format\":{\"numberFormat\":\"2Decimal\"}}}"
jjj5 = "{\"Sheet1\":{\"A1\":{\"value\":9,\"format\":{\"numberFormat\":\"2Decimal\"}}},\"Sheet2\":{\"A1\":{\"value\":2,\"format\":{\"numberFormat\":\"2Decimal\",\"font\":{\"bold\":true,\"color\":\"FF00FF00\"}}},\"B2\":{\"value\":1000,\"format\":{\"numberFormat\":\"yyyy-mm-dd;@\"}},\"A3\":{\"value\":\"abc\",\"format\":{\"font\":{\"family\":\"Script\",\"name\":\"Courier\"}}}}}"
jjj6 = "{\"Sheet1\":{\"A1\":{\"value\":9,\"format\":{\"numberFormat\":\"2Decimal\",\"font\":{\"color\":\"green\"}},\"comment\":\"hi\"}}}"

test5 = decode (fromString jjj5) :: Maybe (Map Text SimpleCellMap)

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
           (decode (fromString jsonString) :: Maybe (Map Text SimpleCellMap))
      sheetnames = M.keys jsonWorksheets
      simplecellmaps = map snd $ M.toList jsonWorksheets
      fcellmaps = map simpleCellMapToFormattedCellMap simplecellmaps
      (stylesheet, worksheets) = makeWorksheets fcellmaps
      namedWorksheets = zip sheetnames worksheets
      xlsx = set xlStyles (renderStyleSheet stylesheet) $
               set xlSheets namedWorksheets emptyXlsx
  ct <- getPOSIXTime
  L.writeFile outfile (fromXlsx ct xlsx)

test2 :: IO()
test2 = writeXlsx jjj6 "test3.xlsx"

writeXlsx2 :: String -> String -> FilePath -> IO()
writeXlsx2 jsonCells jsonImages outfile = do
  let sheets_cells = fromJust
           (decode (fromString jsonCells) :: Maybe (Map Text SimpleCellMap))
      sheets_images = fromJust
           (decode (fromString jsonImages) :: Maybe (Map Text [PictureData]))
      simplecellmaps = map snd $ M.toList sheets_cells
      fcellmaps = map simpleCellMapToFormattedCellMap simplecellmaps
      pictureDatas = map snd $ M.toList sheets_images
  drawings <- mapM drawingPictures pictureDatas
  -- suppose les mêmes sheet
  let (stylesheet, worksheets) = makeWorksheets2 $
                                   zip fcellmaps (map Just drawings)
      sheetnames = M.keys sheets_cells
      namedWorksheets = zip sheetnames worksheets
      xlsx = set xlStyles (renderStyleSheet stylesheet) $
               set xlSheets namedWorksheets emptyXlsx
  ct <- getPOSIXTime
  L.writeFile outfile (fromXlsx ct xlsx)

jjpics = "{\"Sheet1\":[{\"file\":\"image.png\",\"left\":2,\"top\":3,\"width\":200,\"height\":300}]}"

thetest :: IO()
thetest = writeXlsx2 jjj6 jjpics "thetest.xlsx"

--
writeXlsx3 :: String -> String -> FilePath -> IO()
writeXlsx3 jsonCells jsonImages outfile = do
  let sheets_cells = fromJust
           (decode (fromString jsonCells) :: Maybe (Map Text SimpleCellMap))
      -- Map Text FormattedCellMap
      sheets_fcells = M.map simpleCellMapToFormattedCellMap sheets_cells
      sheets_images = fromJust
           (decode (fromString jsonImages) :: Maybe (Map Text [PictureData]))
  -- Map Text Drawing
  sheets_drawings <- T.mapM drawingPictures sheets_images
  -- merge => Map Text (FormattedCellMap, Maybe Drawing)
  let mergedMap = M.mergeWithKey
                    (\k x y -> Just (x, Just y))
                    (M.map (\x -> (x, Nothing)))
                    (M.map (\y -> (M.empty, Just y)))
                    sheets_fcells sheets_drawings
      (stylesheet, worksheets) = makeWorksheets2 $ map snd $ M.toList mergedMap
      sheetnames = M.keys mergedMap
      namedWorksheets = zip sheetnames worksheets
      xlsx = set xlStyles (renderStyleSheet stylesheet) $
               set xlSheets namedWorksheets emptyXlsx
  ct <- getPOSIXTime
  L.writeFile outfile (fromXlsx ct xlsx)

thetest2 :: IO()
thetest2 = writeXlsx3 jjj6 jjpics "thetest2.xlsx"

jjpics2 = "{\"Sheet2\":[{\"file\":\"image.png\",\"left\":2,\"top\":3,\"width\":200,\"height\":300}]}"

thethetest :: IO()
thethetest = writeXlsx3 jjj6 jjpics2 "thethetest.xlsx"

-- tester plusieurs images (le id !)
jjpics3 = "{\"Sheet1\":[{\"file\":\"image.png\",\"left\":3,\"top\":3,\"width\":200,\"height\":300}],\"Sheet2\":[{\"file\":\"image.png\",\"left\":2,\"top\":3,\"width\":200,\"height\":300},{\"file\":\"image2.png\",\"left\":12,\"top\":3,\"width\":200,\"height\":300}]}"
thethetest2 :: IO()
thethetest2 = writeXlsx3 jjj6 jjpics3 "thethetest2.xlsx"
-- après différents id ça fait toujours repair... attends non ils ne sont pas différents là
-- OK c'est le filename !

jjpics4 = "{\"Sheet2\":[{\"file\":\"image.png\",\"left\":2,\"top\":3,\"width\":200,\"height\":300},{\"file\":\"image2.png\",\"left\":12,\"top\":3,\"width\":200,\"height\":300}]}"
thethetest3 :: IO()
thethetest3 = writeXlsx3 jjj6 jjpics4 "thethetest3.xlsx"

-- this works:
-- writeXlsx3 "{}" json "out.xlsx"
-- writeXlsx3 json "{}" "out.xlsx"
