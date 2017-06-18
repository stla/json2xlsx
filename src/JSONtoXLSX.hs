{-# LANGUAGE OverloadedStrings #-}
module JSONtoXLSX
  where
import           Codec.Xlsx
import           Codec.Xlsx.Formatted
import           Control.Lens                    (set)
import           Data.Aeson                      (decode)
import qualified Data.ByteString.Lazy            as L
-- import           Data.ByteString.Lazy.UTF8       (fromString)
-- import Data.ByteString.Lazy.Internal (packChars)
import           Data.Text.Encoding              (encodeUtf8)
-- import qualified Data.HashMap.Lazy         as DHM
import           Data.Map.Lazy                   (Map)
import qualified Data.Map.Lazy                   as M
import           Data.Maybe                      (fromJust)
-- import           Data.Text                 (Text)
import           Data.Text                       (Text, pack)
import           Data.Time.Clock.POSIX           (getPOSIXTime)
import qualified Data.Traversable                as T
import           JSONtoXLSX.JSONtoCellMap        (jsonToFormattedCellMap, simpleCellMapToFormattedCellMap)
import           JSONtoXLSX.JSONtoCellMap.Types  (SimpleCellMap)
import           JSONtoXLSX.MakeWorksheets       (makeWorksheets,
                                                  makeWorksheets2)
import           JSONtoXLSX.Pictures             (drawingPictures)
import           JSONtoXLSX.Pictures.PictureData (PictureData (..))

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

writeXlsx :: String -> FilePath -> IO()
writeXlsx jsonString outfile = do
  let jsonWorksheets = fromJust
           (decode ((L.fromStrict . encodeUtf8 . pack) jsonString) :: Maybe (Map Text SimpleCellMap))
      sheetnames = M.keys jsonWorksheets
      simplecellmaps = map snd $ M.toList jsonWorksheets
      fcellmaps = map simpleCellMapToFormattedCellMap simplecellmaps
      (stylesheet, worksheets) = makeWorksheets fcellmaps
      namedWorksheets = zip sheetnames worksheets
      xlsx = set xlStyles (renderStyleSheet stylesheet) $
               set xlSheets namedWorksheets emptyXlsx
  ct <- getPOSIXTime
  L.writeFile outfile (fromXlsx ct xlsx)

writeXlsx2 :: String -> String -> FilePath -> IO()
writeXlsx2 jsonCells jsonImages outfile = do
  let sheets_cells = fromJust
           (decode ((L.fromStrict . encodeUtf8 . pack) jsonCells) :: Maybe (Map Text SimpleCellMap))
      sheets_images = fromJust
           (decode ((L.fromStrict . encodeUtf8 . pack) jsonImages) :: Maybe (Map Text [PictureData]))
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

writeXlsx3 :: String -> String -> FilePath -> IO()
writeXlsx3 jsonCells jsonImages outfile = do
  let sheets_cells = fromJust
           (decode ((L.fromStrict . encodeUtf8 . pack) jsonCells) :: Maybe (Map Text SimpleCellMap))
      -- Map Text FormattedCellMap
      sheets_fcells = M.map simpleCellMapToFormattedCellMap sheets_cells
      sheets_images = fromJust
           (decode ((L.fromStrict . encodeUtf8 . pack) jsonImages) :: Maybe (Map Text [PictureData]))
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
