{-# LANGUAGE OverloadedStrings #-}
module JSONtoXLSX
  where
import           Codec.Xlsx
import           Control.Lens                    (set)
import           Data.Aeson                      (decode)
import qualified Data.ByteString.Lazy            as L
--import qualified Data.ByteString.Internal as B
import           Data.ByteString.Lazy.UTF8       (fromString)
-- import Data.ByteString.Lazy.Internal (packChars)
-- import           Data.Text.Encoding              (encodeUtf8)
-- import qualified Data.HashMap.Lazy         as DHM
import           Data.Map.Lazy                   (Map)
import qualified Data.Map.Lazy                   as M
import           Data.Maybe                      (fromJust)
-- import           Data.Text                 (Text)
import           Data.Text                       (Text)
import           Data.Time.Clock.POSIX           (getPOSIXTime)
import qualified Data.Traversable                as T
import           JSONtoXLSX.JSONtoCellMap        (simpleCellMapToFormattedCellMap)
import           JSONtoXLSX.JSONtoCellMap.Types  (SimpleCellMap)
import           JSONtoXLSX.MakeWorksheets       (makeWorksheets)
import           JSONtoXLSX.Pictures             (drawingPictures)
import           JSONtoXLSX.Pictures.PictureData (PictureData (..))

emptyXlsx :: Xlsx
emptyXlsx = def

-- writeXlsx3 :: String -> String -> FilePath -> IO()
-- writeXlsx3 jsonCells jsonImages outfile = do
--   let sheets_cells = fromJust
--            (decode ((L.fromStrict . encodeUtf8 . pack) jsonCells) :: Maybe (Map Text SimpleCellMap))
--       -- Map Text FormattedCellMap
--       sheets_fcells = M.map simpleCellMapToFormattedCellMap sheets_cells
--       sheets_images = fromJust
--            (decode ((L.fromStrict . encodeUtf8 . pack) jsonImages) :: Maybe (Map Text [PictureData]))
--   -- Map Text Drawing
--   sheets_drawings <- T.mapM drawingPictures sheets_images
--   -- merge => Map Text (FormattedCellMap, Maybe Drawing)
--   let mergedMap = M.mergeWithKey
--                     (\k x y -> Just (x, Just y))
--                     (M.map (\x -> (x, Nothing)))
--                     (M.map (\y -> (M.empty, Just y)))
--                     sheets_fcells sheets_drawings
--       (stylesheet, worksheets) = makeWorksheets2 $ map snd $ M.toList mergedMap
--       sheetnames = M.keys mergedMap
--       namedWorksheets = zip sheetnames worksheets
--       xlsx = set xlStyles (renderStyleSheet stylesheet) $
--                set xlSheets namedWorksheets emptyXlsx
--   ct <- getPOSIXTime
--   L.writeFile outfile (fromXlsx ct xlsx)


-- writeXlsx5 :: String -> String -> FilePath -> IO()
-- writeXlsx5 jsonCells jsonImages outfile = do
--   let sheets_cells = fromJust
--            (decode (fromString jsonCells) :: Maybe (Map Text SimpleCellMap))
--       -- sheets_fcells :: Map Text FormattedCellMap
--       sheets_fcells = M.map simpleCellMapToFormattedCellMap sheets_cells
--       sheets_images = fromJust
--            (decode (fromString jsonImages) :: Maybe (Map Text [PictureData]))
--   -- Map Text Drawing
--   sheets_drawings <- T.mapM drawingPictures sheets_images
--   -- merge => Map Text (FormattedCellMap, Maybe Drawing)
--   let mergedMap = M.mergeWithKey
--                     (\k x y -> Just (x, Just y))
--                     (M.map (\x -> (x, Nothing)))
--                     (M.map (\y -> (M.empty, Just y)))
--                     sheets_fcells sheets_drawings
--       (stylesheet, worksheets) = makeWorksheets $ map snd $ M.toList mergedMap
--       sheetnames = M.keys mergedMap
--       namedWorksheets = zip sheetnames worksheets
--       xlsx = set xlStyles (renderStyleSheet stylesheet) $
--                set xlSheets namedWorksheets emptyXlsx
--   ct <- getPOSIXTime
--   L.writeFile outfile (fromXlsx ct xlsx)

-- conclusions pour utf8 :
-- le 5 direct dans Haskell ou cmder ne marche pas - à vérifier
-- il marhc avec DLL grâce à peekCAString
-- le 4 avec dll ne marche dans R que si Âµ (obtenu avec iconv et toJSON)
-- mais je ne l'ai pas essayé avec peekCAString
-- le 3 marche direct et pour le exe dans R - exe peut-être que avec toJSON -- à vérifier
-- je n'ai pas essayé DLL+3 avec peekCAString

-- VéRIFICATIONS:
-- le 3 et le 5 marchent bien dans Haskell avec un µ
-- writexlsx (qui utilise writeXlsx3) marche dans cmd
-- le exe dans R (json2xlsx2) a bien marché sans passer par jsonlite

-- CONCLUSIONS:
-- je ne comprends plus ce qui ne marchait pas avec 5

--
writeXlsx6 :: String -> String -> String -> FilePath -> IO()
writeXlsx6 jsonCells jsonImages jsonPasswords outfile = do
  let sheets_cells = fromJust
           (decode (fromString jsonCells) :: Maybe (Map Text SimpleCellMap))
      -- sheets_fcells :: Map Text FormattedCellMap
      sheets_fcells = M.map simpleCellMapToFormattedCellMap sheets_cells
      sheets_images = fromJust
           (decode (fromString jsonImages) :: Maybe (Map Text [PictureData]))
      sheets_passwords = fromJust
           (decode (fromString jsonPasswords) :: Maybe (Map Text Text))
  -- sheets_drawings :: Map Text Drawing
  sheets_drawings <- T.mapM drawingPictures sheets_images
  -- merge 1 => Map Text (FormattedCellMap, Maybe Drawing)
  -- merge 2 => Map Text ((FormattedCellMap, Maybe Drawing), Maybe Text)
  let mergedMap = M.mergeWithKey
                    (\k x y -> Just (x, Just y))
                    (M.map (\x -> (x, Nothing)))
                    (M.map (\y -> ((M.empty, Nothing), Just y)))
                    (M.mergeWithKey
                      (\k x y -> Just (x, Just y))
                      (M.map (\x -> (x, Nothing)))
                      (M.map (\y -> (M.empty, Just y)))
                      sheets_fcells sheets_drawings)
                    sheets_passwords
      (stylesheet, worksheets) = makeWorksheets $ map snd $ M.toList mergedMap
      sheetnames = M.keys mergedMap
      namedWorksheets = zip sheetnames worksheets
      xlsx = set xlStyles (renderStyleSheet stylesheet) $
               set xlSheets namedWorksheets emptyXlsx
  ct <- getPOSIXTime
  L.writeFile outfile (fromXlsx ct xlsx)
