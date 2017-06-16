{-# LANGUAGE OverloadedStrings #-}
module Tests
  where
import JSONtoXLSX
import           Codec.Xlsx
import           Codec.Xlsx.Formatted
import           Control.Lens                    (set)
import           Data.Aeson                      (decode)
import qualified Data.ByteString.Lazy            as L
import           Data.ByteString.Lazy.UTF8       (fromString)
-- import qualified Data.HashMap.Lazy         as DHM
import           Data.Map.Lazy                   (Map)
import qualified Data.Map.Lazy                   as M
import           Data.Maybe                      (fromJust)
-- import           Data.Text                 (Text)
import           Data.Text                       (Text)
import           Data.Time.Clock.POSIX           (getPOSIXTime)
import qualified Data.Traversable                as T
import           JSONtoXLSX.JSONtoCellMap        (jsonToFormattedCellMap, simpleCellMapToFormattedCellMap)
import           JSONtoXLSX.JSONtoCellMap.Types  (SimpleCellMap)
import           JSONtoXLSX.MakeWorksheets       (makeWorksheets,
                                                  makeWorksheets2)
import           JSONtoXLSX.Pictures             (drawingPictures)
import           JSONtoXLSX.Pictures.PictureData (PictureData (..))

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


test :: IO()
test = writeSingleWorksheet jjj "test.xlsx"


test2 :: IO()
test2 = writeXlsx jjj6 "test3.xlsx"


jjpics = "{\"Sheet1\":[{\"file\":\"image.png\",\"left\":2,\"top\":3,\"width\":200,\"height\":300}]}"

thetest :: IO()
thetest = writeXlsx2 jjj6 jjpics "thetest.xlsx"


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
