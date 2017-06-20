{-# LANGUAGE OverloadedStrings #-}
module Tests
  where
import JSONtoXLSX
-- import           Codec.Xlsx
-- import           Codec.Xlsx.Formatted
-- import           Control.Lens                    (set)
-- import           Data.Aeson                      (decode)
-- import qualified Data.ByteString.Lazy            as L
-- import           Data.ByteString.Lazy.UTF8       (fromString)
-- -- import qualified Data.HashMap.Lazy         as DHM
-- import           Data.Map.Lazy                   (Map)
-- import qualified Data.Map.Lazy                   as M
-- import           Data.Maybe                      (fromJust)
-- -- import           Data.Text                 (Text)
-- import           Data.Text                       (Text)

jjj, jjpics :: String
jjj = "{\"Sheet1\":{\"A1\":{\"value\":2,\"comment\":\"hé\",\"format\":{\"numberFormat\":\"2Decimal\",\"font\":{\"bold\":true}}},\"B2\":{\"value\":1000,\"format\":{\"numberFormat\":\"yyyy-mm-dd;@\"}},\"A3\":{\"value\":\"µ\",\"format\":{\"font\":{\"family\":\"Script\",\"name\":\"Courier\"}}}}}"
jjpics = "{\"Sheet2\":[{\"file\":\"image.png\",\"left\":2,\"top\":3,\"width\":200,\"height\":300},{\"file\":\"image2.png\",\"left\":12,\"top\":3,\"width\":200,\"height\":300}]}"

test3 :: IO()
test3 = writeXlsx3 jjj jjpics "test3.xlsx"

test5 :: IO()
test5 = writeXlsx5 jjj jjpics "test5.xlsx"
