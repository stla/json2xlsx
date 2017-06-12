{-# LANGUAGE OverloadedStrings #-}
module Pictures.Tests
  where
import Data.Aeson
import           Data.ByteString.Lazy.UTF8 (fromString)
import qualified Data.HashMap.Lazy as HM
import Data.Maybe (fromJust)
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)

valueNumberToInt :: Value -> Int
valueNumberToInt (Number x) = fromJust $ toBoundedInteger x
valueStringToText :: Value -> Text
valueStringToText (String x) = x

jj = "{\"file\":\"image.png\",\"left\":1,\"top\":1,\"width\":100,\"height\":100}"
test :: Object
test = fromJust (decode (fromString jj) :: Maybe Object)

left :: Int
left = valueNumberToInt $ fromJust $ HM.lookup "left" test
file :: Text
file = valueStringToText $ fromJust $ HM.lookup "file" test

-- two images
jjj = "[{\"file\":\"image.png\",\"left\":1,\"top\":1,\"width\":100,\"height\":100},{\"file\":\"image.png\",\"left\":1,\"top\":1,\"width\":100,\"height\":100}]"
ttest :: Maybe [Object]
ttest = decode (fromString jjj)

-- idea:
-- writeXlsx data images outfile
-- data = Maybe String (json string) - idem already done
-- images = Maybe String (json string)
--  {Sheet1: jjj, Sheet2: ...}
