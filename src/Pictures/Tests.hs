{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass  #-}

module Pictures.Tests
  where
import Data.Aeson
import           Data.ByteString.Lazy.UTF8 (fromString)
import qualified Data.HashMap.Lazy as HM
import Data.Maybe (fromJust)
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import           GHC.Generics

data PictureData = PictureData
                    {
                     file :: FilePath,
                     left :: Int,
                     top :: Int,
                     width :: Int,
                     height :: Int
                    }
                   deriving (Show, Generic, FromJSON, Eq)

valueNumberToInt :: Value -> Int
valueNumberToInt (Number x) = fromJust $ toBoundedInteger x
valueStringToText :: Value -> Text
valueStringToText (String x) = x

jj = "{\"file\":\"image.png\",\"left\":1,\"top\":1,\"width\":100,\"height\":100}"
test :: Object
test = fromJust (decode (fromString jj) :: Maybe Object)

test2 = fromJust (decode (fromString jj) :: Maybe PictureData)

-- left :: Int
-- left = valueNumberToInt $ fromJust $ HM.lookup "left" test
-- file :: Text
-- file = valueStringToText $ fromJust $ HM.lookup "file" test

-- two images
jjj = "[{\"file\":\"image.png\",\"left\":1,\"top\":1,\"width\":100,\"height\":100},{\"file\":\"image.png\",\"left\":1,\"top\":1,\"width\":100,\"height\":100}]"
ttest :: Maybe [Object]
ttest = decode (fromString jjj)

ttest2 = fromJust (decode (fromString jjj) :: Maybe [PictureData])

-- idea:
-- writeXlsx data images outfile
-- data = Maybe String (json string) - idem already done
-- images = Maybe String (json string)
--  {Sheet1: jjj, Sheet2: ...}
