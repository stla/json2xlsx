{-# LANGUAGE OverloadedStrings #-}

module JSONtoXLSX.Pictures.Tests
  where
import           Data.Aeson
import           Data.ByteString.Lazy.UTF8       (fromString)
import qualified Data.Map                        as M
import           Data.Map.Lazy                   (Map)
import           Data.Maybe                      (fromJust)
import           Data.Text                       (Text)
import           JSONtoXLSX.Pictures.PictureData

jj = "{\"file\":\"image.png\",\"left\":1,\"top\":1,\"width\":100,\"height\":100}"
test2 = decode (fromString jj) :: Maybe PictureData

-- two images
jjj = "[{\"file\":\"image.png\",\"left\":1,\"top\":1,\"width\":100,\"height\":100},{\"file\":\"image.png\",\"left\":1,\"top\":1,\"width\":100,\"height\":100}]"
ttest2 = decode (fromString jjj) :: Maybe [PictureData]
