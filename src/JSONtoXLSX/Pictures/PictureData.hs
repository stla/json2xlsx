{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module JSONtoXLSX.Pictures.PictureData
  where
import           Data.Aeson
import           Data.ByteString.Lazy.UTF8 (fromString)
import           GHC.Generics

data PictureData = PictureData
                    {
                     file   :: FilePath,
                     left   :: Int,
                     top    :: Int,
                     width  :: Int,
                     height :: Int
                    }
                   deriving (Show, Generic, FromJSON, Eq)

jj = "{\"file\":\"image.png\",\"left\":1,\"top\":1,\"width\":100,\"height\":100}"
test2 = decode (fromString jj) :: Maybe PictureData

-- two images
jjj = "[{\"file\":\"image.png\",\"left\":1,\"top\":1,\"width\":100,\"height\":100},{\"file\":\"image.png\",\"left\":1,\"top\":1,\"width\":100,\"height\":100}]"
ttest2 = decode (fromString jjj) :: Maybe [PictureData]

-- idea:
-- writeXlsx data images outfile
-- data = Maybe String (json string) - idem already done
-- images = Maybe String (json string)
--  {Sheet1: jjj, Sheet2: ...}
