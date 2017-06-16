{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module JSONtoXLSX.Pictures.PictureData
  where
import           Data.Aeson   (FromJSON)
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
