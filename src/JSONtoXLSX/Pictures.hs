{-# LANGUAGE OverloadedStrings #-}
module JSONtoXLSX.Pictures
  where
import           Codec.Xlsx
import           JSONtoXLSX.Pictures.DrawingPictures (xdrAnchor)
import           JSONtoXLSX.Pictures.PictureData     (PictureData (..))

drawingPictures :: [PictureData] -> IO Drawing
drawingPictures picturesData = do
  xdranchors <- mapM xdrAnchor picturesData
  return Drawing {_xdrAnchors = xdranchors}
