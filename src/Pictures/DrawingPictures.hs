{-# LANGUAGE OverloadedStrings #-}
module Pictures.DrawingPictures
  where
import           Codec.Xlsx
import           Control.Lens
-- import           Data.ByteString.Lazy (ByteString)
import           Control.Monad        (zipWithM)
import qualified Data.ByteString.Lazy as B
import System.FilePath.Posix (takeFileName)

defaultShapeProperties :: ShapeProperties
defaultShapeProperties =
  ShapeProperties {
                    _spXfrm     = Nothing,
                    _spGeometry = Just PresetGeometry,
                    _spFill     = Nothing,
                    _spOutline  = Nothing
                  }

drawingPictures :: [FilePath] -> [(Int, Int, Int, Int)] -> IO Drawing
drawingPictures imageFiles coords = do
  xdranchors <- zipWithM xdrAnchor imageFiles coords
  return Drawing {_xdrAnchors = xdranchors}

xdrAnchor :: FilePath -> (Int, Int, Int, Int) -> IO (Anchor FileInfo ChartSpace)
xdrAnchor imageFile coordinates = do
  image <- B.readFile imageFile
  let fileInfo = FileInfo
                 {
                   _fiFilename = takeFileName imageFile,
                   _fiContentType = "image/png",
                   _fiContents = image
                 }
      anchor = simpleAnchorXY (left-1, top-1) (positiveSize2D cx cy) $
                  picture DrawingElementId{unDrawingElementId = 1} fileInfo
      pic = set picShapeProperties defaultShapeProperties (_anchObject anchor)
  return $ set anchObject pic anchor
  where (top, left, width, height) = coordinates
        cx = toInteger $ 9525*width
        cy = toInteger $ 9525*height
