{-# LANGUAGE OverloadedStrings #-}
module Pictures.DrawingPictures
  where
import           Codec.Xlsx
import           Control.Lens
-- import           Data.ByteString.Lazy (ByteString)
-- import           Control.Monad         (zipWithM)
import qualified Data.ByteString.Lazy  as B
import           System.FilePath.Posix (takeFileName)
import Pictures.PictureData

defaultShapeProperties :: ShapeProperties
defaultShapeProperties =
  ShapeProperties {
                    _spXfrm     = Nothing,
                    _spGeometry = Just PresetGeometry,
                    _spFill     = Nothing,
                    _spOutline  = Nothing
                  }

cantorPairing :: (Int, Int) -> Int
cantorPairing (a,b) = div ((a+b)*(a+b+1)) 2 + a

xdrAnchor :: PictureData -> IO (Anchor FileInfo ChartSpace)
xdrAnchor pictureData = do
  image <- B.readFile imageFile
  let fileInfo = FileInfo
                 {
                   _fiFilename = show id ++ takeFileName imageFile,
                   _fiContentType = "image/png",
                   _fiContents = image
                 }
      anchor = simpleAnchorXY (leftcorner, topcorner) (positiveSize2D cx cy) $
                  picture DrawingElementId{unDrawingElementId = id} fileInfo
      pic = set picShapeProperties defaultShapeProperties (_anchObject anchor)
  return $ set anchObject pic anchor
  where imageFile  = file pictureData
        topcorner = top pictureData - 1
        leftcorner = left pictureData - 1
        id = cantorPairing (topcorner, leftcorner)
        cx = toInteger $ 9525 * width pictureData
        cy = toInteger $ 9525 * height pictureData

drawingPictures :: [PictureData] -> IO Drawing
drawingPictures picturesData = do
  xdranchors <- mapM xdrAnchor picturesData
  return Drawing {_xdrAnchors = xdranchors}


-- drawingPictures :: [FilePath] -> [(Int, Int, Int, Int)] -> IO Drawing
-- drawingPictures imageFiles coords = do
--   xdranchors <- zipWithM xdrAnchor imageFiles coords
--   return Drawing {_xdrAnchors = xdranchors}

-- faut-il différents unDrawingElementId ?
-- xdrAnchor :: FilePath -> (Int, Int, Int, Int) -> IO (Anchor FileInfo ChartSpace)
-- xdrAnchor imageFile coordinates = do
--   image <- B.readFile imageFile
--   let fileInfo = FileInfo
--                  {
--                    _fiFilename = takeFileName imageFile,
--                    _fiContentType = "image/png",
--                    _fiContents = image
--                  }
--       anchor = simpleAnchorXY (left-1, top-1) (positiveSize2D cx cy) $
--                   picture DrawingElementId{unDrawingElementId = 1} fileInfo
--       pic = set picShapeProperties defaultShapeProperties (_anchObject anchor)
--   return $ set anchObject pic anchor
--   where (top, left, width, height) = coordinates
--         cx = toInteger $ 9525*width
--         cy = toInteger $ 9525*height
