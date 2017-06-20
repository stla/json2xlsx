module JSONtoXLSX.Pictures.DrawingPictures
  where
import           Codec.Xlsx
import           Control.Lens                    (set)
-- import           Control.Monad         (zipWithM)
import qualified Data.ByteString                 as SB
import qualified Data.ByteString.Lazy            as LB
import           Data.Text                       (pack)
import           JSONtoXLSX.Pictures.PictureData (PictureData (..))
import           System.FilePath.Posix           (takeFileName)

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
  image <- SB.readFile imageFile
  let fileInfo = FileInfo
                 {
                   _fiFilename = show id ++ takeFileName imageFile,
                   _fiContentType = pack "image/png",
                   _fiContents = LB.fromStrict image
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
