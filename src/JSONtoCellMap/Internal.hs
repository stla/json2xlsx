{-# LANGUAGE OverloadedStrings #-}
module JSONtoCellMap.Internal
  where
import           Codec.Xlsx          (CellValue (..), Font (..),
                                      FontFamily (..), ImpliedNumberFormat (..),
                                      NumberFormat (..), fontBold, fontFamily,
                                      fontName)
import           Control.Lens        (set)
import           Data.Aeson          (Value (..))
import           Data.Maybe          (fromJust, isNothing)
import           Data.Scientific     (toRealFloat)
import           Data.Text           (Text)
import           JSONtoCellMap.Types

valueToCellValue :: Value -> Maybe CellValue
valueToCellValue value =
    case value of
        (Number x) -> Just (CellDouble (toRealFloat x))
        (String x) -> Just (CellText x)
        (Bool x)   -> Just (CellBool x)
        Null       -> Nothing

textToNumberFormat :: Maybe Text -> Maybe NumberFormat
textToNumberFormat x
  | isNothing x = Nothing
  | x == Just "Nf2Decimal" = Just (StdNumberFormat Nf2Decimal)
  | otherwise = Just (UserNumberFormat (fromJust x))

textToFontFamily :: Maybe Text -> Maybe FontFamily
textToFontFamily x
  | isNothing x = Nothing
  | x == Just "Roman" = Just FontFamilyRoman
  | x == Just "Decorative" = Just FontFamilyDecorative
  | x == Just "Swiss" = Just FontFamilySwiss
  | x == Just "Modern" = Just FontFamilyModern
  | x == Just "Script" = Just FontFamilyScript
  | x == Just "NotApplicable" = Just FontFamilyNotApplicable
  | otherwise = Nothing

simpleFontToFont :: Maybe SimpleFont -> Maybe Font
simpleFontToFont sfont =
  Just $ set fontName fname $
    set fontFamily (textToFontFamily ffamily) (fromJust font)
    where font = maybe (Just emptyFont) (\x -> Just (set fontBold (bold x) emptyFont)) sfont
          ffamily = maybe Nothing family sfont
          fname = maybe Nothing name sfont
  -- if isNothing ffamily
  --   then
  --     font
  --   else
  --     Just $ set fontFamily (textToFontFamily ffamily) (fromJust font)
  --   where font = maybe (Just emptyFont) (\x -> Just (set fontBold (bold x) emptyFont)) sfont
  --         ffamily = maybe Nothing family sfont
