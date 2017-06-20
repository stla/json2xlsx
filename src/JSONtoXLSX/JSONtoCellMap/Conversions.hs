{-# LANGUAGE OverloadedStrings #-}
module JSONtoXLSX.JSONtoCellMap.Conversions
  where
import           Codec.Xlsx                         (CellValue (..), Color (..),
                                                     Comment (..), Font (..),
                                                     FontFamily (..),
                                                     ImpliedNumberFormat (..),
                                                     NumberFormat (..),
                                                     XlsxText (..), colorARGB,
                                                     fontBold, fontColor,
                                                     fontFamily, fontName)
import           Control.Lens                       (set)
import           Data.Aeson                         (Value (..))
import           Data.Maybe                         (fromJust, fromMaybe,
                                                     isNothing)
import           Data.Scientific                    (toRealFloat)
import           Data.Text                          (Text)
import           JSONtoXLSX.JSONtoCellMap.ColorsRGB (colorToARGB)
import           JSONtoXLSX.JSONtoCellMap.Types

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
  | x == Just "General" = Just (StdNumberFormat NfGeneral)
  | x == Just "Zero" = Just (StdNumberFormat NfZero)
  | x == Just "2Decimal" = Just (StdNumberFormat Nf2Decimal)
  | x == Just "Max3Decimal" = Just (StdNumberFormat NfMax3Decimal)
  | x == Just "ThousandSeparator2Decimal" = Just (StdNumberFormat NfThousandSeparator2Decimal)
  | x == Just "Percent" = Just (StdNumberFormat NfPercent)
  | x == Just "Percent2Decimal" = Just (StdNumberFormat NfPercent2Decimal)
  | x == Just "DMmm" = Just (StdNumberFormat NfDMmm)
  | x == Just "DMmmYy" = Just (StdNumberFormat NfDMmmYy)
  | x == Just "DoubleSpacedFraction" = Just (StdNumberFormat NfDoubleSpacedFraction)
  | x == Just "Exponent1Decimal" = Just (StdNumberFormat NfExponent1Decimal)
  | x == Just "Exponent2Decimal" = Just (StdNumberFormat NfExponent2Decimal)
  | x == Just "HMm" = Just (StdNumberFormat NfHMm)
  | x == Just "HMm12Hr" = Just (StdNumberFormat NfHMm12Hr)
  | x == Just "HMmSs" = Just (StdNumberFormat NfHMmSs)
  | x == Just "HMmSs12Hr" = Just (StdNumberFormat NfHMmSs12Hr)
  | x == Just "MmDdYy" = Just (StdNumberFormat NfMmDdYy)
  | x == Just "MmSs" = Just (StdNumberFormat NfMmSs)
  | x == Just "MmSs1Decimal" = Just (StdNumberFormat NfMmSs1Decimal)
  | x == Just "MmmYy" = Just (StdNumberFormat NfMmmYy)
  | x == Just "MdyHMm" = Just (StdNumberFormat NfMdyHMm)
  | x == Just "OptHMmSs" = Just (StdNumberFormat NfOptHMmSs)
  | x == Just "SingleSpacedFraction" = Just (StdNumberFormat NfSingleSpacedFraction)
  | x == Just "TextPlaceHolder" = Just (StdNumberFormat NfTextPlaceHolder)
  | x == Just "Thousands2DecimalNegativeParens" = Just (StdNumberFormat NfThousands2DecimalNegativeParens)
  | x == Just "ThousandsNegativeParens" = Just (StdNumberFormat NfThousandsNegativeParens)
  | x == Just "ThousandsNegativeRed" = Just (StdNumberFormat NfThousandsNegativeRed)
  | x == Just "Tousands2DecimalNEgativeRed" = Just (StdNumberFormat NfTousands2DecimalNEgativeRed)
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

-- rgba color: https://css-tricks.com/8-digit-hex-codes/
textToColor :: Maybe Text -> Maybe Color
textToColor color
  | isNothing color = Nothing
  | otherwise = Just $ set colorARGB (fmap colorToARGB color) emptyColor

simpleFontToFont :: Maybe SimpleFont -> Maybe Font
simpleFontToFont sfont =
  Just $
    set fontColor (textToColor fcolor) $
    set fontName fname $
    set fontFamily (textToFontFamily ffamily) (fromJust font)
    where font    = maybe (Just emptyFont)
                          (\x -> Just (set fontBold (bold x) emptyFont)) sfont
          ffamily = maybe Nothing family sfont
          fname   = maybe Nothing name sfont
          fcolor  = maybe Nothing color sfont
  -- if isNothing ffamily
  --   then
  --     font
  --   else
  --     Just $ set fontFamily (textToFontFamily ffamily) (fromJust font)
  --   where font = maybe (Just emptyFont) (\x -> Just (set fontBold (bold x) emptyFont)) sfont
  --         ffamily = maybe Nothing family sfont

-- textToComment :: Maybe Text -> Maybe Comment
-- textToComment comment
--   | isNothing comment = Nothing
--   | otherwise = Just Comment
--                   {
--                     _commentText = XlsxText (fromJust comment),
--                     _commentAuthor = "xlsx",
--                     _commentVisible = False
--                   }

cellCommentToComment :: Maybe CellComment -> Maybe Comment
cellCommentToComment cellcomment
  | isNothing cellcomment = Nothing
  | otherwise = Just Comment
                  {
                    _commentText = XlsxText (text $ fromJust cellcomment),
                    _commentAuthor =
                      fromMaybe "someone" (author $ fromJust cellcomment),
                    _commentVisible = False
                  }
