{-# LANGUAGE OverloadedStrings #-}
module JSONtoCellMap.Internal
  where
import           Codec.Xlsx          (CellValue (..), Font (..),
                                      FontFamily (..), ImpliedNumberFormat (..),
                                      NumberFormat (..), fontBold, fontFamily,
                                      fontName, Color(..), fontColor, colorARGB,
                                      Comment (..), XlsxText (..))
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
  | x == Just "NfGeneral" = Just (StdNumberFormat NfGeneral)
  | x == Just "NfZero" = Just (StdNumberFormat NfZero)
  | x == Just "Nf2Decimal" = Just (StdNumberFormat Nf2Decimal)
  | x == Just "NfMax3Decimal" = Just (StdNumberFormat NfMax3Decimal)
  | x == Just "NfThousandSeparator2Decimal" = Just (StdNumberFormat NfThousandSeparator2Decimal)
  | x == Just "NfPercent" = Just (StdNumberFormat NfPercent)
  | x == Just "NfPercent2Decimal" = Just (StdNumberFormat NfPercent2Decimal)
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
textToColor argb = Just $ set colorARGB argb emptyColor

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

textToComment :: Maybe Text -> Maybe Comment
textToComment comment
  | isNothing comment = Nothing
  | otherwise = Just Comment
                  {
                    _commentText = XlsxText (fromJust comment),
                    _commentAuthor = "xlsx",
                    _commentVisible = False
                  }
