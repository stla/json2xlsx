{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module JSONtoCellMap.Types
  where
import           Codec.Xlsx           (Cell, def, Font)
import           Codec.Xlsx.Formatted
import           Data.Aeson           (FromJSON, Value)
import           Data.HashMap.Lazy    (HashMap)
import           Data.Text            (Text)
import           GHC.Generics

emptyCell :: Cell
emptyCell = def

emptyFormattedCell :: FormattedCell
emptyFormattedCell = def

emptyFormat = _formattedFormat emptyFormattedCell

emptyFont :: Font
emptyFont = def


data SimpleFormat = SimpleFormat {
                                    numberFormat :: Maybe Text,
                                    font         :: Maybe SimpleFont
                                 }
                    deriving (Show, Generic, FromJSON, Eq)

data SimpleFont = SimpleFont {
                               bold :: Maybe Bool,
                               family :: Maybe Text,
                               name :: Maybe Text
                             }
                  deriving (Show, Generic, FromJSON, Eq)

emptySimpleFont :: SimpleFont
emptySimpleFont = SimpleFont {bold= Nothing, family= Nothing, name=Nothing}

emptySimpleFormat :: SimpleFormat
emptySimpleFormat = SimpleFormat {numberFormat=Nothing, font=Nothing}

data SimpleCell = SimpleCell {
                             value  :: Value,
                             format :: Maybe SimpleFormat
                           }
       deriving (Show, Generic, FromJSON)

type SimpleCellMap = HashMap Text SimpleCell
