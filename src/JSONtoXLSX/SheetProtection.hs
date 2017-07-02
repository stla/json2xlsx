module JSONtoXLSX.SheetProtection
  where
import           Codec.Xlsx.Types.Protection
import           Data.Maybe
import           Data.Text                   (Text)

sheetProtection :: Maybe Text -> Maybe SheetProtection
sheetProtection password =
  if isNothing password
    then Nothing
  else
    Just SheetProtection
    { _sprLegacyPassword = Just (legacyPassword (fromJust password))
    , _sprSheet = True
    , _sprAutoFilter = True
    , _sprDeleteColumns = True
    , _sprDeleteRows = True
    , _sprFormatCells = True
    , _sprFormatColumns = False
    , _sprFormatRows = True
    , _sprInsertColumns = True
    , _sprInsertHyperlinks = True
    , _sprInsertRows = True
    , _sprObjects = True
    , _sprPivotTables = True
    , _sprScenarios = True
    , _sprSelectLockedCells = False
    , _sprSelectUnlockedCells = False
    , _sprSort = True
    }
