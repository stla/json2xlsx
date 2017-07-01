module JSONtoXLSX.SheetProtection
  where
import Data.Text (Text)
import Codec.Xlsx.Types.Protection
import Control.Lens (set)
import Data.Maybe

sheetProtection :: Maybe Text -> Maybe SheetProtection
sheetProtection password =
  if isNothing password
    then Nothing
  else
    Just $ set sprLegacyPassword
             (Just (legacyPassword (fromJust password)))
             SheetProtection
              { _sprLegacyPassword = Nothing
              , _sprSheet = True
              , _sprAutoFilter = True
              , _sprDeleteColumns = True
              , _sprDeleteRows = True
              , _sprFormatCells = True
              , _sprFormatColumns = True
              , _sprFormatRows = True
              , _sprInsertColumns = True
              , _sprInsertHyperlinks = True
              , _sprInsertRows = True
              , _sprObjects = True
              , _sprPivotTables = True
              , _sprScenarios = True
              , _sprSelectLockedCells = False
              , _sprSelectUnlockedCells = True
              , _sprSort = True
              }
