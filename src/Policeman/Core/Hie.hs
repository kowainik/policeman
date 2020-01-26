{- | The module contains pure function to parse GHC data types that we get from
@.hie@ files.

-}
module Policeman.Core.Hie
    ( availInfoToExport
    ) where

import Avail (AvailInfo (..))
import FastString (unpackFS)
import FieldLabel (FieldLabel, FieldLbl (..))
import Name (Name, nameOccName)
import OccName (occNameString)

import Policeman.Core.Package (Export (..))


{- | Creates the list of 'Export's from the given 'AvailInfo'.

As the data type or type class can bring to export more than 1 entry
'availInfoToExport' returns the list of the exports. It adds the name of the
data type or the type class as the prefix separated with the dot @.@.
-}
availInfoToExport :: AvailInfo -> [Export]
availInfoToExport (Avail name) = [ExportedFunction $ nameToText name]
availInfoToExport (AvailTC (nameToText -> name) pieces fields) =
    ExportedType name
    :  map piecesToExport pieces
    ++ map fieldsToExport fields
  where
    piecesToExport :: Name -> Export
    piecesToExport = addPrefixName . nameToText

    fieldsToExport :: FieldLabel -> Export
    fieldsToExport FieldLabel{..} = addPrefixName $ toText $ unpackFS flLabel

    addPrefixName :: Text -> Export
    addPrefixName n = ExportedType $ name <> "." <> n

nameToText :: Name -> Text
nameToText = toText . occNameString . nameOccName
