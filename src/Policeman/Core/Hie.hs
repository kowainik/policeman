{- | The module contains pure function to parse GHC data types that we get from
@.hie@ files.

-}
module Policeman.Core.Hie
    ( hieFileToModuleStructure
    , hieFilesToHashMap
    ) where

import Avail (AvailInfo (..))
import FastString (unpackFS)
import FieldLabel (FieldLabel, FieldLbl (..))
import HieTypes (HieFile (..))
import Module (moduleName, moduleNameString)
import Name (Name, nameOccName)
import OccName (occNameString)

import Policeman.Core.Package (Export (..), Module (..), ModuleStructure (..))

import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set


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


hieFileToModuleStructure :: HieFile -> ModuleStructure
hieFileToModuleStructure hf = ModuleStructure
    { msExports = Set.fromList $ concatMap availInfoToExport $ hie_exports hf
    }


hieFileToModule :: HieFile -> Module
hieFileToModule = Module . toText . moduleNameString . moduleName . hie_module

hieFilesToHashMap :: [HieFile] -> HashMap Module ModuleStructure
hieFilesToHashMap = HM.fromList . map (\hf -> (hieFileToModule hf, hieFileToModuleStructure hf))
