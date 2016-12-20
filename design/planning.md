# Planning

## GENERAL

- GetLoadOrder: gets a list of available plugins in the order of loadorder.txt.
- LoadPlugins: loads a specified load order of plugins.
- GetGlobal: returns the value of a global variable.  E.g. DataPath, ProgramPath, ScriptsPath, FileCount, etc.

## FILES

### FILE HANDLING

- AddNewFile
- FileByIndex
- FileByLoadOrder
- FileByName
- FileByAuthor
- GetElementFile
- GetFileNames

### FILE VALUES

- GetFileHeader
- GetNextObjectId
- SetNextObjectID
- GetAuthor
- SetAuthor
- GetDescription
- SetDescription
- OverrideRecordCount
- GetOverrideRecords
- GetIsESM
- SetIsESM

### MASTERS

- GetMaster
- CleanMasters
- SortMasters
- AddMaster

**TODO:**
- GetAllMasters
- GetRequiredBy
- GetAllRequiredBy
- AddMasters
- RemoveMaster


## ELEMENTS

### ELEMENT HANDLING

- GetElement: replaces ElementByName, ElementByPath, ElementByIndex, GroupBySignature, and ElementBySignature.  Supports indexed paths.
- GetElements: gets all elements at the specified path
- NewElement: replaces ElementAssign, Add, AddElement, and InsertElement
- ElementExists: combines functionality of HasGroup and ElementExists.
- ElementCount: same
- Assigned: same
- Equals: same
- GetConflictWinner: replaces WinningOverride, works with elements as well as records.
- GetConflictMaster: replaces MasterOrSelf, works with elements as well as records.
- IsMaster
- IsInjected
- IsOverride
- IsWinningOverride
- IsValue
- RemoveElement: replaces Remove, RemoveByIndex, RemoveElement, and RemoveNode

**TODO:**
- ElementMatches
- StructMatches
- MoveElementToIndex
- GetContainer
- CopyRecord: replaces wbCopyElementToFile
- CopyElement: replaces wbCopyElementToRecord
  
### ELEMENT VALUES

- Name
- EditorID
- Signature
- ShortName
- SortKey
- DefType
- ElementType
- GetValue: replaces GetElementEditValues and GetEditValue
- SetValue: replaces SetElementEditValues and SetEditValue 
- GetIntValue
- SetIntValue
- GetUIntValue
- SetUIntValue
- GetFloatValue
- SetFloatValue
- SetFlag
- GetFlag
- ToggleFlag
- GetEnabledFlags

**TODO:**
- Path
- ConflictAll
- ConflictThis
- GetListValues
- SetListValues
- GetElementListValues
- SetElementListValues
- HasArrayValue
- GetArrayValue
- AddArrayValue
- DeleteArrayValue
- HasArrayStruct
- GetArrayStruct
- AddArrayStruct
- DeleteArrayStruct
  
  
## GROUPS

- HasGroup
- AddGroup
- GetGroupSignatures
- GetChildGroup
- GroupSignatureFromName
- GroupNameFromSignature
- GetGroupSignatureNameMap
  
## RECORDS

### RECORD HANDLING

- AddRecord
- GetRecords
- RecordsBySignature
- RecordByIndex
- RecordByFormID
- RecordByEditorID
- RecordByName


**TODO:**
- OverrideCount
- OverrideByIndex
- GetFormID
- SetFormID

### RECORD ELEMENTS

**TODO:**
- HasKeyword
- AddKeyword
- RemoveKeyword
- HasFormID
- AddFormID
- RemoveFormID
- HasMusicTrack
- AddMusicTrack
- RemoveMusicTrack
- HasFootstep
- AddFootstep
- RemoveFootstep
- HasItem
- AddItem
- RemoveItem
- SetItemCount
- HasLeveledEntry
- AddLeveledEntry
- GetLeveledEntry
- GetMatchingLevedEntries
- RemoveLeveledEntry
- HasEffect
- GetEffect
- AddEffect
- RemoveEffect
- HasAdditionalRace
- GetAdditionalRace
- AddAdditionalRace
- RemoveAdditionalRace
- HasCondition
- GetCondition
- AddCondition
- RemoveCondition
- HasScript
- GetScript
- AddScript
- RemoveScript

### RECORD VALUES

**TODO:**
- GetGoldValue
- SetGoldValue
- GetDamage
- SetDamage
- GetArmorRating
- SetArmorRating
- GetIsFemale
- SetIsFemale
- GetIsEssential
- SetIsEssential
- GetIsUnique
- SetIsUnique
- GetObjectBounds
- SetObjectBounds
- GetModel
- SetModel