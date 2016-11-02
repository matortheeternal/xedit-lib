unit xeRecordValues;

interface

  // common array functions
//  function HasKeyword(_id: Cardinal; keyword: PWideChar; b: WordBool): WordBool; StdCall;
//  function AddKeyword(_id: Cardinal; keyword: PWideChar): WordBool; StdCall;
//  function RemoveKeyword(_id: Cardinal; keyword: PWideChar): WordBool; StdCall;
//  function HasFormID(_id, formID: Cardinal; b: WordBool): WordBool; StdCall;
//  function AddFormID(_id, formID: Cardinal): WordBool; StdCall;
//  function RemoveFormID(_id, formID: Cardinal): WordBool; StdCall;
//  function HasMusicTrack(_id, formID: Cardinal; b: WordBool): WordBool; StdCall;
//  function AddMusicTrack(_id, formID: Cardinal): WordBool; StdCall;
//  function RemoveMusicTrack(_id, formID: Cardinal): WordBool; StdCall;
//  function HasFootstep(_id, formID: Cardinal; b: WordBool): WordBool; StdCall;
//  function AddFootstep(_id, formID: Cardinal): WordBool; StdCall;
//  function RemoveFootstep(_id, formID: Cardinal): WordBool; StdCall;
//  function HasItem(_id, formID: Cardinal; b: WordBool): WordBool; StdCall;
//  function AddItem(_id, formID: Cardinal; count: Integer): WordBool; StdCall;
//  function GetItem(_id, formID, _res: Cardinal): WordBool; StdCall;
//  function RemoveItem(_id, formID: Cardinal): WordBool; StdCall;
//  function SetItemCount(_id, formID: Cardinal; count: Integer): WordBool; StdCall;
//  function HasLeveledEntry(_id, formID: Cardinal; b: WordBool): WordBool; StdCall;
//  function AddLeveledEntry(_id, formID: Cardinal; level, count: Integer): WordBool; StdCall;
//  function GetLeveledEntry(_id, formID, _res: Cardinal): WordBool; StdCall;
  //function GetMatchingLeveledEntries(_id, formID: Cardinal; ???): WordBool; StdCall;
//  function RemoveLeveledEntry(_id, formID: Cardinal): WordBool; StdCall;
//  function HasEffect(_id, formID: Cardinal; b: WordBool): WordBool; StdCall;
//  function GetEffect(_id, formID: Cardinal): WordBool; StdCall;
//  function AddEffect(_id, formID: Cardinal): WordBool; StdCall;
//  function RemoveEffect(_id, formID: Cardinal): WordBool; StdCall;
//  function HasAdditionalRace(_id, formID: Cardinal; b: WordBool): WordBool; StdCall;
//  function GetAdditionalRace(_id, formID: Cardinal): WordBool; StdCall;
//  function AddAdditionalRace(_id, formID: Cardinal): WordBool; StdCall;
//  function RemoveAdditionalRace(_id, formID: Cardinal): WordBool; StdCall;
//  function HasCondition(_id: Cardinal; ctype: PWideChar; b: WordBool): WordBool; StdCall;
//  function GetCondition(_id: Cardinal; ctype: PWideChar): WordBool; StdCall;
//  function AddCondition(_id: Cardinal; ctype: PWideChar): WordBool; StdCall;
//  function RemoveCondition(_id: Cardinal; ctype: PWideChar): WordBool; StdCall;
//  function HasScript(_id: Cardinal; name: PWideChar; b: WordBool): WordBool; StdCall;
//  function GetScript(_id: Cardinal; name: PWideChar): WordBool; StdCall;
//  function AddScript(_id: Cardinal; name: PWideChar): WordBool; StdCall;
//  function RemoveScript(_id: Cardinal; name: PWideChar): WordBool; StdCall;

  // common value getters and setters
//  function GetGoldValue(_id: Cardinal; value: Integer): WordBool; StdCall;
//  function SetGoldValue(_id: Cardinal; value: Integer): WordBool; StdCall;
//  function GetDamage(_id: Cardinal; value: Double): WordBool; StdCall;
//  function SetDamage(_id: Cardinal; value: Double): WordBool; StdCall;
//  function GetArmorRating(_id: Cardinal; value: Double): WordBool; StdCall;
//  function SetArmorRating(_id: Cardinal; value: Double): WordBool; StdCall;
//  function GetWeight(_id: Cardinal; value: Double): WordBool; StdCall;
//  function SetWeight(_id: Cardinal; value: Double): WordBool; StdCall;

  // common flag getters and setters
//  function GetIsFemale(_id: Cardinal; value: WordBool): WordBool; StdCall;
//  function SetIsFemale(_id: Cardinal; value: WordBool): WordBool; StdCall;
//  function GetIsEssential(_id: Cardinal; value: WordBool): WordBool; StdCall;
//  function SetIsEssential(_id: Cardinal; value: WordBool): WordBool; StdCall;
//  function GetIsUnique(_id: Cardinal; value: WordBool): WordBool; StdCall;
//  function SetIsUnique(_id: Cardinal; value: WordBool): WordBool; StdCall;

  // common advanced getters and setters
//  function GetObjectBounds(_id: Cardinal; x1, y1, z1, x2, y2, z2: Integer): WordBool; StdCall;
//  function SetObjectBounds(_id: Cardinal; x1, y1, z1, x2, y2, z2: Integer): WordBool; StdCall;
//  function GetModel(_id: Cardinal; female, world: WordBool; fileName: PWideChar; len: Integer): WordBool; StdCall;
//  function SetModel(_id: Cardinal; female, world: WordBool; fileName: PWideChar): WordBool; StdCall;

implementation

uses
  Classes, SysUtils, Variants,
  // mte modules
  mteHelpers,
  // xedit modules
  wbInterface, wbImplementation,
  // xelib modules
  xeMessages, xeMeta;



end.
