unit txRecords;

interface

uses
  txMeta;

  // RECORD HANDLING METHODS
  function AddRecord(_id: Cardinal; sig: string; _res: PCardinal): WordBool; cdecl;
  function GetRecords(_id: Cardinal; _res: PCardinalArray): WordBool; cdecl;
  function RecordsBySignature(_id: Cardinal; sig: string; _res: PCardinalArray): WordBool; cdecl;
  function RecordByIndex(_id: Cardinal; index: Integer; _res: PCardinal): WordBool; cdecl;
  function RecordByFormID(_id, formID: Cardinal; _res: PCardinal): WordBool; cdecl;
  function RecordByEditorID(_id: Cardinal; edid: string; _res: PCardinal): WordBool; cdecl;
  function RecordByName(_id: Cardinal; full: string; _res: PCardinal): WordBool; cdecl;
  function OverrideCount(_id: Cardinal; count: PInteger): WordBool; cdecl;
  function OverrideByIndex(_id: Cardinal; index: Integer; _res: PCardinal): WordBool; cdecl;
  function GetFormID(_id: Cardinal; formID: PCardinal): WordBool; cdecl;
  function SetFormID(_id: Cardinal; formID: Cardinal): WordBool; cdecl;

  // PUBLIC TESTING INTERFACE
  procedure BuildRecordHandlingTests;

implementation

procedure BuildRecordHandlingTests;
begin

end;

end.
