unit xeFiles;

interface

uses
  Classes,
  // xedit modules
  wbInterface, wbImplementation,
  // xelib modules
  xeMeta;

  // FILE HANDLING
  function NewFile(filename: PAnsiChar): Integer; StdCall;
  function FileByIndex(index: Integer): Integer; StdCall;
  function FileByLoadOrder(load_order: Integer): Integer; StdCall;
  function FileByName(name: PAnsiChar): Integer; StdCall;
  function FileByAuthor(author: PAnsiChar): Integer; StdCall;
  function GetElementFile(_element: Integer): Integer; StdCall;
  function SaveFile(_file: Integer): WordBool; StdCall;

implementation

{******************************************************************************}
{ FILE HANDLING
  Methods for handling loaded files.
}
{******************************************************************************}

function NewFile(filename: PAnsiChar): Cardinal; StdCall;
begin
end;

function FileByIndex(index: Integer): Cardinal; StdCall;
begin
end;

function FileByLoadOrder(load_order: Integer): Cardinal; StdCall;
begin
end;

function FileByName(name: PAnsiChar): Cardinal; StdCall;
begin
end;

function FileByAuthor(author: PAnsiChar): Cardinal; StdCall;
begin
end;

function GetElementFile(_element: Cardinal): Cardinal; StdCall;
begin
end;

function SaveFile(_file: Cardinal): WordBool; StdCall;
begin
end;

end.
