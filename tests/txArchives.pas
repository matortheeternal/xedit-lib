unit txArchives;

interface

  // public testing interface
  procedure BuildArchiveTests;

implementation

uses
  SysUtils, Classes,
  Mahogany,
{$IFDEF USE_DLL}
  txImports,
{$ENDIF}
{$IFNDEF USE_DLL}
  xeArchives, xeMeta,
{$ENDIF}
  txMeta;


procedure TestGetContainerFiles(container, filter: PWideChar; expectedFiles: Integer);
var
  len: Integer;
  sl: TStringList;
begin
  ExpectSuccess(GetContainerFiles(PWideChar(container), filter, @len));
  sl := TStringList.Create;
  try
    sl.Text := grs(len);
    ExpectEqual(sl.Count, expectedFiles, Format('There should be %d files in %s',
      [expectedFiles, container]));
  finally
    sl.Free;
  end;
end;

procedure BuildArchiveTests;
var
  len: Integer;
  dataPath, container: String;
begin
  Describe('Archive Functions', procedure
    begin
      BeforeAll(procedure
        begin
          ExpectSuccess(GetGlobal('DataPath', @len));
          dataPath := grs(len);
        end);

      Describe('GetContainerFiles', procedure
        begin
          It('Should fail if the container is not loaded', procedure
            begin
              ExpectFailure(GetContainerFiles('blah', '', @len));
            end);

          It('Should return files in container', procedure
            begin
              container := dataPath + 'Skyrim - Shaders.bsa';
              TestGetContainerFiles(PWideChar(container), '', 122);
            end);

          It('Should filter properly', procedure
            begin
              container := dataPath + 'Skyrim - Textures.bsa';
              TestGetContainerFiles(PWideChar(container), 'textures\sky\', 47);
            end);
        end);
    end);
end;

end.
