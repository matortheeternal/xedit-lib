unit txResources;

interface

  // public testing interface
  procedure BuildResourceTests;

implementation

uses
  SysUtils, Classes,
  Mahogany,
{$IFDEF USE_DLL}
  txImports,
{$ENDIF}
{$IFNDEF USE_DLL}
  xeResources, xeMeta,
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

procedure BuildResourceTests;
var
  len, w, h: Integer;
  dataPath, container: String;
  bytes: TBytes;
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

      Describe('GetBitmapResource', procedure
        begin
          It('Should fail if the resource does not exist', procedure
            begin
              ExpectFailure(GetTextureData('abcdefghijk', @w, @h));
            end);

          It('Should return correct bitmap if resource exists', procedure
            begin
              ExpectSuccess(GetTextureData('textures\sky\skyrimclouds01.dds', @w, @h));
              bytes := grb(w * h * 4);
            end);
        end);
    end);
end;

end.
