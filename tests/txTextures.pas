unit txTextures;

interface

  // public testing interface
  procedure BuildTextureTests;

implementation

uses
  SysUtils,
  Mahogany,
{$IFDEF USE_DLL}
  txImports,
{$ENDIF}
{$IFNDEF USE_DLL}
  xeTextures,
{$ENDIF}
  txMeta;

procedure BuildTextureTests;
var
  w, h: Integer;
  bytes: TBytes;
begin
  Describe('Texture Functions', procedure
    begin
      Describe('GetBitmapResource', procedure
        begin
          It('Should fail if the resource does not exist', procedure
            begin
              ExpectFailure(GetBitmapResource('abcdefghijk', @w, @h));
            end);

          It('Should return correct bitmap if resource exists', procedure
            begin
              ExpectSuccess(GetBitmapResource('textures\sky\skyrimclouds01.dds', @w, @h));
              bytes := grb(w * h * 4);
            end);
        end);
    end);
end;

end.
