unit xeTextures;

interface

  {$region 'API functions'}
  function GetBitmapResource(resourceName: PWideChar; width, height: PInteger): WordBool; cdecl;
  {$endregion}

implementation

uses
  SysUtils, Vcl.Graphics, Classes,
  // xelib modules
  xeMessages, xeMeta,
  // xedit units
  wbInterface, Imaging, ImagingTypes;

procedure StoreImageBytes(img: TImageData);
var
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  try
    if SaveImageToStream('BMP', ms, img) then begin
      ms.Position := 0;
      ms.Write(resultBytes, Length(resultBytes));
    end;
  finally
    ms.Free;
  end;
end;

function GetBitmapResource(resourceName: PWideChar; width, height: PInteger): WordBool; cdecl;
var
  data: TBytes;
  img: TImageData;
begin
  Result := False;
  try
    data := wbContainerHandler.OpenResourceData('', string(resourceName));
    if not LoadImageFromMemory(@data[0], Length(data), img) then
      Exit;
    try
      width^ := img.Width;
      height^ := img.Height;
      StoreImageBytes(img);
      Result := True;
    finally
      FreeImage(img);
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

end.
