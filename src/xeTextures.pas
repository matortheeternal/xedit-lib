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
  x, y, offset: Integer;
  rect: TColor32Rec;
  bitmap: TImageData;
begin
  SetLength(resultBytes, img.Width * img.Height * 4);
  InitImage(bitmap);
  CloneImage(img, bitmap);
  ConvertImage(bitmap, ifA8R8G8B8);
  for y := 0 to Pred(bitmap.Height) do
    for x := 0 to Pred(bitmap.Width) do begin
      rect := GetPixel32(bitmap, x, y);
      offset := (y * bitmap.Width + x) * 4;
      resultBytes[offset]     := rect.R;
      resultBytes[offset + 1] := rect.G;
      resultBytes[offset + 2] := rect.B;
      resultBytes[offset + 3] := rect.A;
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
