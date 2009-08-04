library rlreportshared;
{$IFDEF LINUX}
{$define KYLIX}
{$ENDIF}
//{$DEFINE SHOWMESS}
{ Important note about exception handling across multiple
  binary modules (executables and shared objects):

  For exception handling to work across multiple binary
  modules, all binary modules need to use the same copy
  of the shared exception handling code.

  To accomplish this, the following must be done:

  1) All binary modules must be built with the same version
  of the rtl runtime package.

  2) All binary modules must include the ShareExcept unit
  as the very first unit in the main project file's uses
  clause.

  If this is not done, exceptions raised in one module may
  cause unintended side effects in other modules. }

uses
  {$IFDEF MSWINDOWS}
  Windows,
  Printers,
  WinSpool,
  Dialogs,
  {$ENDIF}
  SysUtils, Types, Classes, Math,
  {$IFDEF LINUX}
  QGraphics,
  QDialogs,
  {$ELSE}
  Graphics,
  {$ENDIF}
  rlreportqtcall in 'rlreportqtcall.pas';

type
 {$IFDEF KYLIX}
  TRGBQuad=packed record
    rgbBlue    :byte;
    rgbGreen   :byte;
    rgbRed     :byte;
    rgbReserved:byte;
  end;
 {$ENDIF}
  PRGBArray=^TRGBArray;
  TRGBArray=array[0..0] of TRGBQuad;

//function GetFileVersionInfoSize(lptstrFilename: PChar; var lpdwHandle: DWORD): DWORD; stdcall; external version name 'GetFileVersionInfoSizeA';

{$R *.res}

{$IFDEF KYLIX OR FPC}
function RGB(r, g, b: Byte): TColor;
begin
  Result := (r or (g shl 8) or (b shl 16));
end;
{$ENDIF}

function GetBitmapPixel(aBitmap:TBitmap; aX,aY:integer; aDefault:TColor):TColor;{$IFDEF WIN32}stdcall;{$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
begin
  if aY<aBitmap.Height then
    with TRGBArray(aBitmap.ScanLine[aY]^)[aX] do
      Result:=RGB(rgbRed,rgbGreen,rgbBlue)
  else
    Result:=aDefault;
end;

{$IFDEF MSWINDOWS}
function xGetPrinterDevice(Index:integer):pchar;stdcall;
begin
Result:='LPT1:';
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
function ZGetPrinterDevice(PrinterName:pchar):pchar;stdcall;
const
  foodefs:TPrinterDefaults=(pDatatype:nil; pDevMode:nil; DesiredAccess:PRINTER_ACCESS_USE);
var
  s:string;
  foodevice,foodriver,
  fooport    :array[0..255] of char;
  hdevicemode:THandle;
  ok         :boolean;
  pInfo      :PPrinterInfo2;
  bytesNeeded:DWORD;
  hPrinter   :THandle;
begin
  s:=string(Printername);
  {$IFDEF SHOWMESS}
  Showmessage(s);
  {$ENDIF}
  Printer.PrinterIndex:=Printer.Printers.IndexOf(s);
  {$IFDEF SHOWMESS}
  Showmessage(Printer.Printers.Text);
  Showmessage(Printer.Printers[Printer.PrinterIndex]);
  {$ENDIF}  
  try
    // GetCurrentPrinterHandle
    Printer.GetPrinter(foodevice,foodriver,fooport,hdevicemode);
    ok:=OpenPrinter(@foodevice,hPrinter,@foodefs);
{$ifdef DELPHI6}
    if not ok then RaiseLastOSError;
{$else}
    if not ok then RaiseLastWin32Error;
{$endif}
    try
      GetPrinter(hPrinter,2,nil,0,@bytesNeeded);
      if bytesNeeded=0 then
        bytesNeeded:=32768;
      pInfo:=AllocMem(bytesNeeded);
      try
        GetPrinter(hPrinter,2,pInfo,bytesNeeded,@bytesNeeded);
        if pInfo^.pServerName<>emptystr then
          Result:=pchar(pInfo^.pServerName+'\'+pInfo^.pShareName)
        else
          Result:=pchar(pInfo^.pPortName);
      finally
        FreeMem(pInfo);
      end;
    finally
      ClosePrinter(hPrinter);
    end;
  except
    Result:=pchar(emptystr);
  end;
end;
{$ENDIF}


procedure RotatePoints(var aPoints:array of TPoint; const aAngle:double);
var
  theta   :double;
  costheta:double;
  sintheta:double;
  center  :TPoint;
  i,q     :integer;
procedure RotatePoint(var aPoint:TPoint);
var
  saved:TPoint;
begin
  saved   :=aPoint;
  aPoint.x:=round(saved.x*costheta-saved.y*sintheta);
  aPoint.y:=round(saved.x*sintheta+saved.y*costheta);
end;
begin
  theta   :=-aAngle*pi/180; // radians
  sintheta:=sin(theta);
  costheta:=cos(theta);
  // calcula centro
  center.x:=0;
  center.y:=0;
  q:=High(aPoints)+1;
  for i:=0 to q-1 do
  begin
    inc(center.x,aPoints[i].x);
    inc(center.y,aPoints[i].y);
  end;
  center.x:=round(center.x/q);
  center.y:=round(center.y/q);
  // roda
  for i:=0 to q-1 do
  begin
    dec(aPoints[i].x,center.x);
    dec(aPoints[i].y,center.y);
    RotatePoint(aPoints[i]);
    inc(aPoints[i].x,center.x);
    inc(aPoints[i].y,center.y);
  end;
end;

function PointsRect(const aPoints:array of TPoint):TRect;
var
  i:integer;
begin
  for i:=0 to High(aPoints) do
    if i=0 then
    begin
      Result.Left  :=aPoints[i].x;
      Result.Top   :=aPoints[i].y;
      Result.Right :=aPoints[i].x;
      Result.Bottom:=aPoints[i].y;
    end
    else
    begin
      Result.Left  :=Min(Result.Left  ,aPoints[i].x);
      Result.Top   :=Min(Result.Top   ,aPoints[i].y);
      Result.Right :=Max(Result.Right ,aPoints[i].x);
      Result.Bottom:=Max(Result.Bottom,aPoints[i].y);
    end;
end;

function NewBitmap(Width,Height:integer):TBitmap;
begin
  Result:= TBitmap.Create;
  Result.Width      :=Width;
  Result.Height     :=Height;
  Result.PixelFormat:=pf32bit;
end;


procedure RotateBitmap(aSource,aDest:TBitmap; aAngle:double; aAxis,aOffset:TPoint);{$IFDEF WIN32}stdcall;{$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
const
  RGBBlack:TRGBQuad=(rgbBlue:0; rgbGreen:0; rgbRed:0; rgbReserved:0);
var
  x            :integer;
  xDest        :integer;
  xOriginal    :integer;
  xPrime       :integer;
  xPrimeRotated:integer;
  //
  y            :integer;
  yDest        :integer;
  yOriginal    :integer;
  yPrime       :integer;
  yPrimeRotated:integer;
  //
  RowSource    :PRGBArray;
  RowDest      :PRGBArray;
  //
  Radians      :double;
  RadiansCos   :double;
  RadiansSin   :double;
begin
  // Convert degrees to radians. Use minus sign to force clockwise rotation.
  Radians   :=aAngle*PI/180;
  RadiansSin:=sin(Radians);
  RadiansCos:=cos(Radians);
  // Step through each row of rotated image.
  for y:=0 to aDest.Height-1 do
  begin
    RowDest:=aDest.ScanLine[y];
    yDest  :=y-aOffset.y;
    yPrime :=2*(yDest-aAxis.y)+1; // center y: -1,0,+1
    // Step through each col of rotated image.
    for x:=0 to aDest.Width-1 do
    begin
      xDest :=x-aOffset.x;
      xPrime:=2*(xDest-aAxis.x)+1; // center x: -1,0,+1
      // Rotate (xPrime, yPrime) to location of desired pixel
      // Note:  There is negligible difference between floating point and scaled integer arithmetic here, so keep the math simple (and readable).
      xPrimeRotated:=round(xPrime*RadiansCos-yPrime*RadiansSin);
      yPrimeRotated:=round(xPrime*RadiansSin+yPrime*RadiansCos);
      // Transform back to pixel coordinates of image, including translation
      // of origin from axis of rotation to origin of image.
      xOriginal:=(xPrimeRotated-1) div 2+aAxis.x;
      yOriginal:=(yPrimeRotated-1) div 2+aAxis.y;
      // Make sure (xOriginal, yOriginal) is in aSource.  If not, assign blue color to corner points.
      if (xOriginal>=0) and (xOriginal<=aSource.Width-1) and (yOriginal>=0) and (yOriginal<=aSource.Height-1) then
      begin
        // Assign pixel from rotated space to current pixel in aDest
        RowSource :=aSource.ScanLine[yOriginal];
        RowDest[x]:=RowSource[xOriginal];
      end
      else if aSource.Height>0 then
      begin
        RowSource :=aSource.ScanLine[0];
        RowDest[x]:=RowSource[0];
      end
      else
        RowDest[x]:=RGBBlack;
    end;
  end;
end;

function RotatedBitmap(aSource:TBitmap; aAngle:double):TBitmap;
var
  p:array[0..3] of TPoint;
  r:TRect;
begin
  p[0]:=Point(0,0);
  p[1]:=Point(aSource.Width-1,0);
  p[2]:=Point(aSource.Width-1,aSource.Height-1);
  p[3]:=Point(0,aSource.Height-1);
  RotatePoints(p,aAngle);
  r:=PointsRect(p);
  //
  Result:=NewBitmap(r.Right-r.Left,r.Bottom-r.Top);
  try
    Result.Transparent     :=aSource.Transparent;
    Result.TransparentColor:=aSource.TransparentColor;
    Result.TransparentMode :=aSource.TransparentMode;
    RotateBitmap(aSource,Result,aAngle,Point(aSource.Width div 2,aSource.Height div 2),Point(-r.Left,-r.Top));
  except
    Result.free;
    raise;
  end;
end;

//procedure rlRotateBitmap(rlfileBitmap,rlfileBitmapResult:pchar;rlAngle:double);{$IFDEF WIN32}stdcall;{$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
procedure rlRotateBitmap(rlAngle:double);{$IFDEF WIN32}stdcall;{$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
var
bitmap1,bitmap2:TBitMap;
file1:string;
tsfile:TStringlist;
begin
tsfile:=TStringlist.Create;
{$IFDEF LINUX}
tsfile.loadfromfile('/tmp/rlbcod.dat');
{$ENDIF}
writeln('Carregou rlRotateBitmap');
bitmap1:=TBitmap.Create;
writeln('bitmap1 ok');
//file1:=string(rlfileBitmap);
file1:=tsfile[0];
writeln('Carregando '+file1);
bitmap1.loadfromfile(file1);
writeln('Chamando RotatedBitmap');
bitmap2:=RotatedBitmap(bitmap1,rlAngle);
//file1:=string(rlfileBitmapResult);
file1:=tsfile[1];
bitmap2.savetofile(file1);
bitmap1.free;
bitmap2.free;
end;


procedure DoColorTable(aSource,aDest:TBitmap; aContrast:double=2);{$IFDEF WIN32}stdcall;{$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
const
  MatrixSize=4;
  TheMatrix :array[0..MatrixSize-1,0..MatrixSize-1] of byte=((  0,192, 48,240),
                                                             (128, 64,176,112),
                                                             ( 32,224, 16,208),
                                                             (160, 96,144, 80));
  PlotColors:array[boolean] of TColor=(clWhite,clBlack);
var
  x,y,i:integer;
  plot :boolean;
  srcln:PRGBArray;
  dstln:PRGBArray;
begin
  aSource.PixelFormat:=pf32bit;
  aDest.PixelFormat  :=pf32bit;
  aDest.Width        :=aSource.Width;
  aDest.Height       :=aSource.Height;
  for y:=0 to aSource.Height-1 do
  begin
    srcln:=aSource.ScanLine[y];
    dstln:=aDest.ScanLine[y];
    for x:=0 to aSource.Width-1 do
    begin
      with srcln[x] do
      i   :=(rgbRed+rgbGreen+rgbBlue) div 3;
      i   :=Trunc(((i-128)*aContrast+128));
      plot:=(i<TheMatrix[y mod MatrixSize,x mod MatrixSize]);
      with dstln[x] do
      begin
        rgbRed  :=Byte(not plot)*255;
        rgbGreen:=Byte(not plot)*255;
        rgbBlue :=Byte(not plot)*255;
      end;
    end;
  end;
end;

procedure DoErrorDiffusion(aSource,aDest:TBitmap; aContrast:double=2);{$IFDEF WIN32}stdcall;{$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
const
  PlotColors:array[boolean] of TColor=(clWhite,clBlack);
var
  x,y,i:integer;
  plot :boolean;
  srcln:PRGBArray;
  dstln:PRGBArray;
begin
  aSource.PixelFormat:=pf32bit;
  aDest.PixelFormat  :=pf32bit;
  aDest.Width        :=aSource.Width;
  aDest.Height       :=aSource.Height;
  for y:=0 to aSource.Height-1 do
  begin
    srcln:=aSource.ScanLine[y];
    dstln:=aDest.ScanLine[y];
    for x:=0 to aSource.Width-1 do
    begin
      with srcln[x] do
        i:=(rgbRed+rgbGreen+rgbBlue) div 3;
      i   :=Trunc(((i-128)*aContrast+128));
      plot:=(i<Random(256));
      with dstln[x] do
      begin
        rgbRed  :=Byte(not plot)*255;
        rgbGreen:=Byte(not plot)*255;
        rgbBlue :=Byte(not plot)*255;
      end;
    end;
  end;
end;

var
  AuxBitmap:TBitmap;

function AuxBitmapNeeded(Width,Height:integer):TBitmap;
{$IFDEF KYLIX}
type
  TRGBQuad=packed record
    rgbBlue    :byte;
    rgbGreen   :byte;
    rgbRed     :byte;
    rgbReserved:byte;
  end;
{$ENDIF}

begin
  if not Assigned(AuxBitmap) then
  begin
    AuxBitmap:=TBitmap.Create;
    AuxBitmap.Width      :=Width;
    AuxBitmap.Height     :=Height;
    AuxBitmap.PixelFormat:=pf32bit;
  end
  else
  begin
    AuxBitmap.Width :=Width;
    AuxBitmap.Height:=Height;
  end;
  Result:=AuxBitmap;
end;

var
  LastFontName   :string;
  LastFontSize   :integer;
  LastFontStyle  :TFontStyles;
  LastFontDescent:integer=-1;

function CanvasGetDescent(aCanvas:TCanvas):integer;{$IFDEF WIN32}stdcall;{$ENDIF}{$IFDEF LINUX}cdecl;{$ENDIF}
var
  stl:TFontStyles;
  aux:TBitmap;
  x,y:integer;

begin
  stl:=aCanvas.Font.Style-[fsUnderline];
  if (LastFontDescent=-1) or (aCanvas.Font.Name<>LastFontName) or (aCanvas.Font.Size<>LastFontSize) or (stl<>LastFontStyle) then
  begin
    aux:=AuxBitmapNeeded(1,1);
    aux.Canvas.Font.Name  :=aCanvas.Font.Name;
    aux.Canvas.Font.Size  :=aCanvas.Font.Size;
    aux.Canvas.Font.Style :=stl;
    aux.Canvas.Font.Color :=clWhite;
    aux.Canvas.Brush.Style:=bsSolid;
    aux.Canvas.Brush.Color:=clBlack;
    aux.Width :=aux.Canvas.TextWidth('L');
    aux.Height:=aux.Canvas.TextHeight('L');
    aux.Canvas.TextOut(0,0,'L');
    y:=aux.Height-1;
    while y>=0 do
    begin
      x:=0;
      while x<aux.Width do
      begin
        with TRGBArray(aux.ScanLine[y]^)[x] do
          if RGB(rgbRed,rgbGreen,rgbBlue)<>0 then
            Break;
        Inc(x);
      end;
      if x<aux.Width then
        Break;
      Dec(y);
    end;
    LastFontName   :=aCanvas.Font.Name;
    LastFontSize   :=aCanvas.Font.Size;
    LastFontStyle  :=stl;
    LastFontDescent:=aux.Height-1-y;
  end;
  Result:=LastFontDescent;
end;


exports
  {$IFDEF MSWINDOWS}
  ZGetPrinterDevice,// name 'ZGetPrinterDevice',
  {$ENDIF}
  DoColorTable    , //name 'DoColorTable',
  DoErrorDiffusion, //name 'DoErrorDiffusion',
  GetBitmapPixel  , //name 'GetBitmapPixel',
  CanvasGetDescent, //name 'CanvasGetDescent',
  rlRotateBitmap;// name 'rlRotateBitmap';

begin

end.

