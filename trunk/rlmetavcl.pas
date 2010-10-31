{$I RLReport.inc}
{@unit RLMetaVCL
Portado para o Lazarus - Trabalho inicial de Isaac Trindade da Silva contato tioneobrasil@yahoo.com.br dicas4lazarus@yahoo.com.br
Lazarus Ported - initial work by Isaac 07/2009

}
unit RLMetaVCL;

{$MODE DELPHI}{$H+}

interface

uses
  {$ifdef LCLWin32}
  Windows,
  {$endif}
  LCLIntf, LCLType, LCLProc, SysUtils, Graphics, Classes, Math, StdCtrls,
  RLMetaFile, RLUtils, RLConsts;

type
  TPointArray=array of TPoint;

function  ToMetaRect(const aSource:TRect):TRLMetaRect;
function  ToMetaColor(aSource:TColor):TRLMetaColor;
function  ToMetaPenMode(aSource:TPenMode):TRLMetaPenMode;
function  ToMetaPenStyle(aSource:TPenStyle):TRLMetaPenStyle;
procedure ToMetaPen(aSource:TPen; aDest:TRLMetaPen);
function  ToMetaBrushStyle(aSource:TBrushStyle):TRLMetaBrushStyle;
procedure ToMetaBrush(aSource:TBrush; aDest:TRLMetaBrush);
function  ToMetaPoint(const aSource:TPoint):TRLMetaPoint;
function  ToMetaPointArray(const aSource:array of TPoint):TRLMetaPointArray;
function  ToMetaFontCharset(aSource:TFontCharset):TRLMetaFontCharset;
function  ToMetaFontPitch(aSource:TFontPitch):TRLMetaFontPitch;
function  ToMetaFontStyles(aSource:TFontStyles):TRLMetaFontStyles;
procedure ToMetaFont(aSource:TFont; aDest:TRLMetaFont);
function  ToMetaGraphic(aSource:TGraphic):string;
function  ToMetaTextAlignment(aSource:TAlignment):TRLMetaTextAlignment;
function  ToMetaTextLayout(aSource:TTextLayout):TRLMetaTextLayout;  
function  FromMetaRect(const aSource:TRLMetaRect):TRect;
function  FromMetaPoint(const aSource:TRLMetaPoint):TPoint;
function  FromMetaColor(const aSource:TRLMetaColor):TColor;
function  FromMetaPenMode(aSource:TRLMetaPenMode):TPenMode;
function  FromMetaPenStyle(aSource:TRLMetaPenStyle):TPenStyle;
procedure FromMetaPen(aSource:TRLMetaPen; aDest:TPen);
function  FromMetaBrushStyle(aSource:TRLMetaBrushStyle):TBrushStyle;
procedure FromMetaBrush(aSource:TRLMetaBrush; aDest:TBrush);
function  FromMetaFontStyles(aSource:TRLMetaFontStyles):TFontStyles;
function  FromMetaFontCharset(aSource:TRLMetaFontCharset):TFontCharset;
function  FromMetaFontPitch(aSource:TRLMetaFontPitch):TFontPitch;
procedure FromMetaFont(aSource:TRLMetaFont; aDest:TFont; aFactor:double=1);
function  FromMetaGraphic(const aSource:string):TGraphic;
function  FromMetaPointArray(const aSource:TRLMetaPointArray):TPointArray;
function  FromMetaTextAlignment(aSource:TRLMetaTextAlignment):TAlignment;
function  FromMetaTextLayout(aSource:TRLMetaTextLayout):TTextLayout;
//
procedure PenInflate(aPen:TPen; aFactor:double);
procedure CanvasStart(aCanvas:TCanvas);
procedure CanvasStop(aCanvas:TCanvas);
function  CanvasGetClipRect(aCanvas:TCanvas):TRect;
procedure CanvasSetClipRect(aCanvas:TCanvas; const aRect:TRect);
procedure CanvasResetClipRect(aCanvas:TCanvas);
function  CanvasGetRectData(aCanvas:TCanvas; const aRect:TRect):string;
procedure CanvasSetRectData(aCanvas:TCanvas; const aRect:TRect; const aData:string; aParity:boolean);
procedure CanvasStretchDraw(aCanvas:TCanvas; const aRect:TRect; const aData:string; aParity:boolean);
procedure CanvasTextRectEx(aCanvas:TCanvas; const aRect:TRect; aX,aY:integer; const aText:string; aAlignment:TRLMetaTextAlignment; aLayout:TRLMetaTextLayout; aTextFlags:TRLMetaTextFlags);
function  CanvasGetPixels(aCanvas:TCanvas; X,Y:integer):TColor;
function  CanvasTextWidth(aCanvas:TCanvas; const aText:string):integer;
function  CanvasTextHeight(aCanvas:TCanvas; const aText:string):integer;
procedure CanvasLineToEx(aCanvas:TCanvas; X,Y:integer);
procedure FontGetMetrics(const aFontName:string; aFontStyles:TFontStyles; var aFontRec:TRLMetaFontMetrics);
function  CanvasGetDescent(aCanvas:TCanvas):integer;


implementation

uses
  IntfGraphics, FPimage;

{ CONVERSION }

function ToMetaRect(const aSource:TRect):TRLMetaRect;
begin
  result.Left  :=aSource.Left;
  result.Top   :=aSource.Top;
  result.Right :=aSource.Right;
  result.Bottom:=aSource.Bottom;
end;

function ToMetaColor(aSource:TColor):TRLMetaColor;
var
  rgb:cardinal;
begin
  rgb:=ColorToRGB(aSource);
  result.Red  :=byte(rgb);
  result.Green:=byte(rgb shr 8);
  result.Blue :=byte(rgb shr 16);
end;

function ToMetaPenMode(aSource:TPenMode):TRLMetaPenMode;
begin
  case aSource of
    pmBlack      : result:=MetaPenModeBlack;
    pmWhite      : result:=MetaPenModeWhite;
    pmNop        : result:=MetaPenModeNop;
    pmNot        : result:=MetaPenModeNot;
    pmCopy       : result:=MetaPenModeCopy;
    pmNotCopy    : result:=MetaPenModeNotCopy;
    pmMergePenNot: result:=MetaPenModeMergePenNot;
    pmMaskPenNot : result:=MetaPenModeMaskPenNot;
    pmMergeNotPen: result:=MetaPenModeMergeNotPen;
    pmMaskNotPen : result:=MetaPenModeMaskNotPen;
    pmMerge      : result:=MetaPenModeMerge;
    pmNotMerge   : result:=MetaPenModeNotMerge;
    pmMask       : result:=MetaPenModeMask;
    pmNotMask    : result:=MetaPenModeNotMask;
    pmXor        : result:=MetaPenModeXor;
    pmNotXor     : result:=MetaPenModeNotXor;
  else
    result:=MetaPenModeCopy;
  end;
end;

function ToMetaPenStyle(aSource:TPenStyle):TRLMetaPenStyle;
begin
  case aSource of
    psSolid      : result:=MetaPenStyleSolid;
    psDash       : result:=MetaPenStyleDash;
    psDot        : result:=MetaPenStyleDot;
    psDashDot    : result:=MetaPenStyleDashDot;
    psDashDotDot : result:=MetaPenStyleDashDotDot;
    psClear      : result:=MetaPenStyleClear;
    psInsideFrame: result:=MetaPenStyleInsideFrame;
  else
    result:=MetaPenStyleSolid;
  end;
end;

procedure ToMetaPen(aSource:TPen; aDest:TRLMetaPen);
begin
  aDest.Color:=ToMetaColor(aSource.Color);
  aDest.Mode :=ToMetaPenMode(aSource.Mode);
  aDest.Style:=ToMetaPenStyle(aSource.Style);
  aDest.Width:=aSource.Width;
end;

function ToMetaBrushStyle(aSource:TBrushStyle):TRLMetaBrushStyle;
begin
  case aSource of
    bsSolid     : result:=MetaBrushStyleSolid;
    bsClear     : result:=MetaBrushStyleClear;
    bsHorizontal: result:=MetaBrushStyleHorizontal;
    bsVertical  : result:=MetaBrushStyleVertical;
    bsFDiagonal : result:=MetaBrushStyleFDiagonal;
    bsBDiagonal : result:=MetaBrushStyleBDiagonal;
    bsCross     : result:=MetaBrushStyleCross;
    bsDiagCross : result:=MetaBrushStyleDiagCross;
  else
    result:=MetaBrushStyleSolid;
  end;
end;

procedure ToMetaBrush(aSource:TBrush; aDest:TRLMetaBrush);
begin
  aDest.Color:=ToMetaColor(aSource.Color);
  aDest.Style:=ToMetaBrushStyle(aSource.Style);
end;

function ToMetaPoint(const aSource:TPoint):TRLMetaPoint;
begin
  result.X:=aSource.X;
  result.Y:=aSource.Y;
end;

function ToMetaPointArray(const aSource:array of TPoint):TRLMetaPointArray;
var
  i:integer;
begin
  SetLength(result,High(aSource)+1);
  for i:=0 to High(aSource) do
    result[i]:=ToMetaPoint(aSource[i]);
end;

function ToMetaFontCharset(aSource:TFontCharset):TRLMetaFontCharset;
begin
  result:=TRLMetaFontCharset(aSource);
end;

function ToMetaFontPitch(aSource:TFontPitch):TRLMetaFontPitch;
begin
  case aSource of
    fpDefault : result:=MetaFontPitchDefault;
    fpVariable: result:=MetaFontPitchVariable;
    fpFixed   : result:=MetaFontPitchFixed;
  else
    result:=MetaFontPitchDefault;
  end;
end;

function ToMetaFontStyles(aSource:TFontStyles):TRLMetaFontStyles;
begin
  result:=0;
  if fsBold in aSource then
    result:=result or MetaFontStyleBold;
  if fsItalic in aSource then
    result:=result or MetaFontStyleItalic;
  if fsUnderline in aSource then
    result:=result or MetaFontStyleUnderline;
  if fsStrikeOut in aSource then
    result:=result or MetaFontStyleStrikeOut;
end;

procedure ToMetaFont(aSource:TFont; aDest:TRLMetaFont);
begin
  aDest.PixelsPerInch:=aSource.PixelsPerInch;
  aDest.Charset      :=ToMetaFontCharset(aSource.Charset);
  aDest.Color        :=ToMetaColor(aSource.Color);
  aDest.Height       :=aSource.Height;
  aDest.Name         :=aSource.Name;
  aDest.Pitch        :=ToMetaFontPitch(aSource.Pitch);
  aDest.Size         :=aSource.Size;
  aDest.Style        :=ToMetaFontStyles(aSource.Style);
end;

function ToMetaGraphic(aSource:TGraphic):string;
var
  s:TStringStream;
  m:TBitmap;
  g:TGraphic;
begin
  s:=TStringStream.Create('');
  try
    g:=aSource;
    // identifica os tipos nativos
    if g=nil then
      s.WriteString('NIL')
    else if g is TBitmap then
      s.WriteString('BMP')
    else if g is TIcon then
      s.WriteString('ICO')
    else
    begin
      // qualquer outro formato é transformado em bmp para ficar compatível com um carregador de qualquer plataforma
      m:=NewBitmap(aSource.Width,aSource.Height); ///Correção erro de acesso
      g:=m;
      m.Canvas.Draw(0,0,aSource);
      s.WriteString('BMP');
    end;
    if Assigned(g) then
      g.SaveToStream(s);
    result:=s.DataString;
  finally
    FreeAndNil(s); /// Liberar a memoria
  end;
end;

function ToMetaTextAlignment(aSource:TAlignment):TRLMetaTextAlignment;
begin
  case aSource of
    taLeftJustify : result:=MetaTextAlignmentLeft;
    taRightJustify: result:=MetaTextAlignmentRight;
    taCenter      : result:=MetaTextAlignmentCenter;
  else
    if aSource=succ(taCenter) then
      result:=MetaTextAlignmentJustify
    else
      result:=MetaTextAlignmentLeft;
  end;
end;

function ToMetaTextLayout(aSource:TTextLayout):TRLMetaTextLayout;
begin
  case aSource of
    tlTop   : result:=MetaTextLayoutTop;
    tlBottom: result:=MetaTextLayoutBottom;
    tlCenter: result:=MetaTextLayoutCenter;
  else
    if aSource=succ(tlCenter) then
      result:=MetaTextLayoutJustify
    else
      result:=MetaTextLayoutTop;
  end;
end;

function FromMetaRect(const aSource:TRLMetaRect):TRect;
begin
  result.Left  :=aSource.Left;
  result.Top   :=aSource.Top;
  result.Right :=aSource.Right;
  result.Bottom:=aSource.Bottom;
end;

function FromMetaPoint(const aSource:TRLMetaPoint):TPoint;
begin
  result.X:=aSource.X;
  result.Y:=aSource.Y;
end;

function FromMetaColor(const aSource:TRLMetaColor):TColor;
begin
  result:=RGB(aSource.Red,aSource.Green,aSource.Blue);
end;

function FromMetaPenMode(aSource:TRLMetaPenMode):TPenMode;
begin
  case aSource of
    MetaPenModeBlack      : result:=pmBlack;
    MetaPenModeWhite      : result:=pmWhite;
    MetaPenModeNop        : result:=pmNop;
    MetaPenModeNot        : result:=pmNot;
    MetaPenModeCopy       : result:=pmCopy;
    MetaPenModeNotCopy    : result:=pmNotCopy;
    MetaPenModeMergePenNot: result:=pmMergePenNot;
    MetaPenModeMaskPenNot : result:=pmMaskPenNot;
    MetaPenModeMergeNotPen: result:=pmMergeNotPen;
    MetaPenModeMaskNotPen : result:=pmMaskNotPen;
    MetaPenModeMerge      : result:=pmMerge;
    MetaPenModeNotMerge   : result:=pmNotMerge;
    MetaPenModeMask       : result:=pmMask;
    MetaPenModeNotMask    : result:=pmNotMask;
    MetaPenModeXor        : result:=pmXor;
    MetaPenModeNotXor     : result:=pmNotXor;
  else
    result:=pmCopy;
  end;
end;

function FromMetaPenStyle(aSource:TRLMetaPenStyle):TPenStyle;
begin
  case aSource of
    MetaPenStyleSolid      : result:=psSolid;
    MetaPenStyleDash       : result:=psDash;
    MetaPenStyleDot        : result:=psDot;
    MetaPenStyleDashDot    : result:=psDashDot;
    MetaPenStyleDashDotDot : result:=psDashDotDot;
    MetaPenStyleClear      : result:=psClear;
    MetaPenStyleInsideFrame: result:=psInsideFrame;
  else
    result:=psSolid;
  end;
end;

procedure FromMetaPen(aSource:TRLMetaPen; aDest:TPen);
begin
  aDest.Color:=FromMetaColor(aSource.Color);
  aDest.Mode :=FromMetaPenMode(aSource.Mode);
  aDest.Style:=FromMetaPenStyle(aSource.Style);
  aDest.Width:=aSource.Width;
end;

function FromMetaBrushStyle(aSource:TRLMetaBrushStyle):TBrushStyle;
begin
  case aSource of
    MetaBrushStyleSolid     : result:=bsSolid;
    MetaBrushStyleClear     : result:=bsClear;
    MetaBrushStyleHorizontal: result:=bsHorizontal;
    MetaBrushStyleVertical  : result:=bsVertical;
    MetaBrushStyleFDiagonal : result:=bsFDiagonal;
    MetaBrushStyleBDiagonal : result:=bsBDiagonal;
    MetaBrushStyleCross     : result:=bsCross;
    MetaBrushStyleDiagCross : result:=bsDiagCross;
  else
    result:=bsSolid;
  end;
end;

procedure FromMetaBrush(aSource:TRLMetaBrush; aDest:TBrush);
begin
  aDest.Color:=FromMetaColor(aSource.Color);
  aDest.Style:=FromMetaBrushStyle(aSource.Style);
end;

function FromMetaFontStyles(aSource:TRLMetaFontStyles):TFontStyles;
begin
  result:=[];
  if (MetaFontStyleBold and aSource)=MetaFontStyleBold then
    Include(result,fsBold);
  if (MetaFontStyleItalic and aSource)=MetaFontStyleItalic then
    Include(result,fsItalic);
  if (MetaFontStyleUnderline and aSource)=MetaFontStyleUnderline then
    Include(result,fsUnderline);
  if (MetaFontStyleStrikeOut and aSource)=MetaFontStyleStrikeOut then
    Include(result,fsStrikeOut);
end;

function FromMetaFontCharset(aSource:TRLMetaFontCharset):TFontCharset;
begin
  result:=TFontCharset(aSource);
end;

function FromMetaFontPitch(aSource:TRLMetaFontPitch):TFontPitch;
begin
  case aSource of
    MetaFontPitchDefault : result:=fpDefault;
    MetaFontPitchVariable: result:=fpVariable;
    MetaFontPitchFixed   : result:=fpFixed;
  else
    result:=fpDefault;
  end;
end;

procedure FromMetaFont(aSource:TRLMetaFont; aDest:TFont; aFactor:double=1);
var
  a,b:integer;
begin
  a:=aSource.PixelsPerInch;
  if a=0 then
    a:=ScreenPPI;
  b:=aDest.PixelsPerInch;
  if b=0 then
    b:=ScreenPPI;
  //  
//  aDest.PixelsPerInch:=aSource.PixelsPerInch;
  aDest.Charset      :=FromMetaFontCharset(aSource.Charset);
  aDest.Color        :=FromMetaColor(aSource.Color);
  aDest.Height       :=aSource.Height;
  aDest.Name         :=aSource.Name;
  aDest.Pitch        :=FromMetaFontPitch(aSource.Pitch);
  aDest.Size         :=Round(aSource.Size*aFactor*a/b);
  aDest.Style        :=FromMetaFontStyles(aSource.Style);
end;

function FromMetaGraphic(const aSource:string):TGraphic;
var
  s:TStringStream;
  t:string;
begin
  if aSource='' then
    result:=nil
  else
  begin
    s:=TStringStream.Create(aSource);
    try
      s.Seek(0,soFromBeginning);
      t:=s.ReadString(3);
      if t='NIL' then
        result:=nil
      else if t='BMP' then
        result:=NewBitmap
      else if t='ICO' then
        result:=TIcon.Create
      else
        result:=nil;
      if Assigned(result) then
        result.LoadFromStream(s);
    finally
      s.free;
    end;
  end;  
end;

function FromMetaPointArray(const aSource:TRLMetaPointArray):TPointArray;
var
  i:integer;
begin
  SetLength(result,High(aSource)+1);
  for i:=0 to High(aSource) do
    result[i]:=FromMetaPoint(aSource[i]);
end;

function FromMetaTextAlignment(aSource:TRLMetaTextAlignment):TAlignment;
begin
  case aSource of
    MetaTextAlignmentLeft   : result:=taLeftJustify;
    MetaTextAlignmentRight  : result:=taRightJustify;
    MetaTextAlignmentCenter : result:=taCenter;
    MetaTextAlignmentJustify: result:=succ(taCenter);
  else
    result:=taLeftJustify;
  end;
end;

function FromMetaTextLayout(aSource:TRLMetaTextLayout):TTextLayout;
begin
  case aSource of
    MetaTextLayoutTop    : result:=tlTop;
    MetaTextLayoutBottom : result:=tlBottom;
    MetaTextLayoutCenter : result:=tlCenter;
    MetaTextLayoutJustify: result:=succ(tlCenter);
  else
    result:=tlTop;
  end;
end;

{ MISC }

procedure PenInflate(aPen:TPen; aFactor:double);
begin
  if aPen.Width>1 then
    aPen.Width:=Max(1,Trunc(aPen.Width*aFactor));
end;

procedure CanvasStart(aCanvas:TCanvas);
begin
end;

procedure CanvasStop(aCanvas:TCanvas);
begin
end;

function CanvasGetClipRect(aCanvas:TCanvas):TRect;
begin
  GetClipBox(aCanvas.Handle,@result);
end;

procedure CanvasSetClipRect(aCanvas:TCanvas; const aRect:TRect);
var
  isnull:boolean;
begin
  isnull:=((aRect.Right-aRect.Left)=0) or ((aRect.Bottom-aRect.Top)=0);
  if isnull then
    SelectClipRgn(aCanvas.Handle,0)
  else
  begin
    SelectClipRgn(aCanvas.Handle,0);
    IntersectClipRect(aCanvas.Handle,aRect.Left,aRect.Top,aRect.Right,aRect.Bottom);
  end;
end;

procedure CanvasResetClipRect(aCanvas:TCanvas);
begin
  SelectClipRgn(aCanvas.Handle,0);
end;

function CanvasGetRectData(aCanvas:TCanvas; const aRect:TRect):string;
var
  graphic:TBitmap;
begin
  graphic:=AuxBitmapNeeded(aRect.Right-aRect.Left,aRect.Bottom-aRect.Top);
  graphic.Canvas.CopyRect(Rect(0,0,graphic.Width,graphic.Height),aCanvas,aRect);
  Result:=ToMetaGraphic(graphic);
end;

procedure CanvasSetRectData(aCanvas:TCanvas; const aRect:TRect; const aData:string; aParity:boolean);
var
  graphic:TGraphic;
  auxrect:TRect;
  aux    :integer;
begin
  graphic:=FromMetaGraphic(aData);
  if graphic<>nil then
    try
      auxrect:=aRect;
      if aParity then
      begin
        aux          :=(auxrect.Right-auxrect.Left) div graphic.Width;
        auxrect.Right:=auxrect.Left+aux*graphic.Width+1;
      end;
      aCanvas.StretchDraw(auxrect,graphic);
    finally
      graphic.free;
    end;
end;

procedure CanvasStretchDraw(aCanvas:TCanvas; const aRect:TRect; const aData:string; aParity:boolean);
begin
  CanvasSetRectData(aCanvas,aRect,aData,aParity);
end;

procedure CanvasTextRectEx(aCanvas:TCanvas; const aRect:TRect; aX,aY:integer; const aText:string; aAlignment:TRLMetaTextAlignment; aLayout:TRLMetaTextLayout; aTextFlags:TRLMetaTextFlags);
var
  delta,left,top,txtw,txth,wid,i:integer;
  buff:string;
begin
  buff :=aText;
  delta:=aCanvas.TextWidth(' ') div 2;
  txtw :=aCanvas.TextWidth(buff+' ');
  txth :=aCanvas.TextHeight(buff+' ');
  case aAlignment of
    MetaTextAlignmentCenter: left:=(aRect.Left+aRect.Right-txtw) div 2+delta;
    MetaTextAlignmentRight : left:=aRect.Right-txtw+delta;
  else
    left:=aX+delta;
  end;
  case aLayout of
    MetaTextLayoutCenter: top:=(aRect.Top+aRect.Bottom-txth) div 2;
    MetaTextLayoutBottom: top:=aRect.Bottom-txth;
  else
    top:=aY;
  end;
  if aAlignment=MetaTextAlignmentJustify then
  begin
    wid:=aRect.Right-left;
    i  :=Length(buff);
    while (aCanvas.TextWidth(buff+#32)<=wid) and IterateJustification(buff,i) do;
  end;
  if (aTextFlags and MetaTextFlagAutoSize)=MetaTextFlagAutoSize then
    aCanvas.TextOut(left,top,buff)
  else
    aCanvas.TextRect(aRect,left,top,buff);
end;

function CanvasGetPixels(aCanvas:TCanvas; X,Y:integer):TColor;
begin
  result:=aCanvas.Pixels[X,Y];
end;

function CanvasTextWidth(aCanvas:TCanvas; const aText:string):integer;
begin
  Result:=aCanvas.TextWidth(aText);
end;

function CanvasTextHeight(aCanvas:TCanvas; const aText:string):integer;
begin
  Result:=aCanvas.TextHeight(aText);
end;

type
  TLinePattern=record
    Count  :byte;
    Lengths:array[0..5] of byte;
  end;

var
  LinePatterns:array[TPenStyle] of TLinePattern;
  LinePatternsReady:Boolean=False;

procedure InitLinePatterns;
  procedure InitPattern(PenStyle: TPenStyle; Count: Byte; Lengths: array of Byte);
  var
    L: Integer;
  begin
    LinePatterns[PenStyle].Count := Count;
    for L := 0 to 5 do
      LinePatterns[PenStyle].Lengths[L] := Lengths[L];
  end;
begin
  InitPattern(psSolid, 0, [0,0,0,0,0,0]);
  InitPattern(psDash, 2, [3,1,0,0,0,0]);
  InitPattern(psDot, 2, [1,1,0,0,0,0]);
  InitPattern(psDashDot, 4, [2,1,1,1,0,0]);
  InitPattern(psInsideFrame, 6, [3,1,1,1,1,1]);
{$ifndef DELPHI5}
  InitPattern(psDashDotDot, 6, [3,1,1,1,1,1]);
  InitPattern(psClear, 0, [0,0,0,0,0,0]);
{$endif}
{$ifdef DELPHI8AHEAD}
  InitPattern(psUserStyle,0,[0,0,0,0,0,0]);
  InitPattern(psAlternate,0,[0,0,0,0,0,0]);
{$endif}
  LinePatternsReady := True;
end;

procedure CanvasLineToEx(aCanvas:TCanvas; X,Y:integer);
var
  x0,y0   :integer;
  xb,yb   :integer;
  i,p,dist:integer;
  theta   :double;
  sn,cs   :double;
  patt    :^TLinePattern;
  forecl  :TColor;
  backcl  :TColor;
  width0  :integer;
  style0  :TPenStyle;
  factor  :integer;
  cli     :integer;
begin
  if not LinePatternsReady then
    InitLinePatterns;
  if (LinePatterns[aCanvas.Pen.Style].Count=0) or (aCanvas.Pen.Width<=1) then
    aCanvas.LineTo(X,Y)
  else
  begin
    style0:=aCanvas.Pen.Style;
    width0:=aCanvas.Pen.Width;
    x0    :=aCanvas.PenPos.X;
    y0    :=aCanvas.PenPos.Y;
    if X-x0=0 then
      theta:=pi/2
    else
      theta:=ArcTan((Y-y0)/(X-x0));
    sn    :=Sin(theta);
    cs    :=Cos(theta);
    dist  :=Round(Sqrt(Sqr(X-x0)+Sqr(Y-y0)));
    patt  :=@LinePatterns[aCanvas.Pen.Style];
    p     :=0;
    i     :=0;
    forecl:=aCanvas.Pen.Color;
    backcl:=aCanvas.Brush.Color;
    factor:=4*aCanvas.Pen.Width;
    aCanvas.Pen.Style:=psSolid;
    if aCanvas.Brush.Style<>bsClear then
    begin
      aCanvas.Pen.Color:=backcl;
      aCanvas.LineTo(X,Y);
    end;
    aCanvas.Pen.Color:=forecl;
    aCanvas.MoveTo(x0,y0);
    cli:=0;
    while i<dist do
    begin
      Inc(i,patt^.Lengths[p]*factor);
      if not (i<dist) then
        i:=dist;
      xb:=x0+Round(i*cs);
      yb:=y0+Round(i*sn);
      if cli=0 then
        aCanvas.LineTo(xb,yb)
      else
        aCanvas.MoveTo(xb,yb);
      cli:=1-cli;
      p  :=Succ(p) mod patt^.Count;
    end;
    aCanvas.Pen.Style:=style0;
    aCanvas.Pen.Width:=width0;
  end;
end;
{$IFDEF LCLWin32}
procedure FontGetMetrics(const aFontName:string; aFontStyles:TFontStyles; var aFontRec:TRLMetaFontMetrics);
var
  size:integer;
  outl:POutlineTextMetric;
  aux :TBitmap;
begin
  aux:=AuxBitmapNeeded;
  aux.Canvas.Font.Name :=aFontName;
  aux.Canvas.Font.Style:=aFontStyles;
  aux.Canvas.Font.Size :=750;
  //
  size:=GetOutlineTextMetrics(aux.Canvas.Handle,SizeOf(TOutlineTextMetric),nil);
  if size=0 then
    raise Exception.Create(Ls_GetOutlineTextMetrics);
  GetMem(outl,size);
  try
    outl^.otmSize:=size;
    if GetOutlineTextMetrics(aux.Canvas.Handle,size,outl)=0 then
      raise Exception.Create(Ls_GetOutlineTextMetrics);
    //
    aFontRec.TrueType :=(outl^.otmTextMetrics.tmPitchAndFamily=TMPF_TRUETYPE);
    aFontRec.BaseFont :=aFontName;
    aFontRec.FirstChar:=Byte(outl^.otmTextMetrics.tmFirstChar);
    aFontRec.LastChar :=Byte(outl^.otmTextMetrics.tmLastChar);
    GetCharWidth(aux.Canvas.Handle,aFontRec.FirstChar,aFontRec.LastChar,aFontRec.Widths[aFontRec.FirstChar]);
    //
    aFontRec.FontDescriptor.Name        :=aFontName;
    aFontRec.FontDescriptor.Styles      :='';
    if fsBold in aFontStyles then
      aFontRec.FontDescriptor.Styles:=aFontRec.FontDescriptor.Styles+'Bold';
    if fsItalic in aFontStyles then
      aFontRec.FontDescriptor.Styles:=aFontRec.FontDescriptor.Styles+'Italic';
    if fsUnderline in aFontStyles then
      aFontRec.FontDescriptor.Styles:=aFontRec.FontDescriptor.Styles+'Underline';
    if fsStrikeOut in aFontStyles then
      aFontRec.FontDescriptor.Styles:=aFontRec.FontDescriptor.Styles+'StrikeOut';
    aFontRec.FontDescriptor.Flags       :=32;
    aFontRec.FontDescriptor.FontBBox    :=outl^.otmrcFontBox;
    aFontRec.FontDescriptor.MissingWidth:=0;
    aFontRec.FontDescriptor.StemV       :=0;
    aFontRec.FontDescriptor.StemH       :=0;
    aFontRec.FontDescriptor.ItalicAngle :=outl^.otmItalicAngle;
    aFontRec.FontDescriptor.CapHeight   :=outl^.otmsCapEmHeight;
    aFontRec.FontDescriptor.XHeight     :=outl^.otmsXHeight;
    aFontRec.FontDescriptor.Ascent      :=outl^.otmTextMetrics.tmAscent;
    aFontRec.FontDescriptor.Descent     :=outl^.otmTextMetrics.tmDescent;
    aFontRec.FontDescriptor.Leading     :=outl^.otmTextMetrics.tmInternalLeading;
    aFontRec.FontDescriptor.MaxWidth    :=outl^.otmTextMetrics.tmMaxCharWidth;
    aFontRec.FontDescriptor.AvgWidth    :=outl^.otmTextMetrics.tmAveCharWidth;
  finally
    FreeMem(outl,size);
  end;
end;
{$ELSE}
procedure FontGetMetrics(const aFontName:string; aFontStyles:TFontStyles; var aFontRec:TRLMetaFontMetrics);
var
  aux:TBitmap;
  i  :integer;
begin
  aux:=AuxBitmapNeeded(1,1);
  aux.Canvas.Font.Name :=aFontName;
  aux.Canvas.Font.Style:=aFontStyles;
  aux.Canvas.Font.Size :=750;
  //
  aFontRec.TrueType :=true;
  aFontRec.BaseFont :=aFontName;
  aFontRec.FirstChar:=32;
  aFontRec.LastChar :=255;
  for i:=aFontRec.FirstChar to aFontRec.LastChar do
    aFontRec.Widths[i]:=aux.Canvas.TextWidth(Chr(i));
  //
  aFontRec.FontDescriptor.Name        :=aFontName;
  aFontRec.FontDescriptor.Styles      :='';
  if fsBold in aFontStyles then
    aFontRec.FontDescriptor.Styles:=aFontRec.FontDescriptor.Styles+'Bold';
  if fsItalic in aFontStyles then
    aFontRec.FontDescriptor.Styles:=aFontRec.FontDescriptor.Styles+'Italic';
  if fsUnderline in aFontStyles then
    aFontRec.FontDescriptor.Styles:=aFontRec.FontDescriptor.Styles+'Underline';
  if fsStrikeOut in aFontStyles then
    aFontRec.FontDescriptor.Styles:=aFontRec.FontDescriptor.Styles+'StrikeOut';
  aFontRec.FontDescriptor.Flags       :=32;
  aFontRec.FontDescriptor.FontBBox    :=Rect(-498,1023,1120,-307);
  aFontRec.FontDescriptor.MissingWidth:=0;
  aFontRec.FontDescriptor.StemV       :=0;
  aFontRec.FontDescriptor.StemH       :=0;
  aFontRec.FontDescriptor.ItalicAngle :=0;
  aFontRec.FontDescriptor.CapHeight   :=0;
  aFontRec.FontDescriptor.XHeight     :=0;
  aFontRec.FontDescriptor.Ascent      :=0;
  aFontRec.FontDescriptor.Descent     :=0;
  aFontRec.FontDescriptor.Leading     :=0;
  aFontRec.FontDescriptor.MaxWidth    :=0;
  aFontRec.FontDescriptor.AvgWidth    :=0;
end;
{$ENDIF}
var
  LastFontName   :string;
  LastFontSize   :integer;
  LastFontStyle  :TFontStyles;
  LastFontDescent:integer=-1;

function CanvasGetDescent(aCanvas:TCanvas):integer;
var
  stl:TFontStyles;
  aux:TBitmap;
  img: TLazIntfImage;
  x,y:integer;
begin
  stl:=aCanvas.Font.Style-[fsUnderline];
  if (LastFontDescent=-1) or (aCanvas.Font.Name<>LastFontName) or (aCanvas.Font.Size<>LastFontSize) or (stl<>LastFontStyle) then
  begin
    aux:=AuxBitmapNeeded;
    aux.Canvas.Font.Name  :=aCanvas.Font.Name;
    aux.Canvas.Font.Size  :=aCanvas.Font.Size;
    aux.Canvas.Font.Style :=stl;
    aux.Canvas.Font.Color :=clWhite;
    aux.Canvas.Brush.Style:=bsSolid;
    aux.Canvas.Brush.Color:=clBlack;
    aux.Width :=aux.Canvas.TextWidth('L');
    aux.Height:=aux.Canvas.TextHeight('L');
    aux.Canvas.TextOut(0,0,'L');
    img:=aux.CreateIntfImage;
    y:=aux.Height-1;
    while y>=0 do
    begin
      x:=0;
      while x<aux.Width do
      begin
        //original: seems flawed. Should not check RGB = 0 (black)?
        {
          with TRGBArray(aux.ScanLine[y]^)[x] do
          if RGB(rgbRed,rgbGreen,rgbBlue)<>0 then
            Break;
        }
        if img.Colors[x,y]<>colWhite then
          break;
        Inc(x);
      end;
      if x<aux.Width then
        Break;
      Dec(y);
    end;
    img.Destroy;
    LastFontName   :=aCanvas.Font.Name;
    LastFontSize   :=aCanvas.Font.Size;
    LastFontStyle  :=stl;
    LastFontDescent:=aux.Height-1-y;
  end;
  Result:=LastFontDescent;
end;

end.

