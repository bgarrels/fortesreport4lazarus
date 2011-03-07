{$I RLReport.inc}

{@unit RLRichFilter - Implementação do filtro para geração de arquivos no formato RichText. }
unit RLRichFilter;
{$MODE DELPHI}
interface

uses
  LCLIntf, SysUtils, Classes, Types, Graphics, RLMetaVCL,
  RLMetaFile, RLFilters, RLTypes, RLConsts, RLUtils;

type

  { TRLRichFilter }

  {@class TRLRichFilter - Filtro para geração de arquivos no formato RichText (RTF).
   Este filtro permite que os relatórios sejam salvos como arquivos no formato RichText.
   Arquivos neste formato podem ser lidos por editores de texto como MS-Word ou StarOffice.
   @links TRLHTMLFilter, TRLPDFFilter, TRLXLSFilter.
   @ancestor TRLCustomSaveFilter.
   @pub }
  TRLRichFilter=class(TRLCustomSaveFilter)
  private

    // variables

    fTempStream  :TFileStream;
    fTempFileName:string;
    fPrintCut    :TPoint;
    fPrintSize   :TPoint;
    fFontNames   :TStringList;
    fPageNo      :Integer;

    // methods
    
    procedure   Write(const aStr:string='');
    procedure   WriteLn(const aStr:string='');
    procedure   WrapWrite(const aStr:string; var aWidth:integer);
    //
    procedure   GetObjList(aPage:TRLGraphicSurface; aList:TList; aFonts:TStrings);

  protected

    // override methods

    procedure   InternalBeginDoc; override;
    procedure   InternalEndDoc; override;
    procedure   InternalNewPage; override;
    procedure   InternalDrawPage(aPage:TRLGraphicSurface); override;

  public

    // constructors & destructors

    constructor Create(aOwner:TComponent); override;
    destructor  Destroy; override;

  published

    // properties

    {@prop FileName = TRLCustomSaveFilter.FileName :/}
    property    FileName;
    {@prop DisplayName = TRLCustomFilter.DisplayName :/}
    property    DisplayName;
    {@prop ShowProgress - ancestor /}
    property    ShowProgress;
  end;
  {/@class}

{/@unit}

implementation

const
  TWIPININCHES=1/1440;

function PixelsToTwips(aPixels:integer):integer;
var
  inches:double;
begin
  inches:=aPixels/ScreenPPI;
  result:=Round(inches/TWIPININCHES);
end;

function PixelsToExt(aPixels:integer):integer;
begin
  result:=Round(aPixels*26.46875);
end;

function RTF_PenStyle(aPenStyle:TRLMetaPenStyle):string;
begin
  case aPenStyle of
    MetaPenStyleSolid      : result:='\dplinesolid';
    MetaPenStyleDash       : result:='\dplinedash';
    MetaPenStyleDot        : result:='\dplinedot';
    MetaPenStyleDashDot    : result:='\dplinedado';
    MetaPenStyleDashDotDot : result:='\dplinedadodo';
    MetaPenStyleClear      : result:='\dplinehollow';
    MetaPenStyleInsideFrame: result:='\dplinesolid';
  else
    result:='\dplinesolid';  
  end;  
end;

function RTF_BrushStyle(aBrushStyle:TRLMetaBrushStyle):string;
begin    
  case aBrushStyle of
    MetaBrushStyleSolid     : result:='\dpfillpat1';
    MetaBrushStyleClear     : result:='\dpfillpat0';
    MetaBrushStyleHorizontal: result:='\dpfillpat14';
    MetaBrushStyleVertical  : result:='\dpfillpat15';
    MetaBrushStyleFDiagonal : result:='\dpfillpat16';
    MetaBrushStyleBDiagonal : result:='\dpfillpat17';
    MetaBrushStyleCross     : result:='\dpfillpat18';
    MetaBrushStyleDiagCross : result:='\dpfillpat19';
  else
    result:='\dpfillpat1';
  end;
end;

function RTF_BoundsRect(aLeft,aTop,aWidth,aHeight:integer):string;
begin
  result:='\dpx'+IntToStr(PixelsToTwips(aLeft))+
          '\dpy'+IntToStr(PixelsToTwips(aTop))+
          '\dpxsize'+IntToStr(PixelsToTwips(aWidth))+
          '\dpysize'+IntToStr(PixelsToTwips(aHeight));
end;

function RTF_Size(aWidth,aHeight:integer):string;
begin
  result:='\dpxsize'+IntToStr(PixelsToTwips(aWidth))+
          '\dpysize'+IntToStr(PixelsToTwips(aHeight));
end;

function RTF_PenColor(aRed,aGreen,aBlue:integer):string;
const
  prefix='dplineco';
begin
  result:='\'+prefix+'r'+IntToStr(aRed)+'\'+prefix+'g'+IntToStr(aGreen)+'\'+prefix+'b'+IntToStr(aBlue);
end;

function RTF_BrushColor(aRed,aGreen,aBlue:integer):string;
const
  prefix='dpfillbgc';
begin
  result:='\'+prefix+'r'+IntToStr(aRed)+'\'+prefix+'g'+IntToStr(aGreen)+'\'+prefix+'b'+IntToStr(aBlue);
end;

function RTF_PenWidth(aPenWidth:integer):string;
begin
  result:='\dplinew'+IntToStr(PixelsToTwips(aPenWidth));
end;

function RTF_FormatText(const aText:string):string;
var
  i,k:integer;
  HasUnicode: Boolean;
  W: WideString;
  TmpStr: string;
  TmpChar: Char;
  TmpWideChar: WideChar;
begin
  Result:=aText;
  HasUnicode:=false;
  for i:=Length(Result) downto 1 do
  begin
    TmpChar:=Result[i];
    if TmpChar>#127 then
      HasUnicode:=true;
    if TmpChar in ['{','}','\'] then
      Insert('\',Result,i);
  end;
  if HasUnicode then
  begin
    W:=UTF8Decode(Result);
    //Reserve enough space, longest unicode char: "\u-32767? "
    SetLength(Result,Length(W)*10);
    k:=1;
    for i := 1 to Length(W) do
    begin
      TmpWideChar:=W[i];
      if TmpWideChar>#127 then
      begin
        TmpStr:='\u'+IntToStr(SmallInt(TmpWideChar))+'?';
        move(TmpStr[1],Result[k],Length(TmpStr));
        inc(k,Length(TmpStr));
      end
      else
      begin
        Result[k]:=char(ord(TmpWideChar));
        inc(k);
      end;
    end;
    SetLength(Result,k-1);
  end;
end;

// hexadecimal invertido
function RTF_IntToHex(aInt,aWid:integer):string;
var
  i:integer;
  s:string;
begin
  s:=IntToHex(aInt,aWid);
  SetLength(result,aWid);
  i:=1;
  while i<=Length(s) do
  begin
    Move(s[i],result[aWid-i],2);
    Inc(i,2);
  end;
end;

{ TRLRichFilter }

constructor TRLRichFilter.Create(aOwner: TComponent);
begin
  fFontNames   :=nil;
  fTempStream  :=nil;
  fTempFileName:='';
  //
  inherited;
  //
  fFontNames :=TStringList.Create;
  //
  DefaultExt :='.rtf';
  DisplayName:=LS_RichFormatStr;
end;

destructor TRLRichFilter.Destroy;
begin
  FreeObj(fFontNames);
  if Assigned(fTempStream) then
  begin
    fTempStream.free;
    SysUtils.DeleteFile(fTempFileName);
  end;
  //
  inherited;
end;

procedure TRLRichFilter.Write(const aStr:string);
begin
  StreamWrite(fTempStream,aStr);
end;

procedure TRLRichFilter.WriteLn(const aStr:string='');
begin
  StreamWriteLn(fTempStream,aStr);
end;

procedure TRLRichFilter.WrapWrite(const aStr:string; var aWidth:integer);
const
  maxwidth=128;
begin
  Write(aStr);
  Inc(aWidth,Length(aStr));
  if aWidth>maxwidth then
  begin
    WriteLn;
    aWidth:=0;
  end;  
end;

procedure TRLRichFilter.InternalBeginDoc;
begin
  fPageNo     :=1; //Número da página para o flyanchor
  fPrintCut.X :=0;
  fPrintCut.Y :=0;
  fPrintSize.X:=Pages.OrientedWidth;
  fPrintSize.Y:=Pages.OrientedHeight;
  //
  fFontNames.Clear;
  //
  if Assigned(fTempStream) then
  begin
    fTempStream.free;
    SysUtils.DeleteFile(fTempFileName);
  end;
  fTempFileName:=GetTempFileName;
  RegisterTempFile(fTempFileName);
  fTempStream:=TFileStream.Create(fTempFileName,fmCreate);
end;

procedure TRLRichFilter.InternalEndDoc;
var
  endstream:TFileStream;
  i        :integer;
begin
  // cria arquivo destino
  try
    endstream:=TFileStream.Create(FileName,fmCreate);
    try
      // grava header
      StreamWriteLn(endstream,'{\rtf1\ansi\ansicpg1252\deff0\deflang1046');
      // grava fontes
      StreamWriteLn(endstream,'{\fonttbl{');
      for i:=0 to fFontNames.Count-1 do
        StreamWriteLn(endstream,'\f'+IntToStr(i)+'\fnil\fcharset0 '+fFontNames[i]+';');
      StreamWriteLn(endstream,'}}');
      // grava tamanho da página
      StreamWriteLn(endstream,'\paperw'+IntToStr(PixelsToTwips(fPrintSize.X)));
      StreamWriteLn(endstream,'\paperh'+IntToStr(PixelsToTwips(fPrintSize.Y)));
      if Pages.Orientation=MetaOrientationLandscape then
        StreamWriteLn(endstream,'\landscape');
      //
      StreamWriteLn(endstream,'\viewkind1');
      StreamWriteLn(endstream,'\margl0\margt0\margr0\margb0\uc1');
      // transfere dados do temp
      fTempStream.Seek(0,soFromBeginning);
      endstream.CopyFrom(fTempStream,fTempStream.Size);
      // grava fim de arquivo
      StreamWriteLn(endstream,'\par}');
    finally
      endstream.free;
    end;
  except
    SysUtils.DeleteFile(FileName);
    raise;
  end;
  // apaga arquivo temporário
  FreeAndNil(fTempStream);
  SysUtils.DeleteFile(fTempFileName);
  UnregisterTempFile(fTempFileName);
end;

procedure TRLRichFilter.InternalNewPage;
begin
  WriteLn('\par\page\pard\plain\par');
  Inc(fPageNo);
end;

procedure TRLRichFilter.GetObjList(aPage:TRLGraphicSurface; aList:TList; aFonts:TStrings);
var
  obj:TRLGraphicObject;
  ///ref:TRLGraphicObject;
  i{,j}:integer;
  fn :string;
begin
  aList.Clear;
  for i:=0 to aPage.ObjectCount-1 do
  begin
    obj:=aPage.Objects[i];
    // insere texto ordenadamente
    {
    j:=0;
    while j<aList.Count do
    begin
      ref:=TRLGraphicObject(aList[j]);
      if (obj.BoundsRect.Top<ref.BoundsRect.Top) or ((obj.BoundsRect.Top=ref.BoundsRect.Top) and (obj.BoundsRect.Left<ref.BoundsRect.Left)) then
        Break;
      Inc(j);
    end;
    if j<aList.Count then
      aList.Insert(j,obj)
    else
    }///
      aList.Add(obj);
    // adiciona fonte
    if obj is TRLTextObject then
    begin
      fn:=TRLTextObject(obj).Font.Name;
      if aFonts.IndexOf(fn)=-1 then
        aFonts.Add(fn);
    end;
  end;
end;

procedure TRLRichFilter.InternalDrawPage(aPage:TRLGraphicSurface);
var
  objlist  :TList;
  contlist :TList;
  obj      :TRLGraphicObject;
  i        :integer;
  cliprct  :TRect;
  clipstack:TList;
  procedure PushClipRect(const aRect:TRect);
  var
    p:PRect;
  begin
    New(p);
    p^:=aRect;
    clipstack.Insert(0,p);
  end;
  procedure PopClipRect(var aRect:TRect);
  var
    p:PRect;
  begin
    p:=clipstack[0];
    aRect:=p^;
    Dispose(p);
    clipstack.Delete(0);
  end;
  procedure DrawGraphic(Graphic:TGraphic; const Rect:TRect);
  var
    bmp:TBitmap;
    bmx:TBitmap;
    str:TMemoryStream;
    wrp:integer;
    ch :byte;
    n  :integer;
    cut:TRect;
    aux:TRect;
  begin
    bmp:=nil;
    try
      // obriga bitmap 8bits
      aux:=Rect;
      OffsetRect(aux,-aux.Left,-aux.Top);
      bmx:=AuxBitmapNeeded(aux.Right-aux.Left,aux.Bottom-aux.Top);
      bmx.Canvas.StretchDraw(aux,Graphic);
      IntersectRect(cut,Rect,cliprct);
      OffsetRect(cut,-Rect.Left,-Rect.Top);
      bmp:=ClipGraphic(bmx,cut,False);
      //todo: crash em gtk e imagm preta no windows
      //bmp.PixelFormat:=pf8bit;
      //
      with Rect do
        WriteLn('\pard\plain'+
                '\absw'+IntToStr(PixelsToTwips(cut.Right-cut.Left))+
                '\absh-'+IntToStr(PixelsToTwips(cut.Bottom-cut.Top))+
                '\pvpg\posy'+IntToStr(PixelsToTwips(Top-fPrintCut.Y))+
                '\phpg\posx'+IntToStr(PixelsToTwips(Left-fPrintCut.X))+
                '{\*\flyanchor2\flypage'+IntToStr(fPageNo)+'}'+
                '{\pict\wmetafile8'+
                '\picw'+IntToStr(PixelsToExt(Right-Left))+
                '\pich'+IntToStr(PixelsToExt(Bottom-Top))+
                '\picbmp\picbpp8');
      //
      wrp:=0;
      str:=TMemoryStream.Create;
      try
        bmp.SaveToStream(str);
        str.Seek(0,soFromBeginning);
        // metafile data
        str.Position:=str.Position+2; // skip trash
        str.Read(n,4);
        n:=n div 2+7;
        WrapWrite('010009000003'+RTF_IntToHex(n+36,8)+'0000',wrp);
        WrapWrite(RTF_IntToHex(n,8)+'0000050000000B0200000000050000000C02',wrp);
        WrapWrite(RTF_IntToHex(bmp.Height,4),wrp);
        WrapWrite(RTF_IntToHex(bmp.Width,4),wrp);
        WrapWrite('05000000090200000000050000000102FFFFFF000400000007010300',wrp);
        WrapWrite(RTF_IntToHex(n,8),wrp);
        WrapWrite('430F2000CC000000',wrp);
        WrapWrite(RTF_IntToHex(bmp.Height,4),wrp);
        WrapWrite(RTF_IntToHex(bmp.Width,4)+'00000000',wrp);
        WrapWrite(RTF_IntToHex(bmp.Height,4),wrp);
        WrapWrite(RTF_IntToHex(bmp.Width,4)+'00000000',wrp);
        str.Position:=str.Position+8; // skip trash
        while str.Read(ch,1)>0 do
          WrapWrite(IntToHex(ch,2),wrp);
        WrapWrite('030000000000',wrp);
        WriteLn;
      finally
        str.free;
      end;
      //
      WriteLn('}\par\pard');
    finally
      if (bmp<>nil) and (bmp<>Graphic) then
        bmp.free;
    end;
  end;
  function CreateBitmap(aObj:TRLGraphicObject):TBitmap;
  begin
    Result:=NewBitmap(aObj.BoundsRect.Right-aObj.BoundsRect.Left,aObj.BoundsRect.Bottom-aObj.BoundsRect.Top);
    Result.Canvas.Pen.Style  :=psSolid;
    Result.Canvas.Brush.Style:=bsSolid;
    Result.Canvas.Brush.Color:=clWhite;
    Result.Canvas.FillRect(Rect(0,0,Result.Width,Result.Height));
  end;
  procedure CopyBrush(Brush:TRLMetaBrush; Bitmap:TBitmap);
  begin
    with Brush.Color do
      Bitmap.Canvas.Brush.Color:=RGB(Red,Green,Blue);
    Bitmap.Canvas.Brush.Style:=FromMetaBrushStyle(Brush.Style);
  end;
  procedure CopyPen(Pen:TRLMetaPen; Bitmap:TBitmap);
  begin
    with Pen.Color do
      Bitmap.Canvas.Pen.Color:=RGB(Red,Green,Blue);
    Bitmap.Canvas.Pen.Style:=FromMetaPenStyle(Pen.Style);
    Bitmap.Canvas.Pen.Width:=Pen.Width;
  end;
  procedure DrawPixel(aObj:TRLPixelObject);
  begin        
    Write('{\*\do\dobxpage\dobypage\dprect');
    with aObj.BoundsRect do
      Write(RTF_BoundsRect(Left-fPrintCut.X,Top-fPrintCut.Y,Right-Left,Bottom-Top));
    Write(RTF_PenStyle(MetaPenStyleClear));
    Write(RTF_BrushStyle(MetaBrushStyleSolid));
    with aObj.Color do
      Write(RTF_BrushColor(Red,Green,Blue));
    WriteLn('}');
  end;
  procedure DrawLine(aObj:TRLLineObject);
  begin
    Write('{\*\do\dobxpage\dobypage\dpline');
    Write(RTF_BoundsRect(aObj.FromPoint.X-fPrintCut.X,aObj.FromPoint.Y-fPrintCut.Y,aObj.ToPoint.X-aObj.FromPoint.X,aObj.ToPoint.Y-aObj.FromPoint.Y));
    Write(RTF_PenStyle(aObj.Pen.Style));
    with aObj.Pen.Color do
      Write(RTF_PenColor(Red,Green,Blue));
    Write(RTF_PenWidth(aObj.Pen.Width));
    WriteLn('}');
  end;
  procedure DrawRectangle(aObj:TRLRectangleObject);
  begin
    Write('{\*\do\dobxpage\dobypage\dprect');
    with aObj.BoundsRect do
      Write(RTF_BoundsRect(Left-fPrintCut.X,Top-fPrintCut.Y,Right-Left,Bottom-Top));
    Write(RTF_PenStyle(aObj.Pen.Style));
    with aObj.Pen.Color do
      Write(RTF_PenColor(Red,Green,Blue));
    Write(RTF_PenWidth(aObj.Pen.Width));
    Write(RTF_BrushStyle(aObj.Brush.Style));
    with aObj.Brush.Color do
      Write(RTF_BrushColor(Red,Green,Blue));
    WriteLn('}');
  end;
  procedure DrawFillRect(aObj:TRLFillRectObject);
  begin
    Write('{\*\do\dobxpage\dobypage\dprect');
    with aObj.BoundsRect do
      Write(RTF_BoundsRect(Left-fPrintCut.X,Top-fPrintCut.Y,Right-Left,Bottom-Top));
    Write(RTF_PenStyle(MetaPenStyleClear));
    Write(RTF_PenWidth(0));
    Write(RTF_BrushStyle(aObj.Brush.Style));
    with aObj.Brush.Color do
      Write(RTF_BrushColor(Red,Green,Blue));
    WriteLn('}');
  end;
  procedure DrawEllipse(aObj:TRLEllipseObject);
  begin
    Write('{\*\do\dobxpage\dobypage\dpellipse');
    with aObj.BoundsRect do
      Write(RTF_BoundsRect(Left-fPrintCut.X,Top-fPrintCut.Y,Right-Left,Bottom-Top));
    Write(RTF_PenStyle(aObj.Pen.Style));
    with aObj.Pen.Color do
      Write(RTF_PenColor(Red,Green,Blue));
    Write(RTF_PenWidth(aObj.Pen.Width));
    Write(RTF_BrushStyle(aObj.Brush.Style));
    with aObj.Brush.Color do
      Write(RTF_BrushColor(Red,Green,Blue));
    WriteLn('}');
  end;
  procedure DrawPolygon(aObj:TRLPolygonObject);
  var
    i:integer;
  begin
    Write('{\*\do\dobxpage\dobypage\dppolygon');
    with aObj.BoundsRect do
      Write(RTF_BoundsRect(Left-fPrintCut.X,Top-fPrintCut.Y,Right-Left,Bottom-Top));
    Write('\dppolycount'+IntToStr(High(aObj.Points)+1));
    for i:=0 to High(aObj.Points) do
      Write('\dpptx'+IntToStr(PixelsToTwips(aObj.Points[i].X-fPrintCut.X-aObj.BoundsRect.Left))+
            '\dppty'+IntToStr(PixelsToTwips(aObj.Points[i].Y-fPrintCut.Y-aObj.BoundsRect.Top)));
    Write(RTF_PenStyle(aObj.Pen.Style));
    with aObj.Pen.Color do
      Write(RTF_PenColor(Red,Green,Blue));
    Write(RTF_PenWidth(aObj.Pen.Width));
    Write(RTF_BrushStyle(aObj.Brush.Style));
    with aObj.Brush.Color do
      Write(RTF_BrushColor(Red,Green,Blue));
    WriteLn('}');
  end;
  procedure DrawPolyline(aObj:TRLPolylineObject);
  var
    i:integer;
  begin
    Write('{\*\do\dobxpage\dobypage\dppolyline');
    with aObj.BoundsRect do
      Write(RTF_BoundsRect(Left-fPrintCut.X,Top-fPrintCut.Y,Right-Left,Bottom-Top));
    Write('\dppolycount'+IntToStr(High(aObj.Points)+1));
    for i:=0 to High(aObj.Points) do
      Write('\dpptx'+IntToStr(PixelsToTwips(aObj.Points[i].X-fPrintCut.X-aObj.BoundsRect.Left))+
            '\dppty'+IntToStr(PixelsToTwips(aObj.Points[i].Y-fPrintCut.Y-aObj.BoundsRect.Top)));
    Write(RTF_PenStyle(aObj.Pen.Style));
    with aObj.Pen.Color do
      Write(RTF_PenColor(Red,Green,Blue));
    Write(RTF_PenWidth(aObj.Pen.Width));
    WriteLn('}');
  end;
  procedure DrawImage(aObj:TRLImageObject);
  var
    grp:TGraphic;
  begin
    grp:=FromMetaGraphic(aObj.Data);
    try
      DrawGraphic(grp,FromMetaRect(aObj.BoundsRect));
    finally
      grp.free;
    end;
  end;
  procedure DrawContinuousText(aList:TList);
  var
    textrct:TRect;
    newwid :integer;
    magic,i:integer;
    ww     :string;
    tx0    :TRLTextObject;
    txN    :TRLTextObject;
    txi    :TRLTextObject;
    st0    :TRLMetaFontStyles;
    fn0    :string;
    fs0    :integer;
    function CmpSt(s0,s1:TRLMetaFontStyles; s:byte):integer;
    var
      t0,t1:boolean;
    begin
      t0:=(s0 and s)=s;
      t1:=(s1 and s)=s;
      if t0 xor t1 then
        if t0 then
          Result:=-1
        else
          Result:=1
      else
        Result:=0;
    end;
    function ChgSt(st:TRLMetaFontStyles; const fn:string; fs:integer):boolean;
    begin
      Result:=(st<>st0) or (fn<>fn0) or (fs<>fs0);
      // seleciona fonte
      if fn<>fn0 then
        Write('\f'+IntToStr(fFontNames.IndexOf(fn)));
      if fs<>fs0 then
        Write('\fs'+IntToStr(fs*2));
      // efeitos
      case CmpSt(st0,st,MetaFontStyleBold) of
        -1: Write('\b0');
        +1: Write('\b');
      end;
      case CmpSt(st0,st,MetaFontStyleItalic) of
        -1: Write('\i0');
        +1: Write('\i');
      end;
      case CmpSt(st0,st,MetaFontStyleUnderline) of
        -1: Write('\ul0');
        +1: Write('\ul');
      end;
      case CmpSt(st0,st,MetaFontStyleStrikeOut) of
        -1: Write('\strike0');
        +1: Write('\strike');
      end;
      fn0:=fn;
      fs0:=fs;
      st0:=st;
    end;
    procedure EndSt;
    begin
      // efeitos
      if CmpSt(st0,0,MetaFontStyleBold)<0 then
        Write('\b0');
      if CmpSt(st0,0,MetaFontStyleItalic)<0 then
        Write('\i0');
      if CmpSt(st0,0,MetaFontStyleUnderline)<0 then
        Write('\ul0');
      if CmpSt(st0,0,MetaFontStyleStrikeOut)<0 then
        Write('\strike0');
    end;
  begin
    tx0:=TRLTextObject(aList[0]);
    txN:=TRLTextObject(aList[aList.Count-1]);
    textrct.TopLeft    :=FromMetaRect(tx0.BoundsRect).TopLeft;
    textrct.BottomRight:=FromMetaRect(txN.BoundsRect).BottomRight;
    // para compensar a diferença de tamanho da fonte, aumenta-se a largura do rect em 5% (mágico)
    newwid:=textrct.Right-textrct.Left;
    magic :=Trunc(newwid*1.05)-newwid+1;
    case tx0.Alignment of
      MetaTextAlignmentLeft   : Inc(textrct.Right,magic);
      MetaTextAlignmentRight  : Dec(textrct.Left,magic);
      MetaTextAlignmentCenter : begin
                                  Dec(textrct.Left,magic div 2);
                                  Inc(textrct.Right,magic-(magic div 2));
                                end;
      MetaTextAlignmentJustify: ///Inc(textrct.Right,magic);
    end;
    // para evitar o wordwrap
    case tx0.Alignment of
      MetaTextAlignmentLeft   : ww:='\rin-1000';
      MetaTextAlignmentRight  : ww:='\lin-1000';
      MetaTextAlignmentCenter : ww:='\rin-1000\lin-1000';
      MetaTextAlignmentJustify: ww:=emptystr;
    else
      ww:=emptystr;
    end;
    with textrct do
      Write('\pard\plain'+
            '\absw'+IntToStr(PixelsToTwips(Right-Left))+
            '\absh-'+IntToStr(PixelsToTwips(Bottom-Top))+
            ww+
            '\pvpg\posy'+IntToStr(PixelsToTwips(Top-fPrintCut.Y))+
            '\phpg\posx'+IntToStr(PixelsToTwips(Left-fPrintCut.X))+
            '{\*\flyanchor2\flypage'+IntToStr(fPageNo)+'}');
    // justifica o texto
    case tx0.Alignment of
      MetaTextAlignmentLeft   : Write('\ql');
      MetaTextAlignmentRight  : Write('\qr');
      MetaTextAlignmentCenter : Write('\qc');
      MetaTextAlignmentJustify: Write('\qj');
    end;
    Write('{');
    st0:=0;
    fn0:='';
    fs0:=0;
    for i:=0 to aList.Count-1 do
    begin
      txi:=TRLTextObject(aList[i]);
      if ChgSt(txi.Font.Style,txi.Font.Name,txi.Font.Size) then
        Write(' ');
      Write(RTF_FormatText(txi.DisplayText));
    end;
    EndSt;
    // fim de parágrafo
    if txN.Alignment=MetaTextAlignmentJustify then
      Write('\line');
    WriteLn('}\par\pard');
  end;
  procedure DrawText(aObj:TRLTextObject);
  var
    textrct:TRect;
    newwid :integer;
    magic  :integer;
    ww     :string;
  begin
    ///IntersectRect(textrct,FromMetaRect(aObj.BoundsRect),cliprct);
    textrct:=FromMetaRect(aObj.BoundsRect);///
    // para compensar a diferença de tamanho da fonte, aumenta-se a largura do rect em 5% (mágico)
    newwid:=textrct.Right-textrct.Left;
    magic :=Trunc(newwid*1.05)-newwid+1;
    case aObj.Alignment of
      MetaTextAlignmentLeft   : Inc(textrct.Right,magic);
      MetaTextAlignmentRight  : Dec(textrct.Left,magic);
      MetaTextAlignmentCenter : begin
                                  Dec(textrct.Left,magic div 2);
                                  Inc(textrct.Right,magic-(magic div 2));
                                end;
      MetaTextAlignmentJustify: Inc(textrct.Right,magic);
    end;
    // para evitar o wordwrap
    case aObj.Alignment of
      MetaTextAlignmentLeft   : ww:='\rin-1000';
      MetaTextAlignmentRight  : ww:='\lin-1000';
      MetaTextAlignmentCenter : ww:='\rin-1000\lin-1000';
      MetaTextAlignmentJustify: ww:='';
   else
      ww:='';
    end;
    with textrct do
      Write('\pard\plain'+
            '\absw'+IntToStr(PixelsToTwips(Right-Left))+
            '\absh-'+IntToStr(PixelsToTwips(Bottom-Top))+
            ww+
            '\pvpg\posy'+IntToStr(PixelsToTwips(Top-fPrintCut.Y))+
            '\phpg\posx'+IntToStr(PixelsToTwips(Left-fPrintCut.X))+
            '{\*\flyanchor2\flypage'+IntToStr(fPageNo)+'}');
    // justifica o texto
    case aObj.Alignment of
      MetaTextAlignmentLeft   : Write('\ql');
      MetaTextAlignmentRight  : Write('\qr');
      MetaTextAlignmentCenter : Write('\qc');
      MetaTextAlignmentJustify: Write('\qj');
    end;
    Write('{');
    // seleciona fonte
    Write('\f'+IntToStr(fFontNames.IndexOf(aObj.Font.Name))+'\fs'+IntToStr(aObj.Font.Size*2));
    // efeitos
    if (aObj.Font.Style and MetaFontStyleBold)=MetaFontStyleBold then
      Write('\b');
    if (aObj.Font.Style and MetaFontStyleItalic)=MetaFontStyleItalic then
      Write('\i');
    if (aObj.Font.Style and MetaFontStyleUnderline)=MetaFontStyleUnderline then
      Write('\ul');
    if (aObj.Font.Style and MetaFontStyleStrikeOut)=MetaFontStyleStrikeOut then
      Write('\strike');
    // o texto
    Write(' '+RTF_FormatText(aObj.DisplayText));
    // retorna fonte
    if (aObj.Font.Style and MetaFontStyleStrikeOut)=MetaFontStyleStrikeOut then
      Write('\strike0');
    if (aObj.Font.Style and MetaFontStyleUnderline)=MetaFontStyleUnderline then
      Write('\ul0');
    if (aObj.Font.Style and MetaFontStyleItalic)=MetaFontStyleItalic then
      Write('\i0');
    if (aObj.Font.Style and MetaFontStyleBold)=MetaFontStyleBold then
      Write('\b0');
    // fim de parágrafo 
    if aObj.Alignment=MetaTextAlignmentJustify then
      Write('\line');
    WriteLn('}\par\pard');
  end;
  procedure DrawSetClipRect(aObj:TRLSetClipRectObject);
  begin
    PushClipRect(cliprct);
    IntersectRect(cliprct,cliprct,FromMetaRect(aObj.BoundsRect));
  end;
  procedure DrawResetClipRect(aObj:TRLResetClipRectObject);
  begin
    PopClipRect(cliprct);
  end;
begin
  clipstack:=TList.Create;
  try
    cliprct:=Rect(0,0,aPage.Width,aPage.Height);
    objlist:=TList.Create;
    try
      // coleta textos e fontes
      GetObjList(aPage,objlist,fFontNames);
      // grava objetos
      i:=0;
      while i<objlist.Count do
      begin
        obj:=TRLGraphicObject(objlist[i]);
        if obj is TRLPixelObject then
          DrawPixel(TRLPixelObject(obj))
        else if obj is TRLLineObject then
          DrawLine(TRLLineObject(obj))
        else if obj is TRLRectangleObject then
          DrawRectangle(TRLRectangleObject(obj))
        else if obj is TRLTextObject then
          if (TRLTextObject(obj).TextFlags and MetaTextFlagContinuous)<>0 then
          begin
            contlist:=TList.Create;
            try
              repeat
                contlist.Add(obj);
                if (obj is TRLTextObject) and ((TRLTextObject(obj).TextFlags and MetaTextFlagContinuous)<>0) then
                  Inc(i)
                else
                  Break;
                if i>objlist.Count-1 then
                  Break;
                obj:=TRLGraphicObject(objlist[i]);
              until False;
              DrawContinuousText(contlist);
            finally
              contlist.Free;
            end;
          end
          else
            DrawText(TRLTextObject(obj))
        else if obj is TRLFillRectObject then
          DrawFillRect(TRLFillRectObject(obj))
        else if obj is TRLEllipseObject then
          DrawEllipse(TRLEllipseObject(obj))
        else if obj is TRLPolygonObject then
          DrawPolygon(TRLPolygonObject(obj))
        else if obj is TRLPolylineObject then
          DrawPolyline(TRLPolylineObject(obj))
        else if obj is TRLImageObject then
          DrawImage(TRLImageObject(obj))
        else if obj is TRLSetClipRectObject then
          DrawSetClipRect(TRLSetClipRectObject(obj))
        else if obj is TRLResetClipRectObject then
          DrawResetClipRect(TRLResetClipRectObject(obj));
        Inc(i);  
      end;
    finally
      objlist.free;
    end;
  finally
    while clipstack.Count>0 do
      PopClipRect(cliprct);
    clipstack.free;
  end;
end;

end.

