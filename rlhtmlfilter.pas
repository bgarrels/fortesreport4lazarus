{$I RLReport.inc}

{@unit RLHTMLFilter - Implementação do filtro para criação de páginas web.
Portado para o Lazarus - Trabalho inicial de Isaac Trindade da Silva contato tioneobrasil@yahoo.com.br dicas4lazarus@yahoo.com.br
Lazarus Ported - initial work by Isaac 07/2009
}
unit RLHTMLFilter;
{$MODE DELPHI}{$H+}
interface

uses
  SysUtils, Classes, Contnrs,
  {$IFDEF FPC}
    {$ifdef MSWINDOWS}
      Windows,
    {$else}
      LCLIntf,
    {$endif}
      Types,
  {$ELSE}
    {$ifdef MSWINDOWS}
      Windows,
    {$else}
      Types,
    {$endif}
  {$ENDIF}
{$ifdef VCL}
  Graphics, RLMetaVCL,
{$else}
  QGraphics, RLMetaCLX,
{$endif}
{$IFDEF FPC}
  rlshared,
{$ENDIF}
  RLMetaFile, RLConsts, RLFilters, RLUtils, RLTypes;

type
  {@type TRLHTMLDocumentStyle - Estilo para geração de páginas html.
   Pode ser um dos seguintes valores:
   dsCSS2 - O filtro gera apenas uma página html para todo o relatório (além dos arquivos
   de imagens) que inclui instruções especiais da especificação CSS2 para salto de página;
   dsOldStyle - É gerada uma página html para cada página de relatório. :/}
  TRLHTMLDocumentStyle=(dsCSS2,dsOldStyle);

  {@type TRLHTMLSaveGraphicEvent - Evento para a gravação de imagem em disco em formato
  ou localização alternativas. Implementando este evento vc se responsabiliza por gravar
  a imagem em disco, podendo mudar o formato e o nome do arquivo. :/}
  TRLHTMLSaveGraphicEvent=procedure(Sender:TObject; aGraphic:TGraphic; var FileName:string; var Saved:boolean) of object;

  { TRLHTMLFilter }

  {@class TRLHTMLFilter - Filtro para criação de páginas html a partir de um relatório.
   @links TRLRichFilter, TRLPDFFilter, TRLXLSFilter. 
   @ancestor TRLCustomSaveFilter.
   @pub }
  TRLHTMLFilter=class(TRLCustomSaveFilter)
  private

    // variables

    fWorkingFileHandle:textfile;
    fGraphIndex       :integer;
    fWorkingFileName  :string;
    fDocumentStyle    :TRLHTMLDocumentStyle;
    fFileCount        :integer;
    fFileIndex        :integer;
    fFontList         :TStringList;
    fPrintCut         :TPoint;
    fPrintSize        :TPoint;
    fOnSaveGraphic    :TRLHTMLSaveGraphicEvent;
    fImageFiles       :TObjectList;

    // custom methods

    procedure   FontListNeeded;
    function    AddFont(aFont:TRLMetaFont):integer;
    procedure   WritePageHeader(var aFile:textfile);
    procedure   WritePageFooter(var aFile:textfile);
    procedure   WritePageTools(var aFile:textfile);
    function    FileNameByIndex(aFileIndex:integer):string;
    function    DoSaveGraphic(aGraphic:TGraphic; var aFileName:string):boolean;

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
    
    {@prop DocumentStyle - Estilo para a geração das páginas html.
     @links TRLHTMLDocumentStyle. :/}
    property    DocumentStyle:TRLHTMLDocumentStyle read fDocumentStyle write fDocumentStyle;

    {@prop FileName = ancestor /}
    property    FileName;
    {@prop DisplayName = ancestor /}
    property    DisplayName;
    {@prop ShowProgress - ancestor /}
    property    ShowProgress;

    {@event OnSaveGraphic - Ao salvar imagem em disco. Implemente este evento se quiser
    interferir na gravação das imagens em disco. As imagens normalmente são gravadas em
    formato bitmap (.bmp). :/}
    property    OnSaveGraphic:TRLHTMLSaveGraphicEvent read fOnSaveGraphic write fOnSaveGraphic;
  end;
  {/@class}
  

{@func ColorToHex - Devolve uma string com a cor em formato RGB-Hexadecimal. :/}
function ColorToHex(aColor:TColor):string;

{@func EncodeISO - Codifica uma string padrão ascii em formato ISO para html. :/}
function EncodeISO(const aStr:string):string;

{/@unit}

implementation

const
  BEGINPAGETAG='<HTMLFilter.Page>';
  ENDPAGETAG  ='</HTMLFilter.Page>';

// UTILS

function ColorToHex(aColor:TColor):string;
type
  TRGBSplit=record
              Red,Green,Blue,Pallete:byte;
            end;
var
  rgb:record case byte of
    0: (AsDWORD:DWORD);
    1: (AsSplit:TRGBSplit);
  end;
begin
  rgb.AsDWORD:=ColorToRGB(aColor);
  Result:=ByteToHex(rgb.AsSplit.Red)+ByteToHex(rgb.AsSplit.Green)+ByteToHex(rgb.AsSplit.Blue); 
end;

function EncodeISO(const aStr:string):string;
type
  TCharAndSymbol=record
    Code  :char;
    Symbol:string;
  end;
const
  MAXENCODECHARS=99;
  ENCODECHARS:array[1..MAXENCODECHARS] of TCharAndSymbol=(
    (Code:#038; Symbol:'&amp;'), // ampersand
    (Code:#062; Symbol:'&gt;'), // greater than
    (Code:#060; Symbol:'&lt;'), // less than
    (Code:#160; Symbol:'&nbsp;'), // no-break space
    (Code:#161; Symbol:'&iexcl;'), // inverted exclamation mark
    (Code:#162; Symbol:'&cent;'), // cent sign
    (Code:#163; Symbol:'&pound;'), // pound sterling sign
    (Code:#164; Symbol:'&curren;'), // general currency sign
    (Code:#165; Symbol:'&yen;'), // yen sign
    (Code:#166; Symbol:'&brvbar;'), // broken (vertical) bar
    (Code:#167; Symbol:'&sect;'), // section sign
    (Code:#168; Symbol:'&uml;'), // umlaut (dieresis)
    (Code:#169; Symbol:'&copy;'), // copyright sign
    (Code:#170; Symbol:'&ordf;'), // ordinal indicator, feminine
    (Code:#171; Symbol:'&laquo;'), // angle quotation mark, left
    (Code:#172; Symbol:'&not;'), // not sign
    (Code:#173; Symbol:'&shy;'), // soft hyphen
    (Code:#174; Symbol:'&reg;'), // registered sign
    (Code:#175; Symbol:'&macr;'), // macron
    (Code:#176; Symbol:'&deg;'), // degree sign
    (Code:#177; Symbol:'&plusmn;'), // plus-or-minus sign
    (Code:#178; Symbol:'&sup2;'), // superscript two
    (Code:#179; Symbol:'&sup3;'), // superscript three
    (Code:#180; Symbol:'&acute;'), // acute accent
    (Code:#181; Symbol:'&micro;'), // micro sign
    (Code:#182; Symbol:'&para;'), // pilcrow (paragraph sign)
    (Code:#183; Symbol:'&middot;'), // middle dot
    (Code:#184; Symbol:'&cedil;'), // cedilla
    (Code:#185; Symbol:'&sup1;'), // superscript one
    (Code:#186; Symbol:'&ordm;'), // ordinal indicator, masculine
    (Code:#187; Symbol:'&raquo;'), // angle quotation mark, right
    (Code:#188; Symbol:'&frac14;'), // fraction one-quarter
    (Code:#189; Symbol:'&frac12;'), // fraction one-half
    (Code:#190; Symbol:'&frac34;'), // fraction three-quarters
    (Code:#191; Symbol:'&iquest;'), // inverted question mark
    (Code:#192; Symbol:'&Agrave;'), // capital A, grave accent
    (Code:#193; Symbol:'&Aacute;'), // capital A, acute accent
    (Code:#194; Symbol:'&Acirc;'), // capital A, circumflex accent
    (Code:#195; Symbol:'&Atilde;'), // capital A, tilde
    (Code:#196; Symbol:'&Auml;'), // capital A, dieresis or umlaut mark
    (Code:#197; Symbol:'&Aring;'), // capital A, ring
    (Code:#198; Symbol:'&AElig;'), // capital AE diphthong (ligature)
    (Code:#199; Symbol:'&Ccedil;'), // capital C, cedilla
    (Code:#200; Symbol:'&Egrave;'), // capital E, grave accent
    (Code:#201; Symbol:'&Eacute;'), // capital E, acute accent
    (Code:#202; Symbol:'&Ecirc;'), // capital E, circumflex accent
    (Code:#203; Symbol:'&Euml;'), // capital E, dieresis or umlaut mark
    (Code:#204; Symbol:'&Igrave;'), // capital I, grave accent
    (Code:#205; Symbol:'&Iacute;'), // capital I, acute accent
    (Code:#206; Symbol:'&Icirc;'), // capital I, circumflex accent
    (Code:#207; Symbol:'&Iuml;'), // capital I, dieresis or umlaut mark
    (Code:#208; Symbol:'&ETH;'), // capital Eth, Icelandic
    (Code:#209; Symbol:'&Ntilde;'), // capital N, tilde
    (Code:#210; Symbol:'&Ograve;'), // capital O, grave accent
    (Code:#211; Symbol:'&Oacute;'), // capital O, acute accent
    (Code:#212; Symbol:'&Ocirc;'), // capital O, circumflex accent
    (Code:#213; Symbol:'&Otilde;'), // capital O, tilde
    (Code:#214; Symbol:'&Ouml;'), // capital O, dieresis or umlaut mark
    (Code:#215; Symbol:'&times;'), // multiply sign
    (Code:#216; Symbol:'&Oslash;'), // capital O, slash
    (Code:#217; Symbol:'&Ugrave;'), // capital U, grave accent
    (Code:#218; Symbol:'&Uacute;'), // capital U, acute accent
    (Code:#219; Symbol:'&Ucirc;'), // capital U, circumflex accent
    (Code:#220; Symbol:'&Uuml;'), // capital U, dieresis or umlaut mark
    (Code:#221; Symbol:'&Yacute;'), // capital Y, acute accent
    (Code:#222; Symbol:'&THORN;'), // capital THORN, Icelandic
    (Code:#223; Symbol:'&szlig;'), // small sharp s, German (sz ligature)
    (Code:#224; Symbol:'&agrave;'), // small a, grave accent
    (Code:#225; Symbol:'&aacute;'), // small a, acute accent
    (Code:#226; Symbol:'&acirc;'), // small a, circumflex accent
    (Code:#227; Symbol:'&atilde;'), // small a, tilde
    (Code:#228; Symbol:'&auml;'), // small a, dieresis or umlaut mark
    (Code:#229; Symbol:'&aring;'), // small a, ring
    (Code:#230; Symbol:'&aelig;'), // small ae diphthong (ligature)
    (Code:#231; Symbol:'&ccedil;'), // small c, cedilla
    (Code:#232; Symbol:'&egrave;'), // small e, grave accent
    (Code:#233; Symbol:'&eacute;'), // small e, acute accent
    (Code:#234; Symbol:'&ecirc;'), // small e, circumflex accent
    (Code:#235; Symbol:'&euml;'), // small e, dieresis or umlaut mark
    (Code:#236; Symbol:'&igrave;'), // small i, grave accent
    (Code:#237; Symbol:'&iacute;'), // small i, acute accent
    (Code:#238; Symbol:'&icirc;'), // small i, circumflex accent
    (Code:#239; Symbol:'&iuml;'), // small i, dieresis or umlaut mark
    (Code:#240; Symbol:'&eth;'), // small eth, Icelandic
    (Code:#241; Symbol:'&ntilde;'), // small n, tilde
    (Code:#242; Symbol:'&ograve;'), // small o, grave accent
    (Code:#243; Symbol:'&oacute;'), // small o, acute accent
    (Code:#244; Symbol:'&ocirc;'), // small o, circumflex accent
    (Code:#245; Symbol:'&otilde;'), // small o, tilde
    (Code:#246; Symbol:'&ouml;'), // small o, dieresis or umlaut mark
    (Code:#247; Symbol:'&divide;'), // divide sign
    (Code:#248; Symbol:'&oslash;'), // small o, slash
    (Code:#249; Symbol:'&ugrave;'), // small u, grave accent
    (Code:#250; Symbol:'&uacute;'), // small u, acute accent
    (Code:#251; Symbol:'&ucirc;'), // small u, circumflex accent
    (Code:#252; Symbol:'&uuml;'), // small u, dieresis or umlaut mark
    (Code:#253; Symbol:'&yacute;'), // small y, acute accent
    (Code:#254; Symbol:'&thorn;'), // small thorn, Icelandic
    (Code:#255; Symbol:'&yuml;')); // small y, dieresis or umlaut mark
var
  i,j:integer;
begin
  Result:=aStr;
  for i:=Length(Result) downto 1 do
  begin
    for j:=1 to MAXENCODECHARS do
      if Result[i]=ENCODECHARS[j].Code then
      begin
        Delete(Result,i,1);
        Insert(ENCODECHARS[j].Symbol,Result,i);
        Break;
      end;
  end;
end;

function EqualRect(const aRect1,aRect2:TRect):boolean;
begin
  Result:=(aRect1.Left=aRect2.Left) and
          (aRect1.Top=aRect2.Top) and
          (aRect1.Right=aRect2.Right) and
          (aRect1.Bottom=aRect2.Bottom);
end;

type
  TRLImageFile=class
    FileName:string;
    ImageCRC:cardinal;
  end;

{ TRLHTMLFilter }

// CONSTRUCTORS & DESTRUCTORS

constructor TRLHTMLFilter.Create(aOwner: TComponent);
begin
  fWorkingFileName:='';
  fDocumentStyle  :=dsCSS2;
  fFileCount      :=0;
  fFileIndex      :=0;
  fFontList       :=nil;
  fOnSaveGraphic  :=nil;
  fImageFiles     :=nil;
  //
  fImageFiles:=TObjectList.Create;
  //
  inherited;
  //
  DefaultExt :='.htm';
  DisplayName:=LS_WebPageStr;
end;

destructor TRLHTMLFilter.Destroy;
begin
  if Assigned(fImageFiles) then
    fImageFiles.Free;
  if Assigned(fFontList) then
    fFontList.free;
  //
  inherited;
end;

// CUSTOM

procedure TRLHTMLFilter.FontListNeeded;
begin
  if not assigned(fFontList) then
    fFontList:=TStringList.Create;
end;

function TRLHTMLFilter.AddFont(aFont:TRLMetaFont):integer;
var
  fontstr:string;
begin
  fontstr:=aFont.Name+'|'+IntToStr(aFont.Size)+'|'+ColorToHex(FromMetaColor(aFont.Color));
  Result :=fFontList.IndexOf(fontstr);
  if Result=-1 then
    Result:=fFontList.Add(fontstr);
end;

// OVERRIDE

procedure TRLHTMLFilter.InternalBeginDoc;
begin
  fPrintCut.X :=0;
  fPrintCut.Y :=0;
  fPrintSize.X:=Pages.OrientedWidth;
  fPrintSize.Y:=Pages.OrientedHeight;
  //
  fFileCount      :=0;
  fFileIndex      :=0;
  fGraphIndex     :=0;
  fWorkingFileName:=GetTempFileName;
  RegisterTempFile(fWorkingFileName);
  //
  AssignFile(fWorkingFileHandle,fWorkingFileName);
  Rewrite(fWorkingFileHandle);
  // demarca inicio da página
  WriteLn(fWorkingFileHandle);
  WriteLn(fWorkingFileHandle,BEGINPAGETAG);
  //
  FontListNeeded;
  fImageFiles.Clear;
end;

procedure TRLHTMLFilter.InternalEndDoc;
var
  s:string;
  f:textfile;
  n:string;
begin
  // demarca final da página
  WriteLn(fWorkingFileHandle);
  WriteLn(fWorkingFileHandle,ENDPAGETAG);
  Inc(fFileCount);
  CloseFile(fWorkingFileHandle);
  //
  n:='';
  Reset(fWorkingFileHandle);
  try
    while not Eof(fWorkingFileHandle) do
    begin
      ReadLn(fWorkingFileHandle,s);
      if s=BEGINPAGETAG then
        if (fDocumentStyle<>dsCSS2) or (fFileIndex=0) then
        begin
          n:=FileNameByIndex(fFileIndex);
          AssignFile(f,n);
          Rewrite(f);
          WritePageHeader(f);
        end
        else
      else if s=ENDPAGETAG then
      begin
        WritePageTools(f);
        if (fDocumentStyle<>dsCSS2) or (fFileIndex=fFileCount-1) then
        begin
          WritePageFooter(f);
          CloseFile(f);
          n:='';
        end;  
        Inc(fFileIndex);
      end
      else if n<>'' then
        WriteLn(f,s);
    end;
  finally
    CloseFile(fWorkingFileHandle);
    SysUtils.DeleteFile(fWorkingFileName);
    UnregisterTempFile(fWorkingFileName);
  end;
end;

procedure TRLHTMLFilter.InternalNewPage;
begin
  // demarca final da página
  WriteLn(fWorkingFileHandle);
  WriteLn(fWorkingFileHandle,ENDPAGETAG);
  Inc(fFileCount);
  // demarca inicio da página
  WriteLn(fWorkingFileHandle);
  WriteLn(fWorkingFileHandle,BEGINPAGETAG);
end;

procedure TRLHTMLFilter.InternalDrawPage(aPage:TRLGraphicSurface);
var
  obj    :TRLGraphicObject;
  bmpcalc:TBitmap;
  cliprct:TRect;
  i      :integer;
  function TextWidth(const aText:string; aFont:TRLMetaFont):integer;
  begin
    with bmpcalc.Canvas do
    begin
      Font.Name :=aFont.Name;
      Font.Pitch:=FromMetaFontPitch(aFont.Pitch);
      Font.Size :=aFont.Size;
      Font.Style:=FromMetaFontStyles(aFont.Style);
      Result:=TextWidth(aText);
    end;
  end;
  function ClipStr(const aInRect,aOuterRect:TRect):string; overload;
  var
    cut:TRect;
  begin
    IntersectRect(cut,aInRect,aOuterRect);
    if not EqualRect(cut,aInRect) then
      Result:='clip:rect('+IntToStr(cut.Top-aInRect.Top)+'px '+
                           IntToStr(cut.Right-aInRect.Left)+'px '+
                           IntToStr(cut.Bottom-aInRect.Top)+'px '+
                           IntToStr(cut.Left-aInRect.Left)+'px)' // top right bottom left
    else
      Result:='';
  end;
  function ClipStr(const aInPoint:TPoint; const aOuterRect:TRect):string; overload;
  var
    inrect:TRect;
  begin
    inrect.Left  :=aInPoint.X;
    inrect.Top   :=aInPoint.Y;
    inrect.Right :=aOuterRect.Right;
    inrect.Bottom:=aOuterRect.Bottom;
    Result:=ClipStr(inrect,aOuterRect);
  end;
  procedure DrawGraphic(Graphic:TGraphic; const Rect:TRect);
  var
    FileName:string;
  begin
    if DoSaveGraphic(Graphic,FileName) then
    begin
      Write(fWorkingFileHandle,'<div style="position:absolute;'+
                               'left:'+IntToStr(Rect.Left-fPrintCut.X)+'px;'+
                               'top:'+IntToStr(Rect.Top-fPrintCut.Y)+'px;'+
                               'width:'+IntToStr(Rect.Right-Rect.Left)+'px;'+
                               'height:'+IntToStr(Rect.Bottom-Rect.Top)+'px;'+
                               ClipStr(Rect,cliprct)+'">');
      Write(fWorkingFileHandle,'<img src="'+ExtractFileName(FileName)+'" '+
                               'width='+IntToStr(Rect.Right-Rect.Left)+' '+
                               'height='+IntToStr(Rect.Bottom-Rect.Top)+'>');
      Write(fWorkingFileHandle,'</div>');
      WriteLn(fWorkingFileHandle);
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
    Write(fWorkingFileHandle,'<hr style="position:absolute;'+
                             'left:'+IntToStr(aObj.BoundsRect.Left-fPrintCut.X)+'px;'+
                             'top:'+IntToStr(aObj.BoundsRect.Top-fPrintCut.Y)+'px;'+
                             'width:1px;'+
                             'height:1px;'+
                             ClipStr(FromMetaRect(aObj.BoundsRect),cliprct)+';'+
                             'color:#'+ColorToHex(FromMetaColor(aObj.Color))+';">');
    WriteLn(fWorkingFileHandle);
  end;
  procedure DrawLine(aObj:TRLLineObject);
  begin
    Write(fWorkingFileHandle,'<hr style="position:absolute;'+
                             'left:'+IntToStr(aObj.BoundsRect.Left-fPrintCut.X)+'px;'+
                             'top:'+IntToStr(aObj.BoundsRect.Top-fPrintCut.Y)+'px;'+
                             'width:'+IntToStr(aObj.BoundsRect.Right-aObj.BoundsRect.Left)+'px;'+
                             'height:'+IntToStr(aObj.BoundsRect.Bottom-aObj.BoundsRect.Top)+'px;'+
                             ClipStr(FromMetaRect(aObj.BoundsRect),cliprct)+';'+
                             'color:#'+ColorToHex(FromMetaColor(aObj.Pen.Color))+';">');
    WriteLn(fWorkingFileHandle);
  end;
  procedure DrawRectangle(aObj:TRLRectangleObject);
  begin
    Write(fWorkingFileHandle,'<hr style="position:absolute;'+
                             'left:'+IntToStr(aObj.BoundsRect.Left-fPrintCut.X)+'px;'+
                             'top:'+IntToStr(aObj.BoundsRect.Top-fPrintCut.Y)+'px;'+
                             'width:'+IntToStr(aObj.BoundsRect.Right-aObj.BoundsRect.Left)+'px;'+
                             'height:'+IntToStr(aObj.BoundsRect.Bottom-aObj.BoundsRect.Top)+'px;'+
                             ClipStr(FromMetaRect(aObj.BoundsRect),cliprct)+';'+
                             'color:#'+ColorToHex(FromMetaColor(aObj.Brush.Color))+';">');
    WriteLn(fWorkingFileHandle);
  end;
  procedure DrawFillRect(aObj:TRLFillRectObject);
  begin
    if aObj.Brush.Style=MetaBrushStyleClear then
      Exit;
    Write(fWorkingFileHandle,'<div style="position:absolute;'+
                             'left:'+IntToStr(aObj.BoundsRect.Left-fPrintCut.X)+'px;'+
                             'top:'+IntToStr(aObj.BoundsRect.Top-fPrintCut.Y)+'px;'+
                             'width:'+IntToStr(aObj.BoundsRect.Right-aObj.BoundsRect.Left)+'px;'+
                             'height:'+IntToStr(aObj.BoundsRect.Bottom-aObj.BoundsRect.Top)+'px;'+
                             ClipStr(FromMetaRect(aObj.BoundsRect),cliprct)+';'+
                             'background-color:#'+ColorToHex(FromMetaColor(aObj.Brush.Color))+'"></div>');
    WriteLn(fWorkingFileHandle);
  end;
  procedure DrawEllipse(aObj:TRLEllipseObject);
  var
    bmp:TBitmap;
  begin
    bmp:=CreateBitmap(aObj);
    try
      CopyPen(aObj.Pen,bmp);
      CopyBrush(aObj.Brush,bmp);
      //
      bmp.Canvas.Ellipse(aObj.BoundsRect.Left-fPrintCut.X-aObj.BoundsRect.Left,
        aObj.BoundsRect.Top-fPrintCut.Y-aObj.BoundsRect.Top,
        aObj.BoundsRect.Right-fPrintCut.X-aObj.BoundsRect.Left,
        aObj.BoundsRect.Bottom-fPrintCut.Y-aObj.BoundsRect.Top);
      DrawGraphic(bmp,FromMetaRect(aObj.BoundsRect));
    finally
      bmp.Free;
    end;
  end;
  procedure DrawPolygon(aObj:TRLPolygonObject);
  var
    bmp:TBitmap;
    pts:array of TPoint;
    i  :integer;
  begin
    bmp:=CreateBitmap(aObj);
    try
      CopyPen(aObj.Pen,bmp);
      CopyBrush(aObj.Brush,bmp);
      //  
      SetLength(pts,High(aObj.Points)+1);
      for i:=0 to High(aObj.Points) do
        pts[i]:=Point(aObj.Points[i].X-fPrintCut.X-aObj.BoundsRect.Left,aObj.Points[i].Y-fPrintCut.Y-aObj.BoundsRect.Top);
      bmp.Canvas.Polygon(pts);  
      DrawGraphic(bmp,FromMetaRect(aObj.BoundsRect));
    finally
      bmp.Free;
    end;
  end;
  procedure DrawPolyline(aObj:TRLPolylineObject);
  var
    bmp:TBitmap;
    pts:array of TPoint;
    i  :integer;
  begin
    bmp:=CreateBitmap(aObj);
    try
      CopyPen(aObj.Pen,bmp);
      //
      SetLength(pts,High(aObj.Points)+1);
      for i:=0 to High(aObj.Points) do
        pts[i]:=Point(aObj.Points[i].X-fPrintCut.X-aObj.BoundsRect.Left,aObj.Points[i].Y-fPrintCut.Y-aObj.BoundsRect.Top);
      bmp.Canvas.Polyline(pts);  
      DrawGraphic(bmp,FromMetaRect(aObj.BoundsRect));
    finally
      bmp.Free;
    end;
  end;
  procedure DrawImage(aObj:TRLImageObject);
  var
    graphfn:string;
    graph  :TGraphic;
  begin
    /// verificar repetição de arq de imagens aqui (crc?)
    graph:=FromMetaGraphic(aObj.Data);
    if Assigned(graph) then
      try
        if DoSaveGraphic(graph,graphfn) then
        begin
          Write(fWorkingFileHandle,'<div style="position:absolute;'+
                                   'left:'+IntToStr(aObj.BoundsRect.Left-fPrintCut.X)+'px;'+
                                   'top:'+IntToStr(aObj.BoundsRect.Top-fPrintCut.Y)+'px;'+
                                   'width:'+IntToStr(aObj.BoundsRect.Right-aObj.BoundsRect.Left)+'px;'+
                                   'height:'+IntToStr(aObj.BoundsRect.Bottom-aObj.BoundsRect.Top)+'px;'+
                                   ClipStr(FromMetaRect(aObj.BoundsRect),cliprct)+'">');
          Write(fWorkingFileHandle,'<img src="'+ExtractFileName(graphfn)+'" '+
                                   'width='+IntToStr(aObj.BoundsRect.Right-aObj.BoundsRect.Left)+' '+
                                   'height='+IntToStr(aObj.BoundsRect.Bottom-aObj.BoundsRect.Top)+'>');
          Write(fWorkingFileHandle,'</div>');
          WriteLn(fWorkingFileHandle);
        end;  
      finally
        graph.free;
      end;
  end;
  procedure DrawText(aObj:TRLTextObject);
  begin
    // cor de fundo
    if aObj.Brush.Style<>MetaBrushStyleClear then
      Write(fWorkingFileHandle,'<div style="position:absolute;'+
                               'left:'+IntToStr(aObj.BoundsRect.Left-fPrintCut.X)+'px;'+
                               'top:'+IntToStr(aObj.BoundsRect.Top-fPrintCut.Y)+'px;'+
                               'width:'+IntToStr(aObj.BoundsRect.Right-aObj.BoundsRect.Left)+'px;'+
                               'height:'+IntToStr(aObj.BoundsRect.Bottom-aObj.BoundsRect.Top)+'px;'+
                               ClipStr(FromMetaRect(aObj.BoundsRect),cliprct)+';'+
                               'background-color:#'+ColorToHex(FromMetaColor(aObj.Brush.Color))+';"></div>');
    // início de layer
    if (aObj.TextFlags and MetaTextFlagAutoSize)=MetaTextFlagAutoSize then
      Write(fWorkingFileHandle,'<div style="position:absolute;'+
                               'left:'+IntToStr(aObj.Origin.X-fPrintCut.X)+'px;'+
                               'top:'+IntToStr(aObj.Origin.Y-fPrintCut.Y)+'px;'+
                               ClipStr(FromMetaPoint(aObj.Origin),cliprct)+'">')
    else
      case aObj.Alignment of
        MetaTextAlignmentCenter: Write(fWorkingFileHandle,'<div style="position:absolute;'+
                                                          'left:'+IntToStr((aObj.BoundsRect.Right+aObj.BoundsRect.Left-TextWidth(aObj.DisplayText,aObj.Font)) div 2)+'px;'+
                                                          'top:'+IntToStr(aObj.Origin.Y-fPrintCut.Y)+'px;'+
                                                          ClipStr(FromMetaRect(aObj.BoundsRect),cliprct)+'">');
        MetaTextAlignmentRight : Write(fWorkingFileHandle,'<div style="position:absolute;'+
                                                          'left:'+IntToStr(aObj.BoundsRect.Right-TextWidth(aObj.DisplayText,aObj.Font))+'px;'+
                                                          'top:'+IntToStr(aObj.Origin.Y-fPrintCut.Y)+'px;'+
                                                          ClipStr(FromMetaRect(aObj.BoundsRect),cliprct)+'">');
      else //TextAlignmentLeftJustify
        Write(fWorkingFileHandle,'<div style="position:absolute;'+
                                 'left:'+IntToStr(aObj.Origin.X-fPrintCut.X)+'px;'+
                                 'top:'+IntToStr(aObj.Origin.Y-fPrintCut.Y)+'px;'+
                                 ClipStr(FromMetaRect(aObj.BoundsRect),cliprct)+'">');
      end;
    // seleciona fonte
    Write(fWorkingFileHandle,'<font class="f'+IntToStr(AddFont(aObj.Font))+'">');
    // efeitos
    if (aObj.Font.Style and MetaFontStyleBold)=MetaFontStyleBold then
      Write(fWorkingFileHandle,'<b>');
    if (aObj.Font.Style and MetaFontStyleItalic)=MetaFontStyleItalic then
      Write(fWorkingFileHandle,'<i>');
    if (aObj.Font.Style and MetaFontStyleUnderline)=MetaFontStyleUnderline then
      Write(fWorkingFileHandle,'<u>');
    if (aObj.Font.Style and MetaFontStyleStrikeOut)=MetaFontStyleStrikeOut then
      Write(fWorkingFileHandle,'<strike>');
    // o texto
    Write(fWorkingFileHandle,EncodeISO(aObj.DisplayText));
    // retorna fonte
    if (aObj.Font.Style and MetaFontStyleStrikeOut)=MetaFontStyleStrikeOut then
      Write(fWorkingFileHandle,'</strike>');
    if (aObj.Font.Style and MetaFontStyleUnderline)=MetaFontStyleUnderline then
      Write(fWorkingFileHandle,'</u>');
    if (aObj.Font.Style and MetaFontStyleItalic)=MetaFontStyleItalic then
      Write(fWorkingFileHandle,'</i>');
    if (aObj.Font.Style and MetaFontStyleBold)=MetaFontStyleBold then
      Write(fWorkingFileHandle,'</b>');
    Write(fWorkingFileHandle,'</font>');
    // fim de layer
    Write(fWorkingFileHandle,'</div>');
    WriteLn(fWorkingFileHandle);
  end;
  procedure DrawSetClipRect(aObj:TRLSetClipRectObject);
  begin
    cliprct:=FromMetaRect(aObj.BoundsRect);
  end;
  procedure DrawResetClipRect(aObj:TRLResetClipRectObject);
  begin
    cliprct:=Rect(0,0,fPrintSize.X,fPrintSize.Y);
  end;
begin
  WriteLn(fWorkingFileHandle,'<div style="position:relative;'+
                             'width:'+IntToStr(fPrintSize.X)+'px;'+
                             'height:'+IntToStr(fPrintSize.Y)+'px;'+
                             'background-color:#FFFFFF;">');
  cliprct:=Rect(0,0,fPrintSize.X,fPrintSize.Y);
  bmpcalc:=NewBitmap;
  try
    // grava tags
    for i:=0 to aPage.ObjectCount-1 do
    begin
      obj:=aPage.Objects[i];
      if obj is TRLPixelObject then
        DrawPixel(TRLPixelObject(obj))
      else if obj is TRLLineObject then
        DrawLine(TRLLineObject(obj))
      else if obj is TRLRectangleObject then
        DrawRectangle(TRLRectangleObject(obj))
      else if obj is TRLTextObject then
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
    end;
  finally
    bmpcalc.free;
  end;
  WriteLn(fWorkingFileHandle,'</div>');
end;

procedure TRLHTMLFilter.WritePageHeader(var aFile:textfile);
var
  i,p:integer;
  s,fn,fz,fc:string;
begin
  // cabeçalho do documento
  WriteLn(aFile,'<html>');
  WriteLn(aFile);
  WriteLn(aFile,'<head>');
  WriteLn(aFile,'<meta http-equiv="Content-Type" content="text/html" charset="iso-8859-1">');
  WriteLn(aFile,'<meta name="GENERATOR" content="'+CS_ProductTitleStr+' '+IntToStr(CommercialVersion)+'.'+IntToStr(ReleaseVersion)+CommentVersion+'">');
  if Pages.Title<>'' then
    WriteLn(aFile,'<title>'+Pages.Title+'</title>');
  WriteLn(aFile,'<style>');
  //
  if fDocumentStyle=dsCSS2 then
  begin
    WriteLn(aFile,'@media print');
    WriteLn(aFile,'{');
    WriteLn(aFile,'.nonprintable {display:none;}');
    WriteLn(aFile,'}');
  end;
  // write fonts
  for i:=0 to fFontList.Count-1 do
  begin
    s:=fFontList[i];
    p:=pos('|',s); if p=0 then p:=Length(s)+1; fn:=Copy(s,1,p-1); Delete(s,1,p);
    p:=pos('|',s); if p=0 then p:=Length(s)+1; fz:=Copy(s,1,p-1); Delete(s,1,p);
    p:=pos('|',s); if p=0 then p:=Length(s)+1; fc:=Copy(s,1,p-1); Delete(s,1,p);
    WriteLn(aFile,'font.f'+IntToStr(i)+' {font-family:"'+fn+'"; font-size:'+fz+'pt; color:#'+fc+'}');
  end;
  //
  WriteLn(aFile,'</style>');
  WriteLn(aFile,'</head>');
  WriteLn(aFile);
  WriteLn(aFile,'<body>');
  WriteLn(aFile);
end;

procedure TRLHTMLFilter.WritePageFooter(var aFile:textfile);
begin
  // fim de documento
  WriteLn(aFile,'</body>');
  WriteLn(aFile);
  WriteLn(aFile,'</html>');
end;

procedure TRLHTMLFilter.WritePageTools(var aFile:textfile);
var
  i,q,f:integer;
begin
  case fDocumentStyle of
    dsCSS2    : if fFileIndex<fFileCount-1 then
                  WriteLn(aFile,'<div style="position:relative; page-break-after:always"><br></div>');
    dsOldStyle: begin
                  WriteLn(aFile,'<div style="width:'+IntToStr(fPrintSize.X)+'px; height:20px">');
                  WriteLn(aFile,'<table width="100%" height="100%">');
                  WriteLn(aFile,'<tr>');
                  WriteLn(aFile,'<td align="center">');
                  if fFileIndex>0 then
                  begin
                    WriteLn(aFile,'<a href="'+FileNameByIndex(0)+'">'+EncodeISO('|<')+'</a>');
                    WriteLn(aFile,'<a href="'+FileNameByIndex(fFileIndex-1)+'">'+EncodeISO('<')+'</a>');
                  end;
                  //
                  q:=20;
                  i:=fFileIndex-q;
                  if i<0 then
                    i:=0;
                  if i+q>fFileCount-1 then
                    q:=fFileCount-1-i;
                  f:=i+q;
                  while i<f do
                  begin
                    if i=fFileIndex then
                      WriteLn(aFile,IntToStr(i+1))
                    else
                      WriteLn(aFile,'<a href="'+FileNameByIndex(i)+'">'+IntToStr(i+1)+'</a>');
                    Inc(i);
                  end;
                  //
                  if fFileIndex<fFileCount-1 then
                  begin
                    WriteLn(aFile,'<a href="'+FileNameByIndex(fFileIndex+1)+'">'+EncodeISO('>')+'</a>');
                    WriteLn(aFile,'<a href="'+FileNameByIndex(fFileCount-1)+'">'+EncodeISO('>|')+'</a>');
                  end;
                  WriteLn(aFile,'</td>');
                  WriteLn(aFile,'</tr>');
                  WriteLn(aFile,'</table>');
                  WriteLn(aFile,'</div>');
                end;
  end;
end;

function TRLHTMLFilter.FileNameByIndex(aFileIndex:integer):string;
var
  e:string;
begin
  if aFileIndex=0 then
    Result:=FileName
  else
  begin
    e:=ExtractFileExt(FileName);
    Result:=Copy(FileName,1,Length(FileName)-Length(e))+IntToStr(aFileIndex)+e;
  end;
end;

function TRLHTMLFilter.DoSaveGraphic(aGraphic:TGraphic; var aFileName:string):boolean;
var
  graphext:string;
  graphdat:TStringStream;
  graphcrc:cardinal;
  imgfile :TRLImageFile;
  i       :integer;
begin
  if aGraphic is TBitmap then
    graphext:='bmp'
  else if aGraphic is TIcon then
    graphext:='ico'
  else
    graphext:='';
  // AvoidDuplicatedImages
  graphdat:=TStringStream.Create('');
  try
    aGraphic.SaveToStream(graphdat);
    graphcrc:=CRC32(graphdat.DataString);
  finally
    graphdat.Free;
  end;
  i:=0;
  while (i<fImageFiles.Count) and (TRLImageFile(fImageFiles[i]).ImageCRC<>graphcrc) do
    Inc(i);
  if i<fImageFiles.Count then
  begin
    aFileName:=TRLImageFile(fImageFiles[i]).FileName;
    Result:=True;
    Exit;
  end;
  //
  Inc(fGraphIndex);
  aFileName:=FileNameByIndex(fFileIndex);
  aFileName:=Copy(aFileName,1,Length(aFileName)-Length(ExtractFileExt(aFileName)))+'-img'+IntToStr(fGraphIndex)+'.'+graphext;
  if Assigned(fOnSaveGraphic) then
  begin
    Result:=True;
    fOnSaveGraphic(Self,aGraphic,aFileName,Result);
  end
  else if graphext='' then
    Result:=False
  else
  begin
    aGraphic.SaveToFile(aFileName);
    Result:=True;
  end;
  //
  // hoAvoidDuplicatedImages
  if Result then
  begin
    imgfile:=TRLImageFile.Create;
    imgfile.FileName:=aFileName;
    imgfile.ImageCRC:=graphcrc;
    fImageFiles.Add(imgfile);
  end;
end;

end.

