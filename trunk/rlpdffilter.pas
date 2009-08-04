{$I RLReport.inc}

{@unit RLPDFFilter - Implementação do filtro para criação de arquivos PDF.
Portado para o Lazarus - Trabalho inicial de Isaac Trindade da Silva contato tioneobrasil@yahoo.com.br dicas4lazarus@yahoo.com.br
Lazarus Ported - initial work by Isaac 07/2009
}
unit RLPDFFilter;
{$MODE DELPHI}{$H+}

interface

uses
  SysUtils, Classes, Math, Contnrs,
{$ifdef VCL}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ELSE}
  LCLIntf,
  {$ENDIF}
  Graphics, RLMetaVCL,
{$else}
  Types, QGraphics, RLMetaCLX,
{$endif}
  RLMetaFile, RLConsts, RLTypes, RLUtils, RLFilters;

const
  PDF_FILEVERSION   ='1.2';
  PDF_POINTSPERINCH =72; // point size reference : 12p = 72 = 7.2 per character
  PDF_MAXCOLUMNS    =8;
  PDF_MAXROWS       =4;
  PDF_MAXCOLORSTACK =64;
  PDF_EOL           =#13#10;

const
  PS1FONTCOUNT=14; // max. standard postscript font type 1
  PS1FONTNAMES:array[1..PS1FONTCOUNT] of string=(
    'Courier',
    'Courier-Bold',
    'Courier-Oblique',
    'Courier-BoldOblique',
    'Helvetica',
    'Helvetica-Bold',
    'Helvetica-Oblique',
    'Helvetica-BoldOblique',
    'Symbol',
    'Times-Roman',
    'Times-Bold',
    'Times-Italic',
    'Times-BoldItalic',
    'ZapfDingbats');

type
  TRLPDFFilterPageMode     =(pmRegular,pmOutlines,pmThumbs,pmFullScreen);
  TRLPDFFilterPageLayout   =(plRegular,plSinglePage,plOneColumn,plTwoColumnLeft,plTwoColumnRight);
  TRLPDFFilterFontEncoding =(feNoEncoding,feMacRomanEncoding,feWinAnsiEncoding);
  TRLPDFFilterFontType     =(ftPsType1,ftTrueType);
  TRLPDFFilterViewerOption =(voRegular,voHideToolBar,voHideMenuBar,voHideWindowUI,voFitWindow,voCenterWindow);
  TRLPDFFilterViewerOptions=set of TRLPDFFilterViewerOption;
  TRLPDFFilterDashPattern  =(dpRegular,dpSolid,dp33,dp1222,dp2121,dp23535,dp13232);
  TRLPDFFilterColumnBorder =(cbRegular,cbSplitter,cbBorder);

  TRLPDFFilterPaperSize=record
    Width :word;
    Height:word;
  end;

  TRLPDFFilterMargin=record
    Top   :word;
    Bottom:word;
    Left  :word;
    Right :word;
    Width :word;
    Height:word;
  end;

  TRLPDFFilterLocation=record
    X,Y:word;
  end;

  TRLPDFFilterOutline=record
    ObjId:word;
    Page :word;
    Title:string[60];
  end;

  TRLPDFFilterColumn=record
    WorkArea        :TRLPDFFilterMargin;
    CharCount       :word;
    LineCount       :word;
    CurrentPoint    :TRLPDFFilterLocation;
    CurrentCharPoint:word;
    CurrentLine     :word;
  end;

  TRLPDFFilterColumns=array[1..PDF_MAXCOLUMNS,1..PDF_MAXROWS] of TRLPDFFilterColumn;

  TRLPDFFilterFont=record
    FontName :string;
    FontStyle:TRLMetaFontStyles;
    FontObjAt:integer;
  end;

  TRLPDFFilterImage=record
    ImageObjAt:integer;
  end;

const
  DefaultViewerOptions=[voRegular];

type
  TRLPDFFilterPageSetup=class;
  TRLPDFFilterDocumentInfo=class;
  TRLPDFFilterTextControl=class;

  { TRLPDFFilter }

  {@class TRLPDFFilter - Filtro para criação de documentos PDF a partir de um relatório.
   O arquivo gerado pode ser lido pelo aplicativo Adobe AcrobatReader e pelo plugin associado
   ao navegador de internet. Inclui imagens e fontes em um só volume e o resultado final é
   bastante fiel ao relatório original, sendo ideal para distribuição via internet.
   @links TRLHTMLFilter, TRLRichFilter, TRLXLSFilter. 
   @ancestor TRLCustomSaveFilter.
   @pub }
  TRLPDFFilter=class(TRLCustomSaveFilter)
  private
    // variables
    fOutputStream  :TFileStream;
    fPrintCut      :TPoint;
    fPrintSize     :TPoint;
    fWritePos      :integer;
    fCurrentPageNo :integer;
    fObjects       :array of integer; 
    fObjectCount   :integer;
    fOutlines      :array of TRLPDFFilterOutline;
    fOutlineCount  :integer;
    fFonts         :array of TRLPDFFilterFont;
    fFontCount     :integer;
    fImages        :array of TRLPDFFilterImage;
    fImageCount    :integer;
    fPageMode      :TRLPDFFilterPageMode;
    fFullPageMode  :TRLPDFFilterPageMode;
    fPageLayout    :TRLPDFFilterPageLayout;
    fViewerOptions :TRLPDFFilterViewerOptions;
    fForceASCII    :boolean;
    fFontEncoding  :TRLPDFFilterFontEncoding;
    fFontName      :string;
    fPages         :array of integer;
    fPageCount     :integer;
    fPageOffset    :integer;
    fPageTitle     :string;
    fFontColor     :TColor;
    fOldFontColor  :integer;
    fXRefOfs       :integer;
    fContentsObjAt :integer;
    fLengthObjAt   :integer;
    //
    fDocumentInfo  :TRLPDFFilterDocumentInfo;
    fPageSetup     :TRLPDFFilterPageSetup;
    fTextControl   :TRLPDFFilterTextControl;
    //
    fPageTreeObjAt :integer;
    fOutlinesObjAt :integer;
    fResourcesObjAt:integer;
    fCatalogObjAt  :integer;
    fInfoObjAt     :integer;
    //
    fImageFiles    :TObjectList;
    //
    procedure   SetDocumentInfo(const Value:TRLPDFFilterDocumentInfo);
    procedure   SetPageSetup(const Value:TRLPDFFilterPageSetup);
    procedure   SetTextControl(const Value:TRLPDFFilterTextControl);
    //
    procedure   Write(const aStr:string='');
    procedure   WriteLn(const aStr:string='');
    //
    procedure   WriteBOF;
    procedure   WriteInfo;
    procedure   WriteCatalog;
    //
    procedure   WriteFonts;
    procedure   WriteResources;
    procedure   WriteOutlines;
    procedure   WritePageTree;
    procedure   WriteXRef;
    procedure   WriteTrailer;
    procedure   WriteEOF;
    //
    {@method AddObj - Cria um novo objeto e retorna o seu id (índice no array de objetos).:/}
    function    AddObj:integer;
    {@method BeginObj - Inicia a escrita do objeto indicado por aIndex. Se aIndex não for informado,
    neste momento um novo objeto será criado e seu índice retornado pela função.:/}
    function    BeginObj(aIndex:integer=0):integer;
    {@method EndObj - Finaliza a escrita de um objeto qualquer. Não é necessário indicar o índice,
    uma vez que o marcador de fim de objeto não requer.:/}
    procedure   EndObj;
    {@method BeginShortObj - Inicia a escrita do objeto indicado por aIndex. Se aIndex não for informado,
    neste momento um novo objeto será criado e seu índice retornado pela função.:/}
    function    BeginShortObj(aIndex:integer=0):integer;
    {@method EndShortObj - Finaliza a escrita de um objeto qualquer. Não é necessário indicar o índice,
    uma vez que o marcador de fim de objeto não requer.:/}
    procedure   EndShortObj;
    procedure   BeginDoc;
    procedure   EndDoc;
    procedure   BeginPage(aPage:TRLGraphicSurface);
    procedure   EndPage;
    function    BeginStream:integer;
    function    EndStream:integer;
    function    GetFontId(const aFontName:string; aFontStyle:TRLMetaFontStyles):integer;
    procedure   FixupPageSetup;
    procedure   Reset;
    //
    procedure   WriteText(aLeft,aTop:double; const aText:string; aFontId,aFontSize:integer);
    function    WriteBitmap(aBitmap:TBitmap):integer;
    procedure   WriteBitmapData(aBitmap:TBitmap);
    //
    function    PDF_PointStr(X,Y:double):string;
    function    PDF_BoundsStr(aLeft,aTop,aRight,aBottom:double):string;
    function    RGBStr(aRed,aGreen,aBlue:integer):string;
    function    GetTextWidth(const aText:string; aFont:TRLMetaFont):integer;
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
    //
    class function PDF_CalcTextLeadingPointSize(aFontSize: word): word;
    class function PDF_CurrentDateTimeStr: string;
    class function PDF_EncodeText(const aText: string): string;
    class function PDF_FloatToStr(aFloat: double): string;
    class function PDF_GetDashPattern(aDashPattern: TRLPDFFilterDashPattern): string;
    class function PDF_IndirectObjStr(aIndex: integer): string;
    class function PDF_PixelsToPoints(aPixels: double): double;
    class function PDF_Zeros(aValue, aWidth: integer): string;
    //
    property PageSetup   :TRLPDFFilterPageSetup   read fPageSetup   write SetPageSetup;
    property TextControl :TRLPDFFilterTextControl read fTextControl write SetTextControl;
  published
    {@prop DocumentInfo - Informações do documento gerado. @links TRLPDFFilterDocumentInfo. :/}
    property DocumentInfo :TRLPDFFilterDocumentInfo  read fDocumentInfo  write SetDocumentInfo;
    {@prop ViewerOptions - Opções para o visualizador. @links TRLPDFFilterViewerOptions. :/}
    property ViewerOptions:TRLPDFFilterViewerOptions read fViewerOptions write fViewerOptions default DefaultViewerOptions;
    {@prop PageMode - Opções para o visualizador em modo de página. @links TRLPDFFilterPageMode. :/}
    property PageMode     :TRLPDFFilterPageMode      read fPageMode      write fPageMode      default pmRegular;
    {@prop FullPageMode - Opções para o visualizador em modo de tela-cheia. @links TRLPDFFilterPageMode. :/}
    property FullPageMode :TRLPDFFilterPageMode      read fFullPageMode  write fFullPageMode  default pmRegular;
    {@prop PageLayout - Opções para o layout de página. @links TRLPDFFilterPageLayout. :/}
    property PageLayout   :TRLPDFFilterPageLayout    read fPageLayout    write fPageLayout    default plRegular;
    {@prop FontEncoding - Opção para codificação de caracteres. @links TRLPDFFilterFontEncoding. :/}
    property FontEncoding :TRLPDFFilterFontEncoding  read fFontEncoding  write fFontEncoding  default feWinAnsiEncoding;
    {@prop ForceASCII - Força o modo ASCII. :/}
    property ForceASCII   :boolean                   read fForceASCII    write fForceASCII    default False;
    {@prop FileName = ancestor /}
    property FileName;
    {@prop DisplayName = ancestor /}
    property DisplayName;
    {@prop ShowProgress - ancestor /}
    property ShowProgress;
  end;
  {/@class}

  TRLPDFFilterPageSetup=class(TPersistent)
  private
    fPaperSize             :TRLPDFFilterPaperSize;
    fLandScape             :boolean;
    fMediaSize             :TRLPDFFilterPaperSize;
    fMargins               :TRLPDFFilterMargin;
    fPageBorder            :boolean;
    fColumnBorder          :TRLPDFFilterColumnBorder;
    fBorderDashPattern     :TRLPDFFilterDashPattern;
    fColumnMargin          :TRLPDFFilterMargin;
    fColumnGap             :TRLPDFFilterLocation;
    fColumnCount           :word;
    fRowCount              :word;
    fFontPointSize         :word;
    fColumnFontPointSize   :word;
    fLeadingPointSize      :word;
    fColumnLeadingPointSize:word;
    fCharCount             :word;
    fWorkArea              :TRLPDFFilterMargin;
    fColumns               :TRLPDFFilterColumns;
  public
    constructor Create;
    //
    procedure   Assign(Source:TRLPDFFilterPageSetup); reintroduce;
    procedure   Clear;

    {$IFDEF FPC}
    property    WorkArea              :TRLPDFFilterMargin       read fWorkArea               write fWorkArea;
    property    ColumnMargin          :TRLPDFFilterMargin       read fColumnMargin           write fColumnMargin;
    property    ColumnGap             :TRLPDFFilterLocation     read fColumnGap              write fColumnGap;
    property    Margins               :TRLPDFFilterMargin       read fMargins                write fMargins;
    property    PaperSize             :TRLPDFFilterPaperSize    read fPaperSize              write fPaperSize;
    property    MediaSize             :TRLPDFFilterPaperSize    read fMediaSize              write fMediaSize;
    {$ENDIF}

  published
    {$IFNDEF FPC}
    property    WorkArea              :TRLPDFFilterMargin       read fWorkArea               write fWorkArea;
    property    ColumnMargin          :TRLPDFFilterMargin       read fColumnMargin           write fColumnMargin;
    property    ColumnGap             :TRLPDFFilterLocation     read fColumnGap              write fColumnGap;
    property    Margins               :TRLPDFFilterMargin       read fMargins                write fMargins;
    property    PaperSize             :TRLPDFFilterPaperSize    read fPaperSize              write fPaperSize;
    property    MediaSize             :TRLPDFFilterPaperSize    read fMediaSize              write fMediaSize;
    {$ENDIF}
    property    LandScape             :boolean                  read fLandScape              write fLandScape;
    property    PageBorder            :boolean                  read fPageBorder             write fPageBorder;
    property    ColumnBorder          :TRLPDFFilterColumnBorder read fColumnBorder           write fColumnBorder;
    property    BorderDashPattern     :TRLPDFFilterDashPattern  read fBorderDashPattern      write fBorderDashPattern;
    property    ColumnCount           :word                     read fColumnCount            write fColumnCount;
    property    RowCount              :word                     read fRowCount               write fRowCount;
    property    FontPointSize         :word                     read fFontPointSize          write fFontPointSize;
    property    ColumnFontPointSize   :word                     read fColumnFontPointSize    write fColumnFontPointSize;
    property    LeadingPointSize      :word                     read fLeadingPointSize       write fLeadingPointSize;
    property    ColumnLeadingPointSize:word                     read fColumnLeadingPointSize write fColumnLeadingPointSize;
    property    CharCount             :word                     read fCharCount              write fCharCount;
  end;

  {@class TRLPDFFilterDocumentInfo - Informações para a geração de documento PDF. }
  TRLPDFFilterDocumentInfo=class(TPersistent)
  private
    fTitle   :string;
    fSubject :string;
    fAuthor  :string;
    fKeyWords:string;
    fCreator :string;
    fProducer:string;
    fPassword:string;
    fModDate :TDateTime;
  public
    constructor Create;
    //
    procedure Assign(Source:TRLPDFFilterDocumentInfo); reintroduce;
    procedure Clear;
  published
    {@prop Title - Título do documento. :/}
    property Title   :string read fTitle    write fTitle;
    {@prop Subject - Assunto do documento. :/}
    property Subject :string read fSubject  write fSubject;
    {@prop Author - Nome do autor do documento. :/}
    property Author  :string read fAuthor   write fAuthor;
    {@prop KeyWords - Palavras chaves para busca. :/}
    property KeyWords:string read fKeyWords write fKeyWords;
    {@prop Creator - Programa utilitário gerador. :/}
    property Creator :string read fCreator  write fCreator;
    {@prop Producer - Nome da produtora. :/}
    property Producer:string read fProducer write fProducer;
    {@prop Password - Senha para o arquivo. -não implementado :/}
//    property Password:string read fPassword write fPassword;
    {@prop ModDate - Data de modiicação do arquivo. :/}
    property ModDate:TdateTime read fModDate write fModDate;
  end;
  {/@class}

  TRLPDFFilterTextControl=class(TPersistent)
  private
    fFormFeed:boolean;
    fWordWrap:boolean;
    fTabSize :integer;
  public
    constructor Create;
    //
    procedure Assign(Source:TRLPDFFilterTextControl); reintroduce;
    procedure Clear;
  published
    property FormFeed:boolean read fFormFeed write fFormFeed;
    property WordWrap:boolean read fWordWrap write fWordWrap;
    property TabSize :integer read fTabSize  write fTabSize;
  end;

var
  DefaultDocumentInfo:TRLPDFFilterDocumentInfo=nil;
  DefaultTextControl :TRLPDFFilterTextControl =nil;

{/@unit}

implementation

const
  NullPaperSize:TRLPDFFilterPaperSize=(Width:0; Height:0);
  NullMargin   :TRLPDFFilterMargin   =(Top:0; Bottom:0; Left:0; Right:0; Width:0; Height:0);
  NullLocation :TRLPDFFilterLocation =(X:0; Y:0);

const
  ObjectsInc =1024;
  FontsInc   =64;
  ImagesInc  =1024;
  PagesInc   =512;
  OutlinesInc=250;

// UTILS

function IntToOctal(aValue:integer; aWidth:integer=0):string;
begin
  Result:=emptystr;
  repeat
    Result:=IntToStr(aValue mod 8)+Result;
    aValue:=aValue div 8;
  until aValue=0;
  while Length(Result)<aWidth do
    Result:='0'+Result;
end;

{ TRLPDFFilterPageSetup }

constructor TRLPDFFilterPageSetup.Create;
begin
  fPaperSize             :=NullPaperSize;
  fLandScape             :=false;
  fMediaSize             :=NullPaperSize;
  fMargins               :=NullMargin;
  fPageBorder            :=false;
  fColumnBorder          :=cbRegular;
  fBorderDashPattern     :=dpRegular;
  fColumnMargin          :=NullMargin;
  fColumnGap             :=NullLocation;
  fColumnCount           :=0;
  fRowCount              :=0;
  fFontPointSize         :=0;
  fColumnFontPointSize   :=0;
  fLeadingPointSize      :=0;
  fColumnLeadingPointSize:=0;
  fCharCount             :=0;
  fWorkArea              :=NullMargin;
  //
  inherited Create;
end;

procedure TRLPDFFilterPageSetup.Clear;
begin
  fPaperSize             :=NullPaperSize;
  fLandScape             :=false;
  fMediaSize             :=NullPaperSize;
  fMargins               :=NullMargin;
  fPageBorder            :=false;
  fColumnBorder          :=cbRegular;
  fBorderDashPattern     :=dpRegular;
  fColumnMargin          :=NullMargin;
  fColumnGap             :=NullLocation;
  fColumnCount           :=0;
  fRowCount              :=0;
  fFontPointSize         :=0;
  fColumnFontPointSize   :=0;
  fLeadingPointSize      :=0;
  fColumnLeadingPointSize:=0;
  fCharCount             :=0;
  fWorkArea              :=NullMargin;
end;

procedure TRLPDFFilterPageSetup.Assign(Source:TRLPDFFilterPageSetup);
begin
  PaperSize             :=Source.PaperSize;
  LandScape             :=Source.LandScape;
  MediaSize             :=Source.MediaSize;
  Margins               :=Source.Margins;
  PageBorder            :=Source.PageBorder;
  ColumnBorder          :=Source.ColumnBorder;
  BorderDashPattern     :=Source.BorderDashPattern;
  ColumnMargin          :=Source.ColumnMargin;
  ColumnGap             :=Source.ColumnGap;
  ColumnCount           :=Source.ColumnCount;
  RowCount              :=Source.RowCount;
  FontPointSize         :=Source.FontPointSize;
  ColumnFontPointSize   :=Source.ColumnFontPointSize;
  LeadingPointSize      :=Source.LeadingPointSize;
  ColumnLeadingPointSize:=Source.ColumnLeadingPointSize;
  CharCount             :=Source.CharCount;
  WorkArea              :=Source.WorkArea;
end;

{ TRLPDFFilterDocumentInfo }

constructor TRLPDFFilterDocumentInfo.Create;
begin
  fTitle   :=emptystr;
  fSubject :=emptystr;
  fAuthor  :=emptystr;
  fKeyWords:=emptystr;
  fCreator :=emptystr;
  fProducer:=emptystr;
  //
  inherited Create;
end;

procedure TRLPDFFilterDocumentInfo.Clear;
begin
  fTitle   :=emptystr;
  fSubject :=emptystr;
  fAuthor  :=emptystr;
  fKeyWords:=emptystr;
  fCreator :=emptystr;
  fProducer:=emptystr;
end;

procedure TRLPDFFilterDocumentInfo.Assign(Source:TRLPDFFilterDocumentInfo);
begin
  Title   :=Source.Title;
  Subject :=Source.Subject;
  Author  :=Source.Author;
  KeyWords:=Source.KeyWords;
  Creator :=Source.Creator;
  Producer:=Source.Producer;
end;

{ TRLPDFFilterTextControl }

constructor TRLPDFFilterTextControl.Create;
begin
  fFormFeed:=true;
  fWordWrap:=false;
  fTabSize :=8;
  //
  inherited Create;
end;

procedure TRLPDFFilterTextControl.Clear;
begin
  fFormFeed:=false;
  fWordWrap:=false;
  fTabSize :=8;
end;

procedure TRLPDFFilterTextControl.Assign(Source:TRLPDFFilterTextControl);
begin
  FormFeed:=Source.FormFeed;
  WordWrap:=Source.WordWrap;
  TabSize :=Source.TabSize;
end;

type
  TRLImageFile=class
    ImageId :integer;
    ImageCRC:cardinal;
  end;

{ TRLPDFFilter }

constructor TRLPDFFilter.Create(aOwner:TComponent);
begin
  fDocumentInfo  :=nil;
  fPageSetup     :=nil;
  fTextControl   :=nil;
  fImageFiles    :=nil;
  //
  fDocumentInfo:=TRLPDFFilterDocumentInfo.Create;
  fPageSetup   :=TRLPDFFilterPageSetup.Create;
  fTextControl :=TRLPDFFilterTextControl.Create;
  fImageFiles  :=TObjectList.Create;
  //
  inherited Create(aOwner);
  //
  DefaultExt :='.pdf';
  DisplayName:=LS_PDFFormatStr;
  //
  FixupPageSetup;
  Reset;
end;

destructor TRLPDFFilter.Destroy;
begin
  if Assigned(fImageFiles) then
    fImageFiles.Free;
  if Assigned(fDocumentInfo) then
    FreeAndNil(fDocumentInfo);
  if Assigned(fPageSetup) then
    FreeAndNil(fPageSetup);
  if Assigned(fTextControl) then
    FreeAndNil(fTextControl);
  //
  inherited;
end;

procedure TRLPDFFilter.InternalBeginDoc;
begin
  fPrintCut.X :=0;
  fPrintCut.Y :=0;
  fPrintSize.X:=Pages.OrientedWidth;
  fPrintSize.Y:=Pages.OrientedHeight;
  fImageFiles.Clear;
  //
  fOutputStream:=TFileStream.Create(FileName,fmCreate);
  //
  BeginDoc;
end;

procedure TRLPDFFilter.InternalEndDoc;
begin
  EndDoc;
  //
  fOutputStream.free;
end;

function TRLPDFFilter.GetTextWidth(const aText:string; aFont:TRLMetaFont):integer;
var
B: Tbitmap;
begin
B:= AuxBitmapNeeded;
  with B.Canvas do
  begin
    Font.Name:=aFont.Name;
    Font.Size:=aFont.Size;
    Result:=TextWidth(aText);
  end;
  FreeAndNil(B);
end;

procedure TRLPDFFilter.InternalDrawPage(aPage:TRLGraphicSurface);
var
  obj:TRLGraphicObject;
  i  :integer;
  procedure DrawText(aObj:TRLTextObject);
  var
    w,l:integer;
    s  :string;
  begin   
    WriteLn('q');
    // font color
    with aObj.Font.Color do
      WriteLn(RGBStr(Red,Green,Blue)+' rg');
    // define retângulo de corte
    if (aObj.TextFlags and MetaTextFlagAutoSize)=0 then
    begin
      with aObj.BoundsRect do
        WriteLn(PDF_BoundsStr(Left,Top,Right,Bottom)+' re');
      WriteLn('W n');
    end;
    // posicao esquerda
    s:=aObj.DisplayText;
    w:=GetTextWidth(s,aObj.Font);
    case aObj.Alignment of
      MetaTextAlignmentRight : l:=aObj.BoundsRect.Right-w;
      MetaTextAlignmentCenter: l:=(aObj.BoundsRect.Left+aObj.BoundsRect.Right-w) div 2;
    else
      l:=aObj.Origin.X;
    end;
    // textout
    WriteText(PDF_PixelsToPoints(l),
              PDF_PixelsToPoints(fPrintSize.Y-aObj.BoundsRect.Bottom),
              aObj.DisplayText,
              GetFontId(aObj.Font.Name,aObj.Font.Style),
              aObj.Font.Size);
    WriteLn('Q');
  end;
  function CreateImageObj(aObj:TRLImageObject):integer;
  var
    grp:TGraphic;
    bmp:TBitmap;
    crc:cardinal;
    imf:TRLImageFile;
    i  :integer;
  begin
    crc:=CRC32(aObj.Data);
    i:=0;
    while (i<fImageFiles.Count) and (TRLImageFile(fImageFiles[i]).ImageCRC<>crc) do
      Inc(i);
    if i<fImageFiles.Count then
      Result:=TRLImageFile(fImageFiles[i]).ImageId
    else
    begin
      bmp:=nil;
      grp:=FromMetaGraphic(aObj.Data);
      try
        if grp is TBitmap then
          bmp:=TBitmap(grp)
        else
        begin
          bmp:=NewBitmap(grp.Width,grp.Height);
          bmp.Canvas.Draw(0,0,grp);
        end;
        // retorna o indice da imagem
        Result:=WriteBitmap(bmp);
        imf:=TRLImageFile.Create;
        imf.ImageId :=Result;
        imf.ImageCRC:=crc;
        fImageFiles.Add(imf);
      finally
        if (bmp<>nil) and (bmp<>grp) then
          bmp.free;                
        grp.free;
      end;
    end;  
  end;
  procedure DrawImage(aObj:TRLImageObject);
  begin
    WriteLn('q');
    // matriz de posicionamento (translation: 1 0 0 1 left top)
    WriteLn('1 0 0 1 '+PDF_PointStr(aObj.BoundsRect.Left,aObj.BoundsRect.Bottom)+' cm');
    // matriz de rotação (rotation: cosT sinT -sinT cosT 0 0)
    // WriteLn('1 0 0 1 0 0 cm');
    // matriz de escalonameto (scale: width 0 0 height 0 0)
    WriteLn(PDF_FloatToStr(PDF_PixelsToPoints(aObj.BoundsRect.Right-aObj.BoundsRect.Left))+' 0 0 '+
            PDF_FloatToStr(PDF_PixelsToPoints(aObj.BoundsRect.Bottom-aObj.BoundsRect.Top))+' 0 0 cm');
    // matriz de inclinação (skew: 1 tanX tanY 1 0 0)
    // WriteLn('1 0 0 1 0 0 cm');
    WriteLn('/Image'+IntToStr(aObj.Tag)+' Do');
    WriteLn('Q');
  end;
  procedure DrawLine(aObj:TRLLineObject);
  var
    w:integer;
  begin
    // line width
    if aObj.Pen.Style=MetaPenStyleClear then
      w:=0
    else
      w:=aObj.Pen.Width;
    WriteLn(PDF_FloatToStr(PDF_PixelsToPoints(w))+' w');
    // shape of ending points (0 = none)
    WriteLn('0 J');
    // dashing
    case aObj.Pen.Style of
      MetaPenStyleSolid      : WriteLn('[] 0 d');
      MetaPenStyleDash       : WriteLn('[3] 0 d');
      MetaPenStyleDot        : WriteLn('[1] 0 d');
      MetaPenStyleDashDot    : WriteLn('[2 1] 0 d');
      MetaPenStyleDashDotDot : WriteLn('[3 5] 6 d');
      MetaPenStyleClear      :;
      MetaPenStyleInsideFrame: WriteLn('[] 0 d');
    end;
    // color
    with aObj.Pen.Color do
      WriteLn(RGBStr(Red,Green,Blue)+' RG');
    // moveto
    with aObj.FromPoint do
      WriteLn(PDF_PointStr(X,Y)+' m');
    // lineto
    with aObj.ToPoint do
      WriteLn(PDF_PointStr(X,Y)+' l');
    // makeit
    WriteLn('S');
  end;
  procedure DrawRectangle(aObj:TRLRectangleObject);
  var
    w:integer;
  begin
    // line width
    if aObj.Pen.Style=MetaPenStyleClear then
      w:=0
    else
      w:=aObj.Pen.Width;
    WriteLn(PDF_FloatToStr(PDF_PixelsToPoints(w))+' w');
    // shape of ending points (0 = none)
    WriteLn('0 J');
    // dashing
    case aObj.Pen.Style of
      MetaPenStyleSolid      : WriteLn('[] 0 d');
      MetaPenStyleDash       : WriteLn('[3] 0 d');
      MetaPenStyleDot        : WriteLn('[1] 0 d');
      MetaPenStyleDashDot    : WriteLn('[2 1] 0 d');
      MetaPenStyleDashDotDot : WriteLn('[3 5] 6 d');
      MetaPenStyleClear      :;
      MetaPenStyleInsideFrame: WriteLn('[] 0 d');
    end;
    // pen color
    with aObj.Pen.Color do
      WriteLn(RGBStr(Red,Green,Blue)+' RG');
    // brush color
    with aObj.Brush.Color do
      WriteLn(RGBStr(Red,Green,Blue)+' rg');
    // rectangle
    with aObj.BoundsRect do
      WriteLn(PDF_BoundsStr(Left,Top,Right,Bottom)+' re');
    // makeit
    WriteLn('f');
  end;
  procedure DrawSetClipRect(aObj:TRLSetClipRectObject);
  begin
    WriteLn('q');
    // rectangle
    with aObj.BoundsRect do
      WriteLn(PDF_BoundsStr(Left,Top,Right,Bottom)+' re');
    WriteLn('W n');
  end;
  procedure DrawResetClipRect(aObj:TRLResetClipRectObject);
  begin
    WriteLn('Q');
  end;
  procedure DrawFillRect(aObj:TRLFillRectObject);
  begin
    // line width
    WriteLn(PDF_FloatToStr(PDF_PixelsToPoints(0))+' w');
    // brush color
    with aObj.Brush.Color do
      WriteLn(RGBStr(Red,Green,Blue)+' rg');
    // rectangle
    with aObj.BoundsRect do
      WriteLn(PDF_BoundsStr(Left,Top,Right,Bottom)+' re'); 
    // makeit
    WriteLn('f');
  end;
  procedure DrawEllipse(aObj:TRLEllipseObject);
  var
    cx,cy,rx,ry,d,s,c:double;
    i:integer;
    w:integer;
  begin
    // line width
    if aObj.Pen.Style=MetaPenStyleClear then
      w:=0
    else
      w:=aObj.Pen.Width;
    WriteLn(PDF_FloatToStr(PDF_PixelsToPoints(w))+' w');
    // shape of ending points (0 = none)
    WriteLn('0 J');
    // dashing
    case aObj.Pen.Style of
      MetaPenStyleSolid      : WriteLn('[] 0 d');
      MetaPenStyleDash       : WriteLn('[3] 0 d');
      MetaPenStyleDot        : WriteLn('[1] 0 d');
      MetaPenStyleDashDot    : WriteLn('[2 1] 0 d');
      MetaPenStyleDashDotDot : WriteLn('[3 5] 6 d');
      MetaPenStyleClear      :;
      MetaPenStyleInsideFrame: WriteLn('[] 0 d');
    end;
    // pen color
    with aObj.Pen.Color do
      WriteLn(RGBStr(Red,Green,Blue)+' RG');
    // brush color
    with aObj.Brush.Color do
      WriteLn(RGBStr(Red,Green,Blue)+' rg');
    // the curves
    with aObj.BoundsRect do
    begin
      cx:=(Left+Right)/2;
      cy:=(Top+Bottom)/2;
      rx:=(Right-Left)/2;
      ry:=(Bottom-Top)/2;
    end;  
    i:=0;
    while i<36 do
    begin
      d:=2*pi*i/36;
      s:=Sin(d);
      c:=Cos(d);
      Write(PDF_PointStr(cx+c*rx,cy+s*ry));
      if i=0 then
        Write(' m ')
      else
        Write(' l ');
      Inc(i,1);
    end;  
    WriteLn('b');
  end;
  procedure DrawPolygon(aObj:TRLPolygonObject);
  var
    w,i:integer;
  begin
    // line width
    if aObj.Pen.Style=MetaPenStyleClear then
      w:=0
    else
      w:=aObj.Pen.Width;
    WriteLn(PDF_FloatToStr(PDF_PixelsToPoints(w))+' w');
    // shape of ending points (0 = none)
    WriteLn('0 J');
    // dashing
    case aObj.Pen.Style of
      MetaPenStyleSolid      : WriteLn('[] 0 d');
      MetaPenStyleDash       : WriteLn('[3] 0 d');
      MetaPenStyleDot        : WriteLn('[1] 0 d');
      MetaPenStyleDashDot    : WriteLn('[2 1] 0 d');
      MetaPenStyleDashDotDot : WriteLn('[3 5] 6 d');
      MetaPenStyleClear      :;
      MetaPenStyleInsideFrame: WriteLn('[] 0 d');
    end;
    // pen color
    with aObj.Pen.Color do
      WriteLn(RGBStr(Red,Green,Blue)+' RG');
    // brush color
    with aObj.Brush.Color do
      WriteLn(RGBStr(Red,Green,Blue)+' rg');
    // 
    for i:=0 to High(aObj.Points) do
    begin
      Write(PDF_PointStr(aObj.Points[i].X,aObj.Points[i].Y));
      if i=0 then
        WriteLn(' m ')
      else
        WriteLn(' l ');
    end;
    // makeit
    WriteLn('b');
  end;
  procedure DrawPolyline(aObj:TRLPolylineObject);
  var
    w,i:integer;
  begin
    // line width
    if aObj.Pen.Style=MetaPenStyleClear then
      w:=0
    else
      w:=aObj.Pen.Width;
    WriteLn(PDF_FloatToStr(PDF_PixelsToPoints(w))+' w');
    // shape of ending points (0 = none)
    WriteLn('0 J');
    // dashing
    case aObj.Pen.Style of
      MetaPenStyleSolid      : WriteLn('[] 0 d');
      MetaPenStyleDash       : WriteLn('[3] 0 d');
      MetaPenStyleDot        : WriteLn('[1] 0 d');
      MetaPenStyleDashDot    : WriteLn('[2 1] 0 d');
      MetaPenStyleDashDotDot : WriteLn('[3 5] 6 d');
      MetaPenStyleClear      :;
      MetaPenStyleInsideFrame: WriteLn('[] 0 d');
    end;
    // pen color
    with aObj.Pen.Color do
      WriteLn(RGBStr(Red,Green,Blue)+' RG');
    //
    for i:=0 to High(aObj.Points) do
    begin
      Write(PDF_PointStr(aObj.Points[i].X,aObj.Points[i].Y));
      if i=0 then
        WriteLn(' m ')
      else
        WriteLn(' l ');
    end;
    // makeit
    WriteLn('S');
  end;
  procedure DrawPixel(aObj:TRLPixelObject);
  begin
    // line width
    WriteLn(PDF_FloatToStr(PDF_PixelsToPoints(0))+' w');
    // shape of ending points (0 = none)
    WriteLn('0 J');
    // brush color
    with aObj.Color do
      WriteLn(RGBStr(Red,Green,Blue)+' rg');
    // rectangle
    with aObj.BoundsRect do
      WriteLn(PDF_BoundsStr(Left,Top,Right,Bottom)+' re');
    // makeit
    WriteLn('f');
  end;
begin
  // cria imagens
  for i:=0 to aPage.ObjectCount-1 do
  begin
    obj:=aPage.Objects[i];
    if obj is TRLImageObject then
      obj.Tag:=CreateImageObj(TRLImageObject(obj));
  end;    
  //
  BeginPage(aPage);
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
  EndPage;
end;

procedure TRLPDFFilter.InternalNewPage;
begin
end;

procedure TRLPDFFilter.SetDocumentInfo(const Value:TRLPDFFilterDocumentInfo);
begin
  if Value=nil then
    fDocumentInfo.Clear
  else
    fDocumentInfo.Assign(Value);
end;

procedure TRLPDFFilter.SetPageSetup(const Value:TRLPDFFilterPageSetup);
begin
  if Value=nil then
    fPageSetup.Clear
  else
    fPageSetup.Assign(Value);
end;

procedure TRLPDFFilter.SetTextControl(const Value:TRLPDFFilterTextControl);
begin
  if Value=nil then
    fTextControl.Clear
  else
    fTextControl.Assign(Value);
end;

procedure TRLPDFFilter.Write(const aStr:string='');
var
  l:integer;
begin
  l:=Length(aStr);
  if l>0 then
  begin
    fOutputStream.Write(aStr[1],l);
    Inc(fWritePos,l);
  end;
end;

procedure TRLPDFFilter.WriteLn(const aStr:string='');
begin
  Write(aStr);
  Write(PDF_EOL);
end;

function TRLPDFFilter.PDF_PointStr(X,Y:double):string;
begin
  Result:=PDF_FloatToStr(PDF_PixelsToPoints(X))+' '+
          PDF_FloatToStr(PDF_PixelsToPoints(fPrintSize.Y-Y));
end;

function TRLPDFFilter.PDF_BoundsStr(aLeft,aTop,aRight,aBottom:double):string;
begin
  Result:=PDF_FloatToStr(PDF_PixelsToPoints(aLeft))+' '+
          PDF_FloatToStr(PDF_PixelsToPoints(fPrintSize.Y-aBottom))+' '+
          PDF_FloatToStr(PDF_PixelsToPoints(aRight-aLeft))+' '+
          PDF_FloatToStr(PDF_PixelsToPoints(aBottom-aTop)); 
end;

function TRLPDFFilter.RGBStr(aRed,aGreen,aBlue:integer):string;
begin
  Result:=PDF_FloatToStr(aRed/255)+' '+
          PDF_FloatToStr(aGreen/255)+' '+
          PDF_FloatToStr(aBlue/255);
end;

procedure TRLPDFFilter.BeginDoc;
begin
  fWritePos      :=0;
  fCurrentPageNo :=0;
  fObjectCount   :=0;
  fOutlineCount  :=0;
  fPageTreeObjAt :=0;
  fOutlinesObjAt :=0;
  fCatalogObjAt  :=0;
  fInfoObjAt     :=0;
  fPageMode      :=pmRegular;
  fFullPageMode  :=pmRegular;
  fPageLayout    :=plRegular;
  fViewerOptions :=DefaultViewerOptions;
  fForceASCII    :=false;
  fFontEncoding  :=feWinAnsiEncoding;
  fFontName      :=PS1FONTNAMES[5];
  fFontCount     :=0;
  fImageCount    :=0;
  fPageCount     :=0;
  fResourcesObjAt:=0;
  fPageOffset    :=0;
  fPageTitle     :=emptystr;
  fXRefOfs       :=0;
  fContentsObjAt :=0;
  fLengthObjAt   :=0;
  //
  WriteBOF;
  WriteInfo;
  WriteCatalog;
  //
  fResourcesObjAt:=AddObj;
end;

procedure TRLPDFFilter.EndDoc;
begin
  WriteFonts;
  WriteResources;
  if fOutlinesObjAt>0 then
    WriteOutlines;
  WritePageTree;
  WriteXRef;
  WriteTrailer;
  WriteEOF;
end;

function TRLPDFFilter.GetFontId(const aFontName:string; aFontStyle:TRLMetaFontStyles):integer;
var
  i:integer;
  n:string;
  s:TRLMetaFontStyles;
begin
  n:=aFontName;
  s:=aFontStyle;
  i:=0;
  while (i<fFontCount) and not (AnsiSameText(fFonts[i].FontName,n) and (fFonts[i].FontStyle=s)) do
    Inc(i);
  if i<fFontCount then
    Result:=i+1
  else
  begin
    Inc(fFontCount);
    if fFontCount>Length(fFonts) then
      SetLength(fFonts,Length(fFonts)*2+FontsInc);
    with fFonts[fFontCount-1] do
    begin
      FontName :=n;
      FontStyle:=s;
      FontObjAt:=AddObj;
    end;
    Result:=fFontCount;
  end;
end;

procedure TRLPDFFilter.WriteBOF;
begin
  WriteLn('%PDF-'+PDF_FILEVERSION);
end;

procedure TRLPDFFilter.WriteInfo;
begin
  fInfoObjAt:=BeginObj;
  //
  WriteLn('/CreationDate('+PDF_CurrentDateTimeStr+')');
  with fDocumentInfo do
  begin
    if Title<>emptystr then
      WriteLn('/Title('+Title+')');
    if Subject<>emptystr then
      WriteLn('/Subject('+Subject+')');
    if Author<>emptystr then
      WriteLn('/Author('+Author+')');
    if Keywords<>emptystr then
      WriteLn('/Keywords('+Keywords+')');
    if Creator<>emptystr then
      WriteLn('/Creator('+Creator+')');
    if Producer<>emptystr then
      WriteLn('/Producer('+Producer+')');
    if ModDate <> 0 then
      WriteLn('/ModDate('+DateTimetostr(ModDate)+')');
  end;
  //
  EndObj;
end;

procedure TRLPDFFilter.WriteCatalog;
begin
  fCatalogObjAt:=BeginObj;
  //
  WriteLn('/Type/Catalog');
  fPageTreeObjAt:=AddObj;
  WriteLn('/Pages '+PDF_IndirectObjStr(fPageTreeObjAt));
  if fPageMode=pmOutlines then
  begin
    fOutlinesObjAt:=AddObj;
    WriteLn('/Outlines '+PDF_IndirectObjStr(fOutlinesObjAt));
  end;
  // setpagemode
  if fPageMode<>pmRegular then
  begin
    Write('/PageMode');
    case fPageMode of
      pmOutlines  : WriteLn('/UseOutlines');
      pmThumbs    : WriteLn('/UseThumbs');
      pmFullScreen: WriteLn('/FullScreen');
    end;
  end;
  // viewer
  if (fViewerOptions<>[voRegular]) or (fPageLayout<>plRegular) then
  begin
    Write('/ViewerPreferences [');
    if voHideToolBar in fViewerOptions then
      Write('/HideToolBar');
    if voHideMenuBar in fViewerOptions then
      Write('/HideMenuBar');
    if voHideWindowUI in fViewerOptions then
      Write('/HideWindowUI');
    if voFitWindow in fViewerOptions then
      Write('/FitWindow');
    if voCenterWindow in fViewerOptions then
      Write('/CenterWindow');
    if fPageLayout<>plRegular then
    begin
      Write('/PageLayout');
      case fPageLayout of
        plSinglePage    : Write('/SinglePage');
        plOneColumn     : Write('/OneColumn');
        plTwoColumnLeft : Write('/TwoColumnLeft');
        plTwoColumnRight: Write('/TwoColumnRight');
      end;
    end;
    if (fPageMode=pmFullScreen) and (fFullPageMode<>pmRegular) then
    begin
      Write('/PageMode');
      case fFullPageMode of
        pmOutlines  : Write('/UseOutlines');
        pmThumbs    : Write('/UseThumbs');
        pmFullScreen: Write('/FullScreen');
      end;
    end;
    WriteLn(']');
  end;
  //
  EndObj;
end;

procedure TRLPDFFilter.WriteFonts;
var
  i,j,w:integer;
  n  :string;
  s  :TRLMetaFontStyles;
  dsc:integer;
  ttf:boolean;
  rec:TRLMetaFontMetrics;
begin
  dsc:=AddObj;
  for i:=0 to fFontCount-1 do
  begin
    with fFonts[i] do
    begin
      n:=StringReplace(FontName,' ','',[rfReplaceAll]);
      s:=FontStyle;
      BeginObj(FontObjAt);
    end;
    j:=1;
    while (j<=PS1FONTCOUNT) and not AnsiSameText(PS1FONTNAMES[j],n) do
      Inc(j);
    Write('/Type/Font/Subtype');
    ttf:=(j>PS1FONTCOUNT);
    if ttf then
    begin
      FontGetMetrics(fFonts[i].FontName,FromMetaFontStyles(s),rec);
      if (s and MetaFontStyleBold)=MetaFontStyleBold then
        if (s and MetaFontStyleItalic)=MetaFontStyleItalic then
          n:=n+',BoldItalic'
        else
          n:=n+',Bold'
      else if (s and MetaFontStyleItalic)=MetaFontStyleItalic then
        n:=n+',Italic';
      if (s and MetaFontStyleUnderline)=MetaFontStyleUnderline then
        n:=n+',Underline';
      if (s and MetaFontStyleStrikeOut)=MetaFontStyleStrikeOut then
        n:=n+',StrikeOut';
      WriteLn('/TrueType/Name/F'+IntToStr(i+1)+'/BaseFont/'+n);
      WriteLn('/FirstChar '+IntToStr(rec.FirstChar));
      WriteLn('/LastChar '+IntToStr(rec.LastChar));
      WriteLn('/Widths [');
      w:=0;
      for j:=rec.FirstChar to rec.LastChar do
      begin
        if w>17 then
        begin
          WriteLn;
          w:=0;
        end;
        Inc(w);
        Write(IntToStr(rec.Widths[j])+' ');
      end;
      WriteLn;
      WriteLn(']');
      WriteLn('/FontDescriptor '+PDF_IndirectObjStr(dsc));
    end
    else
    begin
      Write('/Type1/Name/F'+IntToStr(i+1)+'/BaseFont/'+n);
      if fForceASCII then
        Write('/FirstChar 0/LastChar 255');
    end;    
    case fFontEncoding of
      feNoEncoding      :;
      feMacRomanEncoding: Write('/Encoding/MacRomanEncoding');
      feWinAnsiEncoding : Write('/Encoding/WinAnsiEncoding');
    end;
    WriteLn;
    EndObj;
    //
    if ttf then
    begin
      BeginObj(dsc);
      WriteLn('/Type/FontDescriptor');
      WriteLn('/FontName/'+n);
      WriteLn('/Flags '+IntToStr(rec.FontDescriptor.Flags));
      with rec.FontDescriptor.FontBBox do
        WriteLn('/FontBBox [ '+IntToStr(Left)+' '+IntToStr(Bottom)+' '+IntToStr(Right)+' '+IntToStr(Top)+' ]');
      WriteLn('/MissingWidth 0');
      WriteLn('/StemV 73');
      WriteLn('/StemH 73');
      WriteLn('/ItalicAngle '+IntToStr(rec.FontDescriptor.ItalicAngle));
      WriteLn('/CapHeight '+IntToStr(rec.FontDescriptor.CapHeight));
      WriteLn('/XHeight '+IntToStr(rec.FontDescriptor.XHeight));
      WriteLn('/Ascent '+IntToStr(rec.FontDescriptor.Ascent));
      WriteLn('/Descent '+IntToStr(rec.FontDescriptor.Descent));
      WriteLn('/Leading '+IntToStr(rec.FontDescriptor.Leading));
      WriteLn('/MaxWidth '+IntToStr(rec.FontDescriptor.MaxWidth));
      WriteLn('/AvgWidth '+IntToStr(rec.FontDescriptor.AvgWidth));
      EndObj;
    end;
  end;
end;

procedure TRLPDFFilter.WriteResources;
var
  i:integer;
begin
  BeginObj(fResourcesObjAt);
  WriteLn('/ProcSet [ /PDF/Text/ImageB ]');
  WriteLn('/Font <<');
  for i:=0 to fFontCount-1 do
    WriteLn('/F'+IntToStr(i+1)+' '+PDF_IndirectObjStr(fFonts[i].FontObjAt));
  WriteLn('>>');
  WriteLn('/XObject <<');
  for i:=0 to fImageCount-1 do
    WriteLn('/Image'+IntToStr(i+1)+' '+PDF_IndirectObjStr(fImages[i].ImageObjAt));
  WriteLn('>>');
  EndObj;
end;

procedure TRLPDFFilter.WriteOutlines;
var
  i,this,prev,next:integer;
begin
  BeginObj(fOutlinesObjAt);
  WriteLn('/Count '+IntToStr(fOutlineCount));
  if fOutlineCount>0 then
  begin
    WriteLn('/First '+PDF_IndirectObjStr(fObjectCount+1));
    WriteLn('/Last ' +PDF_IndirectObjStr(fObjectCount+fOutlineCount));
  end;
  EndObj;
  //
  prev:=0;
  next:=0;
  for i:=1 to fOutlineCount do
    with fOutlines[i] do
    begin
      this:=BeginObj(next);
      if (ObjId=0) and ((Page>0) and (Page<=fPageCount)) then
        ObjId:=fPages[Page-1];
      WriteLn('/Title ('+Title+')');
      WriteLn('/Dest ['+PDF_IndirectObjStr(ObjId)+ {IntToStr(i-1)+} ' /XYZ null null null]');
      WriteLn('/Parent '+PDF_IndirectObjStr(fOutlinesObjAt));
      if prev<>0 then
        WriteLn('/Previous '+PDF_IndirectObjStr(prev));
      prev:=this;  
      if i<fOutlineCount then
      begin
        next:=AddObj;
        WriteLn('/Next '+PDF_IndirectObjStr(next));
      end;
      EndObj;
    end;
end;

procedure TRLPDFFilter.WritePageTree;
var
  i:integer;
begin
  BeginObj(fPageTreeObjAt);
  WriteLn('/Type/Pages');
  WriteLn('/Count '+IntToStr(fPageCount));
  WriteLn('/MediaBox [ 0 0 '+PDF_FloatToStr(PDF_PixelsToPoints(fPrintSize.X))+' '+PDF_FloatToStr(PDF_PixelsToPoints(fPrintSize.Y))+' ]');
  Write('/Kids [ ');
  for i:=0 to fPageCount-1 do
    Write(PDF_IndirectObjStr(fPages[i])+' ');
  WriteLn(']');
  EndObj;
end;

procedure TRLPDFFilter.WriteXRef;
var
  i:integer;
begin
  fXRefOfs:=fWritePos;
  WriteLn('xref');
  WriteLn('0 '+IntToStr(fObjectCount+1)); // mais 1 por causa da linha abaixo:
  WriteLn('0000000000 65535 f');
  for i:=0 to fObjectCount-1 do
    WriteLn(PDF_Zeros(fObjects[i],10)+' 00000 n');
end;

procedure TRLPDFFilter.WriteTrailer;
begin
  WriteLn('trailer');
  WriteLn('<<');
  WriteLn('/Size '+IntToStr(fObjectCount+1));
  WriteLn('/Root '+PDF_IndirectObjStr(fCatalogObjAt));
  WriteLn('/Info '+PDF_IndirectObjStr(fInfoObjAt));
  WriteLn('>>');
end;

procedure TRLPDFFilter.WriteEOF;
begin
  WriteLn('startxref');
  WriteLn(IntToStr(fXRefOfs));
  WriteLn('%%EOF');
end;

function TRLPDFFilter.AddObj:integer;
begin
  Inc(fObjectCount);
  if fObjectCount>Length(fObjects) then
    SetLength(fObjects,Length(fObjects)*2+ObjectsInc);
  Result:=fObjectCount;
end;

function TRLPDFFilter.BeginObj(aIndex:integer=0):integer;
begin
  if aIndex=0 then
    Result:=AddObj
  else
    Result:=aIndex;
  fObjects[Result-1]:=fWritePos;
  WriteLn(IntToStr(Result)+' 0 obj <<');
end;

procedure TRLPDFFilter.EndObj;
begin
  WriteLn('>> endobj');
end;

function TRLPDFFilter.BeginShortObj(aIndex:integer=0):integer;
begin
  if aIndex=0 then
    Result:=AddObj
  else
    Result:=aIndex;
  fObjects[Result-1]:=fWritePos;
  WriteLn(IntToStr(Result)+' 0 obj');
end;

procedure TRLPDFFilter.EndShortObj;
begin
  WriteLn('endobj');
end;

procedure TRLPDFFilter.BeginPage(aPage:TRLGraphicSurface);
begin
  Inc(fPageCount);
  if fPageCount>Length(fPages) then
    SetLength(fPages,Length(fPages)*2+PagesInc);
  fPages[fPageCount-1]:=BeginObj;
  WriteLn('/Type/Page');
  WriteLn('/Parent '+PDF_IndirectObjStr(fPageTreeObjAt));
  WriteLn('/Resources '+PDF_IndirectObjStr(fResourcesObjAt));
  WriteLn('/MediaBox [ 0 0 '+PDF_FloatToStr(PDF_PixelsToPoints(aPage.Width))+' '+PDF_FloatToStr(PDF_PixelsToPoints(aPage.Height))+' ]');
  fContentsObjAt:=AddObj;
  WriteLn('/Contents '+PDF_IndirectObjStr(fContentsObjAt));
  EndObj;
  //
  fLengthObjAt:=AddObj;
  BeginObj(fContentsObjAt);
  WriteLn('/Length '+PDF_IndirectObjStr(fLengthObjAt));
  fPageOffset:=BeginStream;
end;

procedure TRLPDFFilter.EndPage;
var
  stmsize:integer;
begin
  stmsize:=EndStream-fPageOffset;
  //
  BeginShortObj(fLengthObjAt);
  WriteLn(IntToStr(stmsize));
  EndShortObj;
end;

function TRLPDFFilter.BeginStream:integer;
begin
  WriteLn('>>');
  WriteLn('stream');
  // marca o offset de início deste stream
  Result:=fWritePos;
end;

function TRLPDFFilter.EndStream:integer;
begin
  // calcula o tamanho da stream em bytes
  Result:=fWritePos;
  WriteLn('endstream');
  WriteLn('endobj');
end;

procedure TRLPDFFilter.WriteText(aLeft,aTop:double; const aText:string; aFontId,aFontSize:integer);
begin
  // begin text
  WriteLn('BT');
  // font number and size
  WriteLn('/F'+IntToStr(aFontId)+' '+IntToStr(aFontSize)+' Tf');
  // position
  WriteLn(PDF_FloatToStr(aLeft)+' '+PDF_FloatToStr(aTop)+' Td');
  // sub/superscript
  //  +-9 Ts
  // the text
  WriteLn('('+PDF_EncodeText(aText)+') Tj');
  // end text
  WriteLn('ET');
end;

procedure TRLPDFFilter.WriteBitmapData(aBitmap:TBitmap);
var
  x,y:integer;
  wid:integer;
  rgb:TRLMetaColor;
  hex:string;
begin
  wid:=0;
  for y:=0 to aBitmap.Height-1 do
    for x:=0 to aBitmap.Width-1 do
    begin
      rgb:=ToMetaColor(CanvasGetPixels(aBitmap.Canvas,x,y));
      hex:=ByteToHex(rgb.Red)+ByteToHex(rgb.Green)+ByteToHex(rgb.Blue);
      Write(hex);
      Inc(wid,Length(hex));
      if wid>=80 then
      begin
        WriteLn;
        wid:=0;
      end;
    end;
end;

// retorna o id do objeto criado
function TRLPDFFilter.WriteBitmap(aBitmap:TBitmap):integer;
var
  begstm:integer;
  endstm:integer;
  lenat :integer;
begin
  lenat:=AddObj;
  Inc(fImageCount);
  if fImageCount>Length(fImages) then
    SetLength(fImages,Length(fImages)*2+ImagesInc);
  fImages[fImageCount-1].ImageObjAt:=BeginObj;
  Result:=fImageCount;
  WriteLn('/Type/XObject');
  WriteLn('/Subtype/Image');
  WriteLn('/Width '+IntToStr(aBitmap.Width));
  WriteLn('/Height '+IntToStr(aBitmap.Height));
  WriteLn('/ColorSpace/DeviceRGB');
  WriteLn('/BitsPerComponent 8');
  WriteLn('/Length '+PDF_IndirectObjStr(lenat));
  WriteLn('/Filter [/ASCIIHexDecode]');
  begstm:=BeginStream;
  WriteBitmapData(aBitmap);
  WriteLn('>');
  endstm:=EndStream;
  BeginShortObj(lenat);
  WriteLn(IntToStr(endstm-begstm));
  EndShortObj;
end;

procedure TRLPDFFilter.FixupPageSetup;
var
  column:TRLPDFFilterColumn;
  xp,yp,xw,yw,c,r:word;
begin
  with fPageSetup do
  begin
    // <page> calculate "tl" (text leading) in point size from "tf" (text font)
    if FontPointSize<=0 then
      FontPointSize:=10;
    LeadingPointSize:=PDF_CalcTextLeadingPointSize(FontPointSize);
    // <column> calculate "tl" (text leading) in point size from "tf" (text font)
    if ColumnFontPointSize<=0 then
      ColumnFontPointSize:=10;
    ColumnLeadingPointSize:=PDF_CalcTextLeadingPointSize(ColumnFontPointSize);
    // calculate actual height and width for "landscape"
    if LandScape then
    begin
      fMediaSize.Height:=PaperSize.Width;
      fMediaSize.Width :=PaperSize.Height;
    end
    else
    begin
      fMediaSize.Height:=PaperSize.Height;
      fMediaSize.Width :=PaperSize.Width;
    end;
    // calculate "page workarea" - absolute address location in point size
    with WorkArea do
    begin
      Top   :=Margins.Bottom;
      if Length(fPageTitle)>=1 then
        Inc(Top,Round(LeadingPointSize*1.5));
      Bottom:=MediaSize.Height-Margins.Top;
      Left  :=Margins.Left;
      Right :=MediaSize.Width-Margins.Right;
      Height:=Bottom-Top+1; // runtime calculation here
      Width :=Right-Left+1; // runtime calculation here
    end;
    // calculate "characters per line"
    CharCount:=Round(WorkArea.Width*11.5) div PDF_POINTSPERINCH;
    // calculate "column workarea" - absolute address location in point size
    if ColumnCount<=0 then
      ColumnCount:=1;
    if ColumnCount>PDF_MAXCOLUMNS then
      ColumnCount:=PDF_MAXCOLUMNS;
    if RowCount<=0 then
      RowCount:=1;
    if RowCount>PDF_MAXROWS then
      RowCount:=PDF_MAXROWS;
    if (ColumnCount=1) and (RowCount=1) then
    begin
      fColumns[1,1].WorkArea :=WorkArea;
      fColumns[1,1].CharCount:=CharCount;
    end
    else
    begin
      if ColumnCount>1 then
        xw:=(WorkArea.Width div ColumnCount)-(ColumnMargin.Left+ColumnMargin.Right+ColumnGap.x)
      else
        xw:=WorkArea.Width;
      if RowCount>1 then
        yw:=(WorkArea.Height div RowCount)-(ColumnMargin.Top +ColumnMargin.Bottom+ColumnGap.y)
      else
        yw:=WorkArea.Height;
      xp:=0;
      yp:=0;
      c :=1;
      r :=1;
      while (c<=ColumnCount) and (r<=RowCount) do
      begin
        if c=1 then
        begin
          xp:=WorkArea.Left;
          if ColumnCount>1 then
            Inc(xp,ColumnMargin.Left);
        end;
        if r=1 then
        begin
          yp:=WorkArea.Top;
          if RowCount>1 then
            Inc(yp,ColumnMargin.Bottom); 
        end;
        fColumns[c,r].WorkArea.Top   :=yp;
        fColumns[c,r].WorkArea.Bottom:=yp+yw;
        fColumns[c,r].WorkArea.Left  :=xp;
        fColumns[c,r].WorkArea.Right :=xp+xw;
        fColumns[c,r].WorkArea.Width :=fColumns[c,r].WorkArea.Right-fColumns[c,r].WorkArea.Left+1;
        fColumns[c,r].WorkArea.Height:=fColumns[c,r].WorkArea.Bottom-fColumns[c,r].WorkArea.Top+1;
        fColumns[c,r].CharCount      :=Round(fColumns[c,r].WorkArea.Width*11.5) div PDF_POINTSPERINCH;
        //
        Inc(c);
        if c>ColumnCount then
        begin
          Inc(r);
          if not(r>RowCount) then
          begin
            c:=1;
            Inc(yp,yw+ColumnMargin.Bottom+ColumnGap.y+ColumnMargin.Top);
          end;
        end
        else
          Inc(xp,xw+ColumnMargin.Right+ColumnGap.x+ColumnMargin.Left);
      end;
      // pdf always drawing from bottom-up
      yp:=RowCount div 2;
      r :=0;
      while (r<yp) do
      begin
        Inc(r);
        c:=0;
        while c<ColumnCount do
        begin
          Inc(c);
          column:=fColumns[c,r];
          fColumns[c,r]:=fColumns[c,RowCount-r+1];
          fColumns[c,RowCount-r+1]:=column;
        end;
      end;
    end;
  end;
end;

procedure TRLPDFFilter.Reset;
begin
  fPageCount   :=0;
  fObjectCount :=0;
  fWritePos    :=0;
  fFontColor   :=clBlack;
  fOldFontColor:=0;
  //
  with DocumentInfo do
  begin
    Title   :=DefaultDocumentInfo.Title;
    Subject :=DefaultDocumentInfo.Subject;
    Author  :=DefaultDocumentInfo.Author;
    Keywords:=DefaultDocumentInfo.Keywords;
    Creator :=DefaultDocumentInfo.Creator;
    Producer:=DefaultDocumentInfo.Producer;
//    Password:=DefaultDocumentInfo.Password;
    ModDate :=DefaultDocumentInfo.ModDate
  end;
  //
  with TextControl do
  begin
    FormFeed:=DefaultTextControl.FormFeed;
    WordWrap:=DefaultTextControl.WordWrap;
    TabSize :=DefaultTextControl.TabSize;
  end;
end;

class function TRLPDFFilter.PDF_PixelsToPoints(aPixels:double):double;
var
  inches:double;
begin
  inches:=aPixels/ScreenPPI;
  Result:=inches*PDF_POINTSPERINCH;
end;

class function TRLPDFFilter.PDF_Zeros(aValue,aWidth:integer):string;
begin
  Result:=IntToStr(aValue);
  while Length(Result)<aWidth do
    Result:='0'+Result;
end;

class function TRLPDFFilter.PDF_FloatToStr(aFloat:double):string;
begin
  Str(aFloat:0:4,Result);
  while (Result<>emptystr) and (Result[Length(Result)]='0') do
    Delete(Result,Length(Result),1);
  if (Result<>emptystr) and (Result[Length(Result)]='.') then
    Delete(Result,Length(Result),1);
end;

class function TRLPDFFilter.PDF_CurrentDateTimeStr:string;
var
  y,m,d,h,n,s,l:word;
  dt:TDateTime;
begin
  dt:=Now;
  DecodeDate(dt,y,m,d);
  DecodeTime(dt,h,n,s,l);
  Result:='D:'+PDF_Zeros(y,4)+PDF_Zeros(m,2)+PDF_Zeros(d,2)+
               PDF_Zeros(h,2)+PDF_Zeros(n,2)+PDF_Zeros(s,2);
end;

// string de referência ao objeto aIndex
class function TRLPDFFilter.PDF_IndirectObjStr(aIndex:integer):string;
begin
  Result:=IntToStr(aIndex)+' 0 R';
end;

class function TRLPDFFilter.PDF_GetDashPattern(aDashPattern:TRLPDFFilterDashPattern):string;
begin
  case aDashPattern of
    dpSolid: Result:='[] 0 d';
    dp33   : Result:='[3] 0 d';
    dp1222 : Result:='[2] 0 d';
    dp2121 : Result:='[2 1] 0 d';
    dp23535: Result:='[3 5] 6 d';
    dp13232: Result:='[2 3] 11 d';
  else
    // dpRegular
    Result:=emptystr;
  end;
end;

class function TRLPDFFilter.PDF_CalcTextLeadingPointSize(aFontSize:word):word;
begin
  if aFontSize<=0 then
    aFontSize:=10;
  Result:=Round((aFontSize*14.5)/12);
end;

class function TRLPDFFilter.PDF_EncodeText(const aText:string):string;
var
  i:integer;
begin
  Result:=aText;
  for i:=Length(Result) downto 1 do
    if Result[i] in ['(',')','\'] then
    begin
      case Result[i] of
        #08: Result[i]:='b';
        #10: Result[i]:='n';
        #12: Result[i]:='f';
        #13: Result[i]:='r';
      end;
      Insert('\',Result,i);
    end
    else if not (Result[i] in [#32..#126,#128..#255]) then
    begin
      Insert(IntToOctal(Ord(Result[i]),3),Result,i+1);
      Result[i]:='\';
    end;
end;

initialization
  //
  DefaultTextControl:=TRLPDFFilterTextControl.Create;
  with DefaultTextControl do
  begin
    FormFeed:=true;
    WordWrap:=false;
    TabSize :=8;
  end;
  //
  DefaultDocumentInfo:=TRLPDFFilterDocumentInfo.Create;
  with DefaultDocumentInfo do
  begin
    Title   :=emptystr;
    Subject :=emptystr;
    Author  :=emptystr;
    KeyWords:=emptystr;
    Creator :=CS_ProductTitleStr+' v'+IntToStr(CommercialVersion)+'.'+IntToStr(ReleaseVersion)+CommentVersion+' \251 '+CS_CopyrightStr;
    Producer:=emptystr;
  end;

finalization
  DefaultTextControl.free;
  DefaultDocumentInfo.free;

end.
