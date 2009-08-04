{$I RLReport.inc}

{@unit RLXLSFilter - Implementação do filtro para criação de planilhas do Excel.
Portado para o Lazarus - Trabalho inicial de Isaac Trindade da Silva contato tioneobrasil@yahoo.com.br dicas4lazarus@yahoo.com.br
Lazarus Ported - initial work by Isaac 07/2009
}
unit RLXLSFilter;
{$MODE DELPHI}{$H+}
interface

uses
  SysUtils, Classes, Contnrs, Math,
{$ifdef VCL}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ELSE}
  LCLIntf,
  LCLType,
  {$ENDIF}
  Types,
  Graphics, RLMetaVCL,
{$else}
  Types, QGraphics, RLMetaCLX,
{$endif}
  RLMetaFile, RLConsts, RLTypes, RLUtils, RLFilters;

const
  XLSMaxDefaultColors  =16;
  XLSMaxRowsInSheet    =65536;
  XLSMaxRowsInBlock    =32;
  XLSMaxCellsInRow     =256;
  XLSMaxColorsInPalette=56;

const
  XLSDefaultColorPalette:array[0..XLSMaxColorsInPalette-1] of TColor=(
    $000000,$FFFFFF,$0000FF,$00FF00,$FF0000,$00FFFF,$FF00FF,$FFFF00,$000080,
    $008000,$800000,$008080,$800080,$808000,$C0C0C0,$808080,$FF9999,$663399,
    $CCFFFF,$FFFFCC,$660066,$8080FF,$CC6600,$FFCCCC,$800000,$FF00FF,$00FFFF,
    $FFFF00,$800080,$000080,$808000,$FF0000,$FFCC00,$FFFFCC,$CCFFCC,$99FFFF,
    $FFCC99,$CC99FF,$FF99CC,$99CCFF,$FF6633,$CCCC33,$00CC99,$00CCFF,$0099FF,
    $0066FF,$996666,$969696,$663300,$669933,$003300,$003333,$003399,$663399,
    $993333,$333333);

  XLSDefaultColors:array[0..XLSMaxDefaultColors-1] of integer=(
    clWhite,clBlack,clSilver,clGray,clRed,clMaroon,clYellow,clOlive,
    clLime,clGreen,clAqua,clTeal,clBlue,clNavy,clFuchsia,clPurple);

const
  MaxSheetTabs=1024;
  
type
  TRLXLSBiff8BOF=packed record
    vers    :word;
    dt      :word;
    rupBuild:word;
    rupYear :word;
    bfh     :cardinal;
    sfo     :cardinal;
  end;

  TRLXLSBiff8ColumnInfo=packed record
    colFirst:word;
    colLast :word;
    coldx   :word;
    ixfe    :word;
    grbit   :word;
    res1    :byte;
  end;

  TRLXLSBiff8XF=packed record
    ifnt    :word;
    ifmt    :word;
    Opt1    :word;
    Opt2    :byte;
    trot    :byte;
    Opt3    :word;
    Borders1:word;
    Borders2:word;
    Borders3:cardinal;
    Colors  :word;
  end;

  TRLXLSBiff8Dimensions=packed record
    rwMic :cardinal;
    rwMac :cardinal;
    colMic:word;
    colMac:word;
    Res1  :word;
  end;

  TRLXLSBiff8Row=packed record
    rw    :word;
    colMic:word;
    colMac:word;
    miyRw :word;
    irwMac:word;
    Res1  :word;
    grbit :word;
    ixfe  :word;
  end;

  TRLXLSBiff8InterfaceHeader=packed record
    cv:word;
  end;

  TRLXLSBiff8MMS=packed record
    caitm:byte;
    cditm:byte;
  end;

  TRLXLSBiff8CodePage=packed record
    cv:word;
  end;

  TRLXLSBiff8FNGroupCount=packed record
    cFnGroup:word;
  end;

  TRLXLSBiff8WindowProtect=packed record
    fLockWn:word;
  end;

  TRLXLSBiff8Protect=packed record
    fLock:word;
  end;

  TRLXLSBiff8Password=packed record
    wPassword:word;
  end;

  TRLXLSBiff8BACKUP=packed record
    fBackupFile:word;
  end;

  TRLXLSBiff8HIDEOBJ=packed record
    fHideObj:word;
  end;

  TRLXLSBiff81904=packed record
    f1904:word;
  end;

  TRLXLSBiff8PRECISION=packed record
    fFullPrec:word;
  end;

  TRLXLSBiff8BOOKBOOL=packed record
    fNoSaveSupp:word;
  end;

  TRLXLSBiff8FONT=packed record
    dyHeight:word;
    grbit   :word;
    icv     :word;
    bls     :word;
    sss     :word;
    uls     :byte;
    bFamily :byte;
    bCharSet:byte;
    Res1    :byte;
    cch     :byte;
    cchgrbit:byte;
  end;
  PRLXLSBiff8FONT=^TRLXLSBiff8FONT;

  TRLXLSBiff8FORMAT=packed record
    ifmt    :word;
    cch     :word;
    cchgrbit:byte;
  end;
  PRLXLSBiff8FORMAT=^TRLXLSBiff8FORMAT;

  TRLXLSBiff8COUNTRY=packed record
    iCountryDef   :word;
    iCountryWinIni:word;
  end;

  TRLXLSBiff8INDEX=packed record
    Res1 :cardinal;
    rwMic:cardinal;
    rwMac:cardinal;
    Res2 :cardinal;
  end;
  PRLXLSBiff8INDEX=^TRLXLSBiff8INDEX;

  TRLXLSBiff8CALCMODE=packed record
    fAutoRecalc:word;
  end;

  TRLXLSBiff8CALCCOUNT=packed record
    cIter:word;
  end;

  TRLXLSBiff8REFMODE=packed record
    fRefA1:word;
  end;

  TRLXLSBiff8ITERATION=packed record
    fIter:word;
  end;

  TRLXLSBiff8DELTA=packed record
    numDelta:Int64; 
  end;

  TRLXLSBiff8SAVERECALC=packed record
    fSaveRecalc:word;
  end;

  TRLXLSBiff8PRINTHEADERS=packed record
    fPrintRwCol:word;
  end;

  TRLXLSBiff8PRINTGRIDLINES=packed record
    fPrintGrid:word;
  end;

  TRLXLSBiff8GRIDSET=packed record
    fGridSet:word;
  end;

  TRLXLSBiff8GUTS=packed record
    dxRwGut     :word;
    dyColGut    :word;
    iLevelRwMac :word;
    iLevelColMac:word;
  end;

  TRLXLSBiff8DEFAULTROWHEIGHT=packed record
    grbit:word;
    miyRw:word;
  end;

  TRLXLSBiff8WSBOOL=packed record
    grbit:word;
  end;

  TRLXLSBiff8HEADER=packed record
    cch     :word;
    cchgrbit:byte;
  end;
  PRLXLSBiff8HEADER=^TRLXLSBiff8HEADER;

  TRLXLSBiff8FOOTER=packed record
    cch     :word;
    cchgrbit:byte;
  end;
  PRLXLSBiff8FOOTER=^TRLXLSBiff8FOOTER;

  TRLXLSBiff8HCENTER=packed record
    fHCenter:word;
  end;

  TRLXLSBiff8VCENTER=packed record
    fVCenter:word;
  end;

  TRLXLSBiff8DEFCOLWIDTH=packed record
    cchdefColWidth:word;
  end;

  TRLXLSBiff8WRITEACCESS=packed record
    stName:array[0..111] of byte;
  end;

  TRLXLSBiff8DOUBLESTREAMFILE=packed record
    fDSF:word;
  end;

  TRLXLSBiff8PROT4REV=packed record
    fRevLock:word;
  end;

  TRLXLSBiff8PROT4REVPASS=packed record
    wRevPass:word;
  end;

  TRLXLSBiff8WINDOW1=packed record
    xWn      :word;
    yWn      :word;
    dxWn     :word;
    dyWn     :word;
    grbit    :word;
    itabCur  :word;
    itabFirst:word;
    ctabSel  :word;
    wTabRatio:word;
  end;

  TRLXLSBiff8REFRESHALL=packed record
    fRefreshAll:word;
  end;

  TRLXLSBiff8USESELFS=packed record
    fUsesElfs:word;
  end;

  TRLXLSBiff8PALETTE=packed record
    ccv   :word;
    colors:array[0..XLSMaxColorsInPalette-1] of cardinal;
  end;

  TRLXLSBiff8BOUNDSHEET=packed record
    lbPlyPos:cardinal;
    grbit   :word;
    cch     :byte;
    cchgrbit:byte;
  end;
  PRLXLSBiff8BOUNDSHEET=^TRLXLSBiff8BOUNDSHEET;

  TRLXLSBiff8WINDOW2=packed record
    grbit       :word;
    rwTop       :word;
    colLeft     :word;
    icvHdr      :cardinal;
    wScaleSLV   :word;
    wScaleNormal:word;
    Res1        :cardinal;
  end;

  TRLXLSBiff8SELECTION=packed record
    pnn    :byte;
    rwAct  :word;
    colAct :word;
    irefAct:word;
    cref   :word;
  end;
  PRLXLSBiff8SELECTION=^TRLXLSBiff8SELECTION;

  TRLXLSBiff8DBCELL=packed record
    dbRtrw:cardinal;
  end;

  TRLXLSBiff8DBCELLCellsOffsArray=array[0..XLSMaxCellsInRow-1] of word;

  TRLXLSBiff8DBCELLfull=packed record
    dbRtrw   :cardinal;
    cellsOffs:TRLXLSBiff8DBCELLCellsOffsArray;
  end;

  TRLXLSBiff8MERGErec=packed record
    top   :word;
    bottom:word;
    left  :word;
    right :word;
  end;
  PRLXLSBiff8MERGErec=^TRLXLSBiff8MERGErec;

  TRLXLSBiff8MERGE=packed record
    cnt:word; 
  end;
  PRLXLSBiff8MERGE=^TRLXLSBiff8MERGE;

  TRLXLSBiff8LABEL=packed record
    rw      :word;
    col     :word;
    ixfe    :word;
    cch     :word;
    cchgrbit:byte;
  end;
  PRLXLSBiff8LABEL=^TRLXLSBiff8LABEL;

  TRLXLSBiff8BLANK=packed record
    rw  :word;
    col :word;
    ixfe:word;
  end;

  TRLXLSBiff8MULBLANK=packed record
    rw      :word;
    colFirst:word;
  end;
  PRLXLSBiff8MULBLANK=^TRLXLSBiff8MULBLANK;

  TRLXLSBiff8SETUP=packed record
    iPaperSize:word;
    iScale    :word;
    iPageStart:word;
    iFitWidth :word;
    iFitHeight:word;
    grbit     :word;
    iRes      :word;
    iVRes     :word;
    numHdr    :double;
    numFtr    :double;
    iCopies   :word;
  end;

  TRLXLSBiff8SST=packed record
    cstTotal :cardinal;
    cstUnique:cardinal;
  end;
  PRLXLSBiff8SST=^TRLXLSBiff8SST;

  TRLXLSBiff8EXTSST=packed record
    Dsst:word;
  end;
  PRLXLSBiff8EXTSST=^TRLXLSBiff8EXTSST;

  TRLXLSBiff8ISSTINF=packed record
    ib  :cardinal;
    cb  :word;
    res1:word;
  end;
  PRLXLSBiff8ISSTINF=^TRLXLSBiff8ISSTINF;

  TRLXLSBiff8LABELSST=packed record
    rw  :word;
    col :word;
    ixfe:word;
    isst:cardinal;
  end;

  TRLXLSBiff8LEFTMARGIN=packed record
    num:double;
  end;

  TRLXLSBiff8RIGHTMARGIN=packed record
    num:double;
  end;

  TRLXLSBiff8TOPMARGIN=packed record
    num:double;
  end;

  TRLXLSBiff8BOTTOMMARGIN=packed record
    num:double;
  end;

  TRLXLSBiff8NUMBER=packed record
    rw  :word;
    col :word;
    ixfe:word;
    num :double;
  end;

type
  TRLXLSWorkbook=class;
  TRLXLSWorksheet=class;
  TRLXLSRow=class;
  TRLXLSCol=class;
  TRLXLSFilter=class;

  {@type TRLXLSCellDataType - Tipo de dado de uma célula ou faixa de células.
   Pode ser:
   ctNumber - A célula é um valor e pode ser envolvido em cálculos;
   ctString - O conteúdo da célula é um texto. :/}
  TRLXLSCellDataType=(ctNumber,ctString);

  TRLXLSLineStyleType=(lsNone,lsThin,lsMedium,lsDashed,lsDotted,lsThick,lsDouble,
                       lsHair,lsMediumDashed,lsDashDot,lsMediumDashDot,lsDashDotDot,
                       lsMediumDashDotDot,lsSlantedDashDot);

  TRLXLSWeightType=(weHairline,weThin,weMedium,weThick);

  TRLXLSBorderType=(bdDiagonalDown,bdDiagonalUp,bdEdgeBottom,bdEdgeLeft,bdEdgeRight,bdEdgeTop);

  TRLXLSHorizontalAlignmentType=(haGeneral,haLeft,haCenter,haRight,haFill,haJustify,haCenterAcrossSelection);

  TRLXLSVerticalAlignmentType=(vaTop,vaCenter,vaBottom,vaJustify);

  TRLXLSOrderType=(odDownThenOver,odOverThenDown);

  TRLXLSOrientationType=(orPortrait,orLandscape);

  TRLXLSPrintErrorsType=(peBlank,peDash,peDisplayed,peNA);

  TRLXLSFillPattern=(fpNone,fpAutomatic,fpChecker,fpCrissCross,fpDown,fpGray8,
                     fpGray16,fpGray25,fpGray50,fpGray75,fpGrid,fpHorizontal,
                     fpLightDown,fpLightHorizontal,fpLightUp,fpLightVertical,
                     fpSemiGray75,fpSolid,fpUp,fpVertical);
  
  TRLXLSPaperSizeType=(
    szPaperOther,
    szPaperLetter, {8 1/2 x 11"}
    szPaperLetterSmall, {8 1/2 x 11"}
    szPaperTabloid, {11 x 17"}
    szPaperLedger, {17 x 11"}
    szPaperLegal, {8 1/2 x 14"}
    szPaperStatement, {5 1/2 x 8 1/2"}
    szPaperExecutive, {7 1/4 x 10 1/2"}
    szPaperA3, {297 x 420 ìì}
    szPaperA4, {210 x 297 ìì}
    szPaperA4SmallSheet, {210 x 297 ìì}
    szPaperA5, {148 x 210 ìì}
    szPaperB4, {250 x 354 ìì}
    szPaperB5, {182 x 257 ìì}
    szPaperFolio, {8 1/2 x 13"}
    szPaperQuartoSheet, {215 x 275 ìì}
    szPaper10x14, {10 x 14"}
    szPaper11x17, {11 x 17"}
    szPaperNote, {8 1/2 x 11"}
    szPaper9Envelope, {3 7/8 x 8 7/8"}
    szPaper10Envelope, {4 1/8  x 9 1/2"}
    szPaper11Envelope, {4 1/2 x 10 3/8"}
    szPaper12Envelope, {4 3/4 x 11"}
    szPaper14Envelope, {5 x 11 1/2"}
    szPaperCSheet, {17 x 22"}
    szPaperDSheet, {22 x 34"}
    szPaperESheet, {34 x 44"}
    szPaperDLEnvelope, {110 x 220 ìì}
    szPaperC5Envelope, {162 x 229 ìì}
    szPaperC3Envelope, {324 x 458 ìì}
    szPaperC4Envelope, {229 x 324 ìì}
    szPaperC6Envelope, {114 x 162 ìì}
    szPaperC65Envelope, {114 x 229 ìì}
    szPaperB4Envelope, {250 x 353 ìì}
    szPaperB5Envelope, {176 x 250 ìì}
    szPaperB6Envelope, {176 x 125 ìì}
    szPaperItalyEnvelope, {110 x 230 ìì}
    szPaperMonarchEnvelope, {3 7/8 x 7 1/2"}
    szPaper63_4Envelope, {3 5/8 x 6 1/2"}
    szPaperUSStdFanfold, {14 7/8 x 11"}
    szPaperGermanStdFanfold, {8 1/2 x 12"}
    szPaperGermanLegalFanfold, {8 1/2 x 13"}
    szPaperB4_ISO, {250 x 353 ìì}
    szPaperJapanesePostcard, {100 x 148 ìì}
    szPaper9x11, {9 x 11"}
    szPaper10x11, {10 x 11"}
    szPaper15x11, {15 x 11"}
    szPaperEnvelopeInvite, {220 x 220 ìì}
    szPaperLetterExtra, {9 \ 275 x 12"}
    szPaperLegalExtra, {9 \275 x 15"}
    szPaperTabloidExtra, {11.69 x 18"}
    szPaperA4Extra, {9.27 x 12.69"}
    szPaperLetterTransverse, {8 \275 x 11"}
    szPaperA4Transverse, {210 x 297 ìì}
    szPaperLetterExtraTransverse, {9\275 x 12"}
    szPaperSuperASuperAA4, {227 x 356 ìì}
    szPaperSuperBSuperBA3, {305 x 487 ìì}
    szPaperLetterPlus, {8.5 x 12.69"}
    szPaperA4Plus, {210 x 330 ìì}
    szPaperA5Transverse, {148 x 210 ìì}
    szPaperB5_JIS_Transverse, {182 x 257 ìì}
    szPaperA3Extra, {322 x 445 ìì}
    szPaperA5Extra, {174 x 235 ìì}
    szPaperB5_ISO_Extra, {201 x 276 ìì}
    szPaperA2, {420 x 594 ìì}
    szPaperA3Transverse, {297 x 420 ìì}
    szPaperA3ExtraTransverse {322 x 445 ìì});

  { TRLXLSBorder }

  TRLXLSBorder=class
  private
    fColor    :TColor;
    fLineStyle:TRLXLSLineStyleType;
    fWeight   :TRLXLSWeightType;
  public
    constructor Create;
    destructor  Destroy; override;
    //
    property Color    :TColor              read fColor     write fColor;
    property LineStyle:TRLXLSLineStyleType read fLineStyle write fLineStyle;
    property Weight   :TRLXLSWeightType    read fWeight    write fWeight;
  end;

  { TRLXLSBorders }

  TRLXLSBorders=class
  private
    fBorders:array[TRLXLSBorderType] of TRLXLSBorder;
    //
    function GetItem(i:TRLXLSBorderType):TRLXLSBorder;
  public
    constructor Create;
    destructor  Destroy; override;
    //
    property Borders[i:TRLXLSBorderType]:TRLXLSBorder read GetItem; default;
  end;

  { TRLXLSRange }

  {@class TRLXLSRange - Representa uma faixa de células de uma planilha.
   Uma faixa (range) é o meio para se acessar ou modificar o conteúdo e as características das células. }
  TRLXLSRange=class
  private
    fWorksheet          :TRLXLSWorksheet;
    fCellBounds         :TRect;
    fBorders            :TRLXLSBorders;
    fFont               :TFont;
    fHorizontalAlignment:TRLXLSHorizontalAlignmentType;
    fVerticalAlignment  :TRLXLSVerticalAlignmentType;
    fWrapText           :boolean;
    fRotation           :byte;
    fFormat             :string;
    fValue              :string;
    fDataType           :TRLXLSCellDataType;
    fFillPattern        :TRLXLSFillPattern;
    fForegroundColor    :TColor;
    fBackgroundColor    :TColor;
    fExportData         :pointer;
    //
    function  GetWorkbook:TRLXLSWorkbook;
    procedure SetValue(const Value:string);
    //
    property Borders            :TRLXLSBorders                 read fBorders;
    property HorizontalAlignment:TRLXLSHorizontalAlignmentType read fHorizontalAlignment write fHorizontalAlignment;
    property VerticalAlignment  :TRLXLSVerticalAlignmentType   read fVerticalAlignment   write fVerticalAlignment;
    property WrapText           :boolean                       read fWrapText            write fWrapText;
    property Rotation           :byte                          read fRotation            write fRotation;
    property Format             :string                        read fFormat              write fFormat;
    property FillPattern        :TRLXLSFillPattern             read fFillPattern         write fFillPattern;
    property ForegroundColor    :TColor                        read fForegroundColor     write fForegroundColor;
    property BackgroundColor    :TColor                        read fBackgroundColor     write fBackgroundColor;
    property ExportData         :pointer                       read fExportData          write fExportData;
  public
    constructor Create(aWorksheet:TRLXLSWorksheet);
    destructor  Destroy; override;
    //
    {@prop Worksheet - Referência à aba da planilha a qual pertence esta faixa de células. @links TRLXLSWorksheet. :/}
    property Worksheet :TRLXLSWorksheet    read fWorksheet;
    {@prop Workbook - Referência à planilha a qual pertence esta faixa de células. @links TRLXLSWorkbook. :/}
    property Workbook  :TRLXLSWorkbook     read GetWorkbook;
    {@prop Font - Configuração de fonte das células. :/}
    property Font      :TFont              read fFont;
    {@prop CellBounds - Faixa de células compreendidas pela faixa. :/}
    property CellBounds:TRect              read fCellBounds;
    //
    {@prop Value - Valor das células como string. :/}
    property Value     :string             read fValue    write SetValue;
    {@prop DataType - Tipo de dado das celulas. @links TRLXLSCellDataType. :/}
    property DataType  :TRLXLSCellDataType read fDataType write fDataType;
  end;
  {/@class}

  { TRLXLSRow }

  {@class TRLXLSRow - Representa uma linha de uma planilha. }
  TRLXLSRow=class
  private
    fIndex :integer;
    fHeight:integer;
    //
  public
    constructor Create;
    //
    {@prop Height - Altura da linha em pixels. :/}
    property Height:integer read fHeight write fHeight;
    {@prop Index - Índice da linha na lista de linhas. :/}
    property Index :integer read fIndex;
  end;
  {/@class}

  { TRLXLSCol }

  {@class TRLXLSCol - Representa uma coluna de uma planilha. }
  TRLXLSCol=class
  private
    fIndex:integer;
    fWidth:integer;
  public
    constructor Create;
    //
    {@prop Width - Largura da coluna em pontos. :/}
    property Width:integer read fWidth write fWidth;
    {@prop Index - Índice da coluna na lista de colunas. :/}
    property Index:integer read fIndex;
  end;
  {/@class}

  { TRLXLSPageSetup }
  
  {@class TRLXLSPageSetup - Configuração da página de impressão no Excel. }
  TRLXLSPageSetup=class(TPersistent)
  private
    fBlackAndWhite     :boolean;
    fCenterFooter      :string;
    fCenterHeader      :string;
    fCenterHorizontally:boolean;
    fCenterVertically  :boolean;
    fDraft             :boolean;
    fFirstPageNumber   :integer;
    fFitToPagesTall    :boolean;
    fFitToPagesWide    :boolean;
    fLeftFooter        :string;
    fLeftHeader        :string;
    fOrder             :TRLXLSOrderType;
    fOrientation       :TRLXLSOrientationType;
    fPaperSize         :TRLXLSPaperSizeType;
    fPrintGridLines    :boolean;
    fPrintHeaders      :boolean;
    fPrintNotes        :boolean;
    fRightFooter       :string;
    fRightHeader       :string;
    fLeftMargin        :double;
    fRightMargin       :double;
    fTopMargin         :double;
    fBottomMargin      :double;
    fFooterMargin      :double;
    fHeaderMargin      :double;
    fZoom              :integer;
    fCopies            :integer;
    //
    property PaperSize      :TRLXLSPaperSizeType   read fPaperSize       write fPaperSize;
    property Orientation    :TRLXLSOrientationType read fOrientation     write fOrientation;
    property Order          :TRLXLSOrderType       read fOrder           write fOrder;
    //
    property PrintHeaders  :boolean read fPrintHeaders   write fPrintHeaders;
  protected
    procedure ReadBottomMargin(Reader: TReader);
    procedure ReadLeftMargin(Reader: TReader);
    procedure ReadRightMargin(Reader: TReader);
    procedure ReadTopMargin(Reader: TReader);
    procedure WriteBottomMargin(Writer: TWriter);
    procedure WriteLeftMargin(Writer: TWriter);
    procedure WriteRightMargin(Writer: TWriter);
    procedure WriteTopMargin(Writer: TWriter);
    procedure DefineProperties(Filer:TFiler); override;
  public
    constructor Create;
  published
    {@prop Copies - Quantidade inicial de cópias para imprimir. :/}
    property Copies:integer read fCopies write fCopies default 1;
    {@prop Zoom - Percentual de zoom inicial. :/}
    property Zoom  :integer read fZoom   write fZoom default 100;
    {@prop CenterHorizontally - Centralizar página horizontalmente. :/}
    property CenterHorizontally:boolean read fCenterHorizontally write fCenterHorizontally default False;
    {@prop CenterVertically - Centralizar página verticalmente. :/}
    property CenterVertically  :boolean read fCenterVertically   write fCenterVertically default False;
    {@prop BlackAndWhite - Imprimir em preto e branco. :/}
    property BlackAndWhite:boolean read fBlackAndWhite write fBlackAndWhite default False;
    {@prop Draft - Imprimir em modo de rascunho. :/}
    property Draft        :boolean read fDraft         write fDraft default False;
    {@prop PrintNotes - Imprimir notas de rodapé. :/}
    property PrintNotes   :boolean read fPrintNotes    write fPrintNotes default False;
    {@prop PrintGridLines - Imprimir linhas de grade. :/}
    property PrintGridLines:boolean read fPrintGridLines write fPrintGridLines default False;
    {@prop LeftMargin - Margem de impressão a esquerda em cm. :/}
    property LeftMargin  :double read fLeftMargin   write fLeftMargin stored False;
    {@prop TopMargin - Margem de impressão superior em cm. :/}
    property TopMargin   :double read fTopMargin    write fTopMargin stored False;
    {@prop RightMargin - Margem de impressão a direita em cm. :/}
    property RightMargin :double read fRightMargin  write fRightMargin stored False;
    {@prop BottomMargin - Margem de impressão inferior em cm. :/}
    property BottomMargin:double read fBottomMargin write fBottomMargin stored False;
    {@prop FirstPageNumber - Número para a primeira página. :/}
    property FirstPageNumber:integer read fFirstPageNumber write fFirstPageNumber default 1;
    {@prop FitToPagesTall - Encaixar a página de acordo com a altura. :/}
    property FitToPagesTall :boolean read fFitToPagesTall  write fFitToPagesTall default True;
    {@prop FitToPagesWide - Encaixar a página de acordo com a largura. :/}
    property FitToPagesWide :boolean read fFitToPagesWide  write fFitToPagesWide default True;
    {@prop LeftFooter - Texto para rodapé à esquerda. :/}
    property LeftFooter  :string read fLeftFooter   write fLeftFooter;
    {@prop LeftHeader - Texto para cabeçalho à esquerda. :/}
    property LeftHeader  :string read fLeftHeader   write fLeftHeader;
    {@prop CenterFooter - Texto para rodapé centralizado. :/}
    property CenterFooter:string read fCenterFooter write fCenterFooter;
    {@prop CenterHeader - Texto para cabeçalho centralizado. :/}
    property CenterHeader:string read fCenterHeader write fCenterHeader;
    {@prop RightFooter - Texto para rodapé à direita. :/}
    property RightFooter :string read fRightFooter  write fRightFooter;
    {@prop RightHeader - Texto para cabeçalho à direita. :/}
    property RightHeader :string read fRightHeader  write fRightHeader;
    {@prop HeaderMargin - Margem para o cabeçalho em cm. :/}
    property HeaderMargin:double read fHeaderMargin write fHeaderMargin;
    {@prop FooterMargin - Margem para o rodapé em cm. :/}
    property FooterMargin:double read fFooterMargin write fFooterMargin;
  end;
  {/@class}

  { TRLXLSWorksheet }

  {@class TRLXLSWorksheet - Representa uma aba de uma planilha Excel. }
  TRLXLSWorksheet=class
  private
    fWorkbook  :TRLXLSWorkbook;
    fTitle     :string;
    fRanges    :TObjectList;
    fCols      :TObjectList;
    fRows      :TObjectList;
    fCellBounds:TRect;
    //
    function  GetRangeCount:integer;
    function  GetRanges(i:integer):TRLXLSRange;
    function  GetColCount:integer;
    function  GetRowCount:integer;
    function  GetIndex:integer;
    function  GetCols(i:integer):TRLXLSCol;
    function  GetRows(i:integer):TRLXLSRow;
    procedure SetTitle(const Value:string);
    //
    function  NewRange(aLeft,aTop,aRight,aBottom:integer):TRLXLSRange;
    function  NewRow(aRowIndex:integer):TRLXLSRow;
    function  NewCol(aColIndex:integer):TRLXLSCol;
  public
    constructor Create(aWorkbook:TRLXLSWorkbook);
    destructor  Destroy; override;
    //
    {@method FindRange - Retorna a referência para a faixa de células que compreende
    os limites informados. Pode opcionalmente criar a faixa se não a encontrar.
    @links TRLXLSRange. :/}
    function FindRange(aLeft,aTop,aRight,aBottom:integer; aCanCreate:boolean):TRLXLSRange;
    {@method FindRow - Retorna a referência para a linha indicada pelo índice informado.
    Pode opcionalmente criar a linha se não a encontrar.
    @links TRLXLSRow. :/}
    function FindRow(aRowIndex:integer; aCanCreate:boolean):TRLXLSRow;
    {@method FindCol - Retorna a referência para a coluna indicada pelo índice informado.
    Pode opcionalmente criar a coluna se não a encontrar.
    @links TRLXLSCol. :/}
    function FindCol(aColIndex:integer; aCanCreate:boolean):TRLXLSCol;
    //
    {@prop Title - Título da aba. :/}
    property Title     :string         read fTitle write SetTitle;
    {@prop Workbook - Referência à planilha. @links TRLXLSWorkbook. :/}
    property Workbook  :TRLXLSWorkbook read fWorkbook;
    {@prop Index - Índice da aba dentre as abas da planilha. :/}
    property Index     :integer        read GetIndex;
    {@prop CellBounds - Tamanho da aba medida em celulas. :/}
    property CellBounds:TRect          read fCellBounds;
    //
    {@prop Ranges - Referência a i-ésima faixa de células. @links TRLXLSRange. :/}
    property Ranges[i:integer]:TRLXLSRange     read GetRanges;
    {@prop RangeCount - Retorna a quantidade de faixas de células. @links Ranges. :/}
    property RangeCount:integer               read GetRangeCount;
    {@prop Rows - Referência a i-ésima linha da aba da planilha. @links TRLXLSRow. :/}
    property Rows[aRowIndex:integer]:TRLXLSRow read GetRows;
    {@prop RowCount - Quantidade de linhas da aba. :/}
    property RowCount:integer                 read GetRowCount;
    {@prop Cols - Referência a i-ésima coluna da aba da planilha. @links TRLXLSCol. :/}
    property Cols[aColIndex:integer]:TRLXLSCol read GetCols;
    {@prop ColCount - Quantidade de colunas da aba. :/}
    property ColCount:integer                 read GetColCount;
  end;
  {/@class}

  { TRLXLSWorkbook }

  {@class TRLXLSWorkbook - Representa uma planilha Excel. }
  TRLXLSWorkbook=class
  private
    fFilter   :TRLXLSFilter;
    fUserName :string;
    fSheets   :TObjectList;
    fPageSetup:TRLXLSPageSetup;
    //
    procedure SetUserName(const Value:string);
    function  GetSheetCount:integer;
    function  GetWorkSheet(i:integer):TRLXLSWorksheet;
  protected
    function NewSheetTitle:string;
  public
    constructor Create(aFilter:TRLXLSFilter);
    destructor  Destroy; override;
    //
    {@method Clear - Limpa a planilha excluindo todas os textos e abas. :/}
    procedure Clear;
    {@method NewSheet - Adiciona uma nova aba e retorna referência a ela. @links TRLXLSWorksheet. :/}
    function  NewSheet:TRLXLSWorksheet;
    //
    {@prop UserName - Nome do usuário dono da planilha. :/}
    property UserName         :string          read fUserName write SetUserName;
    {@prop SheetCount - Quantidade de abas da planilha. :/}
    property SheetCount      :integer         read GetSheetCount;
    {@prop Sheets - Retorna referência a i-ésima aba da planilha. :/}
    property Sheets[i:integer]:TRLXLSWorksheet read GetWorkSheet;
    {@prop PageSetup - Configuração da impressão das páginas. @links TRLXLSPageSetup:/}
    property PageSetup:TRLXLSPageSetup read fPageSetup;
  end;
  {/@class}

  { TRLXLSFilter }

  TRLXLSRangeRec=record
    iXF    :integer;
    iSST   :integer;
    iFont  :integer;
    iFormat:integer;
  end;

  PXLSRangeRec =^TRLXLSRangeRec;
  TRLXLSRangesRec=array[0..0] of TRLXLSRangeRec;
  PXLSRangesRec=^TRLXLSRangesRec;

  TRLXLSSheetRec=record
    StreamBOFOffset        :integer;
    StreamBOFOffsetPosition:integer;
  end;
  TRLXLSSheetsRecs=array[0..0] of TRLXLSSheetRec;
  PXLSSheetsRecs=^TRLXLSSheetsRecs;

  {@type TRLXLSFilterOptions - Opções para a geração do arquivo planilha.
   Pode ser um conjunto dos seguintes valores:
   foFindNumberCells - Tenta encontrar valores no relatório e formata as células
   correspondentes como numéricas;
   foOneSheetOnly - Cria apenas uma aba para conter todas as páginas do relatório
   ao invés de criar uma aba para cada página (padrão). :}
  TRLXLSFilterOption=(foFindValueCells,foOneSheetOnly);

  TRLXLSFilterOptions=set of TRLXLSFilterOption;
  {/@type}

  TRLXLSTabSize=packed record
    Position:integer;
    Length  :integer;
  end;

  TRLXLSTabs=class
  public
    Count:integer;
    Sizes:packed array[0..MaxSheetTabs-1] of TRLXLSTabSize;
    procedure Add(aPosition,aLength:integer);
    procedure Remove(aTabIndex:integer);
  end;

  {@class TRLXLSFilter - Filtro para criação de planilhas formato Excel XLS a partir de um relatório.
   Este filtro gera arquivos binários compatíveis com o formato XLS legíveis pelo Microsoft Excel ou ExcelViewer.
   São exportados todos os textos presentes no relatório com suas fontes e posições mantidas.
   Para cada página do relatório será criada uma aba na planilha.
   Nota: Gráficos, linhas e cores ainda não são suportados.
   @links TRLHTMLFilter, TRLRichFilter, TRLPDFFilter.
   @ancestor TRLCustomSaveFilter.
   @pub }
  TRLXLSFilter=class(TRLCustomSaveFilter)
  private
    fBOFOffs        :integer;
    fWorkbook       :TRLXLSWorkbook;
    fUsedColors     :TList;
    fRangesRecs     :PXLSRangesRec;
    fColorPalette   :array[0..XLSMaxColorsInPalette-1] of TColor;
    fPaletteModified:Boolean;
    fSheetsRecs     :PXLSSheetsRecs;
    fOptions        :TRLXLSFilterOptions;
    fHorzTabs       :TRLXLSTabs;
    fVertTabs       :TRLXLSTabs;
    fFirstPage      :boolean;
    fOffsetRow      :integer;
    //
    function  GetColorPaletteIndex(aColor:TColor):integer;
    function  GetPageSetup: TRLXLSPageSetup;
    procedure BuildFontList(aDest:TList);
    procedure BuildFormatList(aDest:TStringList);
    procedure BuildXFRecord(aRange:TRLXLSRange; var aXF:TRLXLSBiff8XF; aRec:PXLSRangeRec);
    procedure BuildXFList(aDest:TList);
    procedure WriteSheetToStream(aStream:TStream; aSheet:TRLXLSWorksheet);
    procedure WriteRangeToStream(aStream:TStream; aRange:TRLXLSRange; aCurrentRow:integer; var aIndexInCellsOffsArray:integer; var aCellsOffs:TRLXLSBiff8DBCELLCellsOffsArray);
    procedure WriteBIFF(aStream:TStream; aCode:word; aBuff:pointer; aSize:integer);
    procedure WriteBIFFFont(aStream:TStream; aFont:TFont; aColorPaletteIndex:word);
    procedure WriteBIFFFormat(aStream:TStream; const aFormatString:string; aFormatCode:word);
    procedure WriteBIFFHexString(aStream:TStream; const aHexString:string);
    procedure WriteStorageToStream(aStream:TStream);
    procedure WriteBookToStream(aStream:TStream);
    procedure SetPageSetup(const Value: TRLXLSPageSetup);
  protected
    // override methods
    procedure InternalBeginDoc; override;
    procedure InternalEndDoc; override;
    procedure InternalNewPage; override;
    procedure InternalDrawPage(aPage:TRLGraphicSurface); override;
  public
    constructor Create(aOwner:TComponent); override;
    destructor  Destroy; override;
    //
    {@method SaveToStream - Salva planilha em uma stream. :/}
    procedure SaveToStream(aStream:TStream);
    {@method SaveToFile - Salva planilha em um arquivo. O arquivo gerado pode ser lido a partir do Microsoft Excel ou ExcelViewer. :/}
    procedure SaveToFile(const aFileName:string);
    //
    {@prop WorkBook - Referência o objeto planilha interno do filtro.
     @links TRLXLSWorkbook. :/}
    property  WorkBook:TRLXLSWorkbook read fWorkbook;
  published
    {@prop Options - Opções diversas de geração do arquivo planilha. @links TRLXLSFilterOptions. :/}
    property Options:TRLXLSFilterOptions read fOptions write fOptions default [];
    {@prop PageSetup - Configuração da impressão das páginas. @links TRLXLSPageSetup:/}
    property PageSetup:TRLXLSPageSetup read GetPageSetup write SetPageSetup;
    {@prop FileName = ancestor /}
    property FileName;
    {@prop DisplayName = ancestor /}
    property DisplayName;
    {@prop ShowProgress - ancestor /}
    property ShowProgress;
    {@prop ShowFilterDialog - ancestor /}
    property ShowFilterDialog;
  end;
  {/@class}
  
{/@unit}

implementation

const
  DefaultFontName  ='Arial';
  DefaultCellHeight=255;
  DefaultCellLength=8;

const
  B8_EOF             =$000A;
  B8_BOF             =$0809;
  B8_COLINFO         =$007D;
  B8_XF              =$00E0;
  B8_LABEL           =$0204;
  B8_BLANK           =$0201;
  B8_DIMENSIONS      =$0200;
  B8_ROW             =$0208;
  B8_INTERFACHDR     =$00E1;
  B8_INTERFACEND     =$00E2;
  B8_MMS             =$00C1;
  B8_CODEPAGE        =$0042;
  B8_TABID           =$013D;
  B8_FNGROUPCOUNT    =$009C;
  B8_WINDOWPROTECT   =$0019;
  B8_PROTECT         =$0012;
  B8_PASSWORD        =$0013;
  B8_WINDOW1         =$003D;
  B8_BACKUP          =$0040;
  B8_HIDEOBJ         =$008D;
  B8_1904            =$0022;
  B8_PRECISION       =$000E;
  B8_BOOKBOOL        =$00DA;
  B8_FONT            =$0031; // MSDN=$0231
  B8_FORMAT          =$041E;
  B8_COUNTRY         =$008C;
  B8_INDEX           =$020B;
  B8_CALCMODE        =$000D;
  B8_CALCCOUNT       =$000C;
  B8_REFMODE         =$000F;
  B8_ITERATION       =$0011;
  B8_SAVERECALC      =$005F;
  B8_DELTA           =$0010;
  B8_PRINTHEADERS    =$002A;
  B8_PRINTGRIDLINES  =$002B;
  B8_GRIDSET         =$0082;
  B8_GUTS            =$0080;
  B8_DEFAULTROWHEIGHT=$0225;
  B8_WSBOOL          =$0081;
  B8_HEADER          =$0014;
  B8_FOOTER          =$0015;
  B8_HCENTER         =$0083;
  B8_VCENTER         =$0084;
  B8_DEFCOLWIDTH     =$0055;
  B8_WRITEACCESS     =$005C;
  B8_DOUBLESTREAMFILE=$0161;
  B8_PROT4REV        =$01AF;
  B8_PROT4REVPASS    =$01BC;
  B8_REFRESHALL      =$01B7;
  B8_USESELFS        =$0160;
  B8_BOUNDSHEET      =$0085;
  B8_WINDOW2         =$023E;
  B8_SELECTION       =$001D;
  B8_DBCELL          =$00D7;
  B8_MULBLANK        =$00BE;
  B8_MERGE           =$00E5;
  B8_PALETTE         =$0092;
  B8_CONTINUE        =$003C;
  B8_SETUP           =$00A1;
  B8_SST             =$00FC;
  B8_EXTSST          =$00FF;
  B8_LABELSST        =$00FD;
  B8_NUMBER          =$0203;

  B8_BOF_vers=$0600;

  B8_BOF_dt_WorkbookGlobals  =$0005;
  B8_BOF_dt_VisualBasicModule=$0006;
  B8_BOF_dt_Worksheet        =$0010;
  B8_BOF_dt_Chart            =$0020;
  B8_BOF_dt_MacroSheet       =$0040;
  B8_BOF_dt_WorkspaceFile    =$0100;

  B8_BOF_rupBuild_Excel97=$0DBB;

  B8_BOF_rupYear_Excel07=$07CC;

  B8_XF_Opt1_fLocked   =$0001;
  B8_XF_Opt1_fHidden   =$0002;
  B8_XF_Opt1_fStyleXF  =$0004;
  B8_XF_Opt1_f123Prefix=$0008;
  B8_XF_Opt1_ixfParent =$FFF0;

  B8_XF_Opt2_alcGeneral              =$0000;
  B8_XF_Opt2_alcLeft                 =$0001;
  B8_XF_Opt2_alcCenter               =$0002;
  B8_XF_Opt2_alcRight                =$0003;
  B8_XF_Opt2_alcFill                 =$0004;
  B8_XF_Opt2_alcJustify              =$0005;
  B8_XF_Opt2_alcCenterAcrossSelection=$0006;

  B8_XF_Opt2_fWrap=$0008;

  B8_XF_Opt2_alcVTop    =$0000;
  B8_XF_Opt2_alcVCenter =$0010;
  B8_XF_Opt2_alcVBottom =$0020;
  B8_XF_Opt2_alcVJustify=$0030;

  B8_XF_Opt3_fMergeCell=$0020;
  B8_XF_Opt3_fAtrNum   =$0400;
  B8_XF_Opt3_fAtrFnt   =$0800;
  B8_XF_Opt3_fAtrAlc   =$1000;
  B8_XF_Opt3_fAtrBdr   =$2000;
  B8_XF_Opt3_fAtrPat   =$4000;
  B8_XF_Opt3_fAtrProt  =$8000;

  B8_XF_Border_None            =$0000;
  B8_XF_Border_Thin            =$0001;
  B8_XF_Border_Medium          =$0002;
  B8_XF_Border_Dashed          =$0003;
  B8_XF_Border_Dotted          =$0004;
  B8_XF_Border_Thick           =$0005;
  B8_XF_Border_Double          =$0006;
  B8_XF_Border_Hair            =$0007;
  B8_XF_Border_MediumDashed    =$0008;
  B8_XF_Border_DashDot         =$0009;
  B8_XF_Border_MediumDashDot   =$000A;
  B8_XF_Border_DashDotDot      =$000B;
  B8_XF_Border_MediumDashDotDot=$000C;
  B8_XF_Border_SlantedDashDot  =$000D;

  B8_INTERFACHDR_cv_IBMPC    =$01B5;
  B8_INTERFACHDR_cv_Macintosh=$8000;
  B8_INTERFACHDR_cv_ANSI     =$04E4;

  B8_CODEPAGE_cv_IBMPC    =$01B5;
  B8_CODEPAGE_cv_Macintosh=$8000;
  B8_CODEPAGE_cv_ANSI     =$04E4;

  B8_WINDOW1_grbit_fHidden      =$0001;
  B8_WINDOW1_grbit_fIconic      =$0002;
  B8_WINDOW1_grbit_fDspHScroll  =$0008;
  B8_WINDOW1_grbit_fDspVScroll  =$0010;
  B8_WINDOW1_grbit_fBotAdornment=$0020;

  B8_FONT_grbit_fItalic   =$0002;
  B8_FONT_grbit_fStrikeout=$0008;
  B8_FONT_grbit_fOutline  =$0010;
  B8_FONT_grbit_fShadow   =$0020;

  B8_DEFAULTROWHEIGHT_fUnsynced=$0001;
  B8_DEFAULTROWHEIGHT_fDyZero  =$0002;
  B8_DEFAULTROWHEIGHT_fExAsc   =$0004;
  B8_DEFAULTROWHEIGHT_fExDsc   =$0008;

  B8_WSBOOL_fShowAutoBreaks=$0001;
  B8_WSBOOL_fDialog        =$0010;
  B8_WSBOOL_fApplyStyles   =$0020;
  B8_WSBOOL_fRwSumsBelow   =$0040;
  B8_WSBOOL_fColSumsRight  =$0080;
  B8_WSBOOL_fFitToPage     =$0100;
  B8_WSBOOL_fDspGuts       =$0200;
  B8_WSBOOL_fAee           =$0400;
  B8_WSBOOL_fAfe           =$8000;

  B8_WINDOW1_fHidden      =$0001;
  B8_WINDOW1_fIconic      =$0002;
  B8_WINDOW1_fDspHScroll  =$0008;
  B8_WINDOW1_fDspVScroll  =$0010;
  B8_WINDOW1_fBotAdornment=$0020;


  B8_WINDOW2_grbit_fDspFmla      =$0001;
  B8_WINDOW2_grbit_fDspGrid      =$0002;
  B8_WINDOW2_grbit_fDspRwCol     =$0004;
  B8_WINDOW2_grbit_fFrozen       =$0008;
  B8_WINDOW2_grbit_fDspZeros     =$0010;
  B8_WINDOW2_grbit_fDefaultHdr   =$0020;
  B8_WINDOW2_grbit_fArabic       =$0040;
  B8_WINDOW2_grbit_fDspGuts      =$0080;
  B8_WINDOW2_grbit_fFrozenNoSplit=$0100;
  B8_WINDOW2_grbit_fSelected     =$0200;
  B8_WINDOW2_grbit_fPaged        =$0400;
  B8_WINDOW2_grbit_fSLV          =$0800;

  B8_ROW_grbit_fCollapsed    =$0010;
  B8_ROW_grbit_fDyZero       =$0020;
  B8_ROW_grbit_fUnsynced     =$0040;
  B8_ROW_grbit_fGhostDirty   =$0080;
  B8_ROW_grbit_mask_iOutLevel=$0007;

  B8_COLINFO_fHidden   =$0001;
  B8_COLINFO_fCollapsed=$1000;

  B8_SETUP_fLeftToRight=$0001;
  B8_SETUP_fLandscape  =$0002;
  B8_SETUP_fNoPls      =$0004;
  B8_SETUP_fNoColor    =$0008;
  B8_SETUP_fDraft      =$0010;
  B8_SETUP_fNotes      =$0020;
  B8_SETUP_fNoOrient   =$0040;
  B8_SETUP_fUsePage    =$0080;

  B8_LEFTMARGIN  =$0026;
  B8_RIGHTMARGIN =$0027;
  B8_TOPMARGIN   =$0028;
  B8_BOTTOMMARGIN=$0029;

{ UTILS }

function PointInRect(X, Y:integer; const R:TRect):boolean;
begin
  Result:=((X>=R.Left) and (X<=R.Right)) and ((Y>=R.Top) and (Y<=R.Bottom));
end;

function RectInterceptsRect(const aRect1,aRect2:TRect):boolean;
begin
  Result:=PointInRect(aRect1.Left,aRect1.Top,aRect2) or
          PointInRect(aRect1.Right,aRect1.Top,aRect2) or
          PointInRect(aRect1.Right,aRect1.Bottom,aRect2) or
          PointInRect(aRect1.Left,aRect1.Bottom,aRect2) or
          PointInRect(aRect2.Left,aRect2.Top,aRect1) or
          PointInRect(aRect2.Right,aRect2.Top,aRect1) or
          PointInRect(aRect2.Right,aRect2.Bottom,aRect1) or
          PointInRect(aRect2.Left,aRect2.Bottom,aRect1) or
          ((aRect1.Left>aRect2.Left) and (aRect1.Right<aRect2.Right) and (aRect1.Top<aRect2.Top) and (aRect1.Bottom>aRect2.Bottom)) or
          ((aRect1.Left<aRect2.Left) and (aRect1.Right>aRect2.Right) and (aRect1.Top>aRect2.Top) and (aRect1.Bottom<aRect2.Bottom)) or
          ((aRect2.Left>aRect1.Left) and (aRect2.Right<aRect1.Right) and (aRect2.Top<aRect1.Top) and (aRect2.Bottom>aRect1.Bottom)) or
          ((aRect2.Left<aRect1.Left) and (aRect2.Right>aRect1.Right) and (aRect2.Top>aRect1.Top) and (aRect2.Bottom<aRect1.Bottom));
end;

function WideStringSize(const aStr:string):integer;
begin
  Result:=Length(aStr)*SizeOf(WideChar);
end;

procedure StringToWideChar(const aSource:string; aDest:PWideChar; aDestSize:Integer);
var
  w:WideString;
begin
  w:=aSource;
  if w<>emptystr then
    Move(w[1],aDest^,aDestSize)
  else
    aDest[0]:=#0;
end;

function HexToString(const aStr:string):string;
var
  i,ls:integer;
  b1  :string;
begin
  Result:=emptystr;
  ls:=length(aStr);
  i:=1;
  while i<=ls do
  begin
    while (i<=ls) and not (aStr[i] in ['0'..'9','a'..'f','A'..'F']) do
      Inc(i);
    if i>ls then
      Break;
    b1:=emptystr;
    while (i<=ls) and (aStr[i] in ['0'..'9','a'..'f','A'..'F']) do
    begin
      b1:=b1+aStr[i];
      Inc(i);
    end;
    if b1<>emptystr then
      Result:=Result+Char(StrToInt('$'+b1));
    if (b1=emptystr) or (i>ls) then
      Break;
  end;
end;

procedure StreamWrite(s:tstream; const c; l:integer);
begin
  if (s.Position>$46c-100) and (s.Position<$46c+100) then
    l:=l;
  s.write(c,l);
end;

{ TRLXLSRow }

constructor TRLXLSRow.Create;
begin
  fHeight:=DefaultCellHeight;
  //
  inherited Create;
end;

{ TRLXLSCol }

constructor TRLXLSCol.Create;
begin
  fWidth:=DefaultCellLength*256;
  //
  inherited Create;
end;

{ TRLXLSBorder }

constructor TRLXLSBorder.Create;
begin
  fLineStyle:=lsNone;
  fWeight   :=weHairline;
  fColor    :=clBlack;
  //
  inherited;
end;

destructor TRLXLSBorder.Destroy;
begin
  inherited;
end;

{ TRLXLSBorders }

constructor TRLXLSBorders.Create;
var
  i:TRLXLSBorderType;
begin
  inherited;
  //
  for i:=Low(TRLXLSBorderType) to High(TRLXLSBorderType) do
    fBorders[i]:=TRLXLSBorder.Create;
end;

destructor TRLXLSBorders.Destroy;
var
  i:TRLXLSBorderType;
begin
  for i:=Low(TRLXLSBorderType) to High(TRLXLSBorderType) do
    fBorders[i].Free;
  //
  inherited;
end;

function TRLXLSBorders.GetItem;
begin
  Result:=fBorders[i];
end;

{ TRLXLSRange }

constructor TRLXLSRange.Create(aWorksheet:TRLXLSWorksheet);
begin
  fVerticalAlignment  :=vaBottom;
  fHorizontalAlignment:=haGeneral;
  fWorksheet          :=aWorksheet;
  fBorders            :=TRLXLSBorders.Create;
  fFont               :=TFont.Create;
  fFont.Name          :=DefaultFontName;
  fFont.Size          :=10;
  fFont.Color         :=clBlack;
  fValue              :=emptystr;
  fDataType           :=ctString;
  //
  inherited Create;
end;

destructor TRLXLSRange.Destroy;
begin
  inherited;
  //
  fBorders.Free;
  fFont.Free;
end;

function TRLXLSRange.GetWorkbook;
begin
  if fWorksheet<>nil then
    Result:=fWorksheet.Workbook
  else
    Result:=nil;
end;

procedure TRLXLSRange.SetValue(const Value:string);
var
  foo,mil:string;
  aux    :double;
  err    :integer;
begin
  fValue:=StringReplace(Value,#13#10,#10,[rfReplaceAll]);
  //
  if foFindValueCells in Self.Workbook.fFilter.fOptions then
  begin
    if DecimalSeparator='.' then
      mil:=','
    else
      mil:='.';
    foo:=StringReplace(StringReplace(Trim(StringReplace(fValue,#10,' ',[rfReplaceAll])),mil,'',[rfReplaceAll]),DecimalSeparator,'.',[rfReplaceAll]);
    Val(foo,aux,err);
    if (foo<>emptystr) and (err=0) then
    begin
      fDataType:=ctNumber;
      Str(aux,fValue);
      fValue:=StringReplace(fValue,'.',DecimalSeparator,[rfReplaceAll]);
    end
    else
      fDataType:=ctString;
  end;    
end;

const
  DefaultLeftMargin  =2;
  DefaultTopMargin   =2.5;
  DefaultRightMargin =2;
  DefaultBottomMargin=2.5;

{ TRLXLSPageSetup }

constructor TRLXLSPageSetup.Create;
begin
  fLeftMargin        :=DefaultLeftMargin;
  fTopMargin         :=DefaultTopMargin;
  fRightMargin       :=DefaultRightMargin;
  fBottomMargin      :=DefaultBottomMargin;
  fPaperSize         :=szPaperA4;
  fCopies            :=1;
  fZoom              :=100;
  fFitToPagesTall    :=True;
  fFitToPagesWide    :=True;
  fFirstPageNumber   :=1;
  fCenterHorizontally:=False;
  fCenterVertically  :=False;
  fBlackAndWhite     :=False;
  fDraft             :=False;
  fPrintNotes        :=False;
  fPrintGridLines    :=False;
  fLeftFooter        :=emptystr;
  fLeftHeader        :=emptystr;
  fCenterFooter      :=emptystr;
  fCenterHeader      :=emptystr;
  fRightFooter       :=emptystr;
  fRightHeader       :=emptystr;
  fHeaderMargin      :=0;
  fFooterMargin      :=0;
  //
  inherited;
end;

procedure TRLXLSPageSetup.DefineProperties(Filer: TFiler);
begin
  Filer.DefineProperty('LeftMargin'  ,ReadLeftMargin  ,WriteLeftMargin  ,fLeftMargin<>DefaultLeftMargin);
  Filer.DefineProperty('TopMargin'   ,ReadTopMargin   ,WriteTopMargin   ,fTopMargin<>DefaultTopMargin);
  Filer.DefineProperty('RightMargin' ,ReadRightMargin ,WriteRightMargin ,fRightMargin<>DefaultRightMargin);
  Filer.DefineProperty('BottomMargin',ReadBottomMargin,WriteBottomMargin,fBottomMargin<>DefaultBottomMargin);
end;

procedure TRLXLSPageSetup.ReadLeftMargin(Reader:TReader);
begin
  fLeftMargin:=Reader.ReadFloat;
end;

procedure TRLXLSPageSetup.WriteLeftMargin(Writer:TWriter);
begin
  Writer.WriteFloat(fLeftMargin);
end;

procedure TRLXLSPageSetup.ReadTopMargin(Reader:TReader);
begin
  fTopMargin:=Reader.ReadFloat;
end;

procedure TRLXLSPageSetup.WriteTopMargin(Writer:TWriter);
begin
  Writer.WriteFloat(fTopMargin);
end;

procedure TRLXLSPageSetup.ReadRightMargin(Reader:TReader);
begin
  fRightMargin:=Reader.ReadFloat;
end;

procedure TRLXLSPageSetup.WriteRightMargin(Writer:TWriter);
begin
  Writer.WriteFloat(fRightMargin);
end;

procedure TRLXLSPageSetup.ReadBottomMargin(Reader:TReader);
begin
  fBottomMargin:=Reader.ReadFloat;
end;

procedure TRLXLSPageSetup.WriteBottomMargin(Writer:TWriter);
begin
  Writer.WriteFloat(fBottomMargin);
end;

{ TRLXLSWorksheet }

constructor TRLXLSWorksheet.Create(aWorkbook:TRLXLSWorkbook);
begin
  fWorkbook  :=aWorkbook;
  fCellBounds:=Rect(-1,-1,-1,-1);
  fTitle     :=emptystr;
  fRanges    :=nil;
  fCols      :=nil;
  fRows      :=nil;
  //
  fRanges    :=TObjectList.Create;
  fCols      :=TObjectList.Create;
  fRows      :=TObjectList.Create;
  //
  inherited Create;
end;

destructor TRLXLSWorksheet.Destroy;
begin
  if Assigned(fRanges) then
    fRanges.Free;
  if Assigned(fCols) then
    fCols.Free;
  if Assigned(fRows) then
    fRows.Free;
  //
  inherited;
end;

function TRLXLSWorksheet.GetIndex;
begin
  Result:=fWorkBook.fSheets.IndexOf(Self);
end;

procedure TRLXLSWorksheet.SetTitle(const Value:string);
begin
  fTitle:=Trim(Copy(Value,1,31));
end;

function TRLXLSWorksheet.GetCols(i:integer):TRLXLSCol;
begin
  Result:=TRLXLSCol(fCols[i]);
end;

function TRLXLSWorksheet.GetRows(i:integer):TRLXLSRow;
begin
  Result:=TRLXLSRow(fRows[i]);
end;

function TRLXLSWorksheet.GetColCount:integer;
begin
  Result:=fCols.Count;
end;

function TRLXLSWorksheet.GetRowCount:integer;
begin
  Result:=fRows.Count;
end;

function TRLXLSWorksheet.GetRangeCount:integer;
begin
  Result:=fRanges.Count;
end;

function TRLXLSWorksheet.GetRanges(i:integer):TRLXLSRange;
begin
  Result:=TRLXLSRange(fRanges[i]);
end;

function TRLXLSWorksheet.FindRow(aRowIndex:integer; aCanCreate:boolean):TRLXLSRow;
var
  i:integer;
begin
  i:=0;
  while (i<RowCount) and (Rows[i].Index<>aRowIndex) do
    Inc(i);
  if i<RowCount then
    Result:=Rows[i]
  else if aCanCreate then
    Result:=NewRow(aRowIndex)
  else
    Result:=nil;  
end;

function TRLXLSWorksheet.NewRow(aRowIndex:integer):TRLXLSRow;
begin
  Result:=TRLXLSRow.Create;
  Result.fIndex:=aRowIndex;
  fRows.Add(Result);
  // expande as dimensões da sheet
  if (fCellBounds.Top=-1) or (aRowIndex<fCellBounds.Top) then
    fCellBounds.Top:=aRowIndex;
  if (fCellBounds.Bottom=-1) or (aRowIndex>fCellBounds.Bottom) then
    fCellBounds.Bottom:=aRowIndex;
end;

function TRLXLSWorksheet.FindCol(aColIndex:integer; aCanCreate:boolean):TRLXLSCol;
var
  i:integer;
begin
  i:=0;
  while (i<ColCount) and (Cols[i].Index<>aColIndex) do
    Inc(i);
  if i<ColCount then
    Result:=Cols[i]
  else if aCanCreate then
    Result:=NewCol(aColIndex)
  else
    Result:=nil;
end;

function TRLXLSWorksheet.NewCol(aColIndex:integer):TRLXLSCol;
begin
  Result:=TRLXLSCol.Create;
  Result.fIndex:=aColIndex;
  fCols.Add(Result);
  // expande as dimensões da sheet
  if (fCellBounds.Left=-1) or (aColIndex<fCellBounds.Left) then
    fCellBounds.Left:=aColIndex;
  if (fCellBounds.Right=-1) or (aColIndex>fCellBounds.Right) then
    fCellBounds.Right:=aColIndex;
end;

function SameBounds(aRange:TRLXLSRange; aLeft,aTop,aRight,aBottom:integer):boolean;
begin
  with aRange.CellBounds do
    Result:=(Left=aLeft) and (Top=aTop) and (Right=aRight) and (Bottom=aBottom);
end;

function TRLXLSWorksheet.FindRange(aLeft,aTop,aRight,aBottom:integer; aCanCreate:boolean):TRLXLSRange;
var
  i:integer;
begin
  i:=0;
  while (i<RangeCount) and not SameBounds(Ranges[i],aLeft,aTop,aRight,aBottom) do
    Inc(i);
  if i<RangeCount then
    Result:=Ranges[i]
  else if aCanCreate then
    Result:=NewRange(aLeft,aTop,aRight,aBottom)
  else
    Result:=nil;
end;

function TRLXLSWorksheet.NewRange(aLeft,aTop,aRight,aBottom:integer):TRLXLSRange;
var
  rangebounds:TRect;
  i          :integer;
begin
  // exclui as ranges que se encontram naquela região
  rangebounds:=Rect(aLeft,aTop,aRight,aBottom);
  for i:=RangeCount-1 downto 0 do
    if RectInterceptsRect(rangebounds,Ranges[i].CellBounds) then
      fRanges.Delete(i);
  //
  Result:=TRLXLSRange.Create(Self);
  Result.fCellBounds:=rangebounds;
  fRanges.Add(Result);
  // expande as dimensões da sheet
  if (fCellBounds.Left=-1) or (rangebounds.Left<fCellBounds.Left) then
    fCellBounds.Left:=rangebounds.Left;
  if (fCellBounds.Top=-1) or (rangebounds.Top<fCellBounds.Top) then
    fCellBounds.Top:=rangebounds.Top;
  if (fCellBounds.Right=-1) or (rangebounds.Right>fCellBounds.Right) then
    fCellBounds.Right:=rangebounds.Right;
  if (fCellBounds.Bottom=-1) or (rangebounds.Bottom>fCellBounds.Bottom) then
    fCellBounds.Bottom:=rangebounds.Bottom;
end;

{ TRLXLSWorkbook }
  
constructor TRLXLSWorkbook.Create(aFilter:TRLXLSFilter);
begin
  fFilter   :=aFilter;
  fUserName :=CS_ProductTitleStr;
  fSheets   :=nil;
  fPageSetup:=nil;
  //
  fSheets   :=TObjectList.Create;
  fPageSetup:=TRLXLSPageSetup.Create;
  //
  inherited Create;
end;

destructor TRLXLSWorkbook.Destroy;
begin
  inherited;
  //
  if Assigned(fSheets) then
    fSheets.Free;
  if Assigned(fPageSetup) then
    fPageSetup.Free;
end;

function TRLXLSWorkbook.NewSheetTitle:string;
var
  titleno,i:integer;
begin
  titleno:=fSheets.Count;
  repeat
    Inc(titleno);
    Result:=LS_PageStr+IntToStr(titleno);
    i:=0;
    while (i<SheetCount) and not AnsiSameText(Sheets[i].Title,Result) do
      Inc(i);
  until not (i<SheetCount);
end;

procedure TRLXLSWorkbook.SetUserName(const Value:string);
const
  MaxUserName=66;
begin
  fUserName:=Trim(Copy(Value,1,MaxUserName));
end;

function TRLXLSWorkbook.GetSheetCount:integer;
begin
  Result:=fSheets.Count;
end;

function TRLXLSWorkbook.GetWorkSheet(i:integer):TRLXLSWorksheet;
begin
  Result:=TRLXLSWorksheet(fSheets[i]);
end;

function TRLXLSWorkbook.NewSheet;
begin
  Result:=TRLXLSWorksheet.Create(Self);
  Result.Title:=NewSheetTitle;
  fSheets.Add(Result);
end;

procedure TRLXLSWorkbook.Clear;
begin
  fSheets.Clear;
end;

{ TRLXLSTabs }

procedure TRLXLSTabs.Add(aPosition, aLength: integer);
var
  i:integer;
begin
  i:=0;
  while (i<Count) and (Sizes[i].Position<aPosition) do
    Inc(i);
  if i<Count then
    if Sizes[i].Position=aPosition then
      Sizes[i].Length:=Max(Sizes[i].Length,aLength)
    else
    begin
      Move(Sizes[i],Sizes[i+1],SizeOf(Sizes[0])*(Count-i));
      Sizes[i].Position:=aPosition;
      Sizes[i].Length  :=aLength;
      Inc(Count);
    end
  else
  begin
    Sizes[Count].Position:=aPosition;
    Sizes[Count].Length  :=aLength;
    Inc(Count);
  end;
end;

procedure TRLXLSTabs.Remove(aTabIndex: integer);
begin
  Move(Sizes[aTabIndex+1],Sizes[aTabIndex],SizeOf(Sizes[0])*(Count-(aTabIndex+1)));
  Dec(Count);
  if (aTabIndex>0) and (aTabIndex<Count) then
    Sizes[aTabIndex-1].Length:=Sizes[aTabIndex].Position-Sizes[aTabIndex-1].Position;
end;

{ TRLXLSFilter }

constructor TRLXLSFilter.Create(aOwner:TComponent);
begin
  fUsedColors:=nil;
  fWorkbook  :=nil;
  fOptions   :=[];
  fHorzTabs  :=nil;
  fVertTabs  :=nil;
  //
  fUsedColors:=TList.Create;
  fWorkbook  :=TRLXLSWorkbook.Create(Self);
  fHorzTabs  :=TRLXLSTabs.Create;
  fVertTabs  :=TRLXLSTabs.Create;
  //
  inherited;
  //
  DefaultExt :='.xls';
  DisplayName:=LS_XLSFormatStr;
  FilterStyle:=FilterStyle+[]; ///fsSetupDialog];
end;

destructor TRLXLSFilter.Destroy;
begin
  inherited;
  //
  if Assigned(fUsedColors) then
    fUsedColors.Free;
  if Assigned(fWorkbook) then
    fWorkbook.Free;
  if Assigned(fHorzTabs) then
    fHorzTabs.Free;
  if Assigned(fVertTabs) then
    fVertTabs.Free;
end;

const
  MaxBiffRecordSize=8228;

procedure TRLXLSFilter.WriteBIFF(aStream:TStream; aCode:word; aBuff:pointer; aSize:integer);
var
  sz:word;
begin
  repeat
    StreamWrite(aStream,aCode,2);
    sz:=Min(aSize,MaxBiffRecordSize-4);
    StreamWrite(aStream,sz,2);
    if sz>0 then
    begin
      StreamWrite(aStream,aBuff^,sz);
      aBuff:=Pointer(Integer(aBuff)+sz);
      aSize:=aSize-sz;
      aCode:=B8_CONTINUE;
    end;
  until aSize=0;
end;

procedure TRLXLSFilter.WriteBIFFFont(aStream:TStream; aFont:TFont; aColorPaletteIndex:word);
var
  room:integer;
  font:PRLXLSBiff8FONT;
{$ifdef VCL}
  lf  :TLogFont;
{$endif}
begin
  room:=WideStringSize(aFont.Name);
  font:=AllocMem(SizeOf(TRLXLSBiff8FONT)+room);
  try
{$ifdef VCL}
    GetObject(aFont.Handle,SizeOf(TLogFont),@lf);
{$endif}
    StringToWideChar(aFont.Name,PWideChar(Integer(font)+SizeOf(TRLXLSBiff8FONT)),room);
    font.dyHeight:=aFont.Size*20;
    if fsItalic in aFont.Style then
      font.grbit:=font.grbit or B8_FONT_grbit_fItalic;
    if fsStrikeout in aFont.Style then
      font.grbit:=font.grbit or B8_FONT_grbit_fStrikeout;
    font.icv:=aColorPaletteIndex;
    if fsBold in aFont.Style then
      font.bls:=$3E8 // ref MSDN
    else
      font.bls:=$64; // ref MSDN
    if fsUnderline in aFont.Style then
      font.uls:=1;  // ref MSDN
{$ifdef VCL}
    font.bFamily :=lf.lfPitchAndFamily;
    font.bCharSet:=lf.lfCharSet;
{$else}
    case aFont.Pitch of
      fpDefault : font.bFamily:=0;
      fpFixed   : font.bFamily:=1;
      fpVariable: font.bFamily:=2;
    end;
    case aFont.CharSet of
      fcsLatin1          : font.bCharSet:=0;
      fcsKOI8R           : font.bCharSet:=130;
      fcsSet_Ja          : font.bCharSet:=$80;
      fcsSet_Ko          : font.bCharSet:=129;
      ANSI_CHARSET       : font.bCharSet:=0;
      DEFAULT_CHARSET    : font.bCharSet:=1;
      SYMBOL_CHARSET     : font.bCharSet:=2;
      MAC_CHARSET        : font.bCharSet:=77;
      SHIFTJIS_CHARSET   : font.bCharSet:=$80;
      HANGEUL_CHARSET    : font.bCharSet:=129;
      JOHAB_CHARSET      : font.bCharSet:=130;
      GB2312_CHARSET     : font.bCharSet:=134;
      CHINESEBIG5_CHARSET: font.bCharSet:=136;
      GREEK_CHARSET      : font.bCharSet:=161;
      TURKISH_CHARSET    : font.bCharSet:=162;
      HEBREW_CHARSET     : font.bCharSet:=177;
      ARABIC_CHARSET     : font.bCharSet:=178;
      BALTIC_CHARSET     : font.bCharSet:=186;
      RUSSIAN_CHARSET    : font.bCharSet:=204;
      THAI_CHARSET       : font.bCharSet:=222;
      EASTEUROPE_CHARSET : font.bCharSet:=238;
      OEM_CHARSET        : font.bCharSet:=255;
    else
      font.bCharSet:=0;
    end;
{$endif}
    font.cch     :=Length(aFont.Name);
    font.cchgrbit:=$01;
    WriteBIFF(aStream,B8_FONT,font,SizeOf(TRLXLSBiff8FONT)+room);
  finally
    FreeMem(font);
  end;
end;

procedure TRLXLSFilter.WriteBIFFFormat(aStream:TStream; const aFormatString:string; aFormatCode:word);
var
  format:PRLXLSBiff8FORMAT;
  room  :integer;
begin
  room  :=WideStringSize(aFormatString);
  format:=AllocMem(SizeOf(TRLXLSBiff8FORMAT)+room);
  try
    StringToWideChar(aFormatString,PWideChar(Integer(format)+SizeOf(TRLXLSBiff8FORMAT)),room);
    format.ifmt    :=aFormatCode;
    format.cch     :=Length(aFormatString);
    format.cchgrbit:=$01;
    WriteBIFF(aStream,B8_FORMAT,format,SizeOf(TRLXLSBiff8FORMAT)+room);
  finally
    FreeMem(format);
  end;
end;

procedure TRLXLSFilter.WriteBIFFHexString(aStream:TStream; const aHexString:string);
var
  aux:string;
begin
  aux:=HexToString(aHexString);
  StreamWrite(aStream,aux[1],Length(aux));
end;

function SameFont(aFont1,aFont2:TFont):boolean;
begin
  Result:=(aFont1.Charset=aFont2.Charset) and
          (aFont1.Color=aFont2.Color) and
          (aFont1.Height=aFont2.Height) and
          (aFont1.Name=aFont2.Name) and
          (aFont1.Pitch=aFont2.Pitch) and
          (aFont1.Size=aFont2.Size) and
          (aFont1.Style=aFont2.Style);
end;

procedure TRLXLSFilter.BuildFontList(aDest:TList);
var
  range:TRLXLSRange;
  sheet:TRLXLSWorksheet;
  font :TFont;
  i,j  :integer;
  k,n  :integer;
begin
  n:=0;
  for i:=0 to fWorkbook.SheetCount-1 do
  begin
    sheet:=fWorkbook.Sheets[i];
    for j:=0 to sheet.RangeCount-1 do
    begin
      range:=sheet.Ranges[j];
      range.ExportData:=@fRangesRecs[n];
      k:=0;
      while (k<aDest.Count) and not SameFont(TFont(aDest[k]),range.Font) do
        Inc(k);
      if k<aDest.Count then
      else
      begin
        font:=TFont.Create;
        font.Assign(range.Font);
        aDest.Add(font);
      end;
      fRangesRecs[n].iFont:=k+1;
      Inc(n);
    end;
  end;
end;

procedure TRLXLSFilter.BuildFormatList(aDest:TStringList);
var
  sheet:TRLXLSWorksheet;
  range:TRLXLSRange;
  i,j  :integer;
  k,n  :integer;
  m    :integer;
begin
  n:=aDest.Count;
  m:=0;
  for i:=0 to fWorkbook.SheetCount-1 do
  begin
    sheet:=fWorkbook.Sheets[i];
    for j:=0 to sheet.RangeCount-1 do
    begin
      range:=sheet.Ranges[j];
      if range.Format=emptystr then
        fRangesRecs[m].iFormat:=0
      else
      begin
        k:=aDest.IndexOf(range.Format);
        if k=-1 then
          k:=aDest.AddObject(range.Format,Pointer(aDest.Count-n+$32));
        fRangesRecs[m].iFormat:=Integer(aDest.Objects[k]);
      end;
      Inc(m);
    end;
  end;
end;

procedure TRLXLSFilter.BuildXFRecord(aRange:TRLXLSRange; var aXF:TRLXLSBiff8XF; aRec:PXLSRangeRec);
const
  FillPatterns:array[TRLXLSFillPattern] of integer=(0,-4105,9,16,-4121,18,17,-4124,-4125,-4126,15,-4128,13,11,14,12,10,1,-4162,-4166);
  HorizontalAlignments:array[TRLXLSHorizontalAlignmentType] of integer=
    (B8_XF_Opt2_alcGeneral,
     B8_XF_Opt2_alcLeft,
     B8_XF_Opt2_alcCenter,
     B8_XF_Opt2_alcRight,
     B8_XF_Opt2_alcFill,
     B8_XF_Opt2_alcJustify,
     B8_XF_Opt2_alcCenterAcrossSelection);
  VerticalAlignments:array[TRLXLSVerticalAlignmentType] of integer=
    (B8_XF_Opt2_alcVTop,
     B8_XF_Opt2_alcVCenter,
     B8_XF_Opt2_alcVBottom,
     B8_XF_Opt2_alcVJustify);
  WrapTexts:array[Boolean] of integer=(0,B8_XF_Opt2_fWrap);
  BorderLineStyles:array[TRLXLSLineStyleType] of word=
    (B8_XF_Border_None,
     B8_XF_Border_Thin,
     B8_XF_Border_Medium,
     B8_XF_Border_Dashed,
     B8_XF_Border_Dotted,
     B8_XF_Border_Thick,
     B8_XF_Border_Double,
     B8_XF_Border_Hair,
     B8_XF_Border_MediumDashed,
     B8_XF_Border_DashDot,
     B8_XF_Border_MediumDashDot,
     B8_XF_Border_DashDotDot,
     B8_XF_Border_MediumDashDotDot,
     B8_XF_Border_SlantedDashDot);
  function GetBorderColorIndex(b:TRLXLSBorderType):integer;
  begin
    if aRange.Borders[b].LineStyle=lsNone then
      Result:=0
    else
      Result:=GetColorPaletteIndex(aRange.Borders[b].Color)+8; // ???+8
  end;
var
  DiagBorderLineStyle :TRLXLSLineStyleType;
  DiagBorderColorIndex:integer;
begin
  FillChar(aXF,SizeOf(aXF),0);
  aXF.ifnt:=aRec.iFont;
  aXF.ifmt:=PXLSRangeRec(aRange.ExportData).iFormat;
  aXF.Opt1:=$0001;
  aXF.Opt2:=HorizontalAlignments[aRange.HorizontalAlignment] or
           WrapTexts[aRange.WrapText] or
           VerticalAlignments[aRange.VerticalAlignment];
  aXF.trot:=aRange.Rotation;
  aXF.Opt3:=B8_XF_Opt3_fAtrNum or
           B8_XF_Opt3_fAtrFnt or
           B8_XF_Opt3_fAtrAlc or
           B8_XF_Opt3_fAtrBdr or
           B8_XF_Opt3_fAtrPat;
  if (aRange.CellBounds.Left<>aRange.CellBounds.Right) or (aRange.CellBounds.Top<>aRange.CellBounds.Bottom) then
    aXF.Opt3:=aXF.Opt3 or B8_XF_Opt3_fMergeCell;
  //
  aXF.Borders1:=(BorderLineStyles[aRange.Borders[bdEdgeLeft].LineStyle]) or
                (BorderLineStyles[aRange.Borders[bdEdgeRight].LineStyle] shl 4) or
                (BorderLineStyles[aRange.Borders[bdEdgeTop].LineStyle] shl 8) or
                (BorderLineStyles[aRange.Borders[bdEdgeBottom].LineStyle] shl 12);
  DiagBorderLineStyle :=lsNone;
  DiagBorderColorIndex:=0;
  aXF.Borders2        :=0;
  if aRange.Borders[bdDiagonalDown].LineStyle<>lsNone then
  begin
    aXF.Borders2        :=aXF.Borders2 or $4000;
    DiagBorderLineStyle :=aRange.Borders[bdDiagonalDown].LineStyle;
    DiagBorderColorIndex:=GetColorPaletteIndex(aRange.Borders[bdDiagonalDown].Color)+8;
  end;
  if aRange.Borders[bdDiagonalUp].LineStyle<>lsNone then
  begin
    aXF.Borders2        :=aXF.Borders2 or $8000;
    DiagBorderLineStyle :=aRange.Borders[bdDiagonalUp].LineStyle;
    DiagBorderColorIndex:=GetColorPaletteIndex(aRange.Borders[bdDiagonalUp].Color)+8;
  end;
  aXF.Borders2:=aXF.Borders2 or (GetBorderColorIndex(bdEdgeLeft)) or (GetBorderColorIndex(bdEdgeRight) shl 7);
  aXF.Borders3:=(GetBorderColorIndex(bdEdgeTop)) or (GetBorderColorIndex(bdEdgeBottom) shl 7) or
                (DiagBorderColorIndex shl 14) or (BorderLineStyles[DiagBorderLineStyle] shl 21) or
                (FillPatterns[aRange.FillPattern] shl 26);
  aXF.Colors  :=GetColorPaletteIndex(aRange.ForegroundColor) or (GetColorPaletteIndex(aRange.BackgroundColor) shl 7);
end;

procedure TRLXLSFilter.BuildXFList(aDest:TList);
var
  sheet:TRLXLSWorksheet;
  range:TRLXLSRange;
  xf   :TRLXLSBiff8XF;
  i,j  :integer;
  k,n  :integer;
  p    :pointer;
begin
  n:=0;
  for i:=0 to fWorkbook.SheetCount-1 do
  begin
    sheet:=fWorkbook.Sheets[i];
    for j:=0 to sheet.RangeCount-1 do
    begin
      range:=sheet.Ranges[j];
      BuildXFRecord(range,xf,@fRangesRecs[n]);
      k:=0;
      while (k<aDest.Count) and not CompareMem(aDest[k],@xf,SizeOf(TRLXLSBiff8XF)) do
        Inc(k);
      if k<aDest.Count then
      else
      begin
        GetMem(p,SizeOf(TRLXLSBiff8XF));
        Move(xf,p^,SizeOf(TRLXLSBiff8XF));
        k:=aDest.Add(p);
      end;
      fRangesRecs[n].iXF:=k+15;
      Inc(n);
    end;
  end;
end;

function TRLXLSFilter.GetColorPaletteIndex(aColor:TColor):integer;
  function DefaultColorIndex(c:TColor):integer;
  begin
    Result:=0;
    while (Result<XLSMaxDefaultColors) and (XLSDefaultColors[Result]<>c) do 
      Inc(Result);
    if Result>=XLSMaxDefaultColors then
      Result:=-1;
  end;  
begin
  if (aColor and $80000000)<>0 then
    aColor:=ColorToRGB(aColor and $00FFFFFF);
  if fUsedColors.IndexOf(Pointer(aColor))=-1 then
    fUsedColors.Add(Pointer(aColor));
  Result:=0;
  while (Result<XLSMaxColorsInPalette) and (fColorPalette[Result]<>aColor) do
    Inc(Result);
  if Result<XLSMaxColorsInPalette then
    Exit;
  Result:=0;
  while Result<XLSMaxColorsInPalette do
  begin
    if (DefaultColorIndex(fColorPalette[Result])=-1) and (fUsedColors.IndexOf(Pointer(fColorPalette[Result]))=-1) then
    begin
      fColorPalette[Result]:=aColor;
      fPaletteModified     :=True;
      Exit;
    end;
    Inc(Result);
  end;
  Result:=1;
end;

function RangeSortCallback(Item1,Item2:Pointer):integer;
begin
  Result:=TRLXLSRange(Item1).CellBounds.Left-TRLXLSRange(Item2).CellBounds.Left;
end;

procedure TRLXLSFilter.WriteRangeToStream(aStream:TStream; aRange:TRLXLSRange;
  aCurrentRow:integer; var aIndexInCellsOffsArray:integer;
  var aCellsOffs:TRLXLSBiff8DBCELLCellsOffsArray);
var
  blank   :TRLXLSBiff8BLANK;
  i,left  :integer;
  number  :TRLXLSBiff8NUMBER;
  mulblank:PRLXLSBiff8MULBLANK;
  labelsst:TRLXLSBiff8LABELSST;
  procedure AddToCellsOffsArray;
  begin
    if aIndexInCellsOffsArray=0 then
      aCellsOffs[aIndexInCellsOffsArray]:=aStream.Position
    else
      aCellsOffs[aIndexInCellsOffsArray]:=aStream.Position-aCellsOffs[aIndexInCellsOffsArray-1];
    Inc(aIndexInCellsOffsArray);
  end;
begin
  left:=aRange.CellBounds.Left;
  if aCurrentRow=aRange.CellBounds.Top then
  begin
    AddToCellsOffsArray;
    case aRange.DataType of
      ctNumber:
        begin
          number.rw  :=aCurrentRow;
          number.col :=aRange.CellBounds.Left;
          number.ixfe:=PXLSRangeRec(aRange.ExportData).iXF;
          number.num :=StrToFloat(aRange.Value);
          WriteBIFF(aStream,B8_NUMBER,@number,SizeOf(TRLXLSBiff8NUMBER));
        end;
      ctString:
        begin
          labelsst.rw  :=aCurrentRow;
          labelsst.col :=aRange.CellBounds.Left;
          labelsst.ixfe:=PXLSRangeRec(aRange.ExportData).iXF;
          labelsst.isst:=PXLSRangeRec(aRange.ExportData).iSST;
          WriteBIFF(aStream,B8_LABELSST,@labelsst,SizeOf(labelsst));
        end;
    end;
    Inc(left);
  end;
  //
  if left<aRange.CellBounds.Right then
  begin
    AddToCellsOffsArray;
    mulblank:=AllocMem(SizeOf(TRLXLSBiff8MULBLANK)+(aRange.CellBounds.Right-left+1)*2+2);
    try
      mulblank.rw      :=aCurrentRow;
      mulblank.colFirst:=left;
      for i:=0 to aRange.CellBounds.Right-left do
        PWordArray(PChar(mulblank)+SizeOf(TRLXLSBiff8MULBLANK))^[i]:=PXLSRangeRec(aRange.ExportData).iXF;
      PWord(PChar(mulblank)+SizeOf(TRLXLSBiff8MULBLANK)+(aRange.CellBounds.Right-left+1)*2)^:=aRange.CellBounds.Right;
      WriteBIFF(aStream,B8_MULBLANK,mulblank,SizeOf(TRLXLSBiff8MULBLANK)+(aRange.CellBounds.Right-left+1)*2+2);
    finally
      FreeMem(mulblank);
    end;
  end
  else if left=aRange.CellBounds.Right then
  begin
    AddToCellsOffsArray;
    blank.rw  :=aCurrentRow;
    blank.col :=left;
    blank.ixfe:=PXLSRangeRec(aRange.ExportData).iXF;
    WriteBIFF(aStream,B8_BLANK,@blank,SizeOf(blank));
  end;
end;

procedure TRLXLSFilter.WriteSheetToStream(aStream:TStream; aSheet:TRLXLSWorksheet);
type
  TCardinalArray=array[0..0] of cardinal;
  PCardinalArray=^TCardinalArray;
var
  bof                  :TRLXLSBiff8BOF;
  calcmode             :TRLXLSBiff8CALCMODE;
  calccount            :TRLXLSBiff8CALCCOUNT;
  refmode              :TRLXLSBiff8REFMODE;
  iteration            :TRLXLSBiff8ITERATION;
  saverecalc           :TRLXLSBiff8SAVERECALC;
  printheaders         :TRLXLSBiff8PRINTHEADERS;
  printgridlines       :TRLXLSBiff8PRINTGRIDLINES;
  gridset              :TRLXLSBiff8GRIDSET;
  guts                 :TRLXLSBiff8GUTS;
  defaultrowheight     :TRLXLSBiff8DEFAULTROWHEIGHT;
  wsbool               :TRLXLSBiff8WSBOOL;
  hcenter              :TRLXLSBiff8HCENTER;
  vcenter              :TRLXLSBiff8VCENTER;
  defcolwidth          :TRLXLSBiff8DEFCOLWIDTH;
  dimensions           :TRLXLSBiff8Dimensions;
  window2              :TRLXLSBiff8WINDOW2;
  selection            :PRLXLSBiff8SELECTION;
  header               :PRLXLSBiff8HEADER;
  footer               :PRLXLSBiff8FOOTER;
  INDEXOffs            :integer;
  BlocksInSheet        :integer;
  IndexInDBCELLsOffs   :integer;
  dbcell               :TRLXLSBiff8DBCELLfull;
  IndexInCellsOffsArray:integer;
  ms                   :TMemoryStream;
  FirstRowOffs         :integer;
  SecondRowOffs        :integer;
  merge                :PRLXLSBiff8MERGE;
  colinfo              :TRLXLSBiff8ColumnInfo;
  leftmargin           :TRLXLSBiff8LEFTMARGIN;
  rightmargin          :TRLXLSBiff8RIGHTMARGIN;
  topmargin            :TRLXLSBiff8TOPMARGIN;
  bottommargin         :TRLXLSBiff8BOTTOMMARGIN;
  setup                :TRLXLSBiff8SETUP;
  index                :PRLXLSBiff8INDEX;
  room                 :integer;
  l                    :TList;
  range                :TRLXLSRange;
  rw                   :TRLXLSRow;
  row                  :TRLXLSBiff8Row;
  bc,i,j               :integer;
  aux                  :string;
begin
  FillChar(bof,SizeOf(bof),0);
  bof.vers    :=B8_BOF_vers;
  bof.dt      :=B8_BOF_dt_Worksheet;
  bof.rupBuild:=B8_BOF_rupBuild_Excel97;
  bof.rupYear :=B8_BOF_rupYear_Excel07;
  WriteBIFF(aStream,B8_BOF,@bof,SizeOf(bof));
  //
  if (aSheet.CellBounds.Bottom<>-1) and (aSheet.CellBounds.Top<>-1) then
  begin
    BlocksInSheet:=(aSheet.CellBounds.Bottom-aSheet.CellBounds.Top+1) div XLSMaxRowsInBlock;
    if (aSheet.CellBounds.Bottom=aSheet.CellBounds.Top) or (((aSheet.CellBounds.Bottom-aSheet.CellBounds.Top+1) mod XLSMaxRowsInBlock)<>0) then
      Inc(BlocksInSheet);
  end
  else
    BlocksInSheet:=0;
  //
  index:=AllocMem(SizeOf(TRLXLSBiff8INDEX)+BlocksInSheet*4);
  try
    if (aSheet.CellBounds.Bottom<>-1) and (aSheet.CellBounds.Top<>-1) then
    begin
      index.rwMic:=aSheet.CellBounds.Top;
      index.rwMac:=aSheet.CellBounds.Bottom+1;
    end;
    INDEXOffs         :=aStream.Position;
    IndexInDBCELLsOffs:=0;
    WriteBIFF(aStream,B8_INDEX,index,SizeOf(TRLXLSBiff8INDEX)+BlocksInSheet*4);
    //
    calcmode.fAutoRecalc:=1;
    WriteBIFF(aStream,B8_CALCMODE,@calcmode,SizeOf(calcmode));
    calccount.cIter     :=$0064;
    WriteBIFF(aStream,B8_CALCCOUNT,@calccount,SizeOf(calccount));
    refmode.fRefA1      :=$0001;
    WriteBIFF(aStream,B8_REFMODE,@refmode,SizeOf(refmode));
    iteration.fIter     :=$0000;
    WriteBIFF(aStream,B8_ITERATION,@iteration,SizeOf(iteration));
    //
    aux:=HexToString('10 00 08 00 fc a9 f1 d2 4d 62 50 3f');
    StreamWrite(aStream,aux[1],length(aux));
    //
    saverecalc.fSaveRecalc:=$0001;
    WriteBIFF(aStream,B8_SAVERECALC,@saverecalc,SizeOf(saverecalc));
    //
    if aSheet.Workbook.PageSetup.PrintHeaders then
      printheaders.fPrintRwCol:=1
    else
      printheaders.fPrintRwCol:=0;
    WriteBIFF(aStream,B8_PRINTHEADERS,@printheaders,SizeOf(printheaders));
    //
    if aSheet.Workbook.PageSetup.PrintGridLines then
      printgridlines.fPrintGrid:=1
    else
      printgridlines.fPrintGrid:=0;
    WriteBIFF(aStream,B8_PRINTGRIDLINES,@printgridlines,SizeOf(printgridlines));
    //
    gridset.fGridSet:=$0001;
    WriteBIFF(aStream,B8_GRIDSET,@gridset,SizeOf(gridset));
    //
    FillChar(guts,SizeOf(guts),0);
    WriteBIFF(aStream,B8_GUTS,@guts,SizeOf(guts));
    //
    defaultrowheight.grbit:=$0000;
    defaultrowheight.miyRw:=DefaultCellHeight;
    WriteBIFF(aStream,B8_DEFAULTROWHEIGHT,@defaultrowheight,SizeOf(defaultrowheight));
    //  
    wsbool.grbit:=$04C1;
    WriteBIFF(aStream,B8_WSBOOL,@wsbool,SizeOf(wsbool));
    //
    aux:=emptystr;
    if aSheet.Workbook.PageSetup.LeftHeader<>emptystr then
      aux:=aux+'&L'+aSheet.Workbook.PageSetup.LeftHeader;
    if aSheet.Workbook.PageSetup.CenterHeader<>emptystr then
      aux:=aux+'&C'+aSheet.Workbook.PageSetup.CenterHeader;
    if aSheet.Workbook.PageSetup.RightHeader<>emptystr then
      aux:=aux+'&R'+aSheet.Workbook.PageSetup.RightHeader;
    if aux<>'' then
    begin
      room:=WideStringSize(aux);
      GetMem(header,SizeOf(TRLXLSBiff8HEADER)+room);
      try
        header.cch     :=Length(aux);
        header.cchgrbit:=1;
        StringToWideChar(aux,PWideChar(Integer(header)+SizeOf(TRLXLSBiff8HEADER)),room);
        WriteBIFF(aStream,B8_HEADER,header,SizeOf(TRLXLSBiff8HEADER)+room);
      finally
        FreeMem(header);
      end;
    end;

    aux:=emptystr;
    if aSheet.Workbook.PageSetup.LeftFooter<>emptystr then
      aux:=aux+'&L'+aSheet.Workbook.PageSetup.LeftFooter;
    if aSheet.Workbook.PageSetup.CenterFooter<>emptystr then
      aux:=aux+'&C'+aSheet.Workbook.PageSetup.CenterFooter;
    if aSheet.Workbook.PageSetup.RightFooter<>emptystr then
      aux:=aux+'&R'+aSheet.Workbook.PageSetup.RightFooter;
    if aux<>emptystr then
    begin
      room:=WideStringSize(aux);
      GetMem(footer,SizeOf(TRLXLSBiff8FOOTER)+room);
      try
        footer.cch     :=Length(aux);
        footer.cchgrbit:=1;
        StringToWideChar(aux,PWideChar(Integer(footer)+SizeOf(TRLXLSBiff8HEADER)),room);
        WriteBIFF(aStream,B8_FOOTER,footer,SizeOf(TRLXLSBiff8FOOTER)+room);
      finally
        FreeMem(footer);
      end;
    end;

    if aSheet.Workbook.PageSetup.CenterHorizontally then
      hcenter.fHCenter:=1
    else
      hcenter.fHCenter:=0;
    WriteBIFF(aStream,B8_HCENTER,@hcenter,SizeOf(hcenter));

    if aSheet.Workbook.PageSetup.CenterVertically then
      vcenter.fVCenter:=1
    else
      vcenter.fVCenter:=0;
    WriteBIFF(aStream,B8_VCENTER,@vcenter,SizeOf(vcenter));

    leftmargin.num:=aSheet.Workbook.PageSetup.LeftMargin/2.54;
    WriteBIFF(aStream,B8_LEFTMARGIN,@leftmargin,SizeOf(TRLXLSBiff8LEFTMARGIN));
    rightmargin.num:=aSheet.Workbook.PageSetup.RightMargin/2.54;
    WriteBIFF(aStream,B8_RIGHTMARGIN,@rightmargin,SizeOf(TRLXLSBiff8RIGHTMARGIN));
    topmargin.num:=aSheet.Workbook.PageSetup.TopMargin/2.54;
    WriteBIFF(aStream,B8_TOPMARGIN,@topmargin,SizeOf(TRLXLSBiff8TOPMARGIN));
    bottommargin.num:=aSheet.Workbook.PageSetup.BottomMargin/2.54;
    WriteBIFF(aStream,B8_BOTTOMMARGIN,@bottommargin,SizeOf(TRLXLSBiff8BOTTOMMARGIN));
    //
    FillChar(setup,SizeOf(TRLXLSBiff8SETUP),0);
    setup.iPaperSize:=word(aSheet.Workbook.PageSetup.PaperSize);
    setup.iPageStart:=aSheet.Workbook.PageSetup.FirstPageNumber;
    setup.iFitWidth :=Byte(aSheet.Workbook.PageSetup.FitToPagesWide);
    setup.iFitHeight:=Byte(aSheet.Workbook.PageSetup.FitToPagesTall);
    setup.numHdr    :=aSheet.Workbook.PageSetup.HeaderMargin/2.54;
    setup.numFtr    :=aSheet.Workbook.PageSetup.FooterMargin/2.54;
    setup.iCopies   :=aSheet.Workbook.PageSetup.Copies;
    setup.iScale    :=aSheet.Workbook.PageSetup.Zoom;
    //
    if aSheet.Workbook.PageSetup.Order=odOverThenDown then
      setup.grbit:=setup.grbit or B8_SETUP_fLeftToRight;
    if aSheet.Workbook.PageSetup.Orientation=orPortrait then
      setup.grbit:=setup.grbit or B8_SETUP_fLandscape;
    if aSheet.Workbook.PageSetup.BlackAndWhite then
      setup.grbit:=setup.grbit or B8_SETUP_fNoColor;
    if aSheet.Workbook.PageSetup.Draft then
      setup.grbit:=setup.grbit or B8_SETUP_fDraft;
    if aSheet.Workbook.PageSetup.PrintNotes then
      setup.grbit:=setup.grbit or B8_SETUP_fNotes;
    if aSheet.Workbook.PageSetup.FirstPageNumber<>1 then
      setup.grbit:=setup.grbit or B8_SETUP_fUsePage;
    WriteBIFF(aStream,B8_SETUP,@setup,SizeOf(TRLXLSBiff8SETUP));
    //
    defcolwidth.cchdefColWidth:=DefaultCellLength;
    WriteBIFF(aStream,B8_DEFCOLWIDTH,@defcolwidth,SizeOf(defcolwidth));
    //
    for i:=0 to aSheet.ColCount-1 do
      with aSheet.FindCol(i,True) do
      begin
        FillChar(colinfo,SizeOf(colinfo),0);
        colinfo.colFirst:=Index;
        colinfo.colLast :=Index;
        colinfo.coldx   :=Width;
        WriteBIFF(aStream,B8_COLINFO,@colinfo,SizeOf(colinfo));
      end;
    //  
    FillChar(dimensions,SizeOf(dimensions),0);
    if (aSheet.CellBounds.Left<>-1) and (aSheet.CellBounds.Right<>-1) and (aSheet.CellBounds.Top<>-1) and (aSheet.CellBounds.Bottom<>-1) then
    begin
      dimensions.rwMic :=aSheet.CellBounds.Top;
      dimensions.rwMac :=aSheet.CellBounds.Bottom+1;
      dimensions.colMic:=aSheet.CellBounds.Left;
      dimensions.colMac:=aSheet.CellBounds.Right+1;
    end;
    WriteBIFF(aStream,B8_DIMENSIONS,@dimensions,SizeOf(dimensions));
    //
    if (aSheet.CellBounds.Top<>-1) and (aSheet.CellBounds.Bottom<>-1) then
    begin
      l:=TList.Create;
      ms:=TMemoryStream.Create;
      try
        bc:=0;
        FirstRowOffs:=0;
        SecondRowOffs:=0;
        for i:=aSheet.CellBounds.Top to aSheet.CellBounds.Bottom do
        begin
          l.Clear;
          for j:=0 to aSheet.RangeCount-1 do
          begin
            range:=aSheet.Ranges[j];
            if (range.CellBounds.Top<=i) and (i<=range.CellBounds.Bottom) then
              l.Add(range);
          end;
          l.Sort(RangeSortCallback);
          //
          if bc=0 then
            FirstRowOffs:=aStream.Position;
          FillChar(row,SizeOf(row),0);  
          row.rw:=i;
          if l.Count>0 then
          begin
            row.colMic:=TRLXLSRange(l[0]).CellBounds.Left;
            row.colMac:=TRLXLSRange(l[l.Count-1]).CellBounds.Right+1;
          end
          else
          begin
            row.colMic:=0;
            row.colMac:=0;
          end;
          //
          rw:=aSheet.FindRow(i,False);
          if rw=nil then
          begin
            row.miyRw:=DefaultCellHeight;
            row.grbit:=0;
          end
          else
          begin
            row.miyRw:=rw.Height*20;
            row.grbit:=B8_ROW_grbit_fUnsynced;
          end;
          WriteBIFF(aStream,B8_ROW,@row,SizeOf(row));
          if bc=0 then
            SecondRowOffs:=aStream.Position;
          //
          IndexInCellsOffsArray:=0;
          for j:=0 to l.Count-1 do
            WriteRangeToStream(ms,TRLXLSRange(l[j]),i,IndexInCellsOffsArray,dbcell.CellsOffs);
          Inc(bc);
          if (bc=XLSMaxRowsInBlock) or (i=aSheet.CellBounds.Bottom) then
          begin
            dbcell.CellsOffs[0]:=aStream.Position-SecondRowOffs;
            ms.SaveToStream(aStream);
            PCardinalArray(PChar(index)+SizeOf(TRLXLSBiff8INDEX))^[IndexInDBCELLsOffs]:=aStream.Position-FBOFOffs;
            Inc(IndexInDBCELLsOffs);
            dbcell.dbRtrw:=aStream.Position-FirstRowOffs;
            WriteBIFF(aStream,B8_DBCELL,@dbcell,
              SizeOf(TRLXLSBiff8DBCELL)+IndexInCellsOffsArray*2);
            //
            ms.Clear;
            bc:=0;
          end;
        end;
      finally
        l.Free;
        ms.Free;
      end;
      //
      aStream.Position:=INDEXOffs;
      WriteBIFF(aStream,B8_INDEX,index,SizeOf(TRLXLSBiff8INDEX)+BlocksInSheet*4);
      aStream.Seek(0,soFromEnd);
    end;
  finally
    FreeMem(index);
  end;

  FillChar(window2,SizeOf(window2),0);
  window2.grbit:=B8_WINDOW2_grbit_fPaged or
                 B8_WINDOW2_grbit_fDspGuts or
                 B8_WINDOW2_grbit_fDspZeros or
                 B8_WINDOW2_grbit_fDefaultHdr or
                 B8_WINDOW2_grbit_fDspGrid or
                 B8_WINDOW2_grbit_fDspRwCol;
  if aSheet.Index=0 then
    window2.grbit:=window2.grbit+B8_WINDOW2_grbit_fSelected;
  window2.rwTop:=0;
  window2.colLeft:=0;
  window2.icvHdr:=$00000040;
  window2.wScaleSLV:=0;
  window2.wScaleNormal:=0;
  WriteBIFF(aStream,B8_WINDOW2,@window2,SizeOf(window2));
  //
  selection:=AllocMem(SizeOf(TRLXLSBiff8SELECTION)+6);
  try
    selection.pnn :=3;
    selection.cref:=1;
    WriteBIFF(aStream,B8_SELECTION,selection,SizeOf(TRLXLSBiff8SELECTION)+6);
  finally
    FreeMem(selection);
  end;
  //
  if aSheet.RangeCount>0 then
  begin
    j:=0;
    for i:=0 to aSheet.RangeCount-1 do
    begin
      range:=aSheet.Ranges[i];
      if (range.CellBounds.Left<>range.CellBounds.Right) or
         (range.CellBounds.Top<>range.CellBounds.Bottom) then
        Inc(j);
    end;
    if j>0 then
    begin
      merge:=AllocMem(SizeOf(TRLXLSBiff8MERGE)+j*8);
      try
        merge.cnt:=j;
        j:=0;
        for i:=0 to aSheet.RangeCount-1 do
        begin
          range:=aSheet.Ranges[i];
          if (range.CellBounds.Left<>range.CellBounds.Right) or
             (range.CellBounds.Top<>range.CellBounds.Bottom) then
          begin
            with PRLXLSBiff8MERGErec(PChar(merge)+SizeOf(TRLXLSBiff8MERGE)+j*8)^ do
            begin
              left  :=range.CellBounds.Left;
              top   :=range.CellBounds.Top;
              right :=range.CellBounds.Right;
              bottom:=range.CellBounds.Bottom;
            end;
            Inc(j);
          end;
        end;
        WriteBIFF(aStream,B8_MERGE,merge,SizeOf(TRLXLSBiff8MERGE)+j*8);
      finally
        FreeMem(merge);
      end;
    end;
  end;
  WriteBIFF(aStream,B8_EOF,nil,0);
end;

procedure TRLXLSFilter.WriteBookToStream(aStream:TStream);
var
  codepage        :TRLXLSBiff8CodePage;
  interfachdr     :TRLXLSBiff8InterfaceHeader;
  fngroupcount    :TRLXLSBiff8FNGroupCount;
  windowprotect   :TRLXLSBiff8WindowProtect;
  protect         :TRLXLSBiff8Protect;
  password        :TRLXLSBiff8Password;
  backup          :TRLXLSBiff8BACKUP;
  hideobj         :TRLXLSBiff8HIDEOBJ;
  s1904           :TRLXLSBiff81904;
  precision       :TRLXLSBiff8PRECISION;
  bookbool        :TRLXLSBiff8BOOKBOOL;
  writeaccess     :TRLXLSBiff8WRITEACCESS;
  doublestreamfile:TRLXLSBiff8DOUBLESTREAMFILE;
  prot4rev        :TRLXLSBiff8PROT4REV;
  prot4revpass    :TRLXLSBiff8PROT4REVPASS;
  window1         :TRLXLSBiff8WINDOW1;
  refreshall      :TRLXLSBiff8REFRESHALL;
  useselfs        :TRLXLSBiff8USESELFS;
  boundsheet      :PRLXLSBiff8BOUNDSHEET;
  country         :TRLXLSBiff8COUNTRY;
  palette         :TRLXLSBiff8PALETTE;
  extsst          :PRLXLSBiff8EXTSST;
  bof             :TRLXLSBiff8BOF;
  mms             :TRLXLSBiff8MMS;
  sst,sstbuf      :PChar;
  sstsizeoffset   :integer;
  ltitleoffset    :integer;
  sstblockoffset  :integer;
  lsstbuf         :integer;
  sstsize         :integer;
  extsstsize      :integer;
  ltitle          :integer;
  i,j,k,m         :integer;
  buf             :pointer;
  sz              :word;
  sl              :TStringList;
  sheet           :TRLXLSWorksheet;
  aux             :string;
  l               :TList;
begin
  j:=0;
  for i:=0 to fWorkbook.SheetCount-1 do
    j:=j+fWorkbook.Sheets[i].RangeCount;
  GetMem(fRangesRecs,j*SizeOf(TRLXLSRangeRec));
  GetMem(fSheetsRecs,fWorkbook.SheetCount*SizeOf(TRLXLSSheetRec));
  try
    Move(XLSDefaultColorPalette[0],fColorPalette[0],XLSMaxColorsInPalette*4);
    fPaletteModified:=False;
    fUsedColors.Clear;

    FBOFOffs:=aStream.Position;

    FillChar(bof,SizeOf(bof),0);
    bof.vers    :=B8_BOF_vers;
    bof.dt      :=B8_BOF_dt_WorkbookGlobals;
    bof.rupBuild:=B8_BOF_rupBuild_Excel97;
    bof.rupYear :=B8_BOF_rupYear_Excel07;
    bof.sfo     :=B8_BOF_vers;
    WriteBIFF(aStream,B8_BOF,@bof,SizeOf(bof));

    FillChar(interfachdr,SizeOf(interfachdr),0);
    interfachdr.cv:=B8_INTERFACHDR_cv_ANSI;
    WriteBIFF(aStream,B8_INTERFACHDR,@interfachdr,SizeOf(interfachdr));

    FillChar(mms,SizeOf(mms),0);
    WriteBIFF(aStream,B8_MMS,@mms,SizeOf(mms));
    WriteBIFF(aStream,B8_INTERFACEND,nil,0);
    
    FillChar(writeaccess,SizeOf(writeaccess),32);
    StringToWideChar(WorkBook.UserName,@writeaccess.stName,sizeof(writeaccess));
    WriteBIFF(aStream,B8_WRITEACCESS,@writeaccess,SizeOf(writeaccess));

    codepage.cv:=B8_CODEPAGE_cv_ANSI;
    WriteBIFF(aStream,B8_CODEPAGE,@codepage,SizeOf(codepage));
  
    doublestreamfile.fDSF:=0;
    WriteBIFF(aStream,B8_DOUBLESTREAMFILE,@doublestreamfile,SizeOf(doublestreamfile));
  
    WriteBIFF(aStream,$01C0,nil,0);

    GetMem(buf,WorkBook.SheetCount*2);
    try
      for i:=0 to WorkBook.SheetCount-1 do
        PWordArray(buf)^[i]:=i;
      WriteBIFF(aStream,B8_TABID,buf,WorkBook.SheetCount*2);
    finally
      FreeMem(buf);
    end;
    
    fngroupcount.cFnGroup:=$000E;
    WriteBIFF(aStream,B8_FNGROUPCOUNT,@fngroupcount,SizeOf(fngroupcount));
  
    windowprotect.fLockWn:=0; 
    WriteBIFF(aStream,B8_WINDOWPROTECT,@windowprotect,SizeOf(windowprotect));
  
    protect.fLock:=0;
    WriteBIFF(aStream,B8_PROTECT,@protect,SizeOf(protect));
  
    password.wPassword:=0; 
    WriteBIFF(aStream,B8_PASSWORD,@password,SizeOf(password));
  
    prot4rev.fRevLock:=0; 
    WriteBIFF(aStream,B8_PROT4REV,@prot4rev,SizeOf(prot4rev));
  
    prot4revpass.wrevPass:=0; 
    WriteBIFF(aStream,B8_PROT4REVPASS,@prot4revpass,SizeOf(prot4revpass));

    FillChar(window1,SizeOf(window1),0);
    window1.xWn      :=$0168;
    window1.yWn      :=$001E;
    window1.dxWn     :=$1D1E;
    window1.dyWn     :=$1860;
    window1.grbit    :=$0038;
    window1.itabCur  :=$0000;
    window1.itabFirst:=$0000;
    window1.ctabSel  :=$0001;
    window1.wTabRatio:=$0258;
    WriteBIFF(aStream,B8_WINDOW1,@window1,SizeOf(window1));
  
    backup.fBackupFile:=0;  
    WriteBIFF(aStream,B8_BACKUP,@backup,SizeOf(backup));
  
    hideobj.fHideObj:=0;  
    WriteBIFF(aStream,B8_HIDEOBJ,@hideobj,SizeOf(hideobj));
  
    s1904.f1904:=0; 
    WriteBIFF(aStream,B8_1904,@s1904,SizeOf(s1904));
  
    precision.fFullPrec:=1; 
    WriteBIFF(aStream,B8_PRECISION,@precision,SizeOf(precision));
  
    refreshall.fRefreshAll:=0;
    WriteBIFF(aStream,B8_REFRESHALL,@refreshall,SizeOf(refreshall));
  
    bookbool.fNoSaveSupp:=0;
    WriteBIFF(aStream,B8_BOOKBOOL,@bookbool,SizeOf(bookbool));

    // SAVE FONTS
    l:=TList.Create;
    try
      for i:=0 to 3 do
        with TFont(l[l.Add(TFont.Create)]) do
        begin
          Name:=DefaultFontName;
          Size:=10;
        end;
      BuildFontList(l);
      for i:=0 to l.Count-1 do
        WriteBIFFFont(aStream,TFont(l[i]),GetColorPaletteIndex(TFont(l[i]).Color));
    finally
      for i:=0 to l.Count-1 do
        TFont(l[i]).Free;
      l.Free;
    end;

    // SAVE FORMATS
    sl:=TStringList.Create;
    try
      sl.AddObject('#,##0"ð.";\-#,##0"ð."',Pointer($0005));
      sl.AddObject('#,##0"ð.";[Red]\-#,##0"ð."',Pointer($0006));
      sl.AddObject('#,##0.00"ð.";\-#,##0.00"ð."',Pointer($0007));
      sl.AddObject('#,##0.00"ð.";[Red]\-#,##0.00"ð."',Pointer($0008));
      sl.AddObject('_-* #,##0"ð."_-;\-* #,##0"ð."_-;_-* "-""ð."_-;_-@_-',Pointer($002A));
      sl.AddObject('_-* #,##0_ð_._-;\-* #,##0_ð_._-;_-* "-"_ð_._-;_-@_-',Pointer($0029));
      sl.AddObject('_-* #,##0.00"ð."_-;\-* #,##0.00"ð."_-;_-* "-"??"ð."_-;_-@_-', Pointer($002C));
      sl.AddObject('_-* #,##0.00_ð_._-;\-* #,##0.00_ð_._-;_-* "-"??_ð_._-;_-@_-', Pointer($002B));
      BuildFormatList(sl);
      for i:=0 to sl.Count-1 do
        WriteBIFFFormat(aStream,sl[i],word(sl.Objects[i]));
    finally
      sl.Free;
    end;

    // SAVE STYLE
    WriteBIFFHexString(aStream,'e0 00 14 00 00 00 00 00 f5 ff 20 00 00 00 00 00 00 00 00 00 00 00 c0 20');
    WriteBIFFHexString(aStream,'e0 00 14 00 01 00 00 00 f5 ff 20 00 00 f4 00 00 00 00 00 00 00 00 c0 20');
    WriteBIFFHexString(aStream,'e0 00 14 00 01 00 00 00 f5 ff 20 00 00 f4 00 00 00 00 00 00 00 00 c0 20');
    WriteBIFFHexString(aStream,'e0 00 14 00 02 00 00 00 f5 ff 20 00 00 f4 00 00 00 00 00 00 00 00 c0 20');
    WriteBIFFHexString(aStream,'e0 00 14 00 02 00 00 00 f5 ff 20 00 00 f4 00 00 00 00 00 00 00 00 c0 20');
    WriteBIFFHexString(aStream,'e0 00 14 00 00 00 00 00 f5 ff 20 00 00 f4 00 00 00 00 00 00 00 00 c0 20');
    WriteBIFFHexString(aStream,'e0 00 14 00 00 00 00 00 f5 ff 20 00 00 f4 00 00 00 00 00 00 00 00 c0 20');
    WriteBIFFHexString(aStream,'e0 00 14 00 00 00 00 00 f5 ff 20 00 00 f4 00 00 00 00 00 00 00 00 c0 20');
    WriteBIFFHexString(aStream,'e0 00 14 00 00 00 00 00 f5 ff 20 00 00 f4 00 00 00 00 00 00 00 00 c0 20');
    WriteBIFFHexString(aStream,'e0 00 14 00 00 00 00 00 f5 ff 20 00 00 f4 00 00 00 00 00 00 00 00 c0 20');
    WriteBIFFHexString(aStream,'e0 00 14 00 00 00 00 00 f5 ff 20 00 00 f4 00 00 00 00 00 00 00 00 c0 20');
    WriteBIFFHexString(aStream,'e0 00 14 00 00 00 00 00 f5 ff 20 00 00 f4 00 00 00 00 00 00 00 00 c0 20');
    WriteBIFFHexString(aStream,'e0 00 14 00 00 00 00 00 f5 ff 20 00 00 f4 00 00 00 00 00 00 00 00 c0 20');
    WriteBIFFHexString(aStream,'e0 00 14 00 00 00 00 00 f5 ff 20 00 00 f4 00 00 00 00 00 00 00 00 c0 20');
    WriteBIFFHexString(aStream,'e0 00 14 00 00 00 00 00 f5 ff 20 00 00 f4 00 00 00 00 00 00 00 00 c0 20');
  
    // XF
    l:=TList.Create;
    try
      aux:=HexToString('00 00 00 00 01 00 20 00 00 00 00 00 00 00 00 00 00 00 c0 20');
      GetMem(buf,Length(aux));
      Move(aux[1],buf^,Length(aux));
      l.Add(buf);
      BuildXFList(l);
      for i:=0 to l.Count-1 do
        WriteBIFF(aStream,B8_XF,l[i],SizeOf(TRLXLSBiff8XF));
    finally
      for i:=0 to l.Count-1 do
        FreeMem(l[i]);
      l.Free;
    end;
  
    // SAVE PALETTE
    if fPaletteModified then
    begin
      palette.ccv:=XLSMaxColorsInPalette;
      for i:=0 to XLSMaxColorsInPalette-1 do
        palette.colors[i]:=fColorPalette[i];
      WriteBIFF(aStream,B8_PALETTE,@palette,SizeOf(palette));
    end;
  
    WriteBIFFHexString(aStream,'93 02 04 00 10 80 04 FF');
    WriteBIFFHexString(aStream,'93 02 04 00 11 80 07 FF');
    WriteBIFFHexString(aStream,'93 02 04 00 00 80 00 FF');
    WriteBIFFHexString(aStream,'93 02 04 00 12 80 05 FF');
    WriteBIFFHexString(aStream,'93 02 04 00 13 80 03 FF');
    WriteBIFFHexString(aStream,'93 02 04 00 14 80 06 FF');
  
    useselfs.fUsesElfs:=0;
    WriteBIFF(aStream,B8_USESELFS,@useselfs,SizeOf(useselfs));
  
    // SAVE SHEETS
    for i:=0 to fWorkbook.SheetCount-1 do
    begin
      sheet     :=fWorkbook.Sheets[i];
      fSheetsRecs[i].StreamBOFOffsetPosition:=aStream.Position+4;
      ltitle    :=WideStringSize(sheet.Title);
      boundsheet:=AllocMem(SizeOf(TRLXLSBiff8BOUNDSHEET)+ltitle);
      try
        boundsheet.grbit   :=0;
        boundsheet.cch     :=Length(sheet.Title);
        boundsheet.cchgrbit:=1;
        if boundsheet.cch>0 then
          StringToWideChar(sheet.Title,PWideChar(Integer(boundsheet)+SizeOf(TRLXLSBiff8BOUNDSHEET)),ltitle);
        WriteBIFF(aStream,B8_BOUNDSHEET,boundsheet,SizeOf(TRLXLSBiff8BOUNDSHEET)+ltitle);
      finally
        FreeMem(boundsheet);
      end;
    end;
  
    country.iCountryDef   :=$07;
    country.iCountryWinIni:=$07;
    WriteBIFF(aStream,B8_COUNTRY,@country,SizeOf(country));
  
    // SST TABLE
    extsstsize :=SizeOf(TRLXLSBiff8EXTSST);
    extsst     :=AllocMem(extsstsize);
    extsst.Dsst:=8;
  
    sstsize       :=SizeOf(TRLXLSBiff8SST)+4;
    sst           :=AllocMem(sstsize);
    PWord(sst)^   :=B8_SST;
    sstsizeoffset :=2;
    PWord(sst+sstsizeoffset)^:=SizeOf(TRLXLSBiff8SST);
    sstblockoffset:=sstsize;
    lsstbuf       :=0;
    sstbuf        :=nil;
  
    k:=0;
    m:=0;
    try
      for i:=0 to fWorkbook.SheetCount-1 do
      begin
        sheet:=fWorkbook.Sheets[i];
        for j:=0 to sheet.RangeCount-1 do
        begin
          if sheet.Ranges[j].DataType=ctString then
          begin
            aux:=sheet.Ranges[j].Value;
            if aux<>emptystr then
            begin
              fRangesRecs[m].iSST:=k;
              Inc(k);
              //
              ltitle:=WideStringSize(aux);
              if lsstbuf<ltitle then
              begin
                lsstbuf:=ltitle;
                ReallocMem(sstbuf,lsstbuf);
              end;
              StringToWideChar(aux,PWideChar(sstbuf),ltitle);

              if MaxBiffRecordSize-sstblockoffset<=4 then
              begin
                ReallocMem(sst,sstsize+4);
                PWord(sst+sstsize)^:=B8_CONTINUE;
                sstsize            :=sstsize+2;
                sstsizeoffset      :=sstsize;
                PWord(sst+sstsize)^:=0;
                sstsize            :=sstsize+2;
                sstblockoffset     :=4;
              end;

              if (k mod 8)=1 then
              begin
                ReallocMem(extsst,extsstsize+SizeOf(TRLXLSBiff8ISSTINF));
                PRLXLSBiff8ISSTINF(PChar(extsst)+extsstsize).cb  :=sstblockoffset;
                PRLXLSBiff8ISSTINF(PChar(extsst)+extsstsize).ib  :=aStream.Position+sstsize;
                PRLXLSBiff8ISSTINF(PChar(extsst)+extsstsize).res1:=0;
                extsstsize:=extsstsize+SizeOf(TRLXLSBiff8ISSTINF);
              end;

              ReallocMem(sst,sstsize+3);
              PWord(sst+sstsize)^      :=Length(aux);
              sstsize                  :=sstsize+2;
              PByte(sst+sstsize)^      :=1;
              sstsize                  :=sstsize+1;
              PWord(sst+sstsizeoffset)^:=PWord(sst+sstsizeoffset)^ +3;
              sstblockoffset:=sstblockoffset+3;

              ltitleoffset:=0;
              repeat
                sz            :=(Min(ltitle-ltitleoffset,MaxBiffRecordSize-sstblockoffset)) and (not 1);
                ReallocMem(sst,sstsize+sz);
                Move(Pointer(Integer(sstbuf)+ltitleoffset)^,Pointer(Integer(sst)+sstsize)^,sz);
                sstsize       :=sstsize+sz;
                sstblockoffset:=sstblockoffset+sz;
                ltitleoffset  :=ltitleoffset+sz;
                PWord(sst+sstsizeoffset)^:=PWord(sst+sstsizeoffset)^ +sz;
                if (ltitle>ltitleoffset) and ((MaxBiffRecordSize-sstblockoffset)<=4) then
                begin
                  ReallocMem(sst,sstsize+5);
                  PWord(sst+sstsize)^:=B8_CONTINUE;
                  sstsize            :=sstsize+2;
                  sstsizeoffset      :=sstsize;
                  PWord(sst+sstsize)^:=1;
                  sstsize            :=sstsize+2;
                  PByte(sst+sstsize)^:=1;
                  sstsize            :=sstsize+1;
                  sstblockoffset     :=5;
                end;
              until ltitle<=ltitleoffset;
            end;
          end;
          Inc(m);
        end;
      end;
      if k<>0 then
      begin
        PRLXLSBiff8SST(sst+4).cstTotal :=k;
        PRLXLSBiff8SST(sst+4).cstUnique:=k;
        StreamWrite(aStream,sst^,sstsize);
        WriteBIFF(aStream,B8_EXTSST,extsst,extsstsize);
      end;
    finally
      FreeMem(sst);
      FreeMem(sstbuf);
      FreeMem(extsst);
    end;
  
    WriteBIFF(aStream,B8_EOF,nil,0);
    //
    for i:=0 to fWorkbook.SheetCount-1 do
    begin
      sheet:=fWorkbook.Sheets[i];
      fSheetsRecs[i].StreamBOFOffset:=aStream.Position;
      WriteSheetToStream(aStream,sheet);
    end;
    //
    for i:=0 to fWorkbook.SheetCount-1 do
    begin
      aStream.Position:=fSheetsRecs[i].StreamBOFOffsetPosition;
      StreamWrite(aStream,fSheetsRecs[i].StreamBOFOffset,4);
    end;
  finally
    fUsedColors.Clear;
    FreeMem(fRangesRecs);
    fRangesRecs:=nil;
    FreeMem(fSheetsRecs);
    fSheetsRecs:=nil;
  end;
end;

procedure TRLXLSFilter.WriteStorageToStream(aStream:TStream);
type
  CHAR8  =packed array[0..7] of char;
  CHAR16 =packed array[0..15] of char;
  CHAR64 =packed array[0..32*SizeOf(WideChar)-1] of char;
  SECT109=packed array[0..108] of cardinal;
  TStructuredStorageHeader=packed record
    _abSig             :CHAR8;
    _clid              :CHAR16;
    _uMinorVersion     :word;
    _uDllVersion       :word;
    _uByteOrder        :word;
    _uSectorShift      :word;
    _uMiniSectorShift  :word;
    _usReserved        :word;
    _ulReserved1       :cardinal;
    _ulReserved2       :cardinal;
    _csectFat          :cardinal;
    _sectDirStart      :cardinal;
    _signature         :cardinal;
    _ulMiniSectorCutoff:cardinal;
    _sectMiniFatStart  :cardinal;
    _csectMiniFat      :cardinal;
    _sectDifStart      :cardinal;
    _csectDif          :cardinal;
    _sectFat           :SECT109;
  end;
  TIME_T=packed record
    dwLowDateTime :cardinal;
    dwHighDateTime:cardinal;
  end;
  TIME_T2=packed array[0..1] of TIME_T;
  TStructuredStorageDirectoryEntry=packed record
    _ab         :CHAR64;
    _cb         :WORD;
    _mse        :BYTE;
    _bflags     :BYTE;
    _sidLeftSib :cardinal;
    _sidRightSib:cardinal;
    _sidChild   :cardinal;
    _clsId      :CHAR16;
    _dwUserFlags:cardinal;
    _time       :TIME_T2;
    _sectStart  :cardinal;
    _ulSizeLow  :cardinal;
    _ulSizeHigh :cardinal;
  end;
  TStructuredStorageFAT=packed array[0..128-1] of cardinal;
const
  StorageSignature=#$D0#$CF#$11#$E0#$A1#$B1#$1A#$E1;
  RootEntry       ='R'#0'o'#0'o'#0't'#0' '#0'E'#0'n'#0't'#0'r'#0'y'#0#0#0;
  Workbook        ='W'#0'o'#0'r'#0'k'#0'b'#0'o'#0'o'#0'k'#0#0#0;
  //
  MAXREGSECT:cardinal=$FFFFFFFA;
  DIFSECT   :cardinal=$FFFFFFFC;
  FATSECT   :cardinal=$FFFFFFFD;
  ENDOFCHAIN:cardinal=$FFFFFFFE;
  FREESECT  :cardinal=$FFFFFFFF;
  MAXREGSID :cardinal=$FFFFFFFA; // maximum directory entry ID
  NOSTREAM  :cardinal=$FFFFFFFF; // unallocated directory entry
const
  // STGTY
  STGTY_INVALID  =0;
  STGTY_STORAGE  =1;
  STGTY_STREAM   =2;
  STGTY_LOCKBYTES=3;
  STGTY_PROPERTY =4;
  STGTY_ROOT     =5;
  // DECOLOR
  DE_RED         =0;
  DE_BLACK       =1;
var
  header  :TStructuredStorageHeader;
  fat     :TStructuredStorageFAT;
  dir     :TStructuredStorageDirectoryEntry;
  i       :cardinal;
  buf512  :packed array[0..512-1] of char;
  datsec1 :cardinal;
  datsecN :cardinal;
  datsec  :cardinal;
  datasize:cardinal;
  fatsec1 :cardinal;
  fatsecN :cardinal;
  fatsec  :cardinal;
  dirsec1 :cardinal;
  dirsecN :cardinal;
  dirsec  :cardinal;
  headerof:cardinal;
  seccount:cardinal;
  fatcount:cardinal;
  fatof   :cardinal;
  sectno  :cardinal;
  dirof   :cardinal;
  tempname:string;
  temp    :TFileStream;
  procedure WriteSect;
  begin
    aStream.Write(buf512,SizeOf(buf512));
    Inc(sectno);
  end;
begin
  datasize:=0;
  
  // reserva setor -1 para o header
  sectno:=Cardinal(-1);

  headerof:=aStream.Position;
  WriteSect;

  // escreve o arquivo xls puro
  tempname:=GetTempFileName;
  temp:=TFileStream.Create(tempname,fmCreate);
  try
    WriteBookToStream(temp);
    temp.Position:=0;
    datsec1:=sectno;
    datsecN:=sectno;
    repeat
      FillChar(buf512,SizeOf(buf512),0);
      i:=temp.Read(buf512,SizeOf(buf512));
      if i=0 then
        Break;
      datsecN:=sectno;
      WriteSect;
      Inc(datasize,i);
    until False;
  finally
    temp.Free;
    SysUtils.DeleteFile(tempname);
  end;
  seccount:=(datasize+512-1) div 512;
  fatcount:=(seccount+128-1) div 128;

  // reserva setores para as fats
  fatof  :=aStream.Position;
  fatsec1:=sectno;
  fatsecN:=sectno;
  for i:=0 to fatcount-1 do
  begin
    fatsecN:=sectno;
    WriteSect;
  end;

  // reserva setor para diretorio
  dirof  :=aStream.Position;
  dirsec1:=sectno;
  dirsecN:=sectno;
  WriteSect;

  // grava header atualizado
  FillChar(header,SizeOf(header),0);
  Move(StorageSignature[1],header._abSig,Length(StorageSignature));
  header._uMinorVersion     :=$003E;
  header._uDllVersion       :=3;
  header._uByteOrder        :=$FFFE;
  header._uSectorShift      :=9;
  header._uMiniSectorShift  :=6;
  header._csectFat          :=fatcount;
  header._sectDirStart      :=dirsec1; 
  header._ulMiniSectorCutoff:=$00001000;
  header._sectMiniFatStart  :=ENDOFCHAIN;
  header._sectDifStart      :=ENDOFCHAIN;
  i:=0;
  while i<fatcount do
  begin
    header._sectFat[i]:=fatsec1+i;
    Inc(i);
  end;
  while i<109 do
  begin
    header._sectFat[i]:=FREESECT;
    Inc(i);
  end;
  aStream.Position:=headerof;
  aStream.Write(header,SizeOf(header));

  // grava fats
  aStream.Position:=fatof;
  i:=0;
  datsec:=datsec1;
  while datsec<datsecN do
  begin
    fat[i]:=datsec+1;
    Inc(datsec);
    Inc(i);
    if i>=128 then
    begin
      aStream.Write(fat,SizeOf(fat));
      i:=0;
    end;
  end;
  fat[i]:=ENDOFCHAIN;
  Inc(i);
  fatsec:=fatsec1;
  while fatsec<=fatsecN do
  begin
    fat[i]:=FATSECT;
    Inc(fatsec);
    Inc(i);
    if i>=128 then
    begin
      aStream.Write(fat,SizeOf(fat));
      i:=0;
    end;
  end;
  dirsec:=dirsec1;
  while dirsec<dirsecN do
  begin
    fat[i]:=dirsec+1;
    Inc(dirsec);
    Inc(i);
    if i>=128 then
    begin
      aStream.Write(fat,SizeOf(fat));
      i:=0;
    end;
  end;
  fat[i]:=ENDOFCHAIN;
  Inc(i);
  while i<128 do
  begin
    fat[i]:=FREESECT;
    Inc(i);
  end;
  aStream.Write(fat,SizeOf(fat));

  // grava diretorio
  aStream.Position  :=dirof;
  // ROOT
  FillChar(dir,SizeOf(dir),0);
  Move(RootEntry[1],dir._ab,Length(RootEntry));
  dir._cb         :=Length(RootEntry);
  dir._mse        :=STGTY_ROOT;
  dir._bflags     :=DE_BLACK;
  dir._sidLeftSib :=NOSTREAM;
  dir._sidRightSib:=NOSTREAM;
  dir._sidChild   :=1;
  dir._sectStart  :=ENDOFCHAIN;
  aStream.Write(dir,SizeOf(dir));
  // STREAM
  FillChar(dir,SizeOf(dir),0);
  Move(Workbook[1],dir._ab,Length(Workbook));
  dir._cb         :=Length(Workbook);
  dir._mse        :=STGTY_STREAM;
  dir._bflags     :=DE_BLACK;
  dir._sidLeftSib :=NOSTREAM;
  dir._sidRightSib:=NOSTREAM;
  dir._sidChild   :=NOSTREAM;
  dir._sectStart  :=datsec1;
  dir._ulSizeLow  :=datasize;
  aStream.Write(dir,SizeOf(dir));
  // 2*NULL
  FillChar(dir,SizeOf(dir),0);
  dir._sidLeftSib :=NOSTREAM;
  dir._sidRightSib:=NOSTREAM;
  dir._sidChild   :=NOSTREAM;
  aStream.Write(dir,SizeOf(dir));
  aStream.Write(dir,SizeOf(dir));
end;

procedure TRLXLSFilter.SaveToStream(aStream:TStream);
begin
  WriteStorageToStream(aStream);
end;

procedure TRLXLSFilter.SaveToFile(const aFileName:string);
var
  Stream:TFileStream;
begin
  Stream:=TFileStream.Create(aFileName,fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TRLXLSFilter.InternalBeginDoc;
begin
  WorkBook.Clear;
  fFirstPage:=True;
  fOffsetRow:=0;
end;

procedure TRLXLSFilter.InternalDrawPage(aPage:TRLGraphicSurface);
var
  i       :integer;
  sheet   :TRLXLSWorksheet;
  range   :TRLXLSRange;
  bounds  :TRect;
  obj     :TRLTextObject;
  x0,y0   :integer;
  x1,y1   :integer;
  deltax  :integer;
  deltay  :integer;
  aux     :string;
  breset  :boolean;
  function TwipsX(x:integer):integer;
  begin
    Result:=Round((x/96)*1440*2.54);
  end;
  function TwipsY(y:integer):integer;
  begin
    Result:=y;
  end;
begin
  deltax:=TwipsX(10);
  deltay:=TwipsY(10);
  //
  breset    :=fFirstPage or not (foOneSheetOnly in Options);
  fFirstPage:=False;
  //
  if breset then
  begin
    sheet:=WorkBook.NewSheet;
    fHorzTabs.Count:=0;
    fVertTabs.Count:=0;
  end
  else
    sheet:=WorkBook.Sheets[0];
  for i:=0 to aPage.ObjectCount-1 do
    if (aPage.Objects[i] is TRLTextObject) and (TRLTextObject(aPage.Objects[i]).DisplayText<>emptystr) then
      with aPage.Objects[i].BoundsRect do
      begin
        if breset then
          fHorzTabs.Add(TwipsX(Left),TwipsX(Right)-TwipsX(Left));
        fVertTabs.Add(TwipsY(Top+fOffsetRow),TwipsY(Bottom)-TwipsY(Top));
      end;
  // calcula larguras
  if breset then
    with fHorzTabs do
      for i:=1 to Count-1 do
        Sizes[i-1].Length:=Sizes[i].Position-Sizes[i-1].Position;
  with fVertTabs do
    for i:=1 to Count-1 do
      Sizes[i-1].Length:=Sizes[i].Position-Sizes[i-1].Position;
  // retira as colunas nulas
  if breset then
    with fHorzTabs do
      for i:=Count-1 downto 0 do
        if Sizes[i].Length<deltax then
          fHorzTabs.Remove(i);
  with fVertTabs do
    for i:=Count-1 downto 0 do
      if Sizes[i].Length<deltay then
        fVertTabs.Remove(i);
  // seta largura das celulas
  if breset then
    with fHorzTabs do
      for i:=0 to Count-1 do
        sheet.FindCol(i,True).Width:=Sizes[i].Length;
  with fVertTabs do
    for i:=0 to Count-1 do
      sheet.FindRow(i,True).Height:=Sizes[i].Length;
  // distribui textos e faz colspan
  for i:=0 to aPage.ObjectCount-1 do
    if (aPage.Objects[i] is TRLTextObject) and (TRLTextObject(aPage.Objects[i]).DisplayText<>emptystr) then
    begin
      obj   :=TRLTextObject(aPage.Objects[i]);
      bounds:=FromMetaRect(obj.BoundsRect);
      bounds.Left  :=TwipsX(bounds.Left);
      bounds.Top   :=TwipsY(bounds.Top+fOffsetRow);
      bounds.Right :=TwipsX(bounds.Right);
      bounds.Bottom:=TwipsY(bounds.Bottom+fOffsetRow);
      // procura faixa de células
      x0:=0;
      while (x0<fHorzTabs.Count) and (fHorzTabs.Sizes[x0].Position<bounds.Left) do
        Inc(x0);
      x1:=x0;
      while (x1<fHorzTabs.Count) and (fHorzTabs.Sizes[x1].Position<bounds.Right-deltax) do
        Inc(x1);
      y0:=0;
      while (y0<fVertTabs.Count) and (fVertTabs.Sizes[y0].Position<bounds.Top) do
        Inc(y0);
      y1:=y0;
      while (y1<fVertTabs.Count) and (fVertTabs.Sizes[y1].Position<bounds.Bottom-deltay) do
        Inc(y1);
      aux:=obj.DisplayText;
      range:=sheet.FindRange(x0,y0,x1-1,y1-1,True);
      if aux=emptystr then
        range.Value:=' '
      else
        range.Value:=aux;
      FromMetaFont(obj.Font,range.Font);
      case obj.Alignment of
        MetaTextAlignmentLeft   : range.HorizontalAlignment:=haLeft;
        MetaTextAlignmentRight  : range.HorizontalAlignment:=haRight;
        MetaTextAlignmentCenter : range.HorizontalAlignment:=haCenter;
        MetaTextAlignmentJustify: range.HorizontalAlignment:=haJustify;
      end;
      case obj.Layout of
        MetaTextLayoutTop    : range.VerticalAlignment:=vaTop;
        MetaTextLayoutBottom : range.VerticalAlignment:=vaBottom;
        MetaTextLayoutCenter : range.VerticalAlignment:=vaCenter;
        MetaTextLayoutJustify: range.VerticalAlignment:=vaJustify;
      end;
    end;
  Inc(fOffsetRow,aPage.Height);  
end;

procedure TRLXLSFilter.InternalNewPage;
begin
end;

procedure TRLXLSFilter.InternalEndDoc;
begin
  SaveToFile(FileName);
end;

function TRLXLSFilter.GetPageSetup: TRLXLSPageSetup;
begin
  Result:=fWorkbook.PageSetup;
end;

procedure TRLXLSFilter.SetPageSetup(const Value: TRLXLSPageSetup);
begin
  PageSetup.Assign(Value);
end;

end.

