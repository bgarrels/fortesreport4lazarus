{$I RLReport.inc}

{@unit RLDraftFilter - Implementação do filtro de impressão draft. 
Portado para o Lazarus - Trabalho inicial de Isaac Trindade da Silva contato tioneobrasil@yahoo.com.br dicas4lazarus@yahoo.com.br
Lazarus Ported - initial work by Isaac 07/2009
}
unit RLDraftFilter;
{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}
//{$DEFINE SHOWMESS}
interface

uses
  SysUtils, Classes, Math, Contnrs, Dialogs,
{$ifdef MSWINDOWS}
{$IFDEF FPC}
  LCLIntf, LCLType, ShellApi,
{$ELSE}
  Windows, ShellApi,
{$ENDIF}
{$else}
  Types, Libc,
{$endif}
{$ifdef VCL}
  Graphics, RLMetaVCL,
{$else}
  QGraphics, RLMetaCLX,
{$endif}
{$IFDEF FPC}
  rlshared,
{$ENDIF}
  RLMetaFile, RLConsts, RLUtils, RLFilters, RLTypes, RLPrinters;

type
  {@type TRLDraftAccentMethod - Comportamento do filtro em relação aos caracteres acentuados.
   Algumas impressoras não suportam ou podem não estar adequadamente configuradas para imprimir
   caracteres acentuados.
   O método de acentuação do filtro pode ser:
   amOverwrite - O filtro provoca o retorno do carro para imprimir o caractere de acento sobre a letra;
   amTakeOut - O caractere é impresso sem o acento;
   amSustain - O caractere é enviado sem modificações (exige configuração da impressora). :/}
  TRLDraftAccentMethod=(amOverwrite,amTakeOut,amSustain);

  {@type TRLDraftEjectMethod - Comportamento do filtro em relação ao salto de página.
   Quando o formulário é de um tamanho específico, pode ser necessário modificar a maneira
   como o filtro executa os saltos de página.
   O método de salto do filtro pode ser:
   ejCompletePage - Envia códigos para salto de linha até completar a página. A distância entre os páginas pode
   dilatar ou contrair ao longo da impressão;
   ejForceWithCode - O filtro envia um código de salto de página para a impressora. Este é o melhor método para formulários
   contínuos, porém pode não servir para formulários de tamanho personalizado;
   ejLeavePage - O filtro deixa a cabeça de impressão na posição aonde ela parou. Pode ser utilizado com formulários sem picote
   como rolos, por exemplo, ou quando for desejável economizar papel durante testes. :/}
  TRLDraftEjectMethod=(ejCompletePage,ejForceWithCode,ejLeavePage);

  {@type TRLPrinterFamily - Família de impressoras.
   Além dos códigos de programação, pode ser interessante conhecer algumas particularidades da impressora a
   ser utilizada e que não podem ser informadas somente com comandos, como a impressão de gráficos, por exemplo.
   A família pode ser:
   fmCustom - Códigos de programação informados pelo usuário na prop Commands;
   fmEpsonLike - Códigos de programação da especificação EPSON/FX;
   fmESCP2 - Códigos de programação da especificação EPSON ESC/P2. :/}
  TRLPrinterFamily=(fmCustom,fmEpsonLike,fmESCP2);

  {@type TRLDeviceKind - Tipo de dispositivo de impressão.
   Indica como o relatório deve ser despachado.
   Pode assumir um dos seguintes valores:
   dkPrinterPort - Neste caso, a prop DevicePath será preenchida em runtime de acordo com a impressora
   selecionada no diálogo de impressão. Este é o padrão recomendado para o Windows, pois o próprio sistema
   operacional se encarregará de descobrir o caminho para o dispositivo através de informações do SO;
   dkProgram - A prop DevicePath indica o nome de um programa spooler. Padrão recomendado para o Linux;
   dkFileName - A prop DevicePath aponta para um nome de arquivo. Este arquivo poderá ser copiado para
   outra impressora, ou ser utilizado como debug. :/}
  TRLDeviceKind=(dkPrinterPort,dkProgram,dkFileName);

  {@type TRLDitheringMethod - Método para impressão de imagens.
   Esta propriedade indica que técnica deve ser utilizada para transformar imagens coloridas em pontos preto e branco.
   Pode ser um dos seguintes valores:
   dmNone - Indica que nenhuma imagem será impressa;
   dmColorTable - Tabela de associação de cores. Este método geralmente apresenta os melhores resultados;
   dmErrorDiffusion - Difusão de erros. :/}
  TRLDitheringMethod=(dmNone,dmColorTable,dmErrorDiffusion);

  {@type TRLLineDrawMethod - Método para impressão de linhas e traços.
   Para imprimir linhas retas em modo texto, é necessário informar o conjunto de caracteres a utilizar.
   Pode ser um dos seguintes valores:
   ldNone - Nenhuma linha ou traço será impresso;
   ldMinusAndPipe - Linhas horizontais como sinal negativo "-" e verticais como pipes "|";
   ldGraphicCharset - Utilizar os conectores gráficos do padrão ProPrinter (exige configuração da impressora). :/}
  TRLLineDrawMethod=(ldNone,ldMinusAndPipe,ldGraphicCharset);

  {@type TRLFillArtMethod - Método para preenchimento de áreas.
   Define o método para representação de retângulos sólidos ou linhas grossas em impressoras matriciais.
   Pode ser um dos seguintes valores:
   fmNone - Nenhum preenchimento será impresso;
   fmLetterX - A letra X será utilizada ;
   fmGraphicCharset - Utilizar os caracteres gráficos do padrão ProPrinter (exige configuração da impressora). :/}
  TRLFillArtMethod=(fmNone,fmLetterX,fmGraphicCharset);

  {@type TRLFormSelection - Seleção da largura do formulário contínuo.
   Indica como será escolhido formulário em relação à largura.  
   Pode ser um dos seguintes valores:
   fsNone - Nenhuma adaptação é feita e nenhum diálogo é exibido;
   fsAccordingToOrientation - O diálogo apresentará as opções de 80cols para Portrait e 132cols para Landscape,
   e o padrão será de acordo com a orientação do relatório;
   fs80Cols - O padrão será o formulário de 80cols;
   fs132Cols - O padrão será o formulário de 132cols. :/}
  TRLFormSelection=(fsNone,fsAccordingToOrientation,fs80Cols,fs132Cols);

  {@type TRLStretchCharWidth - Método de adaptação do tamanho das fontes para o formulário escolhido.
   Pode ser um dos seguintes valores:
   scNone - Nenhuma adaptação será feita;
   scEnlargementsOnly - A fonte deverá ser aumentada quando o formulário for maior;
   scShrinksOnly - A fonte deverá ser encolhida quando o formulário for menor;
   scAlways - A fonte deverá ser aumentada ou encolhida de acordo com a escolha do formulário. :/}
  TRLStretchCharWidth=(scNone,scEnlargementsOnly,scShrinksOnly,scAlways);

  {@type TRLDraftTextDecoration - Define como os efeitos de fonte serão implementados.
   Pode ser um dos seguintes valores:
   ddIncludeNone - Nenhum efeito é realizado;
   ddIncludeAll - Todos os efeitos são realizados;
   ddCustomized - Somente os efeitos indicados na prop TextStyles. :/}
  TRLDraftTextDecoration=(ddIncludeNone,ddIncludeAll,ddCustomized);

  {@type TRLDraftTextStyles - Indica que efeitos de fonte devem ser realizados ou ignorados pelo filtro.
   Pode ser nenhum ou uma combinação dos seguintes valores:
   tsItalic - Efeito itálico (fonte inclinada);
   tsBold - Efeito negrito (passada dupla);
   tsUnderline - Efeito sublinhado. :/}
  TRLDraftTextStyles=set of (tsItalic,tsBold,tsUnderline);

  {@type TRLCPPSelection - Indica a política de compressão dos caracteres, e pode ser fixa ou variável.
   Pode ser um dos seguintes valores:
   csAutomatic - A compressão varia de acordo com a fonte de cada label;
   csFixed5CPP - Compressão fixa em 5cpp;
   csFixed10CPP - Compressão fixa em 10cpp;
   csFixed12CPP - Compressão fixa em 12cpp;
   csFixed17CPP - Compressão fixa em 17cpp;
   csFixed20CPP - Compressão fixa em 20cpp. :/}
  TRLCPPSelection=(csAutomatic,csFixed5CPP,csFixed10CPP,csFixed12CPP,csFixed17CPP,csFixed20CPP);

  TDraftObj=class;
  TDraftText=class;
  TDraftRaw=class;

  { TRLDraftFilter }

  {@class TRLDraftFilter - Filtro de impressão para impressoras matriciais.
   Este filtro age substituindo os comandos gráficos que seriam enviados para o driver da impressora por códigos de
   programação. Estes códigos são enviados diretamente para o dispositivo de impressão (ou programa de spool). Assim, é
   possível imprimir o mesmo relatório em impressoras de tecnologias diferentes, mantendo-se o design gráfico original
   com impressoras jato de tinta ou laser, e com alta velocidade em uma matricial.
   Há várias propriedades e maneiras de se conseguir bons resultados, equilibrando velocidade e qualidade de impressão.   
   Nota: O algorítmo implementado pelo filtro conseguirá fazer uma melhor aproximação de fontes TrueType variáveis do
   que de fontes fixas. Portanto, não é necessário desenhar os relatórios em uma fonte com pitch fixo, como: Courier
   ou Terminal.
   @ancestor TRLCustomPrintFilter.
   @links TRLHTMLFilter, TRLRichFilter.
   @pub }
  TRLDraftFilter=class(TRLCustomPrintFilter)
  private

    // state variables

    fDeviceHandle         :file;
    fSendFirstReset       :boolean;
    fDeviceFileName       :string;
    fPrintCut             :TPoint;
    fPrintSize            :TPoint;
    fCurrentPrintPos      :TPoint;
    fCurrentCharWidth     :integer;
    fCurrentCharHeight    :integer;
    fCurrentCPP           :integer;
    fCurrentBoldState     :boolean;
    fCurrentItalicState   :boolean;
    fCurrentUnderlineState:boolean;
    fFontSizeTextSize     :boolean;
    fTraceFontName        :string;
    fTraceFontSize        :integer;
    fTraceVertTabOrigin   :integer;
    fTraceVertTabStep     :integer;

    // property variables

    fDriverName           :string;
    fCommands             :TStrings;
    fDeviceKind           :TRLDeviceKind;
    fDevicePath           :string;
    fAccentMethod         :TRLDraftAccentMethod;
    fEjectMethod          :TRLDraftEjectMethod;
    fPrinterFamily        :TRLPrinterFamily;
    fDitheringMethod      :TRLDitheringMethod;
    fLineDrawMethod       :TRLLineDrawMethod;
    fFillArtMethod        :TRLFillArtMethod;
    fFormSelection        :TRLFormSelection;
    fStretchCharWidth     :TRLStretchCharWidth;
    fTextDecoration       :TRLDraftTextDecoration;
    fTextStyles           :TRLDraftTextStyles;
    fCPPSelection         :TRLCPPSelection;

    // assign methods

    procedure   SetCommands(const Value: TStrings);

    // custom methods

    procedure   GetObjList(aPage:TRLGraphicSurface; aList:TObjectList);
    procedure   AddObj(aObj:TDraftObj; aList:TObjectList);
    procedure   AddSimpleText(const aText:string; aCPP,aStyle,aPinX,aPinY:integer; aList:TObjectList);
    procedure   AddText(aObj:TRLTextObject; aList:TObjectList);
    procedure   AddImage(aObj:TRLImageObject; aList:TObjectList);
    procedure   AddFillRect(aObj:TRLFillRectObject; aList:TObjectList);
    procedure   AddLine(aObj:TRLLineObject; aList:TObjectList);
    procedure   AddRectangle(aObj:TRLRectangleObject; aList:TObjectList);
    procedure   AddEllipse(aObj: TRLEllipseObject; aList: TObjectList);
    procedure   AddPixel(aObj: TRLPixelObject; aList: TObjectList);
    procedure   AddPolygon(aObj: TRLPolygonObject; aList: TObjectList);
    procedure   AddPolyline(aObj: TRLPolylineObject; aList: TObjectList);
    procedure   AddResetClipRect(aObj: TRLResetClipRectObject; aList: TObjectList);
    procedure   AddSetClipRect(aObj: TRLSetClipRectObject; aList: TObjectList);
    //
    procedure   ResetPage;
    procedure   SetPrintPos(aY,aX:integer);
    procedure   SetHorzCompression(aCPP:integer; aForce:boolean=False);
    procedure   SetVertCompression(aLPP:integer; aForce:boolean=False);
    procedure   SetCharHeight(aCharHeight:integer; aForce:boolean=False);
    procedure   SetPrintStyle(aStyle:byte);
    function    PixelToPinX(X:integer):integer;
    function    PixelToPinY(Y:integer):integer;
    function    PrintCode(const aCommand:string; aParameter:integer=0):string;
    procedure   SetPrinterFamily(const Value:TRLPrinterFamily);
    procedure   DeviceWrite(const aData:string);
    function    FormFactorX:double;
    function    SelectFontCPP(const aFontName:string; aSize:integer):integer;
    function    FontSizeToCPP(const aFontName:string; aSize:integer):integer;
    function    CPPSelectionToCPP(aCPP:TRLCPPSelection):integer;
    function    StretchFontSize(aSize:integer):integer;
    procedure   SetTextDecoration(const Value: TRLDraftTextDecoration);
    procedure   SetTextStyle(const Value: TRLDraftTextStyles);

    // misc

    function    IsCustomPrinterFamily:boolean;
    function    IsCustomDevice:boolean;
    function    IsCustomTextStyle: Boolean;
    procedure   GetProgrammingCodes(aPrinterFamily: TRLPrinterFamily; aDest: TStrings);
    procedure   FindDominantFont(aPage: TRLGraphicSurface; var aFontName: string; var aFontSize: integer);
    procedure   FindVertTabs(aPage: TRLGraphicSurface; var aOrigin,aStep:integer);
    procedure   AddGraphic(aGraphic: TGraphic; const aRect: TRect; aList: TObjectList);
  protected

    // override methods

    procedure   InternalBeginDoc; override;
    procedure   InternalEndDoc; override;
    procedure   InternalDrawPage(aPage:TRLGraphicSurface); override;
    procedure   InternalNewPage; override;
    //
    procedure   Loaded; override;
  public
    // constructors & destructors
    constructor Create(aOwner:TComponent); override;
    destructor  Destroy; override;

    {@method DefaultCommands - Preenche a lista de códigos de programação de acordo com a família de impressoras
     escolhida. :/}
    procedure   DefaultCommands;
    procedure   ExecuteDialog; override;
  published

    {@prop Commands - Códigos de programação para a impressora.
     Esta propriedade especifica os códigos de compressão, efeitos de fonte e controle de página nativos da impressora.
     Os códigos da configuração default são para impressoras do tipo EPSON. É possível porém adaptá-los a praticamente
     qualquer impressora matricial.
     Cada linha deve indicar o comando e a sequencia de bytes na seguinte forma: "NOME=Asc1,'Chr1',Asc2,'ChrN',Asc2".
     Pode conter as seguintes variáveis: 
     CR - Retorno do carro (ex.: CR=13);
     LF - Avanço de linha (ex.: LF=10);
     BS - Retrocesso de 1 caractere (ex.: BS=8);
     FF - Avanço de página (ex.: FF=12);
     Reset - Inicialização da impressora (ex.: RESET=27,'@');
     MicroOn - Micro salto de N/12 de linha (ex.: MicroOn=27,'A',#);
     MicroOff - Volta ao salto normal de linha (ex.: MicroOff=27,'2');
     Space - O caractere de espaço (ex.: Space=32);
     CPP10 - Compressão a 10 caracteres/polegada (ex.: CPP10=27,'P',18);
     CPP12 - Compressão a 12 caracteres/polegada (ex.: CPP12=27,'M',18);
     CPP17 - Compressão a 17 caracteres/polegada (ex.: CPP17=27,'P',15);
     CPP20 - Compressão a 20 caracteres/polegada (ex.: CPP20=27,'M',15);
     ExpandOn - Liga modo expandido (ex.: ExpandOn=27,'W',1);
     ExpandOff - Modo expandido desligado (ex.: ExpandOff=27,'W',0);
     BoldOn - Liga modo negrito (ex.: BoldOn=27,'G');
     BoldOff - Modo negrito desligado (ex.: BoldOff=27,'H');
     ItalicOn - Liga modo itálico (ex.: ItalicOn=27,'4');
     ItalicOff - Modo itálico desligado (ex.: ItalicOff=27,'5');
     UnderlineOn - Liga modo itálico (ex.: UnderlineOn=27,'-1');
     UnderlineOff - Modo itálico desligado (ex.: UnderlineOff=27,'-0');
     RAW - Envio de sequência de bytes para impressão de gráficos (ex.: RAW=27,'L',#l,#h).
     @links DefaultCommands, PrinterFamily. :/}

    property    Commands        :TStrings             read fCommands         write SetCommands stored IsCustomPrinterFamily;

    {@prop FontSizeReal, implementado para imprimir conforme o tamanho real da fonte, sem levar em
    consideração as medidas canvas}
    
    property    FontSizeReal    :Boolean              read fFontSizeTextSize  write fFontSizeTextSize;

    {@prop DriverName - Nome de arquivo que contém os códigos de programação da impressora (driver).
     Utilize esta propriedade quando quiser manter os códigos de programação da impressora
     em um arquivo texto externo ao programa. A sintaxe do arquivo é a mesma da propriedade
     Commands.
     @links Commands, DefaultCommands. :/}
    property    DriverName      :string               read fDriverName       write fDriverName;

    {@prop DeviceKind - Tipo de dispositivo de impressão.
     Indica que tipo de dispositivo está identificado na propriedade DevicePath.
     @links TRLDeviceKind, DevicePath. :/}
    property    DeviceKind      :TRLDeviceKind        read fDeviceKind       write fDeviceKind stored IsCustomDevice;

    {@prop DevicePath - Caminho para o dispositivo de impressão.
     Esta propriedade indica o nome ou o caminho do dispositivo de impressão ou programa spooler.
     No Windows pode-se utilizar um dos dispositivos padrões: PRN, LPT1, LPT2 etc, ou um caminho de
     rede no formato "\\computador\impressora". O dispositivo PRN é especialmente interessante, pois
     sempre representa a impressora atualmente selecionada pelo sistema.
     No Linux pode-se informar tanto um caminho para um dispositivo (ex.: "/dev/lp0") como para um
     programa de controle de spool como o lpr (ex.: "lpr -P%p %f"). Neste último caso, "%p" representa
     o nome de uma impressora válida cadastrada pelo linuxconf, e "%f" o nome de um arquivo temporário
     gerado pelo FR.
     Variáveis possíveis:
     %p - Nome da impressora como ela é conhecida pelo sistema operacional;
     %f - Nome do arquivo temporário gerado com texto e códigos de impressão;
     %d - Nome e caminho do dispositivo de impressão correspondente à impressora selecionada.
     @links DeviceKind. :/}
    property    DevicePath      :string               read fDevicePath       write fDevicePath stored IsCustomDevice;

    {@prop AccentMethod - Comportamento do filtro em relação a caracteres acentuados.
     Algumas impressoras não suportam ou podem não estar adequadamente configuradas para imprimir
     caracteres acentuados.
     @links TRLDraftAccentMethod. :/}
    property    AccentMethod    :TRLDraftAccentMethod read fAccentMethod     write fAccentMethod default amOverwrite;

    {@prop EjectMethod - Comportamento do filtro em relação aos saltos de páginas.
     Em alguns casos quando o formulário é de um tamanho específico, pode ser necessário
     modificar a maneira como o filtro executa os saltos de página.
     @links TRLDraftEjectMethod, Commands. :/}
    property    EjectMethod     :TRLDraftEjectMethod  read fEjectMethod      write fEjectMethod default ejCompletePage;

    {@prop PrinterFamily - Família de impressoras.
     Às vezes é desejável conhecer algumas particularidades da impressora a ser utilizada e que
     não podem ser informadas somente com comandos, como a impressão de gráficos.
     @links TRLPrinterFamily. :/}
    property    PrinterFamily   :TRLPrinterFamily     read fPrinterFamily    write SetPrinterFamily default fmEpsonLike;

    {@prop DitheringMethod - Método para impressão de imagens.
     Esta propriedade indica qual método deve ser utilizado para transformar imagens coloridas
     em preto e branco.
     @links TRLDitheringMethod. :/}
    property    DitheringMethod :TRLDitheringMethod   read fDitheringMethod  write fDitheringMethod default dmColorTable;

    {@prop LineDrawMethod - Método para impressão de linhas.
     Esta propriedade indica qual método deve ser utilizado para desenhar linhas numa impressora
     matricial.
     @links TRLLineDrawMethod. :/}
    property    LineDrawMethod  :TRLLineDrawMethod    read fLineDrawMethod   write fLineDrawMethod default ldMinusAndPipe;

    {@prop FillArtMethod - Método para o preenchimento de retângulos ou traços grossos. :/}
    property    FillArtMethod    :TRLFillArtMethod read fFillArtMethod write fFillArtMethod default fmNone;

    {@prop FormSelection - Política de seleção de tamanho para formulários contínuos.
     @links TRLFormSelection. :/}
    property    FormSelection   :TRLFormSelection     read fFormSelection    write fFormSelection default fsAccordingToOrientation;

    {@prop StretchCharWidth - Método de adaptação do tamanho das fontes.
     @links TRLStretchCharWidth. :/}
    property    StretchCharWidth:TRLStretchCharWidth  read fStretchCharWidth write fStretchCharWidth default scShrinksOnly;

    {@prop TextDecoration - Decoração do texto.
     @links TRLDraftTextDecoration. :/}
    property    TextDecoration:TRLDraftTextDecoration read fTextDecoration write SetTextDecoration default ddIncludeAll;
    
    {@prop TextStyles - Estilos de texto.
     @links TRLDraftTextStyles. :/}
    property    TextStyles:TRLDraftTextStyles read fTextStyles write SetTextStyle stored IsCustomTextStyle;

    {@prop CPPSelection - Fixa uma compressão padrão para todo o relatório.
     @links Commands, TRLCPPSelection. :/}
    property    CPPSelection:TRLCPPSelection read fCPPSelection write fCPPSelection default csAutomatic;

    {@prop DisplayName - ancestor /}
    property    DisplayName;

    {@prop ShowProgress - ancestor /}
    property    ShowProgress;
    {@prop ShowFilterDialog - ancestor /}
    property    ShowFilterDialog;
  end;
  {/@class}

  { TDraftObj }

  TDraftObj=class
  public
    Enabled  :boolean;
    PinBounds:TRect;
    //
    constructor Create; virtual;
  end;

  { TDraftText }

  TDraftText=class(TDraftObj)
  public
    CPP  :integer;
    Style:byte;
    Text :string;
  end;

  { TDraftRaw }

  TDraftRaw=class(TDraftObj)
  public
    Bitmap:TBitmap;
    //
    constructor Create; override;
    destructor  Destroy; override;
  end;

{/@unit}

implementation

uses
  RLDraftFilterDialog;

const
  AspectratioX=126/100;
  AspectratioY=72/100;
  //
  StandardLPP    =Trunc(66/11); // linhas por polegada (66 lins / 11 pol = 6 lpp)
  StandardCPP    =Trunc(80/8);  // colunas por polegada (80 cols / 8 pol = 10 cpp)
  DefaultFontName='Arial';
  DefaultFontSize=6;
  //
  AccentLetters='áàãâäÁÀÃÂÄéèêëÉÈÊËíìîïÍÌÎÏóòõôöÓÒÕÔÖúùûüÚÙÛÜçÇºª';
  NormalLetters='aaaaaAAAAAeeeeEEEEiiiiIIIIoooooOOOOOuuuuUUUUcCoa';
  AccentChars  ='''`~^¨''`~^¨''`^¨''`^¨''`^¨''`^¨''`~^¨''`~^¨''`^¨''`^¨,,__';

var
  PinsPerRow:integer=12;
  PinsPerCol:integer=12;

procedure DoColorTable(aSource,aDest:TBitmap; aContrast:double=2);
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
  {$IFDEF FPC}
  aDest.assign(aSource);
  {$ELSE}
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
  {$ENDIF}
end;

procedure DoErrorDiffusion(aSource,aDest:TBitmap; aContrast:double=2);
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
  {$IFDEF FPC}
  aDest.assign(aSource);
  {$ELSE}
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
  {$ENDIF}
end;

function TranslateAccents(const aString,aBS:string):string;
var
  i,p:integer;
begin
  Result:='';
  for i:=1 to Length(aString) do
  begin
    p:=Pos(aString[i],AccentLetters);
    if p>0 then
      Result:=Result+Copy(AccentChars,p,1)+aBS+Copy(NormalLetters,p,1)
    else
      Result:=Result+aString[i];
  end;
end;

function RemoveAccents(const aString:string):string;
var
  i,p:integer;
begin
  Result:=aString;
  for i:=1 to Length(aString) do
  begin
    p:=Pos(aString[i],AccentLetters);
    if p>0 then
      Result[i]:=NormalLetters[p];
  end;
end;

function CPPToPins(aCPP:integer):integer;
begin
  Result:=Round(PinsPerCol*StandardCPP/aCPP);
end;

function LPPToPins(aLPP:integer):integer;
begin
  Result:=Round(PinsPerRow*StandardLPP/aLPP);
end;

function GetBitmapPixel(aBitmap:TBitmap; aX,aY:integer; aDefault:TColor):TColor;
begin
  {$IFDEF FPC}
  Result:=aDefault;
  {$ELSE}
  if aY<aBitmap.Height then
    with TRGBArray(aBitmap.ScanLine[aY]^)[aX] do
      Result:=RGB(rgbRed,rgbGreen,rgbBlue)
  else
    Result:=aDefault;
  {$ENDIF}
end;

function PinToCol(aPin,aCharWidth:integer):integer;
begin
  Result:=aPin div aCharWidth+1;
end;

function PinToRow(aPin,aCharHeight:integer):integer;
begin
  Result:=aPin div aCharHeight+1;
end;

function LengthToPins(aLength,aCharWidth:integer):integer;
begin
  Result:=aLength*aCharWidth;
end;

function ColToPin(aPos,aCharWidth:integer):integer;
begin
  Result:=(aPos-1)*aCharWidth;
end;

function PinXToGrid(aPinX,aCharWidth:integer):integer;
begin
  Result:=ColToPin(PinToCol(aPinX,aCharWidth),aCharWidth);
end;

function PinYToGrid(aPinY,aStart,aStep:integer):integer;
begin
  Result:=aStart;
  while not (Result+aStep>aPinY) do
    Inc(Result,aStep);
end;

{ TDraftObj }

constructor TDraftObj.Create;
begin
  Enabled:=True;
  //
  inherited;
end;

{ TDraftRaw }

constructor TDraftRaw.Create;
begin
  Bitmap:=nil;
  //
  inherited;
end;

destructor TDraftRaw.Destroy;
begin
  inherited;
  //
  FreeObj(Bitmap);
end;

{ TRLDraftFilter }

constructor TRLDraftFilter.Create(aOwner: TComponent);
begin
  fDriverName      :='';
  fCommands        :=nil;
  fAccentMethod    :=amOverwrite;
  fEjectMethod     :=ejCompletePage;
  fSendFirstReset  :=False;
{$ifdef MSWINDOWS}
  fDeviceKind      :=dkPrinterPort;
  fDevicePath      :='prn';
{$else}
  fDeviceKind      :=dkProgram;
  fDevicePath      :='lpr -P%p %f';
{$endif};
  fCommands        :=nil;
  fPrinterFamily   :=fmEpsonLike;
  fDitheringMethod :=dmColorTable;
  fLineDrawMethod  :=ldMinusAndPipe;
  fFillArtMethod   :=fmNone;
  fFormSelection   :=fsAccordingToOrientation;
  fStretchCharWidth:=scShrinksOnly;
  fTextDecoration  :=ddIncludeAll;
  fTextStyles      :=[tsItalic,tsBold,tsUnderline];
  fCPPSelection    :=csAutomatic;
  //
  inherited;
  //
  fCommands:=TStringList.Create;
  DefaultCommands;
  //
  FilterStyle:=FilterStyle+[fsEmulateCopies,fsSetupDialog];
end;

destructor TRLDraftFilter.Destroy;
begin
  if Assigned(fCommands) then
    FreeAndNil(fCommands);
  //
  inherited;
end;

procedure TRLDraftFilter.InternalBeginDoc;
var
  metrics:TRLPrinterMetrics;
begin
  RLPrinter.SetPaperSize(Pages.PaperWidth,Pages.PaperHeight,Pages.Orientation=MetaOrientationLandscape,False);
  RLPrinter.LoadMetrics(metrics);
  /// verificar verdade sobre landscape
  if Pages.Orientation=MetaOrientationLandscape then
  begin
    fPrintCut.X:=Round(metrics.MarginBottom*Pages.Width /metrics.PhysicalWidth);
    fPrintCut.Y:=Round(metrics.MarginLeft  *Pages.Height/metrics.PhysicalHeight);
  end
  else
  begin
    fPrintCut.X:=Round(metrics.MarginLeft*Pages.Width /metrics.PhysicalWidth);
    fPrintCut.Y:=Round(metrics.MarginTop *Pages.Height/metrics.PhysicalHeight);
  end;
  ///
  fPrintSize.X:=Pages.Width;
  fPrintSize.Y:=Pages.Height;
  //
  fSendFirstReset:=True;
  // carrega comandos
  if (fDriverName<>emptystr) and FileExists(fDriverName) then
    fCommands.LoadFromFile(fDriverName);
  if fCommands.Count=0 then
    DefaultCommands;
  if fPrinterFamily=fmESCP2 then
    PinsPerRow:=60  // 0..255 360/6LPP
  else
    PinsPerRow:=12; // 0..85 60/6LPP
  // medidas padrão papel carta
  fCurrentCPP           :=StandardCPP;
  fCurrentBoldState     :=False;
  fCurrentItalicState   :=False;
  fCurrentUnderlineState:=False;
  fCurrentCharWidth     :=CPPToPins(fCurrentCPP);
  fCurrentCharHeight    :=LPPToPins(StandardLPP);
  //
  {$IFDEF SHOWMESS}
  ShowMessage('InternalBegindoc');
  {$ENDIF}

  case fDeviceKind of
    dkPrinterPort: fDeviceFileName:=RLPrinter.PrinterPort;
    dkProgram    : begin
                     fDeviceFileName:=GetTempFileName;
                     RegisterTempFile(fDeviceFileName);
                   end;  
    dkFileName   : fDeviceFileName:=fDevicePath;
  else
    fDeviceFileName:=fDevicePath;
  end;
  if fDeviceFileName=emptystr then
    raise Exception.Create(LS_NoPathToPrinterStr);
  //
  AssignFile(fDeviceHandle,fDeviceFileName);
  Try
  Rewrite(fDeviceHandle,1);
    except
    Raise exception.Create(LS_PrinterNotFoundStr); //3.24b3
    end;
  //
  ResetPage;
end;

procedure TRLDraftFilter.InternalEndDoc;
var
  cmd:string;
{$ifdef MSWINDOWS}
var
  par:string;
  i  :integer;
{$endif}
begin
  NewPage;
  CloseFile(fDeviceHandle);
  case fDeviceKind of
    dkPrinterPort: ;
    dkProgram    : begin
                     cmd:=fDevicePath;
                     cmd:=StringReplace(cmd,'%p',RLPrinter.PrinterName,[rfReplaceAll,rfIgnoreCase]);
                     cmd:=StringReplace(cmd,'%d',RLPrinter.PrinterPort,[rfReplaceAll,rfIgnoreCase]);
                     cmd:=StringReplace(cmd,'%f',fDeviceFileName      ,[rfReplaceAll,rfIgnoreCase]);
{$ifdef MSWINDOWS}
                     i:=Pos(' ',cmd);
                     if i=0 then
                       i:=Length(cmd)+1;
                     par:=Copy(cmd,i+1,Length(cmd));
                     cmd:=Copy(cmd,1,i-1);
                     {$IFDEF SHOWMESS}
                     ShowMessage(cmd+' '+par);
                     {$ENDIF}

                     ShellExecute(0,'open',PChar(cmd),PChar(par),nil,SW_SHOWNORMAL);
{$else}
                     {$IFDEF SHOWMESS}
                     ShowMessage(cmd);
                     {$ENDIF}
                     Libc.system(PChar(cmd));
{$endif};
                   end;
    dkFileName   : ;
  end;
end;

procedure TRLDraftFilter.InternalNewPage;
begin
  case fEjectMethod of
    ejCompletePage : SetPrintPos(PixelToPinY(fPrintSize.Y),PixelToPinX(0)); // posicionamento vertical da proxima página em agulhas
    ejForceWithCode: DeviceWrite(PrintCode('FF'));
    ejLeavePage    : SetPrintPos(fCurrentPrintPos.Y+PinsPerRow,PixelToPinX(0)); // apenas salta uma linha
  end;
  ResetPage;
end;

procedure TRLDraftFilter.ResetPage;
begin
  fCurrentPrintPos.X:=0;
  fCurrentPrintPos.Y:=0;
end;

procedure TRLDraftFilter.GetProgrammingCodes(aPrinterFamily:TRLPrinterFamily; aDest:TStrings);
begin
  case aPrinterFamily of
    fmEpsonLike: with aDest do
                 begin
                   Values['CR']          :='13';
                   Values['LF']          :='10';
                   Values['BS']          :='8';
                   Values['FF']          :='12';
                   Values['Reset']       :='27,''@'',27,''x0''';
                   Values['MicroOn']     :='27,''A'',#';
                   Values['MicroOff']    :='27,''2''';
                   Values['Space']       :='32';
                   Values['CPP10']       :='27,''P'',18';
                   Values['CPP12']       :='27,''M'',18';
                   Values['CPP17']       :='27,''P'',15';
                   Values['CPP20']       :='27,''M'',15';
                   Values['ExpandOn']    :='27,''W'',1';
                   Values['ExpandOff']   :='27,''W'',0';
                   Values['BoldOn']      :='27,''G''';
                   Values['BoldOff']     :='27,''H''';
                   Values['ItalicOn']    :='27,''4''';
                   Values['ItalicOff']   :='27,''5''';
                   Values['UnderlineOn'] :='27,''-1''';
                   Values['UnderlineOff']:='27,''-0''';
                   Values['RAW']         :='27,''L'',#l,#h';
                 end;
    fmESCP2    : begin
                   GetProgrammingCodes(fmEpsonLike,aDest);
                   with aDest do
                     Values['MicroOn']:='27,''+'',#';
                 end;
  end;
end;

procedure TRLDraftFilter.DefaultCommands;
begin
  if fPrinterFamily=fmCustom then
    Exit;
  fCommands.Clear;
  GetProgrammingCodes(fPrinterFamily,fCommands);
end;

function TRLDraftFilter.PrintCode(const aCommand:string; aParameter:integer=0):string;
var
  s,chr:string;
  i:integer;
begin
  s:=fCommands.Values[aCommand];
  Result:='';
  if s='' then
    Exit;
  i:=0;
  repeat
    Inc(i);
    chr:=Token(s,i,',');
    if chr='' then
      Break
    else if (chr[1]='''') and (chr[Length(chr)]='''') then
      Result:=Result+Copy(chr,2,Length(chr)-2)
    else if chr='#' then
      Result:=Result+Char(aParameter)
    else if LowerCase(chr)='#l' then
      Result:=Result+Char(aParameter mod 256)
    else if LowerCase(chr)='#h' then
      Result:=Result+Char(aParameter div 256)
    else if chr='$' then
      Result:=Result+IntToStr(aParameter)
    else
      Result:=Result+Char(strtoint(chr));
  until False;
end;

procedure TRLDraftFilter.DeviceWrite(const aData:string);
begin
  if aData<>'' then
    BlockWrite(fDeviceHandle,aData[1],Length(aData));
end;

function TRLDraftFilter.FormFactorX:double;
begin
  case fFormSelection of
    fsAccordingToOrientation:
    Result:=1;

    fs80Cols:
    if Pages.Orientation=MetaOrientationPortrait then
      Result:=1
    else
      Result:=80/132;

    fs132Cols:
    if Pages.Orientation=MetaOrientationPortrait then
      Result:=132/80
    else
      Result:=1;
  else // fsNone
    Result:=1;
  end;
end;

function TRLDraftFilter.PixelToPinX(X:integer):integer;
var
  qpol,qcol:double;
begin
  Dec(X,fPrintCut.X);
  qpol  :=(X*FormFactorX)/ScreenPPI; // transforma X em polegadas
  qcol  :=qpol*StandardCPP;          // transforma polegadas em colunas de acordo com o cpp padrão
  Result:=Trunc(qcol*PinsPerCol);    // transforma colunas em pinos
end;

function TRLDraftFilter.PixelToPinY(Y:integer):integer;
var
  qpol,qlin:double;
begin
  Dec(Y,fPrintCut.Y);
  qpol  :=Y/ScreenPPI;            // transforma Y em polegadas
  qlin  :=qpol*StandardLPP;       // transforma polegadas em linhas de acordo com o lpp padrão
  Result:=Trunc(qlin*PinsPerRow); // transforma linhas em pinos
end;

procedure TRLDraftFilter.SetPrintPos(aY,aX:integer);
var
  standardpins:integer;
begin
  // posicionamento vertical
  standardpins:=LPPToPins(StandardLPP);
  if (aY-fCurrentPrintPos.Y>fCurrentCharHeight) and (fCurrentCharHeight<standardpins) then
    SetCharHeight(standardpins);
  while aY-fCurrentPrintPos.Y>fCurrentCharHeight do
  begin
    DeviceWrite(PrintCode('CR'));
    DeviceWrite(PrintCode('LF'));
    Inc(fCurrentPrintPos.Y,fCurrentCharHeight);
    fCurrentPrintPos.X:=0;
  end;
  if aY-fCurrentPrintPos.Y>0 then
  begin
    SetCharHeight(aY-fCurrentPrintPos.Y);
    DeviceWrite(PrintCode('CR'));
    DeviceWrite(PrintCode('LF'));
    Inc(fCurrentPrintPos.Y,fCurrentCharHeight);
    fCurrentPrintPos.X:=0;
  end;
  // posicionamento horizontal
  if aX<fCurrentPrintPos.X then
  begin
    DeviceWrite(PrintCode('CR'));
    fCurrentPrintPos.X:=0;
  end;
  while aX-fCurrentPrintPos.X>=fCurrentCharWidth do
  begin
    DeviceWrite(PrintCode('Space'));
    Inc(fCurrentPrintPos.X,fCurrentCharWidth);
  end;
end;

function TRLDraftFilter.StretchFontSize(aSize:integer):integer;
var
  f:double;
begin
  f:=FormFactorX;
  case fStretchCharWidth of
    scEnlargementsOnly: if f>1 then
                          Result:=Round(aSize*f)
                        else
                          Result:=aSize;
    scShrinksOnly     : if f<1 then
                          Result:=Round(aSize*f)
                        else
                          Result:=aSize;
    scAlways          : Result:=Round(aSize*f);
  else
    Result:=aSize;
  end;
end;

function TRLDraftFilter.SelectFontCPP(const aFontName:string; aSize:integer):integer;
begin
  if fCPPSelection=csAutomatic then
    Result:=FontSizeToCPP(aFontName,StretchFontSize(aSize))
  else
    Result:=CPPSelectionToCPP(fCPPSelection);
end;

function TRLDraftFilter.FontSizeToCPP(const aFontName:string; aSize:integer):integer;
const
  TextSample='In addition, a check is made to see if the screen or printer is a '+
             'palette device, and if so, palette handling for the device is enabled.';
  MagicDelta=14/10;
var
  OneCharWidth:double;
  CharsPerInch:double;
  IsPitchFixed:boolean;
  B           : Tbitmap;
begin
  B:= AuxBitmapNeeded;
  B.Canvas.Font.Name:=aFontName;
  B.Canvas.Font.Size:=aSize;
  {redução de leak}
//  AuxBitmapNeeded.Canvas.Font.Name:=aFontName;
//  AuxBitmapNeeded.Canvas.Font.Size:=aSize;
  { a largura média de um caractere em pixels é dada pela largura da amostra em
    pixels dividida pela largura da amostra em caracteres com a fonte indicada.
    um fator "mágico" foi calculado através de testes para chegar ao valor ideal}
  IsPitchFixed:=(B.Canvas.TextWidth('.')=B.Canvas.TextWidth('W'));
  if IsPitchFixed then
    OneCharWidth:=B.Canvas.TextWidth(TextSample)/Length(TextSample)
    else
    OneCharWidth:=MagicDelta*B.Canvas.TextWidth(TextSample)/Length(TextSample);

  CharsPerInch:=ScreenPPI/OneCharWidth;
  //
  if fFontSizeTextSize = true then
     Result:= aSize else begin
  if CharsPerInch<=5 then
    Result:=5
  else if CharsPerInch<=10 then
    Result:=10
  else if CharsPerInch<=12 then
    Result:=12
  else if CharsPerInch<=17 then
    Result:=17
  else
    Result:=20;
  end;

  FreeObj(B);
end;

function TRLDraftFilter.CPPSelectionToCPP(aCPP:TRLCPPSelection):integer;
begin
  case aCPP of
    csFixed5CPP : Result:=5;
    csFixed10CPP: Result:=10;
    csFixed12CPP: Result:=12;
    csFixed17CPP: Result:=17;
    csFixed20CPP: Result:=20;
  else
    Result:=fCurrentCPP;
  end;
end;

// ajusta fonte da impressora
procedure TRLDraftFilter.SetHorzCompression(aCPP:integer; aForce:boolean=False);
begin
  // verifica a real necessidade
  if (aCPP=fCurrentCPP) and not aForce then
    Exit;
  if fCurrentCPP=5 then
    DeviceWrite(PrintCode('ExpandOff'));
  case aCPP of
    5 : begin
          DeviceWrite(PrintCode('CPP10'));
          DeviceWrite(PrintCode('ExpandOn')); // CPP10 + expandido
        end;
    10: DeviceWrite(PrintCode('CPP10'));
    12: DeviceWrite(PrintCode('CPP12'));
    17: DeviceWrite(PrintCode('CPP17'));
    20: DeviceWrite(PrintCode('CPP20'));
  end;
  fCurrentCPP      :=aCPP;
  fCurrentCharWidth:=Trunc(PinsPerCol*StandardCPP/fCurrentCPP);
  // força o reposicionamento do carro para a nova compressão
  with fCurrentPrintPos do
    SetPrintPos(Y,0);
end;

procedure TRLDraftFilter.SetVertCompression(aLPP:integer; aForce:boolean=False);
begin
  SetCharHeight(LPPToPins(aLPP),aForce);
end;

procedure TRLDraftFilter.SetCharHeight(aCharHeight:integer; aForce:boolean=False);
var
  standardpins:integer;
begin
  if (aCharHeight=fCurrentCharHeight) and not aForce then
    Exit;
  standardpins      :=LPPToPins(StandardLPP);
  fCurrentCharHeight:=aCharHeight;
  if fCurrentCharHeight=standardpins then
    DeviceWrite(PrintCode('MicroOff'))
  else  
    DeviceWrite(PrintCode('MicroOn',fCurrentCharHeight));
end;

procedure TRLDraftFilter.SetPrintStyle(aStyle:byte);
var
  newbold     :boolean;
  newitalic   :boolean;
  newunderline:boolean;
begin
  // negrito
  newbold:=((aStyle and MetaFontStyleBold)=MetaFontStyleBold) and (tsBold in fTextStyles);
  if newbold<>fCurrentBoldState then
  begin
    if newbold then
      DeviceWrite(PrintCode('BoldOn'))
    else
      DeviceWrite(PrintCode('BoldOff'));
    fCurrentBoldState:=newbold;
  end;
  // itálico
  newitalic:=((aStyle and MetaFontStyleItalic)=MetaFontStyleItalic) and (tsItalic in fTextStyles);
  if newitalic<>fCurrentItalicState then
  begin
    if newitalic then
      DeviceWrite(PrintCode('ItalicOn'))
    else
      DeviceWrite(PrintCode('ItalicOff'));
    fCurrentItalicState:=newitalic;
  end;
  // underline
  newunderline:=((aStyle and MetaFontStyleUnderline)=MetaFontStyleUnderline) and (tsUnderline in fTextStyles);
  if newunderline<>fCurrentUnderlineState then
  begin
    if newunderline then
      DeviceWrite(PrintCode('UnderlineOn'))
    else
      DeviceWrite(PrintCode('UnderlineOff'));
    fCurrentUnderlineState:=newunderline;
  end;
end;

function IsSameFont(aObj1,aObj2:TObject):boolean;
begin
  Result:=(TDraftText(aObj1).CPP=TDraftText(aObj2).CPP) and (TDraftText(aObj1).Style=TDraftText(aObj2).Style);
end;

procedure TRLDraftFilter.AddObj(aObj:TDraftObj; aList:TObjectList);
var
  i:integer;
  o:TDraftObj;
begin
  // posicionamento na lista
  i:=0;
  while i<aList.Count do
  begin
    o:=TDraftObj(aList[i]);
    if aObj.PinBounds.Top<o.PinBounds.Top then
      break
    else if aObj.PinBounds.Top=o.PinBounds.Top then
      if aObj is TDraftText then
        if not (o is TDraftText) then
          break
        else if IsSameFont(aObj,o) then
          if aObj.PinBounds.Left<o.PinBounds.Left then
            break
          else
        else
      else if aObj.PinBounds.Left<o.PinBounds.Left then
        break;
    Inc(i);
  end;
  aList.Insert(i,aObj);
end;

procedure TRLDraftFilter.AddSimpleText(const aText:string; aCPP,aStyle,aPinX,aPinY:integer; aList:TObjectList);
var
  ob:TDraftText;
  cw:integer;
begin
  cw:=CPPToPins(aCPP);
  ob:=TDraftText.Create;
  ob.CPP             :=aCPP;
  ob.Style           :=aStyle;
  ob.PinBounds.Left  :=PinXToGrid(aPinX,cw);
  ob.PinBounds.Top   :=aPinY;
  ob.PinBounds.Right :=ob.PinBounds.Left+LengthToPins(Length(aText),cw);
  ob.PinBounds.Bottom:=aPinY+LPPToPins(StandardLPP);
  ob.Text            :=aText;
  AddObj(ob,aList);
end;

procedure TRLDraftFilter.AddText(aObj:TRLTextObject; aList:TObjectList);
var
  w,i:integer;
  ob :TDraftText;
  r  :TRect;
  s  :string;
  cw :integer;
begin
  ob:=TDraftText.Create;
  ob.CPP  :=SelectFontCPP(aObj.Font.Name,aObj.Font.Size);
  ob.Style:=aObj.Font.Style;
  r.Left  :=aObj.BoundsRect.Left;
  r.Top   :=aObj.BoundsRect.Top;
  r.Right :=aObj.BoundsRect.Right;
  r.Bottom:=aObj.BoundsRect.Bottom;
  s       :=aObj.DisplayText;
  cw      :=CPPToPins(ob.CPP);
  if (aObj.TextFlags and MetaTextFlagAutoSize)=0 then
  begin
    // corta o texto
    w:=PinToCol(PixelToPinX(r.Right),cw)-PinToCol(PixelToPinX(r.Left),cw)+1;
    case aObj.Alignment of
      MetaTextAlignmentLeft   : s:=Copy(s,1,w);
      MetaTextAlignmentRight  : s:=Copy(s,Max(1,Length(s)-w+1),w);
      MetaTextAlignmentCenter : s:=Copy(s,Max(1,(Length(s)-w) div 2),w);
      MetaTextAlignmentJustify: begin
                                 s:=Copy(s,1,w);
                                 i:=Length(s);
                                 while (Length(s)<w) and IterateJustification(s,i) do
                                   ;
                               end;
    end;
  end;
  // posicao esquerda
  case aObj.Alignment of
    MetaTextAlignmentLeft   : ob.PinBounds.Left:=PinXToGrid(PixelToPinX(aObj.Origin.X),cw);
    MetaTextAlignmentRight  : ob.PinBounds.Left:=ColToPin(PinToCol(PixelToPinX(r.Right),cw)-Length(s),cw);
    MetaTextAlignmentCenter : ob.PinBounds.Left:=ColToPin((PinToCol(PixelToPinX(r.Left),cw)+PinToCol(PixelToPinX(r.Right),cw)-Length(s)) div 2,cw);
    MetaTextAlignmentJustify: ob.PinBounds.Left:=PinXToGrid(PixelToPinX(aObj.Origin.X),cw);
  end;
  //
  ob.PinBounds.Top   :=PixelToPinY(aObj.Origin.Y);
  ob.PinBounds.Right :=ob.PinBounds.Left+Length(s)*cw;
  ob.PinBounds.Bottom:=ob.PinBounds.Top +LPPToPins(StandardLPP);
  ob.Text            :=s;
  //
  if (ob.Style and MetaFontStyleUnderline)<>MetaFontStyleUnderline then
  begin
    ob.Text:=TrimRight(ob.Text);
    while (ob.Text<>'') and (ob.Text[1]=#32) do
    begin
      Inc(ob.PinBounds.Left,CPPToPins(ob.CPP));
      Delete(ob.Text,1,1);
    end;
  end;
  //
  AddObj(ob,aList);
end;

procedure TRLDraftFilter.AddGraphic(aGraphic:TGraphic; const aRect:TRect; aList:TObjectList);
var
  asbitmap :TBitmap;
  tempbmp  :TBitmap;
  thewidth :integer;
  theheight:integer;
  y,ystep  :integer;
  ob       :TDraftRaw;
begin
  if fDitheringMethod=dmNone then
    Exit;
  thewidth :=Round((aRect.Right-aRect.Left)*AspectratioX);
  theheight:=Round((aRect.Bottom-aRect.Top)*AspectratioY);
  asbitmap:=NewBitmap(thewidth,theheight);
  try
    tempbmp:=NewBitmap(thewidth,theheight);
    try
      tempbmp.Canvas.StretchDraw(Rect(0,0,thewidth,theheight),aGraphic);
      case fDitheringMethod of
        dmNone          :;
        dmColorTable    : DoColorTable(tempbmp,asbitmap);
        dmErrorDiffusion: DoErrorDiffusion(tempbmp,asbitmap);
      end;
    finally
      tempbmp.Free;
    end;
    //
    y:=0;
    while y<asbitmap.Height do
    begin
      ystep:=asbitmap.Height-y;
      if ystep>8 then
        ystep:=8;
      ob:=TDraftRaw.Create;
      try
        ob.Bitmap:=NewBitmap(asbitmap.Width,ystep);
        ob.Bitmap.Canvas.Draw(0,-y,asbitmap);
        ob.PinBounds.Left  :=PixelToPinX(aRect.Left);
        ob.PinBounds.Top   :=PixelToPinY(aRect.Top)+y;
        ob.PinBounds.Right :=PixelToPinX(aRect.Right);
        ob.PinBounds.Bottom:=ob.PinBounds.Top+ystep;
        AddObj(ob,aList);
      except
        ob.Free;
        raise;
      end;
      Inc(y,ystep);
    end;
  finally
    asbitmap.Free;
  end;
end;

procedure TRLDraftFilter.AddImage(aObj:TRLImageObject; aList:TObjectList);
var
  thegraphic:TGraphic;
begin
  if fDitheringMethod=dmNone then
    Exit;
  thegraphic:=FromMetaGraphic(aObj.Data);
  try
    AddGraphic(thegraphic,FromMetaRect(aObj.BoundsRect),aList);
  finally
    thegraphic.Free;
  end;
end;

procedure TRLDraftFilter.AddRectangle(aObj:TRLRectangleObject; aList:TObjectList);
const
  HorzTraceChars:array[TRLLineDrawMethod,Boolean] of char=((#32,#32),('-','='),(#196,#205));
  VertTraceChars:array[TRLLineDrawMethod,Boolean] of char=((#32,#32),('|','|'),(#179,#186));
var
  RectHorzLength       :integer;
  RectVertLength       :integer;
  OneLetterWidthInPins :integer;
  OneLetterHeightInPins:integer;
  VertY1,VertY2        :integer;
  TraceCPP             :integer;
  ReplCh               :char;
  aux                  :string;
begin
  if fLineDrawMethod=ldNone then
    Exit;
  // se não for sólido
  if aObj.Pen.Style<>MetaPenStyleSolid then
    Exit;
  // se for de cor clara
  with aObj.Pen.Color do
    if (Red+Green+Blue)/3>127 then
      Exit;
  // escolhe o cpp default para o retângulo
  TraceCPP:=SelectFontCPP(fTraceFontName,fTraceFontSize);
  // tamanho de um caractere em agulhas no cpp padrão
  OneLetterWidthInPins :=CPPToPins(TraceCPP);
  OneLetterHeightInPins:=LPPToPins(StandardLPP);
  // calcula as dimensões do retangulo em caracteres
  RectHorzLength:=PinToCol(PixelToPinX(aObj.BoundsRect.Right ),OneLetterWidthInPins )-PinToCol(PixelToPinX(aObj.BoundsRect.Left),OneLetterWidthInPins );
  RectVertLength:=PinToRow(PixelToPinY(aObj.BoundsRect.Bottom),OneLetterHeightInPins)-PinToRow(PixelToPinY(aObj.BoundsRect.Top ),OneLetterHeightInPins);
  // decide a direção do traço quando largura e altura são iguais à unidade
  if (RectHorzLength=1) and (RectVertLength=1) then
    if Abs(aObj.BoundsRect.Right-aObj.BoundsRect.Left)>Abs(aObj.BoundsRect.Bottom-aObj.BoundsRect.Top) then
      RectVertLength:=0
    else
      RectHorzLength:=0;
  // linhas horizontais    
  if RectHorzLength>0 then
  begin
    ReplCh:=HorzTraceChars[fLineDrawMethod,aObj.Pen.Width>1];
    aux   :=StringOfChar(ReplCh,RectHorzLength);
    AddSimpleText(aux,TraceCPP,0,PixelToPinX(aObj.BoundsRect.Left),PixelToPinY(aObj.BoundsRect.Top)-OneLetterHeightInPins div 2,aList);
    if RectVertLength>1 then
      AddSimpleText(aux,TraceCPP,0,PixelToPinX(aObj.BoundsRect.Left),PixelToPinY(aObj.BoundsRect.Bottom)-OneLetterHeightInPins div 2,aList);
  end;
  // linhas verticais
  if RectVertLength>0 then
  begin
    ReplCh:=VertTraceChars[fLineDrawMethod,aObj.Pen.Width>1];
    VertY1:=PinYToGrid(PixelToPinY(aObj.BoundsRect.Top   ),fTraceVertTabOrigin,fTraceVertTabStep);
    VertY2:=PinYToGrid(PixelToPinY(aObj.BoundsRect.Bottom),fTraceVertTabOrigin,fTraceVertTabStep);
    while not (VertY1+fTraceVertTabStep div 2>VertY2) do
    begin
      AddSimpleText(ReplCh,TraceCPP,0,PixelToPinX(aObj.BoundsRect.Left)-OneLetterWidthInPins div 2,VertY1,aList);
      if RectHorzLength>1 then
        AddSimpleText(ReplCh,TraceCPP,0,PixelToPinX(aObj.BoundsRect.Right)-OneLetterWidthInPins div 2,VertY1,aList);
      Inc(VertY1,fTraceVertTabStep);
    end;
  end;
end;

procedure TRLDraftFilter.AddFillRect(aObj:TRLFillRectObject; aList:TObjectList);
var
  w,h,i,gh,chw,chh,cpp:integer;
  gch:char;
  s:string;
begin
  if fFillArtMethod=fmNone then
    Exit;
  // se não for sólido  
  if aObj.Brush.Style<>MetaBrushStyleSolid then
    Exit;
  // se for de cor clara  
  with aObj.Brush.Color do
    if (Red+Green+Blue)/3>127 then
      Exit;
  //    
  cpp:=SelectFontCPP(fTraceFontName,fTraceFontSize);
  chw:=CPPToPins(cpp);
  chh:=LPPToPins(StandardLPP);
  w  :=PinToCol(PixelToPinX(aObj.BoundsRect.Right),chw)-PinToCol(PixelToPinX(aObj.BoundsRect.Left),chw);
  h  :=(PixelToPinY(aObj.BoundsRect.Bottom)-PixelToPinY(aObj.BoundsRect.Top)) div chh;
  if w>0 then
  begin
    case fFillArtMethod of
      fmLetterX       : gch:='x';
      fmGraphicCharset: gch:=#219;
    else
      gch:=' ';
    end;
    s:=StringOfChar(gch,w);
    AddSimpleText(s,cpp,0,PixelToPinX(aObj.BoundsRect.Left),PixelToPinY(aObj.BoundsRect.Top-chh div 2),aList);
  end
  else if h>0 then
  begin
    case fFillArtMethod of
      fmLetterX       : gch:='x';
      fmGraphicCharset: gch:=#219;
    else
      gch:=' ';
    end;
    gh:=(aObj.BoundsRect.Bottom-aObj.BoundsRect.Top) div h;
    for i:=0 to h-1 do
      AddSimpleText(gch,cpp,0,PixelToPinX(aObj.BoundsRect.Left-chw div 2),PixelToPinY(aObj.BoundsRect.Top+i*gh),aList);
  end;
end;

procedure TRLDraftFilter.AddLine(aObj:TRLLineObject; aList:TObjectList);
const
  HorzTraceChars:array[TRLLineDrawMethod,Boolean] of char=((#32,#32),('-','='),(#196,#205));
  VertTraceChars:array[TRLLineDrawMethod,Boolean] of char=((#32,#32),('|','|'),(#179,#186));
var
  RectHorzLength       :integer;
  RectVertLength       :integer;
  OneLetterWidthInPins :integer;
  OneLetterHeightInPins:integer;
  VertY1,VertY2        :integer;
  TraceCPP             :integer;
  ReplCh               :char;
  aux                  :string;
begin
  if fLineDrawMethod=ldNone then
    Exit;
  // se não for sólido
  if aObj.Pen.Style<>MetaPenStyleSolid then
    Exit;
  // se for de cor clara
  with aObj.Pen.Color do
    if (Red+Green+Blue)/3>127 then
      Exit;
  // se não for uma linha horizontal ou vertical
  if (Abs(aObj.FromPoint.X-aObj.ToPoint.X)>0) and (Abs(aObj.FromPoint.Y-aObj.ToPoint.Y)>0) then
    Exit;     
  // escolhe o cpp default para a linha
  TraceCPP:=SelectFontCPP(fTraceFontName,fTraceFontSize);
  // tamanho de um caractere em agulhas no cpp padrão
  OneLetterWidthInPins :=CPPToPins(TraceCPP);
  OneLetterHeightInPins:=LPPToPins(StandardLPP);
  // calcula as dimensões do retangulo em caracteres
  RectHorzLength:=PinToCol(PixelToPinX(aObj.ToPoint.X),OneLetterWidthInPins)-PinToCol(PixelToPinX(aObj.FromPoint.X),OneLetterWidthInPins);
  RectVertLength:=(PixelToPinY(aObj.BoundsRect.Bottom)-PixelToPinY(aObj.BoundsRect.Top)) div OneLetterHeightInPins;
  // decide a direção do traço quando largura e altura são iguais à unidade
  if (RectHorzLength=1) and (RectVertLength=1) then
    if Abs(aObj.BoundsRect.Right-aObj.BoundsRect.Left)>Abs(aObj.BoundsRect.Bottom-aObj.BoundsRect.Top) then
      RectVertLength:=0
    else
      RectHorzLength:=0;
  // linhas horizontais    
  if RectHorzLength>0 then
  begin
    ReplCh:=HorzTraceChars[fLineDrawMethod,aObj.Pen.Width>1];
    aux   :=StringOfChar(ReplCh,RectHorzLength);
    AddSimpleText(aux,TraceCPP,0,PixelToPinX(aObj.BoundsRect.Left),PixelToPinY(aObj.BoundsRect.Top)-OneLetterHeightInPins div 2,aList);
    if RectVertLength>1 then
      AddSimpleText(aux,TraceCPP,0,PixelToPinX(aObj.BoundsRect.Left),PixelToPinY(aObj.BoundsRect.Bottom)-OneLetterHeightInPins div 2,aList);
  end;
  // linhas verticais
  if RectVertLength>0 then
  begin
    ReplCh:=VertTraceChars[fLineDrawMethod,aObj.Pen.Width>1];
    VertY2:=PixelToPinY(aObj.BoundsRect.Top);
    VertY1:=fTraceVertTabOrigin;
    while VertY1+fTraceVertTabStep<VertY2 do
      Inc(VertY1,fTraceVertTabStep);
    VertY2:=PixelToPinY(aObj.BoundsRect.Bottom);
    while VertY1<VertY2 do
    begin
      AddSimpleText(ReplCh,TraceCPP,0,PixelToPinX(aObj.BoundsRect.Left)-OneLetterWidthInPins div 2,VertY1,aList);
      if RectHorzLength>1 then
        AddSimpleText(ReplCh,TraceCPP,0,PixelToPinX(aObj.BoundsRect.Right)-OneLetterWidthInPins div 2,VertY1,aList);
      Inc(VertY1,fTraceVertTabStep);
    end;
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

procedure TRLDraftFilter.AddPixel(aObj:TRLPixelObject; aList:TObjectList);
var
  bmp:TBitmap;
begin
  bmp:=CreateBitmap(aObj);
  try
    with aObj.Color do
      bmp.Canvas.Pen.Color:=RGB(Red,Green,Blue);
    bmp.Canvas.MoveTo(aObj.BoundsRect.Left-fPrintCut.X-aObj.BoundsRect.Left,aObj.BoundsRect.Top-fPrintCut.Y-aObj.BoundsRect.Top);
    bmp.Canvas.LineTo(aObj.BoundsRect.Left-fPrintCut.X-aObj.BoundsRect.Left+1,aObj.BoundsRect.Top-fPrintCut.Y-aObj.BoundsRect.Top);
    AddGraphic(bmp,FromMetaRect(aObj.BoundsRect),aList);
  finally
    bmp.Free;
  end;
end;

procedure TRLDraftFilter.AddEllipse(aObj:TRLEllipseObject; aList:TObjectList);
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
    AddGraphic(bmp,FromMetaRect(aObj.BoundsRect),aList);
  finally
    bmp.Free;
  end;
end;

procedure TRLDraftFilter.AddPolygon(aObj:TRLPolygonObject; aList:TObjectList);
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
    AddGraphic(bmp,FromMetaRect(aObj.BoundsRect),aList);
  finally
    bmp.Free;
  end;
end;

procedure TRLDraftFilter.AddPolyline(aObj:TRLPolylineObject; aList:TObjectList);
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
    AddGraphic(bmp,FromMetaRect(aObj.BoundsRect),aList);
  finally
    bmp.Free;
  end;
end;

procedure TRLDraftFilter.AddSetClipRect(aObj:TRLSetClipRectObject; aList:TObjectList);
begin
end;

procedure TRLDraftFilter.AddResetClipRect(aObj:TRLResetClipRectObject; aList:TObjectList);
begin
end;

procedure TRLDraftFilter.FindDominantFont(aPage:TRLGraphicSurface; var aFontName:string; var aFontSize:integer);
var
  obj:TRLGraphicObject;
  i,j:integer;
  l  :TStringList;
  s  :string;
begin
  l:=TStringList.Create;
  try
    for i:=0 to aPage.ObjectCount-1 do
    begin
      obj:=aPage.Objects[i];
      if obj is TRLTextObject then
      begin
        s:=TRLTextObject(obj).Font.Name+'|'+IntToStr(TRLTextObject(obj).Font.Size);
        j:=l.IndexOf(s);
        if j=-1 then
          l.AddObject(s,Pointer(1))
        else
          l.Objects[j]:=Pointer(Integer(l.Objects[j])+1);
      end;
    end;
    j:=-1;
    for i:=0 to l.Count-1 do
      if (j=-1) or (Integer(l.Objects[i])>Integer(l.Objects[j])) then
        j:=i;
    if j=-1 then
    begin
      aFontName:=DefaultFontName;
      aFontSize:=DefaultFontSize;
    end
    else
    begin
      aFontName:=Token(l[j],1);
      aFontSize:=StrToInt(Token(l[j],2));
    end;
  finally
    l.Free;
  end;
end;

procedure TRLDraftFilter.FindVertTabs(aPage:TRLGraphicSurface; var aOrigin,aStep:integer);
type
  TTab=record
         Tab  :integer;
         Delta:integer;
       end;
var
  tabs:record
         Count:integer;
         Items:packed array of TTab;
       end;
  procedure AddTab(aTab:integer);
  var
    i:integer;
  begin
    i:=0;
    while (i<tabs.Count) and (tabs.Items[i].Tab<aTab) do
      Inc(i);
    if (i<tabs.Count) and (tabs.Items[i].Tab=aTab) then
      Exit;
    if tabs.Count+1>Length(tabs.Items) then
      SetLength(tabs.Items,Length(tabs.Items)+1024);
    Move(tabs.Items[i],tabs.Items[i+1],(tabs.Count-i)*SizeOf(tabs.Items[0]));
    tabs.Items[i].Tab:=aTab;
    Inc(tabs.Count);
  end;
type
  THeight=record
            Height:integer;
            Count :integer;
          end;
var
  heights:record
            Count:integer;
            Items:packed array of THeight;
          end;
  procedure AddHeight(aHeight:integer; var aTheMost:integer);
  var
    i:integer;
  begin
    i:=0;
    while (i<heights.Count) and (heights.Items[i].Height<>aHeight) do
      Inc(i);
    if i<heights.Count then
    begin
      Inc(heights.Items[i].Count);
      if (aTheMost=-1) or (heights.Items[i].Count>heights.Items[aTheMost].Count) then
        aTheMost:=i;
    end
    else
    begin
      if heights.Count+1>Length(heights.Items) then
        SetLength(heights.Items,Length(heights.Items)+1024);
      heights.Items[i].Height:=aHeight;
      heights.Items[i].Count :=1;
      if aTheMost=-1 then
        aTheMost:=i;
      Inc(heights.Count);
    end;
  end;
var
  obj:TRLGraphicObject;
  i,j:integer;
  aux:integer;
begin
  // procura e ordena as tabulações verticais
  tabs.Count:=0;
  for i:=0 to aPage.ObjectCount-1 do
  begin
    obj:=aPage.Objects[i];
    if obj is TRLTextObject then
      AddTab(PixelToPinY(TRLTextObject(obj).Origin.Y));
  end;
  // contabiliza os espaços verticais e encontra o predominante
  heights.Count:=0;
  j:=-1;
  i:=0;
  while i<tabs.Count-1 do
  begin
    AddHeight(tabs.Items[i+1].Tab-tabs.Items[i].Tab,j);
    Inc(i);
  end;
  //
  aux:=LPPToPins(StandardLPP);
  if heights.Count>0 then
  begin
    aStep:=heights.Items[j].Height;
    if aStep>aux then
      aStep:=aux;
    // pesquisa a geratriz com menor desvio padrão
    for i:=0 to tabs.Count-1 do
    begin
      tabs.Items[i].Delta:=0;
      for j:=0 to tabs.Count-1 do
        if j<>i then
          Inc(tabs.Items[i].Delta,Abs(tabs.Items[i].Tab-tabs.Items[j].Tab) mod aStep);
    end;
    j:=-1;
    for i:=0 to tabs.Count-1 do
      if (j=-1) or (tabs.Items[i].Delta<tabs.Items[j].Delta) then
        j:=i;
    //
    aOrigin:=tabs.Items[j].Tab;
    while aOrigin-aStep>=0 do
      Dec(aOrigin,aStep);
  end
  else
  begin
    aStep  :=aux;
    aOrigin:=0;
  end;
end;

// seleciona objetos interessantes e os ordena
procedure TRLDraftFilter.GetObjList(aPage:TRLGraphicSurface; aList:TObjectList);
var
  obj:TRLGraphicObject;
  i  :integer;
begin
  aList.Clear;
  // verifica qual a fonte predominate nos textos. ela será a fonte dos traços
  FindDominantFont(aPage,fTraceFontName,fTraceFontSize);
  // analisa a página e calcula a geratriz para os traços verticais
  FindVertTabs(aPage,fTraceVertTabOrigin,fTraceVertTabStep);
  //
  for i:=0 to aPage.ObjectCount-1 do
  begin
    obj:=aPage.Objects[i];
    if obj is TRLPixelObject then
      AddPixel(TRLPixelObject(obj),aList)
    else if obj is TRLLineObject then
      AddLine(TRLLineObject(obj),aList)
    else if obj is TRLRectangleObject then
      AddRectangle(TRLRectangleObject(obj),aList)
    else if obj is TRLTextObject then
      AddText(TRLTextObject(obj),aList)
    else if obj is TRLFillRectObject then
      AddFillRect(TRLFillRectObject(obj),aList)
    else if obj is TRLEllipseObject then
      AddEllipse(TRLEllipseObject(obj),aList)
    else if obj is TRLPolygonObject then
      AddPolygon(TRLPolygonObject(obj),aList)
    else if obj is TRLPolylineObject then
      AddPolyline(TRLPolylineObject(obj),aList)
    else if obj is TRLImageObject then
      AddImage(TRLImageObject(obj),aList)
    else if obj is TRLSetClipRectObject then
      AddSetClipRect(TRLSetClipRectObject(obj),aList)
    else if obj is TRLResetClipRectObject then
      AddResetClipRect(TRLResetClipRectObject(obj),aList);
  end;
end;

procedure StuffText(const aText:string; var aInto:string; aPos:integer);
var
  intolen,textlen,fill,i,j:integer;
begin
  textlen:=Length(aText);
  intolen:=Length(aInto);
  fill   :=aPos+textlen-1-intolen;
  if fill>0 then
    Insert(StringOfChar(#32,fill),aInto,intolen+1);
  for i:=1 to textlen do
  begin
    j:=aPos+i-1;
    if ((aText[i]='-') and (aInto[j]='|')) or ((aText[i]='|') and (aInto[j]='-')) then
      aInto[j]:='+'
    else
      aInto[j]:=aText[i];
  end;
end;

procedure TRLDraftFilter.InternalDrawPage(aPage:TRLGraphicSurface);
type
  TRGB4=packed record
    rgbBlue    :byte;
    rgbGreen   :byte;
    rgbRed     :byte;
    rgbReserved:byte;
  end;
var
  objlist:TObjectList;
  obj    :TDraftObj;
  draft  :TDraftText;
  draft2 :TDraftText;
  b      :TBitmap;
  i,j,k  :integer;
  fmttext:string;
  function IsNextCompatibleText(aDraft1:TDraftText; var aDraft2:TDraftText):boolean;
  begin
    Result:=False;
    if (i<objlist.Count-1) and (TDraftObj(objlist[i+1]) is TDraftText) then
    begin
      aDraft2:=TDraftText(objlist[i+1]);
      if IsSameFont(aDraft1,aDraft2) and (aDraft2.PinBounds.Top=aDraft1.PinBounds.Top) then
        Result:=True;
    end;
  end;
begin
  objlist:=TObjectList.Create;
  try
    // seleciona textos
    GetObjList(aPage,objlist);
    // inicialização da impressora
    if fSendFirstReset then
      DeviceWrite(PrintCode('Reset'));
    fSendFirstReset:=False;
    // reseta fonte e compressão
    SetHorzCompression(StandardCPP,True);
    SetVertCompression(StandardLPP,True);
    SetPrintStyle(0);
    // imprime textos e imagens
    i:=0;
    while i<objlist.Count do
    begin
      obj:=TDraftObj(objlist[i]);
      if obj.Enabled then
      begin
        //
        if obj is TDraftText then
        begin
          draft:=TDraftText(obj);
          // aglutina textos compatíveis
          while IsNextCompatibleText(draft,draft2) do
          begin
            j:=PinToCol(draft.PinBounds.Left,CPPToPins(draft.CPP));
            k:=PinToCol(draft2.PinBounds.Left,CPPToPins(draft2.CPP));
            StuffText(draft2.Text,draft.Text,k-j+1);
            Inc(i);
          end;
          // ajusta compressão
          SetHorzCompression(draft.CPP);
          // não pode imprimir antes da linha atual (ou fora da área imprimível)
          if draft.PinBounds.Top<fCurrentPrintPos.Y then
            OffsetRect(draft.PinBounds,0,fCurrentPrintPos.Y-draft.PinBounds.Top);
          // posiciona
          SetPrintPos(draft.PinBounds.Top,draft.PinBounds.Left);
          // transforma acentos
          case fAccentMethod of
            amOverwrite: fmttext:=TranslateAccents(draft.Text,PrintCode('BS'));
            amTakeOut  : fmttext:=RemoveAccents(draft.Text);
          else
            fmttext:=draft.Text;
          end;
          // envolve o texto com o estilo
          SetPrintStyle(draft.Style);
          DeviceWrite(fmttext);
          Inc(fCurrentPrintPos.X,Length(draft.Text)*fCurrentCharWidth);
          SetPrintStyle(0);
        end
        else if obj is TDraftRaw then
        begin
          // não pode imprimir antes da linha atual (ou fora da área imprimível)
          if obj.PinBounds.Top<fCurrentPrintPos.Y then
            OffsetRect(obj.PinBounds,0,fCurrentPrintPos.Y-obj.PinBounds.Top);
          // posicionamento
          SetPrintPos(obj.PinBounds.Top,obj.PinBounds.Left);
          //
          b:=TDraftRaw(obj).Bitmap;
          DeviceWrite(PrintCode('RAW',b.Width));
          for j:=0 to b.Width-1 do
            DeviceWrite(Char(Byte(GetBitmapPixel(b,j,0,clWhite)=clBlack)*128+
                             Byte(GetBitmapPixel(b,j,1,clWhite)=clBlack)*64+
                             Byte(GetBitmapPixel(b,j,2,clWhite)=clBlack)*32+
                             Byte(GetBitmapPixel(b,j,3,clWhite)=clBlack)*16+
                             Byte(GetBitmapPixel(b,j,4,clWhite)=clBlack)*8+
                             Byte(GetBitmapPixel(b,j,5,clWhite)=clBlack)*4+
                             Byte(GetBitmapPixel(b,j,6,clWhite)=clBlack)*2+
                             Byte(GetBitmapPixel(b,j,7,clWhite)=clBlack)*1));
          //
          fCurrentPrintPos.X:=PixelToPinX(obj.PinBounds.Right);
        end;
      end;
      Inc(i);
    end;
    // reseta fonte e compressão
    SetHorzCompression(StandardCPP);
    SetVertCompression(StandardLPP);
    SetPrintStyle(0);
  finally
    objlist.Free;
  end;
end;

procedure TRLDraftFilter.SetCommands(const Value: TStrings);
begin
  fCommands.Assign(Value);
end;

procedure TRLDraftFilter.SetPrinterFamily(const Value: TRLPrinterFamily);
begin
  if Value=fPrinterFamily then
    Exit;
  fPrinterFamily:=Value;
  //
  DefaultCommands;
end;

procedure TRLDraftFilter.Loaded;
begin
  inherited;
  //
{$ifdef LINUX}
  if (fDeviceKind=dkProgram) and (Copy(fDevicePath,1,5)='/dev/') then
    fDevicePath:='lpr -P%p %f';
{$endif};
end;

function TRLDraftFilter.IsCustomPrinterFamily:boolean;
begin
  Result:=(fPrinterFamily=fmCustom);
end;

function TRLDraftFilter.IsCustomDevice:boolean;
begin
{$ifdef MSWINDOWS}
  Result:=(fDeviceKind<>dkPrinterPort) or not AnsiSameText(fDevicePath,'prn');
{$else}
  Result:=(fDeviceKind<>dkProgram) or (fDevicePath<>'lpr -P%p %f');
{$endif};
end;

function TRLDraftFilter.IsCustomTextStyle: Boolean;
begin
  Result:=(fTextDecoration=ddCustomized);
end;

procedure TRLDraftFilter.SetTextDecoration(const Value: TRLDraftTextDecoration);
begin
  if fTextDecoration=Value then
    Exit;
  fTextDecoration:=Value;
  //
  case fTextDecoration of
    ddIncludeNone: fTextStyles:=[];
    ddIncludeAll : fTextStyles:=[tsItalic,tsBold,tsUnderline];
    ddCustomized :;
  end;
end;

procedure TRLDraftFilter.SetTextStyle(const Value: TRLDraftTextStyles);
begin
  if fTextStyles=Value then
    Exit;
  fTextStyles:=Value;
  //
  fTextDecoration:=ddCustomized;
end;

procedure TRLDraftFilter.ExecuteDialog;
begin
  TRLDraftFilterDialogForm.Execute(Self);
end;

end.

