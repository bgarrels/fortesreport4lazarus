{@unit RLBarcode - Implementação dos componentes para código de barras.
Portado para o Lazarus - Trabalho inicial de Isaac Trindade da Silva contato tioneobrasil@yahoo.com.br dicas4lazarus@yahoo.com.br
Lazarus Ported - initial work by Isaac 07/2009
}
unit RLBarcode;

{$MODE DELPHI}
{$I RLReport.inc}

interface

uses
  LCLIntf, Graphics, Dialogs,
  Classes, SysUtils, DB,
  RLReport, RLConsts;

type
  {@type TRLBarcodeType - Padrão de codificação para o código de barras.
  Pode ser um dos seguintes valores:
  bcCode2OF5Interleaved - Código 25, também conhecido como "Código 2 de 5". É
    utilizado sobretudo no manuseio de inventários, em fichas de compensação
    bancária, na identificação de envelopes de acabamento de fotografias, em
    passagens aéreas, no manuseio de bagagens e cargas e em dezenas de outras
    aplicações. É um formato de código distinto, de comprimento variável e
    consiste em duas barras espessas em um total de cinco barras para cada
    caractere codificado. O código deve ter comprimento par;
  bcCode2OF5Industry - ITF ou "Entrelaçado de 2 de 5". Esse código de barras é um
    dos formatos mais populares utilizados pelas indústrias de transporte e de
    armazenamento e foi desenvolvido com base no Código 25. Ambos os formatos
    utilizam as mesmas técnicas de codificação, exceto que, no formato ITF,
    tanto as barras quanto os espaços transportam dados. Os dígitos de posição
    ímpar são codificados nas barras e os dígitos de posição par são codificados
    nos espaços. O ITF é um formato de alta densidade, de comprimento variável,
    exclusivamente numérico;
  bcCode2OF5Matrix - ver bcCode2OF5Industry;
  bcCode39 - Código 39, também conhecido como "Código 3 de 9", é o formato mais
    popular utilizado em inventário e controle não varejista. O formato consiste
    em três elementos espessos (barras ou espaços) em um totalizado em manufatura,
    aplicações militares e de saúde. O formato distinto de comprimento variável
    aceita os 44 caracteres seguintes: 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ.*$/+%. O
    asterisco (*) é utilizado como caractere de início/parada, não podendo ser
    utilizado no corpo da mensagem. Você também pode adicionar um dígito de verificação
    que ajude a garantir a segurança do código de barras. O Código 39 suporta os
    formatos de dígito de verificação Módulo 43 e xxx-nnnnnnn-c utilizados pela
    alfândega dos E.U.A. para remessas de importação/exportação e em dezenas de
    outras aplicações;
  bcCode39Extended - O código extendido 39 foi desenvolvido para proporcionar
    meios de codificar os caracteres adicionais que não são normalmente parte
    do conjunto de caracteres do código 39 (caracteres minúsculos e símbolos). Os
    caracteres extendidos são codificados por um par de caracteres normais do
    código 39; por exemplo, uma minúscula "a" (que não faz parte do conjunto de
    caracteres do código 39) pode ser codificado pelo par "+A". Um código de controle
    de retorno do carro pode ser codificado pelo par "#";
  bcCode128A - Código 128 é um formato alfanumérico de alta densidade e comprimento
    variável utilizado na indústria de transporte e etiquetagem. Esse código possui
    106 padrões de barras e espaços. Cada padrão pode ter três significados, dependendo
    de qual dos três conjuntos de caracteres é empregado. Um conjunto de caracteres
    codifica todos os caracteres de controle ASCII e maiúsculos, um outro codifica
    todos os caracteres maiúsculos e minúsculos e o terceiro conjunto codifica os
    pares de dígitos numéricos de 00 a 99. O conjunto de caracteres utilizado é
    determinado pelo caractere inicial. O Código 128 também permite codificar quatro
    códigos de função:
    FNC1 - reservado para uso em EAN (European Article Numbering);
    FNC2 - utilizado para instruir o leitor de código de barras na concatenação da
      mensagem em um símbolo de código de barras com a mensagem no símbolo de texto;
    FNC3 - utilizado para instruir o leitor de código de barras a efetuar uma redefinição;
    FNC4 - utilizado em aplicações de sistemas fechados.
    Uma variação do formato Código 128 é o EAN 128. Esse símbolo utiliza o mesmo
    conjunto de códigos que o Código 128, mas os códigos de função de FNC2 a FNC4
    não podem ser utilizados e FNC1 é utilizado como parte do código inicial;
  bcCode128B - ver bcCode128A;
  bcCode128C - ver bcCode128A;
  bcCode93 - O código 93 é uma versão mais compacta do código 39. Codifica
    exatamente os mesmos caracteres que o código 39, mas utiliza 9 elementos de
    barra por caractere ao invés de 15. O dígito verificador o dígito verificador
    módulo 43 é opcional, como no código 39;
  bcCode93Extended - ver bcCode93;
  bcMSI - O código de barras MSI Plessey é utilizado principalmente em bibliotecas e
    em etiquetagem de prateleiras de lojas. O MSI Plessey é um formato de comprimento
    variável que permite codificar os 10 caracteres seguintes: 0123456789. Cada caractere
    consiste em oito elementos: quatro barras e quatro espaços;
  bcPostNet - Os códigos de barras POSTNET (Postal Numeric Encoding Technique) são
    utilizados para codificar códigos de endereçamento postal no correio dos
    E.U.A. O processo de manuseio de correspondência do Serviço postal foi
    desenvolvido para ser totalmente automatizado e os códigos de barras POSTNET
    alimentam o equipamento automatizado. O POSTNET difere dos outros formatos em
    que a altura das barras varia, e não a largura das barras. Cada número é
    representado por um padrão de cinco barras. Uma única barra alta é utilizada
    para as barras de início e parada. O POSTNET pode ser utilizado como código
    de barras de ponto de entrega de cinco dígitos, de nove dígitos e de 11
    dígitos. Esses códigos são freqüentemente utilizados em conjunto com as barras
    FIM que se encontram no canto superior direito de uma correspondência, como
    cartões-resposta comerciais;
  bcCodaBar - O CodBar é utilizado freqüentemente em bibliotecas, bancos de
    sangue e na atividade de encomendas aéreas. O formato de comprimento variável
    permite a codificação dos 20 caracteres seguintes: 0123456789-$:/.+ABCD. Os
    caracteres de início e de parada de uma mensagem CodBar precisam ser A, B, C ou D;
  bcEAN8 - O sistema EAN (European Article Numbering) é uma versão européia do
    código UPC (Universal Product Code). Atualmente, esse código é denominado
    International Article Number, mas a abreviação EAN permanece. Os códigos
    EAN encontram-se em itens de varejo na Europa. Esse número é apropriado para uso
    em publicações e periódicos, aparecendo como um código de barras adicional
    no lado direito do código de barras principal. É a versão simplificada do padrão
    EAN-13 para aplicação em produtos onde a etiqueta no padrão EAN-13 fique muito
    grande. O EAN-8 codifica até oito dígitos, consistindo em dois dígitos do código
    do país, cinco dígitos de dados e um dígito de verificação. Um número opcional de
    dois ou cinco dígitos pode ser acrescentado ao código de barras principal;
  bcEAN13 - O EAN-13 é a versão européia do UPC (A) (Universal Product Code). É o
    padrão adotado pela ABAC (EAN Brasil) para codificação de produtos em
    supermercados. Também é designado para uso em publicações e periódicos, aparecendo
    como um código de barras adicional no lado direito do código de barras principal. Permite
    a codificação de até 13 dígitos numéricos. A diferença entre o EAN-13 e o
    UPC (A) é que o EAN-13 codifica um 13° dígito no padrão de paridade dos seis dígitos
    da esquerda de um símbolo UPC (A). Esse 13° dígito, combinado com o 12°, representa um
    código de país. Um número opcional de dois ou cinco dígitos pode ser acrescentado ao
    código de barras principal;
  bcUPC_A - Os símbolos UPC (Universal Product Code) são usados em aplicações de
    varejo nos Estados Unidos e no Canadá. O UPC(A) é um formato de 12
    dígitos. O símbolo consiste em 11 dígitos de dados e um dígito de
    verificação. Normalmente, o primeiro dígito representa o tipo de produto
    sendo identificado. Os cinco dígitos seguintes são um código de fabricante
    e os cinco dígitos seguintes são utilizados para identificar um produto específico;
  bcUPC_E0 - Como o UPC(A), o UPC(E) é utilizado em aplicações de varejo, no entanto,
    como o código de barras é menor, ele é mais adequado para itens menores. Esse formato
    também é chamado de "zero suprimido" porque o UPC(E) compacta um código de 12 dígitos
    UPC(A) em um código de seis dígitos. O UPC(E) suprime o dígito de sistema numérico,
    os dígitos finais no código de fabricante e os zeros iniciais na parte de identificação
    de produto do código. Um número opcional de dois ou cinco dígitos pode ser adicionado
    ao do código de barras UPC(A) e UPC(E) principal. Esse número é designado para uso em
    publicações e periódicos, aparecendo como um código de barras adicional no lado direito
    do código de barras principal;
  bcUPC_E1 - ver bcUPC_E0;
  bcUPC_Supp2 - ver bcUPC_Supp;
  bcUPC_Supp5 - ver bcUPC_Supp;
  bcEAN128A - Mais abrangente que os demais códigos, o UCC/EAN-128 é complementar,
    baseado em Identificadores de Aplicação (AI), identificando o significado e o
    formato de dados. O UCC/EAN-128 pode, inclusive, ser aplicado em unidades de
    distribuição, permitindo a identificação do número de lote, série, data de
    fabricação, validade, textos livres e outros dados. A utilização do UCC/EAN-128
    é múltipla, podendo ser aplicado na logística e automação de vários setores
    produtivos e comerciais, como o ramo alimentício, farmacêutico, vestuário e
    de papel, entre outros. Além disso, pode ser usado na distribuição, armazenamento,
    inventários e gestão de estoque, proporcionando agilidade na captura de informações,
    com menor margem de erros. Trata-se de um sistema que possui abrangência necessária
    para a obtenção de grandes ganhos na cadeia distributiva, sempre objetivando a
    otimizar e a maximizar, por meio da informação rápida e precisa;
  bcEAN128B - ver bcEAN128A;
  bcEAN128C - ver bcEAN128A.
  :}
  TRLBarcodeType=(bcCode2OF5Interleaved,bcCode2OF5Industry,bcCode2OF5Matrix,
                  bcCode39,bcCode39Extended,bcCode128A,bcCode128B,bcCode128C,
                  bcCode93,bcCode93Extended,bcMSI,bcPostNet,bcCodaBar,bcEAN8,
                  bcEAN13,bcUPC_A,bcUPC_E0,bcUPC_E1,bcUPC_Supp2,bcUPC_Supp5,
                  bcEAN128A,bcEAN128B,bcEAN128C);
  {/@type}

  // for internal use only
  TRLBarcodeLineType=(blFilled,blNotFilled,blHalfFilled);
                     // blHalfFilled means a black line with 2/5 height (used for PostNet)

  TRLBarcodeBarWidth=(bw100,bw100Ratio,bw150Ratio,bw200Ratio);

  // type of text to show
  TRLBarcodeTextOption=(boNone,boCode,boType,boBoth);

  TRLBarcodeCheckSumMethod=(cmNone,cmModule10);

  {@type TRLBarcodeOrientation - Orientação do desenho das barras.
   Pode ser um dos seguintes valores:
   boLeftToRight - Da esquerda para a direita;
   boBottomToTop - De baixo para cima;
   boTopToBottom - De cima para baixo. :}
  TRLBarcodeOrientation=(boLeftToRight,boBottomToTop,boTopToBottom);
  {/@type}

  {@type TRLBarcodeInvalidCode - O que deve ser exibido se o código contiver erros.
   Pode ser um dos seguintes valores:
   icEmptyRect - Apresenta um retângulo vazio;
   icCrossOut - Apresenta o código de barras rasurado com uma cruz vermelha;
   icDrawAnyway - Desenha as barras extraindo os dígitos inválidos.:}
  TRLBarcodeInvalidCode=(icEmptyRect,icCrossOut,icDrawAnyway);
  {/@type}

  { TRLCustomBarcode }

  {@class TRLCustomBarcode - Classe base da qual podem derivar componentes para códigos de barras. @ancestor TRLCustomControl. }
  TRLCustomBarcode=class(TRLCustomControl)
  private
    fBeforeText    :TRLBeforeTextEvent;
    fBarColor      :TColor;
    fShowText      :TRLBarcodeTextOption;
    fOrientation   :TRLBarcodeOrientation;
    fMargins       :TRLMargins;
    //
    fModule        :integer;
    fRatio         :double;
    fBarcodeType   :TRLBarcodeType;
    fCheckSum      :boolean;
    fCheckSumMethod:TRLBarcodeCheckSumMethod;
    fModules       :array[TRLBarcodeBarWidth] of shortint;
    fInvalid       :boolean;
    fInvalidCode   :TRLBarcodeInvalidCode;
    //
    procedure   GetBarInfo(aChar:char; var aBarWidth:integer; var aLineType:TRLBarcodeLineType);
    function    GetTypeText:string;
    function    GetImageWidth(const aBarData:string):integer;
    function    GetBarData(const aText:string):string;
    procedure   SetModule(Value:integer);
    procedure   SetBarColor(const Value:TColor);
    procedure   SetShowText(const Value: TRLBarcodeTextOption);
    procedure   SetBarcodeType(const Value: TRLBarcodeType);
    procedure   SetRatio(const Value: double);
    procedure   SetOrientation(const Value: TRLBarcodeOrientation);
    procedure   SetMargins(const aValue:TRLMargins);
    procedure   SetInvalidCode(const Value: TRLBarcodeInvalidCode);
    procedure   SetCheckSum(const Value: boolean);
    procedure   SetCheckSumMethod(const Value: TRLBarcodeCheckSumMethod);
    //
    function    GetAs2OF5Interleaved(const aText:string):string;
    function    GetAs2OF5Industry(const aText:string):string;
    function    GetAs2OF5Matrix(const aText:string):string;
    function    GetAs39(const aText:string):string;
    function    GetAs39Extended(const aText:string):string;
    function    GetAs128(const aText:string):string;
    function    GetAs93(const aText:string):string;
    function    GetAs93Extended(const aText:string):string;
    function    GetAsMSI(const aText:string):string;
    function    GetAsPostNet(const aText:string):string;
    function    GetAsCodaBar(const aText:string):string;
    function    GetAsEAN8(const aText:string):string;
    function    GetAsEAN13(const aText:string):string;
    function    GetAsUPC_A(const aText:string):string;
    function    GetAsUPC_E0(const aText:string):string;
    function    GetAsUPC_E1(const aText:string):string;
    function    GetAsUPC_Supp5(const aText:string):string;
    function    GetAsUPC_Supp2(const aText:string):string;
    //
    procedure   MakeModules;
    function    DoCheckSumming(const aData:string):string;
    function    CreateBitmap(const aText:string; aWidth,aHeight:integer):TBitmap;
    //
    function    IsRatio:Boolean;
    function    CalcMarginalPixels: TRect;
  protected
    // override methods
    procedure   CalcSize(var aSize:TPoint); override;
    procedure   InternalPrint; override;
  public
    // constructors & destructors
    constructor Create(aOwner:TComponent); override;
    destructor  Destroy; override;
    // override methods
    procedure   InternalPaint; override;
    // custom properties
    {@prop AutoSize - Redimensionamento automático. Determina se o controle irá se redimensionar automaticamente de acordo com o tamanho do seu conteúdo. :/}
    property    AutoSize default True;
    {@prop Caption - Texto a ser impresso como código de barras. :/}
    property    Caption;
    {@prop BarColor - Cor das barras. Determina a cor das barras cheias. :/}
    property    BarColor   :TColor               read fBarColor   write SetBarColor default clBlack;
    {@prop ShowText - Determina se e como serão exibidas as informações junto com as barras.
     Pode ser um dos seguintes valores:
     boNone - Nenhum texto é exibido;
     boCode - Apenas o valor do código de barras;
     boType - Apenas o tipo de código de barras utilizado;
     boBoth - Ambos o valor e o tipo de código. :/}
    property    ShowText      :TRLBarcodeTextOption     read fShowText       write SetShowText     default boNone;
    {@prop Module - Fator de ampliação da largura das barras. :/}
    property    Module        :integer                  read fModule         write SetModule       default 1;
    {@prop Ratio - Razão entre as larguras das barras. :/}
    property    Ratio         :double                   read fRatio          write SetRatio        stored IsRatio;
    {@prop BarcodeType - Padrão de código de barras. @links TRLBarcodeType. :/}
    property    BarcodeType   :TRLBarcodeType           read fBarcodeType    write SetBarcodeType  default bcCode2of5Interleaved;
    {@prop Orientation - Orientação da leitura das barras. :/}
    property    Orientation   :TRLBarcodeOrientation    read fOrientation    write SetOrientation  default boLeftToRight;
    {@prop Margins - Margens externas do código de barras. @links TRLMargins. :/}
    property    Margins       :TRLMargins               read fMargins        write SetMargins;
    {@prop InvalidCode - Determina o que deve ser exibido se o código tiver algum erro. @links TRLBarcodeInvalidCode. :/}
    property    InvalidCode   :TRLBarcodeInvalidCode    read fInvalidCode    write SetInvalidCode  default icEmptyRect;
    // build checksum
    property    CheckSum      :boolean                  read fCheckSum       write SetCheckSum       default False;
    property    CheckSumMethod:TRLBarcodeCheckSumMethod read fCheckSumMethod write SetCheckSumMethod default cmModule10;
    // events
    {@prop BeforePrint - Antes da impressão. Ocorre antes da impressão do controle para alterar o texto ou anular a sua impressão. :/}
    property    BeforePrint   :TRLBeforeTextEvent       read fBeforeText     write fBeforeText;
  end;
  {/@class}
  

  { TRLCustomDBBarcode }

  {@class TRLCustomDBBarcode - Classe base da qual podem derivar componentes para códigos de barras dataware. @ancestor TRLCustomBarcode.}
  TRLCustomDBBarcode=class(TRLCustomBarcode)
  private
    // variables
    fDataField  :TRLDataFieldProperty;
    fDataFormula:string;
    fDataSource :TDataSource;
    // assign methods
    function    GetField:TField;
    function    GetFieldLabel:string;
    function    GetDataSet:TDataSet;
    procedure   SetDataField(const aValue:TRLDataFieldProperty);
    procedure   SetDataFormula(const aValue:string);
    procedure   SetDataSource(const aValue:TDataSource);
  protected
    // override & reintroduce
    function    InternalMakeCaption:string; override;
    procedure   Notification(aComponent:TComponent; Operation:TOperation); override;
    // dynamic
    function    GetFieldText:string; dynamic;
  public
    // constructors & destructors
    constructor Create(aOwner:TComponent); override;
    // custom properties
    {@prop DataField - Nome do campo associado. :/}
    property    DataField  :TRLDataFieldProperty read fDataField   write SetDataField;
    {@prop DataFormula - Expressão matemática envolvendo campos, valores e literais. :/}
    property    DataFormula:string               read fDataFormula write SetDataFormula;
    {@prop DataSource - Referência ao DataSource que controle utiliza para se conectar ao DataSet. :/}
    property    DataSource :TDataSource          read fDataSource  write SetDataSource;
    // readonly
    {@prop Field - Referência para o objeto TField determinado pelas props DataField e DataSource. :/}
    property    Field      :TField               read GetField;
    {@prop DataSet - Referência para o objeto TDataSet determinado pela prop DataSource. :/}
    property    DataSet    :TDataSet             read GetDataSet;
  end;
  {/@class}


  { TRLBarcode }

  {@class TRLBarcode - Componente para códigos de barras. @pub. @ancestor TRLCustomBarcode. }
  TRLBarcode=class(TRLCustomBarcode)
  published
  
    // properties

    {@prop Align = ancestor /}
    property    Align;
    {@prop Alignment = ancestor /}
    property    Alignment;
    {@prop Anchors = ancestor /}
    property    Anchors;
    {@prop AutoSize = ancestor /}
    property    AutoSize;
    {@prop BarcodeType = ancestor /}
    property    BarcodeType;
    {@prop BarColor = ancestor /}
    property    BarColor;
    {@prop Behavior = ancestor /}
    property    Behavior;
    {@prop Borders = ancestor /}
    property    Borders;
    {@prop Caption = ancestor /}
    property    Caption;
    {@prop CheckSum = ancestor /}
    property    CheckSum;
    {@prop CheckSumMethod = ancestor /}
    property    CheckSumMethod;
    {@prop Color = ancestor /}
    property    Color;
    {@prop Font = ancestor /}
    property    Font;
    {@prop FriendlyName = ancestor /}
    property    FriendlyName;
    {@prop Holder = ancestor /}
    property    Holder;
    {@prop HoldStyle = ancestor /}
    property    HoldStyle;
    {@prop InvalidCode = ancestor /}
    property    InvalidCode;
    {@prop Layout = ancestor /}
    property    Layout;
    {@prop Margins = ancestor /}
    property    Margins;
    {@prop Module = ancestor /}
    property    Module;
    {@prop Orientation = ancestor /}
    property    Orientation;
    {@prop ParentColor = ancestor /}
    property    ParentColor;
    {@prop ParentFont = ancestor /}
    property    ParentFont;
    {@prop Ratio = ancestor /}
    property    Ratio;
    {@prop RealBounds = ancestor /}
    property    RealBounds;
    {@prop SecondHolder = ancestor /}
    property    SecondHolder;
    {@prop SecondHoldStyle = ancestor /}
    property    SecondHoldStyle;
    {@prop ShowText = ancestor /}
    property    ShowText;
    {@prop Transparent = ancestor /}
    property    Transparent;
    {@prop Visible = ancestor /}
    property    Visible;

    // events

    {@prop AfterPrint = ancestor /}
    property    AfterPrint;
    {@prop BeforePrint = ancestor /}
    property    BeforePrint;
    {@prop OnMeasureHeight = ancestor /}
    property    OnMeasureHeight;
  end;
  {/@class}


  {@class TRLDBBarcode - Componente para códigos de barras dataware. @pub. @ancestor TRLCustomDBBarcode. }
  TRLDBBarcode=class(TRLCustomDBBarcode)
  published

    // properties

    {@prop Align = ancestor /}
    property    Align;
    {@prop Alignment = ancestor /}
    property    Alignment;
    {@prop Anchors = ancestor /}
    property    Anchors;
    {@prop AutoSize = ancestor /}
    property    AutoSize;
    {@prop BarcodeType = ancestor /}
    property    BarcodeType;
    {@prop BarColor = ancestor /}
    property    BarColor;
    {@prop Behavior = ancestor /}
    property    Behavior;
    {@prop Borders = ancestor /}
    property    Borders;
    {@prop CheckSum = ancestor /}
    property    CheckSum;
    {@prop CheckSumMethod = ancestor /}
    property    CheckSumMethod;
    {@prop Color = ancestor /}
    property    Color;
    {@prop DataField = ancestor /}
    property    DataField;
    {@prop DataFormula = ancestor /}
    property    DataFormula;
    {@prop DataSource = ancestor /}
    property    DataSource;
    {@prop Font = ancestor /}
    property    Font;
    {@prop FriendlyName = ancestor /}
    property    FriendlyName;
    {@prop Holder = ancestor /}
    property    Holder;
    {@prop HoldStyle = ancestor /}
    property    HoldStyle;
    {@prop InvalidCode = ancestor /}
    property    InvalidCode;
    {@prop Layout = ancestor /}
    property    Layout;
    {@prop Margins = ancestor /}
    property    Margins;
    {@prop Module = ancestor /}
    property    Module;
    {@prop Orientation = ancestor /}
    property    Orientation;
    {@prop ParentColor = ancestor /}
    property    ParentColor;
    {@prop ParentFont = ancestor /}
    property    ParentFont;
    {@prop Ratio = ancestor /}
    property    Ratio;
    {@prop RealBounds = ancestor /}
    property    RealBounds;
    {@prop SecondHolder = ancestor /}
    property    SecondHolder;
    {@prop SecondHoldStyle = ancestor /}
    property    SecondHoldStyle;
    {@prop ShowText = ancestor /}
    property    ShowText;
    {@prop Transparent = ancestor /}
    property    Transparent;
    {@prop Visible = ancestor /}
    property    Visible;

    // events

    {@prop AfterPrint = ancestor /}
    property    AfterPrint;
    {@prop BeforePrint = ancestor /}
    property    BeforePrint;
    {@prop OnMeasureHeight = ancestor /}
    property    OnMeasureHeight;
  end;
  {/@class}


{/@unit}

implementation

uses
  RLUtils;

type
  TRLBarcodeTypeInfo=record
    Name      :string;  // name of barcode
    DigitsOnly:boolean; // numeric data only
  end;

var
  BarcodeTypeInfo:array[TRLBarcodeType] of TRLBarcodeTypeInfo=(
    (Name:'2OF5 Interleaved'; DigitsOnly:True),
    (Name:'2OF5 Industrial';  DigitsOnly:True),
    (Name:'2OF5 Matrix';      DigitsOnly:True),
    (Name:'Code 39';          DigitsOnly:False),
    (Name:'Code 39 Extended'; DigitsOnly:False),
    (Name:'Code 128A';        DigitsOnly:False),
    (Name:'Code 128B';        DigitsOnly:False),
    (Name:'Code 128C';        DigitsOnly:True),
    (Name:'Code 93';          DigitsOnly:False),
    (Name:'Code 93 Extended'; DigitsOnly:False),
    (Name:'MSI';              DigitsOnly:True),
    (Name:'PostNet';          DigitsOnly:True),
    (Name:'CodaBar';          DigitsOnly:False),
    (Name:'EAN8';             DigitsOnly:True),
    (Name:'EAN13';            DigitsOnly:True),
    (Name:'UPC A';            DigitsOnly:True),
    (Name:'UPC E0';           DigitsOnly:True),
    (Name:'UPC E1';           DigitsOnly:True),
    (Name:'UPC Supp2';        DigitsOnly:True),
    (Name:'UPC Supp5';        DigitsOnly:True),
    (Name:'EAN 128A';         DigitsOnly:False),
    (Name:'EAN 128B';         DigitsOnly:False),
    (Name:'EAN 128C';         DigitsOnly:True));

// UTILS

function CheckSumModule10(const aData:string):string;
var
  i,fak,sum:integer;
begin
  sum:=0;
  fak:=Length(aData);
  for i:=1 to Length(aData) do
  begin
    if (fak mod 2)=0 then
      Inc(sum,StrToInt(aData[i])*1)
    else
      Inc(sum,StrToInt(aData[i])*3);
    Dec(fak);
  end;
  if (sum mod 10)=0 then
    Result:=aData+'0'
  else
    Result:=aData+IntToStr(10-(sum mod 10));
end;

// converts a string from '321' to the internal representation '715'
// i need this function because some pattern tables have a different
// format :
// '00111'
// converts to '05161'
function Convert(const s:string):string;
var
  i,v:integer;
begin
  Result:=emptystr;
  for i:=1 to Length(s) do
  begin
    v:=Ord(s[i])-1;
    if Odd(i) then
      Inc(v,5);
    Result:=Result+Char(v);
  end;
end;

function PadZ(const aText:string; aWidth:integer):string;
begin
  Result:=aText;
  while Length(Result)<aWidth do
    Result:='0'+Result;
end;

function EvenZ(const aText:string):string;
begin
  Result:=aText;
  if Odd(Length(Result)) then
    Result:='0'+Result;
end;

{ TRLCustomBarcode }

constructor TRLCustomBarcode.Create(aOwner:TComponent);
var
  size:TPoint;
begin
  fShowText      :=boNone;
  fBarColor      :=clBlack;
  fOrientation   :=boLeftToRight;
  //
  fRatio         :=2;
  fModule        :=1;
  fBarcodeType   :=bcCode2of5Interleaved;
  fCheckSum      :=False;
  fCheckSumMethod:=cmModule10;
  fInvalidCode   :=icEmptyRect;
  fMargins       :=TRLMargins.Create(Self);
  //
  inherited;
  //
  CalcSize(size);
  Width      :=size.X;
  Height     :=34;
  AutoSizeDir:=[asWidthDir];
  AutoSize   :=True;
  //
  with fMargins do
  begin
    LeftMargin  :=1;
    TopMargin   :=0;
    RightMargin :=1;
    BottomMargin:=0;
  end;
end;

destructor TRLCustomBarcode.Destroy;
begin
  FreeObj(fMargins);
  //
  inherited;
end;

function TRLCustomBarcode.IsRatio: Boolean;
begin
  Result:=(fRatio<>2);
end;

// margens em pixels
function TRLCustomBarcode.CalcMarginalPixels:TRect;
begin
  Result.Left  :=Round(ScreenPPI*fMargins.LeftMargin  /InchAsMM);
  Result.Top   :=Round(ScreenPPI*fMargins.TopMargin   /InchAsMM);
  Result.Right :=Round(ScreenPPI*fMargins.RightMargin /InchAsMM);
  Result.Bottom:=Round(ScreenPPI*fMargins.BottomMargin/InchAsMM);
end;

procedure TRLCustomBarcode.CalcSize(var aSize:TPoint);
var
  w:integer;
  m:TRect;
  p:^integer;
begin
  aSize:=Point(Width,Height);
  if not AutoSize then
    Exit;
  w:=GetImageWidth(GetBarData(Caption))+1;
  if fOrientation=boLeftToRight then
    p:=@aSize.X
  else
    p:=@aSize.Y;
  p^:=w;
  //
  m:=CalcMarginalPixels;
  Inc(p^,m.Left+m.Right);
  // adicional das bordas
  w:=Self.Borders.Width;
  if w>0 then
  begin
    Inc(w);
    if Self.Borders.CanDrawLeft then
      Inc(p^,w);
    if Self.Borders.CanDrawRight then
      Inc(p^,w);
  end;
end;

function TRLCustomBarcode.CreateBitmap(const aText:string; aWidth,aHeight:integer):TBitmap;
var
  barwidth:integer;
  linetype:TRLBarcodeLineType;
  data,s  :string;
  fullrect:TRect;
  imgrect :TRect;
  aux     :TRect;
  i,w,h   :integer;
begin
  Result:=NewBitmap(aWidth,aHeight);
  try
    data:=GetBarData(aText);
    // desenha o código de barras
    fullrect:=Rect(0,0,aWidth,aHeight);
    imgrect :=fullrect;
    with Result.Canvas do
    begin
      Brush.Style:=bsSolid;
      Brush.Color:=Self.Color;
      FillRect(fullrect);
      //
      Pen.Style  :=psSolid;
      Pen.Width  :=1;
      // examine the pattern string
      if fShowText<>boNone then
      begin
        Font.Assign(Self.Font);
        // reserva uma linha no topo para o tipo
        if fShowText in [boType,boBoth] then
          Inc(imgrect.Top,TextHeight(' '));
        // reserva meia linha em baixo para o código (postnet é um linha)
        if fShowText in [boCode,boBoth] then
          if fBarcodeType=bcPostNet then
            Dec(imgrect.Bottom,TextHeight(' '))
          else
            Dec(imgrect.Bottom,TextHeight(' ') div 2);
      end;
      // centraliza a imagem das barras
      imgrect.Right:=imgrect.Left+GetImageWidth(data);
      case Alignment of
        taCenter      : OffsetRect(imgrect,(RectWidth(fullrect)-RectWidth(imgrect)) div 2,0);
        taRightJustify: OffsetRect(imgrect,RectWidth(fullrect)-RectWidth(imgrect),0);
      end;
      // desenha as barras
      if (fInvalid and (fInvalidCode=icEmptyRect)) or (Trim(aText)=emptystr) then
      else
      begin
        aux:=imgrect;
        for i:=1 to Length(data) do
        begin
          GetBarInfo(data[i],barwidth,linetype);
          // determina a cor da barra
          if linetype in [blNotFilled,blHalfFilled] then
            Brush.Color:=Self.BarColor
          else
            Brush.Color:=Self.Color;
          //
          if linetype=blHalfFilled then
            aux.Top:=imgrect.Bottom-(imgrect.Bottom-imgrect.Top)*2 div 5
          else
            aux.Top:=imgrect.Top;
          aux.Right:=aux.Left+barwidth;
          // draw the rectangle
          FillRect(aux);
          // step it
          Inc(aux.Left,barwidth);
        end;
        if fInvalid and (InvalidCode=icCrossOut) then
        begin
          Pen.Width:=4;
          Pen.Color:=clRed;
          MoveTo(imgrect.Left+Pen.Width,imgrect.Top+Pen.Width);
          LineTo(imgrect.Right-Pen.Width-1,imgrect.Bottom-Pen.Width-1);
          MoveTo(imgrect.Right-Pen.Width-1,imgrect.Top+Pen.Width);
          LineTo(imgrect.Left+Pen.Width,imgrect.Bottom-Pen.Width-1);
        end;
      end;
      // desenha o texto
      if fShowText<>boNone then
      begin
        Font.Assign(Self.Font);
        Brush.Style:=bsSolid;
        Brush.Color:=Self.Color;
        if fShowText in [boType,boBoth] then
        begin
          s:=GetTypeText;
          w:=TextWidth(s);
          h:=TextHeight(s+' ');
          aux.Left  :=(imgrect.Left+imgrect.Right-w) div 2;
          aux.Top   :=fullrect.Top;
          aux.Right :=aux.Left+w;
          aux.Bottom:=aux.Top+h;
          FillRect(aux);
          TextRect(aux,aux.Left,aux.Top,s);
        end;
        if fShowText in [boCode,boBoth] then
        begin
          s:=Self.Caption;
          w:=TextWidth(s);
          h:=TextHeight(s+' ');
          aux.Left  :=(imgrect.Left+imgrect.Right-w) div 2;
          aux.Top   :=fullrect.Bottom-h;
          aux.Right :=aux.Left+w;
          aux.Bottom:=aux.Top+h;
          FillRect(aux);
          TextRect(aux,aux.Left,aux.Top,s);
        end;
      end;
    end;
  except
    Result.free;
    raise;
  end;
end;

procedure TRLCustomBarcode.InternalPaint;
var
  m,a:TBitmap;
  ang:double;
  l,t:integer;
  w,h:integer;
  k  :integer;
  r  :TRect;
begin
  PaintAsCustomControl;
  r:=ReduceRect(GetClientRect,CalcMarginalPixels);
  w:=RectWidth(r);
  h:=RectHeight(r);
  if fOrientation in [boBottomToTop,boTopToBottom] then
  begin
    k:=w;
    w:=h;
    h:=k;
  end;
  if (w>0) and (h>0) then
  begin
    m:=CreateBitmap(Caption,w,h);
    try
      case fOrientation of
        boBottomToTop: ang:=90;
        boTopToBottom: ang:=-90;
      else // boLeftToRight
        ang:=0;
      end;
      a:=RotatedBitmap(m,ang);
      try
        case Alignment of
          taCenter      : l:=(r.Left+r.Right-a.Width) div 2;
          taRightJustify: l:=r.Right-a.Width;
        else
          l:=r.Left;
        end;
        case Layout of
          tlCenter: t:=(r.Top+r.Bottom-a.Height) div 2;
          tlBottom: t:=r.Bottom-a.Height;
        else
          t:=r.Top;
        end;
        Canvas.Draw(l,t,a);
      finally
        a.free;
      end;
    finally
      m.free;
    end;
  end;
end;


procedure TRLCustomBarcode.InternalPrint;
var
  m,a:TBitmap;
  ang:double;
  l,t:integer;
  w,h:integer;
  k  :integer;
  r  :TRect;
begin
  inherited;
  //
  r:=ReduceRect(CalcPrintClientRect,CalcMarginalPixels);
  w:=RectWidth(r);
  h:=RectHeight(r);
  if fOrientation in [boBottomToTop,boTopToBottom] then
  begin
    k:=w;
    w:=h;
    h:=k;
  end;
  if (w>0) and (h>0) then
  begin
    m:=CreateBitmap(Caption,w,h);
    try
      case fOrientation of
        boBottomToTop: ang:=90;
        boTopToBottom: ang:=-90;
      else // boLeftToRight
        ang:=0;
      end;
      a:=RotatedBitmap(m,ang);
      try
        case Alignment of
          taCenter      : l:=(r.Left+r.Right-a.Width) div 2;
          taRightJustify: l:=r.Right-a.Width;
        else
          l:=r.Left;
        end;
        case Layout of
          tlCenter: t:=(r.Top+r.Bottom-a.Height) div 2;
          tlBottom: t:=r.Bottom-a.Height;
        else
          t:=r.Top;
        end;
        RequestParentSurface.Draw(l,t,a);
      finally
        a.free;
      end;
    finally
      m.free;
    end;
  end;
end;

procedure TRLCustomBarcode.SetBarColor(const Value: TColor);
begin
  if fBarColor=Value then
    Exit;
  fBarColor:=Value;
  Invalidate;
end;

procedure TRLCustomBarcode.SetShowText(const Value: TRLBarcodeTextOption);
begin
  if fShowText=Value then
    Exit;
  fShowText:=Value;
  Invalidate;
end;

procedure TRLCustomBarcode.SetBarcodeType(const Value: TRLBarcodeType);
begin
  if fBarcodeType=Value then
    Exit;
  fBarcodeType:=Value;
  AdjustBounds;
  Invalidate;
end;

procedure TRLCustomBarcode.SetRatio(const Value: double);
begin
  if fRatio=Value then
    Exit;
  fRatio:=Value;
  AdjustBounds;
  Invalidate;
end;

procedure TRLCustomBarcode.SetOrientation(const Value: TRLBarcodeOrientation);
var
  w:integer;
begin
  if fOrientation=Value then
    Exit;
  if (Value in [boLeftToRight])<>(fOrientation in [boLeftToRight]) then
  begin
    w     :=Width;
    Width :=Height;
    Height:=w;
  end;
  fOrientation:=Value;
  AdjustBounds;
  Invalidate;
end;

function TRLCustomBarcode.GetTypeText:string;
begin
  Result:=BarcodeTypeInfo[fBarcodeType].Name;
end;

procedure TRLCustomBarcode.SetModule(Value:integer);
begin
  if fModule=Value then
    Exit;
  if (Value<1) or (Value>=50) then
    Exit;
  fModule:=Value;
  AdjustBounds;
  Invalidate;
end;

// calculate the width and the linetype of a sigle bar
procedure TRLCustomBarcode.GetBarInfo(aChar:char; var aBarWidth:integer; var aLineType:TRLBarcodeLineType);
begin
  {
  Code Color Width      Height
  -----------------------------------
  '0'  white 100%       full
  '1'  white 100%*Ratio full
  '2'  white 150%*Ratio full
  '3'  white 200%*Ratio full
  '5'  black 100%       full
  '6'  black 100%*Ratio full
  '7'  black 150%*Ratio full
  '8'  black 200%*Ratio full
  'A'  black 100%       2/5 (PostNet)
  'B'  black 100%*Ratio 2/5 (PostNet)
  'C'  black 150%*Ratio 2/5 (PostNet)
  'D'  black 200%*Ratio 2/5 (PostNet)
  }
  if aChar in ['0','5','A'] then
    aBarWidth:=fModules[bw100]
  else if aChar in ['1','6','B'] then
    aBarWidth:=fModules[bw100Ratio]
  else if aChar in ['2','7','C'] then
    aBarWidth:=fModules[bw150Ratio]
  else if aChar in ['3','8','D'] then
    aBarWidth:=fModules[bw200Ratio]
  else
    aBarWidth:=fModules[bw100];
  if aChar in ['0','1','2','3'] then
    aLineType:=blFilled
  else if aChar in ['5','6','7','8'] then
    aLineType:=blNotFilled
  else if aChar in ['A','B','C','D'] then
    aLineType:=blHalfFilled
  else
    aLineType:=blNotFilled;
end;

procedure TRLCustomBarcode.MakeModules;
begin
  case BarcodeType of
    bcCode2OF5Interleaved,
    bcCode2OF5Industry,
    bcCode39,
    bcEAN8,
    bcEAN13,
    bcCode39Extended,
    bcCodaBar,
    bcUPC_A,
    bcUPC_E0,
    bcUPC_E1,
    bcUPC_Supp2,
    bcUPC_Supp5     : if fRatio<2 then
                        fRatio:=2
                      else if fRatio>3 then
                        fRatio:=3;
    bcCode2OF5Matrix: if fRatio<2.25 then
                        fRatio:=2.25
                      else if fRatio>3 then
                        fRatio:=3;
    bcCode128A,
    bcCode128B,
    bcCode128C,
    bcCode93,
    bcCode93Extended,
    bcMSI,
    bcPostNet:;
  end;
  //
  fModules[bw100]     :=fModule;
  fModules[bw100Ratio]:=Round(fModule*fRatio);
  fModules[bw150Ratio]:=fModules[bw100Ratio]*3 div 2;
  fModules[bw200Ratio]:=fModules[bw100Ratio]*2;
end;

function TRLCustomBarcode.GetBarData(const aText:string):string;
var
  s:string;
  i:integer;
begin
  fInvalid:=False;
  // calculate the with of the different lines (fModules)
  MakeModules;
  // numeric barcode type?
  s:=Caption;
  if BarcodeTypeInfo[BarcodeType].DigitsOnly then
  begin
    s:=Trim(s);
    for i:=1 to Length(s) do
      if not (s[i] in ['0'..'9']) then
      begin
        s[i]    :='0';
        fInvalid:=True;
      end;
  end;
  // get the pattern of the barcode
  case BarcodeType of
    bcCode2of5Interleaved: Result:=GetAs2OF5Interleaved(s);
    bcCode2OF5Industry:    Result:=GetAs2OF5Industry(s);
    bcCode2OF5Matrix:      Result:=GetAs2OF5Matrix(s);
    bcCode39:              Result:=GetAs39(s);
    bcCode39Extended:      Result:=GetAs39Extended(s);
    bcCode128A,
    bcCode128B,
    bcCode128C,
    bcEAN128A,
    bcEAN128B,
    bcEAN128C:             Result:=GetAs128(s);
    bcCode93:              Result:=GetAs93(s);
    bcCode93Extended:      Result:=GetAs93Extended(s);
    bcMSI:                 Result:=GetAsMSI(s);
    bcPostNet:             Result:=GetAsPostNet(s);
    bcCodaBar:             Result:=GetAsCodaBar(s);
    bcEAN8:                Result:=GetAsEAN8(s);
    bcEAN13:               Result:=GetAsEAN13(s);
    bcUPC_A:               Result:=GetAsUPC_A(s);
    bcUPC_E0:              Result:=GetAsUPC_E0(s);
    bcUPC_E1:              Result:=GetAsUPC_E1(s);
    bcUPC_Supp2:           Result:=GetAsUPC_Supp2(s);
    bcUPC_Supp5:           Result:=GetAsUPC_Supp5(s);
  else
    // raise Exception.Create(Self.Name+': Wrong barcode type');
    Result:=emptystr;
  end;
end;

function TRLCustomBarcode.GetImageWidth(const aBarData:string):integer;
var
  l:TRLBarcodeLineType;
  w:integer;
  i:integer;
begin
  // examine the pattern string
  Result:=0;
  for i:=1 to Length(aBarData) do
  begin
    GetBarInfo(aBarData[i],w,l);
    Inc(Result,w);
  end;
end;

function TRLCustomBarcode.DoCheckSumming(const aData:string):string;
begin
  if fCheckSum then
    case fCheckSumMethod of
      cmModule10: Result:=CheckSumModule10(aData);
    else
      Result:=aData;
    end
  else
    Result:=aData;
end;

// Pattern for Barcode EAN Charset A (L1   S1   L2   S2)
const
  Table_EAN_A:array['0'..'9'] of string=
    (('2605'),    // 0
     ('1615'),    // 1
     ('1516'),    // 2
     ('0805'),    // 3
     ('0526'),    // 4
     ('0625'),    // 5
     ('0508'),    // 6
     ('0706'),    // 7
     ('0607'),    // 8
     ('2506'));   // 9

// Pattern for Barcode EAN Charset C (S1   L1   S2   L2)
const
  Table_EAN_C:array['0'..'9'] of string =
    (('7150' ),    // 0
     ('6160' ),    // 1
     ('6061' ),    // 2
     ('5350' ),    // 3
     ('5071' ),    // 4
     ('5170' ),    // 5
     ('5053' ),    // 6
     ('5251' ),    // 7
     ('5152' ),    // 8
     ('7051' ));   // 9

function TRLCustomBarcode.GetAsEAN8(const aText:string):string;
var
  tmp:string;
  i  :integer;
begin
  if fCheckSum then
  begin
    tmp:=PadZ(aText,7);
    tmp:=DoCheckSumming(Copy(tmp,Length(tmp)-6,7));
  end
  else
    tmp:=PadZ(aText,8);
  //
  Result:=emptystr;
  if Length(tmp)=8 then
  begin
    // startcode
    Result:='505';
    for i:=1 to 4 do
      Result:=Result+Table_EAN_A[tmp[i]];
    // center guard pattern
    Result:=Result+'05050';
    for i:=5 to 8 do
      Result:=Result+Table_EAN_C[tmp[i]];
    // stopcode
    Result:=Result+'505';
  end
  else
    fInvalid:=True;
end;

// Pattern for Barcode EAN Zeichensatz B {L1   S1   L2   S2}
const
  Table_EAN_B:array['0'..'9'] of string=
    (('0517'),    // 0
     ('0616'),    // 1
     ('1606'),    // 2
     ('0535'),    // 3
     ('1705'),    // 4
     ('0715'),    // 5
     ('3505'),    // 6
     ('1525'),    // 7
     ('2515'),    // 8
     ('1507'));   // 9

// Zuordung der Paraitaetsfolgen für EAN13
const
  Table_ParityEAN13:array[0..9, 1..6] of char=
    (('A','A','A','A','A','A'),  // 0
     ('A','A','B','A','B','B'),  // 1
     ('A','A','B','B','A','B'),  // 2
     ('A','A','B','B','B','A'),  // 3
     ('A','B','A','A','B','B'),  // 4
     ('A','B','B','A','A','B'),  // 5
     ('A','B','B','B','A','A'),  // 6
     ('A','B','A','B','A','B'),  // 7
     ('A','B','A','B','B','A'),  // 8
     ('A','B','B','A','B','A')); // 9

function TRLCustomBarcode.GetAsEAN13(const aText:string):string;
var
  i,lk:integer;
  tmp :string;
begin
  if fCheckSum then
  begin
    tmp:=PadZ(aText,12);
    tmp:=DoCheckSumming(tmp);
  end
  else
    tmp:=PadZ(aText,13);
  //
  Result:=emptystr;
  if Length(tmp)=13 then
  begin
    lk :=StrToInt(tmp[1]);
    tmp:=copy(tmp,2,12);
    // startcode
    Result:='505';
    for i:=1 to 6 do
      case Table_ParityEAN13[lk,i] of
      'A': Result:=Result+Table_EAN_A[tmp[i]];
      'B': Result:=Result+Table_EAN_B[tmp[i]];
      'C': Result:=Result+Table_EAN_C[tmp[i]];
      end;
    // center guard pattern}
    Result:=Result+'05050';
    for i:=7 to 12 do
      Result:=Result+Table_EAN_C[tmp[i]];
    // stopcode
    Result:=Result+'505';
  end
  else
    fInvalid:=True;
end;

// pattern for barcode 2 of 5
const
  Table_2_5:array['0'..'9',1..5] of char=
    (('0','0','1','1','0'),  {'0'}
     ('1','0','0','0','1'),  {'1'}
     ('0','1','0','0','1'),  {'2'}
     ('1','1','0','0','0'),  {'3'}
     ('0','0','1','0','1'),  {'4'}
     ('1','0','1','0','0'),  {'5'}
     ('0','1','1','0','0'),  {'6'}
     ('0','0','0','1','1'),  {'7'}
     ('1','0','0','1','0'),  {'8'}
     ('0','1','0','1','0')); {'9'}

function TRLCustomBarcode.GetAs2OF5Interleaved(const aText:string):string;
var
  i,j:integer;
  c  :char;
begin
  // startcode
  Result:='5050';
  for i:=1 to Length(aText) div 2 do
    for j:=1 to 5 do
    begin
      if Table_2_5[aText[i*2-1],j]='1' then
        c:='6'
      else
        c:='5';
      Result:=Result+c;
      if Table_2_5[aText[i*2],j]='1' then
        c:='1'
      else
        c:='0';
      Result:=Result+c;
    end;
  // stopcode
  Result:=Result+'605';
end;

function TRLCustomBarcode.GetAs2OF5Industry(const aText:string):string;
var
  i,j:integer;
begin
  // startcode
  Result:='606050';
  for i:=1 to Length(aText) do
    for j:=1 to 5 do
      if Table_2_5[aText[i],j]='1' then
        Result:=Result+'60'
      else
        Result:=Result+'50';
  // stopcode
  Result:=Result+'605060';
end;

function TRLCustomBarcode.GetAs2OF5Matrix(const aText:string):string;
var
  i,j:integer;
  c  :char;
begin
  // startcode
  Result:='705050';
  for i:=1 to Length(aText) do
  begin
    for j:=1 to 5 do
    begin
      if Table_2_5[aText[i],j]='1' then
        c:='1'
      else
        c:='0';
      if Odd(j) then
        c:=Char(Ord(c)+5);
      Result:=Result+c;
    end;
    Result:=Result+'0';
  end;
  // stopcode
  Result:=Result+'70505';
end;


type
  TCode39=record
            c   :char;
            data:array[0..9] of char;
            chk :shortint;
          end;
          
const
  Table_39: array[0..43] of TCode39=(
     (c:'0'; data:'505160605'; chk:0 ),
     (c:'1'; data:'605150506'; chk:1 ),
     (c:'2'; data:'506150506'; chk:2 ),
     (c:'3'; data:'606150505'; chk:3 ),
     (c:'4'; data:'505160506'; chk:4 ),
     (c:'5'; data:'605160505'; chk:5 ),
     (c:'6'; data:'506160505'; chk:6 ),
     (c:'7'; data:'505150606'; chk:7 ),
     (c:'8'; data:'605150605'; chk:8 ),
     (c:'9'; data:'506150605'; chk:9 ),
     (c:'A'; data:'605051506'; chk:10),
     (c:'B'; data:'506051506'; chk:11),
     (c:'C'; data:'606051505'; chk:12),
     (c:'D'; data:'505061506'; chk:13),
     (c:'E'; data:'605061505'; chk:14),
     (c:'F'; data:'506061505'; chk:15),
     (c:'G'; data:'505051606'; chk:16),
     (c:'H'; data:'605051605'; chk:17),
     (c:'I'; data:'506051605'; chk:18),
     (c:'J'; data:'505061605'; chk:19),
     (c:'K'; data:'605050516'; chk:20),
     (c:'L'; data:'506050516'; chk:21),
     (c:'M'; data:'606050515'; chk:22),
     (c:'N'; data:'505060516'; chk:23),
     (c:'O'; data:'605060515'; chk:24),
     (c:'P'; data:'506060515'; chk:25),
     (c:'Q'; data:'505050616'; chk:26),
     (c:'R'; data:'605050615'; chk:27),
     (c:'S'; data:'506050615'; chk:28),
     (c:'T'; data:'505060615'; chk:29),
     (c:'U'; data:'615050506'; chk:30),
     (c:'V'; data:'516050506'; chk:31),
     (c:'W'; data:'616050505'; chk:32),
     (c:'X'; data:'515060506'; chk:33),
     (c:'Y'; data:'615060505'; chk:34),
     (c:'Z'; data:'516060505'; chk:35),
     (c:'-'; data:'515050606'; chk:36),
     (c:'.'; data:'615050605'; chk:37),
     (c:' '; data:'516050605'; chk:38),
     (c:'*'; data:'515060605'; chk:0 ),
     (c:'$'; data:'515151505'; chk:39),
     (c:'/'; data:'515150515'; chk:40),
     (c:'+'; data:'515051515'; chk:41),
     (c:'%'; data:'505151515'; chk:42));

function FindIdx(z:char):integer;
var
  i:integer;
begin
  Result:=-1;
  for i:=0 to High(Table_39) do
    if Table_39[i].c=z then
    begin
      Result:=i;
      Break;
    end;
end;

function TRLCustomBarcode.GetAs39(const aText:string):string;
var
  checksum,i,idx:integer;
begin
  checksum:=0;
  // startcode
  Result:=Table_39[FindIdx('*')].data+'0';
  for i:=1 to Length(aText) do
  begin
    idx:=FindIdx(aText[i]);
    if idx<0 then
      fInvalid:=True
    else
    begin
      Result:=Result+Table_39[idx].data+'0';
      Inc(checksum,Table_39[idx].chk);
    end;
  end;
  // calculate checksum data
  if fCheckSum then
  begin
    checksum:=checksum mod 43;
    for i:=0 to High(Table_39) do
      if checksum=Table_39[i].chk then
      begin
        Result:=Result+Table_39[i].data+'0';
        Break;
      end;
  end;
  // stopcode
  Result:=Result+Table_39[FindIdx('*')].data;
end;

const
  code39x:array[0..127] of string[2]=(
    ('%U'), ('$A'), ('$B'), ('$C'), ('$D'), ('$E'), ('$F'), ('$G'),
    ('$H'), ('$I'), ('$J'), ('$K'), ('$L'), ('$M'), ('$N'), ('$O'),
    ('$P'), ('$Q'), ('$R'), ('$S'), ('$T'), ('$U'), ('$V'), ('$W'),
    ('$X'), ('$Y'), ('$Z'), ('%A'), ('%B'), ('%C'), ('%D'), ('%E'),
    (' '),  ('/A'), ('/B'), ('/C'), ('/D'), ('/E'), ('/F'), ('/G'),
    ('/H'), ('/I'), ('/J'), ('/K'), ('/L'), ('/M'), ('/N'), ('/O'),
    ( '0'), ('1'),  ('2'),  ('3'),  ('4'),  ('5'),  ('6'),  ('7'),
    ('8'),  ('9'),  ('/Z'), ('%F'), ('%G'), ('%H'), ('%I'), ('%J'),
    ('%V'), ('A'),  ('B'),  ('C'),  ('D'),  ('E'),  ('F'),  ('G'),
    ('H'),  ('I'),  ('J'),  ('K'),  ('L'),  ('M'),  ('N'),  ('O'),
    ('P'),  ('Q'),  ('R'),  ('S'),  ('T'),  ('U'),  ('V'),  ('W'),
    ('X'),  ('Y'),  ('Z'),  ('%K'), ('%L'), ('%M'), ('%N'), ('%O'),
    ('%W'), ('+A'), ('+B'), ('+C'), ('+D'), ('+E'), ('+F'), ('+G'),
    ('+H'), ('+I'), ('+J'), ('+K'), ('+L'), ('+M'), ('+N'), ('+O'),
    ('+P'), ('+Q'), ('+R'), ('+S'), ('+T'), ('+U'), ('+V'), ('+W'),
    ('+X'), ('+Y'), ('+Z'), ('%P'), ('%Q'), ('%R'), ('%S'), ('%T')
    );
    
function TRLCustomBarcode.GetAs39Extended(const aText:string):string;
var
  s:string;
  i:integer;
begin
  s:=emptystr;
  for i:=1 to Length(aText) do
    if Ord(aText[i])<=127 then
      s:=s+code39x[Ord(aText[i])]
    else
      fInvalid:=True;
  Result:=GetAs39(s);
end;

function TRLCustomBarcode.GetAs128(const aText:string):string;
const
  StartA='211412';
  StartB='211214';
  StartC='211232';
  Stop  ='2331112';
type
  TCode128=record
             a,b :char;
             c   :string[2];
             data:string[6];
           end;
const
  Table_128:array[0..102] of TCode128=(
    (a:' '; b:' '; c:'00'; data:'212222' ),
    (a:'!'; b:'!'; c:'01'; data:'222122' ),
    (a:'"'; b:'"'; c:'02'; data:'222221' ),
    (a:'#'; b:'#'; c:'03'; data:'121223' ),
    (a:'$'; b:'$'; c:'04'; data:'121322' ),
    (a:'%'; b:'%'; c:'05'; data:'131222' ),
    (a:'&'; b:'&'; c:'06'; data:'122213' ),
    (a:''''; b:''''; c:'07'; data:'122312' ),
    (a:'('; b:'('; c:'08'; data:'132212' ),
    (a:')'; b:')'; c:'09'; data:'221213' ),
    (a:'*'; b:'*'; c:'10'; data:'221312' ),
    (a:'+'; b:'+'; c:'11'; data:'231212' ),
    (a:'´'; b:'´'; c:'12'; data:'112232' ),
    (a:'-'; b:'-'; c:'13'; data:'122132' ),
    (a:'.'; b:'.'; c:'14'; data:'122231' ),
    (a:'/'; b:'/'; c:'15'; data:'113222' ),
    (a:'0'; b:'0'; c:'16'; data:'123122' ),
    (a:'1'; b:'1'; c:'17'; data:'123221' ),
    (a:'2'; b:'2'; c:'18'; data:'223211' ),
    (a:'3'; b:'3'; c:'19'; data:'221132' ),
    (a:'4'; b:'4'; c:'20'; data:'221231' ),
    (a:'5'; b:'5'; c:'21'; data:'213212' ),
    (a:'6'; b:'6'; c:'22'; data:'223112' ),
    (a:'7'; b:'7'; c:'23'; data:'312131' ),
    (a:'8'; b:'8'; c:'24'; data:'311222' ),
    (a:'9'; b:'9'; c:'25'; data:'321122' ),
    (a:':'; b:':'; c:'26'; data:'321221' ),
    (a:';'; b:';'; c:'27'; data:'312212' ),
    (a:'<'; b:'<'; c:'28'; data:'322112' ),
    (a:'='; b:'='; c:'29'; data:'322211' ),
    (a:'>'; b:'>'; c:'30'; data:'212123' ),
    (a:'?'; b:'?'; c:'31'; data:'212321' ),
    (a:'@'; b:'@'; c:'32'; data:'232121' ),
    (a:'A'; b:'A'; c:'33'; data:'111323' ),
    (a:'B'; b:'B'; c:'34'; data:'131123' ),
    (a:'C'; b:'C'; c:'35'; data:'131321' ),
    (a:'D'; b:'D'; c:'36'; data:'112313' ),
    (a:'E'; b:'E'; c:'37'; data:'132113' ),
    (a:'F'; b:'F'; c:'38'; data:'132311' ),
    (a:'G'; b:'G'; c:'39'; data:'211313' ),
    (a:'H'; b:'H'; c:'40'; data:'231113' ),
    (a:'I'; b:'I'; c:'41'; data:'231311' ),
    (a:'J'; b:'J'; c:'42'; data:'112133' ),
    (a:'K'; b:'K'; c:'43'; data:'112331' ),
    (a:'L'; b:'L'; c:'44'; data:'132131' ),
    (a:'M'; b:'M'; c:'45'; data:'113123' ),
    (a:'N'; b:'N'; c:'46'; data:'113321' ),
    (a:'O'; b:'O'; c:'47'; data:'133121' ),
    (a:'P'; b:'P'; c:'48'; data:'313121' ),
    (a:'Q'; b:'Q'; c:'49'; data:'211331' ),
    (a:'R'; b:'R'; c:'50'; data:'231131' ),
    (a:'S'; b:'S'; c:'51'; data:'213113' ),
    (a:'T'; b:'T'; c:'52'; data:'213311' ),
    (a:'U'; b:'U'; c:'53'; data:'213131' ),
    (a:'V'; b:'V'; c:'54'; data:'311123' ),
    (a:'W'; b:'W'; c:'55'; data:'311321' ),
    (a:'X'; b:'X'; c:'56'; data:'331121' ),
    (a:'Y'; b:'Y'; c:'57'; data:'312113' ),
    (a:'Z'; b:'Z'; c:'58'; data:'312311' ),
    (a:'['; b:'['; c:'59'; data:'332111' ),
    (a:'\'; b:'\'; c:'60'; data:'314111' ),
    (a:']'; b:']'; c:'61'; data:'221411' ),
    (a:'^'; b:'^'; c:'62'; data:'431111' ),
    (a:'_'; b:'_'; c:'63'; data:'111224' ),
    (a:' '; b:'`'; c:'64'; data:'111422' ),
    (a:' '; b:'a'; c:'65'; data:'121124' ),
    (a:' '; b:'b'; c:'66'; data:'121421' ),
    (a:' '; b:'c'; c:'67'; data:'141122' ),
    (a:' '; b:'d'; c:'68'; data:'141221' ),
    (a:' '; b:'e'; c:'69'; data:'112214' ),
    (a:' '; b:'f'; c:'70'; data:'112412' ),
    (a:' '; b:'g'; c:'71'; data:'122114' ),
    (a:' '; b:'h'; c:'72'; data:'122411' ),
    (a:' '; b:'i'; c:'73'; data:'142112' ),
    (a:' '; b:'j'; c:'74'; data:'142211' ),
    (a:' '; b:'k'; c:'75'; data:'241211' ),
    (a:' '; b:'l'; c:'76'; data:'221114' ),
    (a:' '; b:'m'; c:'77'; data:'413111' ),
    (a:' '; b:'n'; c:'78'; data:'241112' ),
    (a:' '; b:'o'; c:'79'; data:'134111' ),
    (a:' '; b:'p'; c:'80'; data:'111242' ),
    (a:' '; b:'q'; c:'81'; data:'121142' ),
    (a:' '; b:'r'; c:'82'; data:'121241' ),
    (a:' '; b:'s'; c:'83'; data:'114212' ),
    (a:' '; b:'t'; c:'84'; data:'124112' ),
    (a:' '; b:'u'; c:'85'; data:'124211' ),
    (a:' '; b:'v'; c:'86'; data:'411212' ),
    (a:' '; b:'w'; c:'87'; data:'421112' ),
    (a:' '; b:'x'; c:'88'; data:'421211' ),
    (a:' '; b:'y'; c:'89'; data:'212141' ),
    (a:' '; b:'z'; c:'90'; data:'214121' ),
    (a:' '; b:'{'; c:'91'; data:'412121' ),
    (a:' '; b:'|'; c:'92'; data:'111143' ),
    (a:' '; b:'}'; c:'93'; data:'111341' ),
    (a:' '; b:'~'; c:'94'; data:'131141' ),
    (a:' '; b:' '; c:'95'; data:'114113' ),
    (a:' '; b:' '; c:'96'; data:'114311' ),
    (a:' '; b:' '; c:'97'; data:'411113' ),
    (a:' '; b:' '; c:'98'; data:'411311' ),
    (a:' '; b:' '; c:'99'; data:'113141' ),
    (a:' '; b:' '; c:'  '; data:'114131' ),
    (a:' '; b:' '; c:'  '; data:'311141' ),
    (a:' '; b:' '; c:'  '; data:'411131' ));
var
  i,j,idx     :integer;
  startcode   :string;
  checksum    :integer;
  codeword_pos:integer;
  text        :string;
// find code 128 codeset a or b
function Find_Code128AB(c:char):integer;
var
  i:integer;
  v:char;
begin
  Result:=-1;
  for i:=0 to High(Table_128) do
  begin
    if fBarcodeType=bcCode128A then
      v:=Table_128[i].a
    else
      v:=Table_128[i].b;
    if c=v then
    begin
      Result:=i;
      Break;
    end;
  end;
end;
// find code 128 codeset c
function Find_Code128C(c:string):integer;
var
  i:integer;
begin
  Result:=-1;
  for i:=0 to High(Table_128) do
    if Table_128[i].c=c then
    begin
      Result:=i;
      Break;
    end;
end;
begin
  text:=aText;
  //
  case fBarcodeType of
    bcCode128A,
    bcEAN128A: begin
                 checksum :=103;
                 startcode:=StartA;
               end;
    bcCode128B,
    bcEAN128B: begin
                 checksum :=104;
                 startcode:=StartB;
               end;
    bcCode128C,
    bcEAN128C: begin
                 checksum :=105;
                 startcode:=StartC;
               end;
  else
    checksum:=0;
  end;

  // startcode
  Result:=Convert(startcode);
  codeword_pos:=1;

  case fBarcodeType of
    bcEAN128A,
    bcEAN128B,
    bcEAN128C: begin // special identifier FNC1 = function code 1 for EAN 128 barcodes
                 Result:=Result+Convert(Table_128[102].data);
                 Inc(checksum,102*codeword_pos);
                 Inc(codeword_pos);
                 // if there is no checksum at the end of the string the EAN128 needs one (modulo 10)
                 if fCheckSum then
                   text:=DoCheckSumming(text);
               end;
  end;
  if (fBarcodeType = bcCode128C) or (fBarcodeType = bcEAN128C) then
  begin
    if Length(text) mod 2<>0 then
      text:='0'+text;
    for i:=1 to Length(text) div 2 do
    begin
      j:=(i-1)*2+1;
      idx:=Find_Code128C(copy(text,j,2));
      if idx<0 then
        idx:=Find_Code128C('00');
      Result:=Result+Convert(Table_128[idx].data);
      Inc(checksum,idx*codeword_pos);
      Inc(codeword_pos);
    end;
  end
  else
    for i:=1 to Length(text) do
    begin
      idx:=Find_Code128AB(text[i]);
      if idx<0 then
        idx:=Find_Code128AB(' ');
      Result:=Result+Convert(Table_128[idx].data);
      Inc(checksum,idx*codeword_pos);
      Inc(codeword_pos);
    end;
  checksum:=checksum mod 103;
  Result:=Result+Convert(Table_128[checksum].data);
  // stopcode
  Result:=Result+Convert(Stop);
end;

type
  TCode93=record
            c   :char;
            data:array[0..5] of char;
          end;

const
  Table_93:array[0..46] of TCode93=(
    (c:'0'; data:'131112'),
    (c:'1'; data:'111213'),
    (c:'2'; data:'111312'),
    (c:'3'; data:'111411'),
    (c:'4'; data:'121113'),
    (c:'5'; data:'121212'),
    (c:'6'; data:'121311'),
    (c:'7'; data:'111114'),
    (c:'8'; data:'131211'),
    (c:'9'; data:'141111'),
    (c:'A'; data:'211113'),
    (c:'B'; data:'211212'),
    (c:'C'; data:'211311'),
    (c:'D'; data:'221112'),
    (c:'E'; data:'221211'),
    (c:'F'; data:'231111'),
    (c:'G'; data:'112113'),
    (c:'H'; data:'112212'),
    (c:'I'; data:'112311'),
    (c:'J'; data:'122112'),
    (c:'K'; data:'132111'),
    (c:'L'; data:'111123'),
    (c:'M'; data:'111222'),
    (c:'N'; data:'111321'),
    (c:'O'; data:'121122'),
    (c:'P'; data:'131121'),
    (c:'Q'; data:'212112'),
    (c:'R'; data:'212211'),
    (c:'S'; data:'211122'),
    (c:'T'; data:'211221'),
    (c:'U'; data:'221121'),
    (c:'V'; data:'222111'),
    (c:'W'; data:'112122'),
    (c:'X'; data:'112221'),
    (c:'Y'; data:'122121'),
    (c:'Z'; data:'123111'),
    (c:'-'; data:'121131'),
    (c:'.'; data:'311112'),
    (c:' '; data:'311211'),
    (c:'$'; data:'321111'),
    (c:'/'; data:'112131'),
    (c:'+'; data:'113121'),
    (c:'%'; data:'211131'),
    (c:'['; data:'121221'),   {only used for Extended Code 93}
    (c:']'; data:'312111'),   {only used for Extended Code 93}
    (c:'{'; data:'311121'),   {only used for Extended Code 93}
    (c:'}'; data:'122211'));  {only used for Extended Code 93}

function FindCode93(c:char):integer;
var
  i:integer;
begin
  for i:=0 to High(Table_93) do
    if c = Table_93[i].c then
    begin
      Result:=i;
      Exit;
    end;
  Result:=-1;
end;

function TRLCustomBarcode.GetAs93(const aText:string):string;
var
  checkC,checkK,weightC,weightK,i,idx:integer;
begin
  // startcode
  Result:=Convert('111141');
  for i:=1 to Length(aText) do
  begin
    idx:=FindCode93(aText[i]);
    if idx<0 then
      fInvalid:=True
    else
      Result:=Result+Convert(Table_93[idx].data);
  end;

  checkC :=0;
  checkK :=0;
  weightC:=1;
  weightK:=2;

  for i:=Length(aText) downto 1 do
  begin
    idx:=FindCode93(aText[i]);
    Inc(checkC,idx*weightC);
    Inc(checkK,idx*weightK);
    Inc(weightC);
    if weightC>20 then
      weightC:=1;
    Inc(weightK);
    if weightK>15 then
      weightC:=1;
  end;

  Inc(checkK,checkC);
  checkC:=checkC mod 47;
  checkK:=checkK mod 47;

  Result:=Result+Convert(Table_93[checkC].data) +
  Convert(Table_93[checkK].data);
  // stopcode
  Result:=Result+Convert('1111411');
end;

const
  Code93x:array[0..127] of string[2]=(
    (']U'), ('[A'), ('[B'), ('[C'), ('[D'), ('[E'), ('[F'), ('[G'),
    ('[H'), ('[I'), ('[J'), ('[K'), ('[L'), ('[M'), ('[N'), ('[O'),
    ('[P'), ('[Q'), ('[R'), ('[S'), ('[T'), ('[U'), ('[V'), ('[W'),
    ('[X'), ('[Y'), ('[Z'), (']A'), (']B'), (']C'), (']D'), (']E'),
    (' '),  ('{A'), ('{B'), ('{C'), ('{D'), ('{E'), ('{F'), ('{G'),
    ('{H'), ('{I'), ('{J'), ('{K'), ('{L'), ('{M'), ('{N'), ('{O'),
    ( '0'), ('1'),  ('2'),  ('3'),  ('4'),  ('5'),  ('6'),  ('7'),
    ('8'),  ('9'),  ('{Z'), (']F'), (']G'), (']H'), (']I'), (']J'),
    (']V'), ('A'),  ('B'),  ('C'),  ('D'),  ('E'),  ('F'),  ('G'),
    ('H'),  ('I'),  ('J'),  ('K'),  ('L'),  ('M'),  ('N'),  ('O'),
    ('P'),  ('Q'),  ('R'),  ('S'),  ('T'),  ('U'),  ('V'),  ('W'),
    ('X'),  ('Y'),  ('Z'),  (']K'), (']L'), (']M'), (']N'), (']O'),
    (']W'), ('}A'), ('}B'), ('}C'), ('}D'), ('}E'), ('}F'), ('}G'),
    ('}H'), ('}I'), ('}J'), ('}K'), ('}L'), ('}M'), ('}N'), ('}O'),
    ('}P'), ('}Q'), ('}R'), ('}S'), ('}T'), ('}U'), ('}V'), ('}W'),
    ('}X'), ('}Y'), ('}Z'), (']P'), (']Q'), (']R'), (']S'), (']T')
    );
    
function TRLCustomBarcode.GetAs93Extended(const aText:string):string;
var
  s:string;
  i:integer;
begin
  s:='';
  for i:=0 to Length(aText)-1 do
    if Ord(aText[i])<=127 then
      s:=s+Code93x[Ord(aText[i])]
    else
      fInvalid:=True;
  Result:=GetAs93(s);
end;

const
  Table_MSI:array['0'..'9'] of string[8]=(
    ('51515151'), {'0'}
    ('51515160'), {'1'}
    ('51516051'), {'2'}
    ('51516060'), {'3'}
    ('51605151'), {'4'}
    ('51605160'), {'5'}
    ('51606051'), {'6'}
    ('51606060'), {'7'}
    ('60515151'), {'8'}
    ('60515160')  {'9'}
    );

function Quersumme(x:integer):integer;
begin
  Result:=0;
  while x>0 do
  begin
    Inc(Result,x mod 10);
    x:=x div 10;
  end;
end;

function TRLCustomBarcode.GetAsMSI(const aText:string):string;
var
  check_even,check_odd,checksum:integer;
  i:integer;
begin
  // startcode
  Result:='60';
  check_even:=0;
  check_odd :=0;

  for i:=1 to Length(aText) do
  begin
    if Odd(i-1) then
      check_odd:=check_odd*10+Ord(aText[i])
    else
      check_even:=check_even+Ord(aText[i]);
    Result:=Result+Table_MSI[aText[i]];
  end;

  checksum:=Quersumme(check_odd*2)+check_even;

  checksum:=checksum mod 10;
  if checksum>0 then
    checksum:=10-checksum;

  Result:=Result+Table_MSI[Char(Ord('0')+checksum)];
  // stopcode
  Result:=Result+'515';
end;

const
  Table_PostNet:array['0'..'9'] of string[10]=(
    ('5151A1A1A1'), {'0'}
    ('A1A1A15151'), {'1'}
    ('A1A151A151'), {'2'}
    ('A1A15151A1'), {'3'}
    ('A151A1A151'), {'4'}
    ('A151A151A1'), {'5'}
    ('A15151A1A1'), {'6'}
    ('51A1A1A151'), {'7'}
    ('51A1A151A1'), {'8'}
    ('51A151A1A1')  {'9'}
    );
    
function TRLCustomBarcode.GetAsPostNet(const aText:string):string;
var
  i:integer;
begin
  Result:='51';
  for i:=1 to Length(aText) do
    Result:=Result+Table_PostNet[aText[i]];
  Result:=Result+'5';
end;

type
  TCodabar=record
             c   :char;
             data:array[0..6] of char;
           end;
           
const
  Table_cb: array[0..19] of TCodabar=(
    (c:'1'; data:'5050615'),
    (c:'2'; data:'5051506'),
    (c:'3'; data:'6150505'),
    (c:'4'; data:'5060515'),
    (c:'5'; data:'6050515'),
    (c:'6'; data:'5150506'),
    (c:'7'; data:'5150605'),
    (c:'8'; data:'5160505'),
    (c:'9'; data:'6051505'),
    (c:'0'; data:'5050516'),
    (c:'-'; data:'5051605'),
    (c:'$'; data:'5061505'),
    (c:':'; data:'6050606'),
    (c:'/'; data:'6060506'),
    (c:'.'; data:'6060605'),
    (c:'+'; data:'5060606'),
    (c:'A'; data:'5061515'),
    (c:'B'; data:'5151506'),
    (c:'C'; data:'5051516'),
    (c:'D'; data:'5051615')
    );
    
function FindCodabar(c:char):integer;
var
  i:integer;
begin
  Result:=-1;
  for i:=0 to High(Table_cb) do
    if c=Table_cb[i].c then
    begin
      Result:=i;
      Break;
    end;
end;

function TRLCustomBarcode.GetAsCodaBar(const aText:string):string;
var
  i,idx:integer;
begin
  Result:=Table_cb[FindCodabar('A')].data+'0';
  for i:=1 to Length(aText) do
  begin
    idx:=FindCodabar(aText[i]);
    if idx<0 then
      fInvalid:=True
    else
      Result:=Result+Table_cb[idx].data+'0';
  end;
  Result:=Result+Table_cb[FindCodabar('B')].data;
end;

function TRLCustomBarcode.GetAsUPC_A(const aText:string):string;
var
  tmp:string;
  i  :integer;
begin
  tmp:=PadZ(aText,12);
  if fCheckSum then
    tmp:=DoCheckSumming(Copy(tmp,1,11));
  // startcode
  Result:='505';
  for i:=1 to 6 do
    Result:=Result+Table_EAN_A[tmp[i]];
  Result:=Result+'05050';   {Trennzeichen}
  for i:=7 to 12 do
    Result:=Result+Table_EAN_C[tmp[i]];
  // stopcode
  Result:=Result+'505';
end;

{UPC E Parity Pattern Table, Number System 0}
const
  Table_UPC_E0:array['0'..'9', 1..6] of char=(
    ('E', 'E', 'E', 'o', 'o', 'o' ),    { 0 }
    ('E', 'E', 'o', 'E', 'o', 'o' ),    { 1 }
    ('E', 'E', 'o', 'o', 'E', 'o' ),    { 2 }
    ('E', 'E', 'o', 'o', 'o', 'E' ),    { 3 }
    ('E', 'o', 'E', 'E', 'o', 'o' ),    { 4 }
    ('E', 'o', 'o', 'E', 'E', 'o' ),    { 5 }
    ('E', 'o', 'o', 'o', 'E', 'E' ),    { 6 }
    ('E', 'o', 'E', 'o', 'E', 'o' ),    { 7 }
    ('E', 'o', 'E', 'o', 'o', 'E' ),    { 8 }
    ('E', 'o', 'o', 'E', 'o', 'E' )     { 9 }
    );
function TRLCustomBarcode.GetAsUPC_E0(const aText:string):string;
var
  i,j:integer;
  tmp:string;
  c  :char;
begin
  tmp:=PadZ(aText,7);
  tmp:=DoCheckSumming(Copy(tmp,1,6));
  c  :=tmp[7];
  // startcode
  Result:='505';
  for i:=1 to 6 do
    if Table_UPC_E0[c,i]='E' then
      for j:=1 to 4 do
        Result:=Result+Table_EAN_C[tmp[i],5-j]
    else
      Result:=Result+Table_EAN_A[tmp[i]];
  // stopcode
  Result:=Result+'05050';
end;

function TRLCustomBarcode.GetAsUPC_E1(const aText:string):string;
var
  i,j:integer;
  tmp:string;
  c  :char;
begin
  tmp:=PadZ(aText,7);
  tmp:=DoCheckSumming(Copy(tmp,1,6));
  c  :=tmp[7];
  // startcode
  Result:='505';
  for i:=1 to 6 do
    if Table_UPC_E0[c,i]='E' then
      Result:=Result+Table_EAN_A[tmp[i]]
    else
      for j:= 1 to 4 do
        Result:=Result+Table_EAN_C[tmp[i],5-j];
  // stopcode
  Result:=Result+'05050';
end;

function GetSupp(const aNumber:string):string;
var
  i,fak,sum:Integer;
  tmp      :string;
begin
  sum:=0;
  tmp:=copy(aNumber,1,Length(aNumber)-1);
  fak:=Length(tmp);
  for i:=1 to Length(tmp) do
  begin
    if (fak mod 2) = 0 then
      sum:=sum+(StrToInt(tmp[i])*9)
    else
      sum:=sum+(StrToInt(tmp[i])*3);
    Dec(fak);
  end;
  sum:=((sum mod 10) mod 10) mod 10;
  //
  Result:=tmp+IntToStr(sum);
end;

function TRLCustomBarcode.GetAsUPC_Supp5(const aText:string):string;
var
  i,j:integer;
  tmp:string;
  c  :char;
begin
  tmp:=PadZ(aText,5);
  tmp:=GetSupp(copy(tmp,1,5)+'0');
  c  :=tmp[6];
  // startcode
  Result:='506';
  for i:=1 to 5 do
  begin
    if Table_UPC_E0[c,(6-5)+i]='E' then
      for j:=1 to 4 do
        Result:=Result+Table_EAN_C[tmp[i],5-j]
    else
      Result:=Result+Table_EAN_A[tmp[i]];
    // character delineator
    if i<5 then
      Result:=Result+'05';
  end;
end;

function TRLCustomBarcode.GetAsUPC_Supp2(const aText:string):string;
var
  tmp,mS:string;
  i,j   :integer;
begin
  tmp:=PadZ(aText,2);
  i  :=StrToInt(tmp);
  case i mod 4 of
    3: mS:='EE';
    2: mS:='Eo';
    1: mS:='oE';
    0: mS:='oo';
  end;
  tmp:=GetSupp(copy(tmp,1,5)+'0');
  // startcode
  Result:='506';
  for i:=1 to 2 do
  begin
    if mS[i]='E' then
      for j:= 1 to 4 do
        Result:=Result+Table_EAN_C[tmp[i],5-j]
    else
      Result:=Result+Table_EAN_A[tmp[i]];
    if i<2 then
      Result:=Result+'05'; // character delineator
  end;
end;

procedure TRLCustomBarcode.SetMargins(const aValue: TRLMargins);
begin
  fMargins.Assign(aValue);
  Invalidate;
end;

procedure TRLCustomBarcode.SetInvalidCode(const Value: TRLBarcodeInvalidCode);
begin
  if Value=fInvalidCode then
    Exit;
  fInvalidCode:=Value;
  Invalidate;
end;

procedure TRLCustomBarcode.SetCheckSum(const Value: boolean);
begin
  if Value=fCheckSum then
    Exit;
  AdjustBounds;
  Invalidate;
  fCheckSum:=Value;
end;

procedure TRLCustomBarcode.SetCheckSumMethod(
  const Value: TRLBarcodeCheckSumMethod);
begin
  if Value=fCheckSumMethod then
    Exit;
  AdjustBounds;
  Invalidate;
  fCheckSumMethod:=Value;
end;

{ TRLCustomDBBarcode }

constructor TRLCustomDBBarcode.Create(aOwner: TComponent);
begin
  fDataField  :=emptystr;
  fDataFormula:=emptystr;
  fDataSource :=nil;
  //
  inherited Create(aOwner);
end;

procedure TRLCustomDBBarcode.Notification(aComponent: TComponent; Operation: TOperation);
begin
  inherited;
  //
  if Operation=opRemove then
    if aComponent=fDataSource then
      fDataSource:=nil;
end;

procedure TRLCustomDBBarcode.SetDataSource(const aValue: TDataSource);
begin
  if aValue=fDataSource then
    Exit;
  fDataSource:=aValue;
  if aValue<>nil then
    aValue.FreeNotification(Self);
  MakeCaption;
end;

procedure TRLCustomDBBarcode.SetDataField(const aValue: TRLDataFieldProperty);
begin
  if aValue=fDataField then
    Exit;
  if aValue<>emptystr then
    fDataFormula:=emptystr;
  fDataField:=aValue;
  MakeCaption;
end;

procedure TRLCustomDBBarcode.SetDataFormula(const aValue: string);
begin
  if aValue=fDataFormula then
    Exit;
  if aValue<>emptystr then
    fDataField:=emptystr;
  fDataFormula:=aValue;
  MakeCaption;
end;

function TRLCustomDBBarcode.GetDataSet: TDataSet;
begin
  if Assigned(fDataSource) then
    Result:=fDataSource.DataSet
  else
    Result:=nil;
end;

function TRLCustomDBBarcode.GetField: TField;
begin
  if (DataSet<>nil) and (fDataField<>emptystr) then
  begin
    Result:=DataSet.FindField(fDataField);
    if Result=nil then
      raise Exception.Create(LS_NotFoundStr+': '+Name+'.DataField "'+fDataField+'"');
  end
  else
    Result:=nil;
end;

function TRLCustomDBBarcode.GetFieldText: string;
var
  d:TDataSet;
  f:TField;
  p:TRLCustomReport;
begin
  p:=FindParentReport;
  if not IsPreparing then
    if FriendlyName<>emptystr then
      Result:=FriendlyName
    else if fDataField<>emptystr then
      Result:=GetFieldLabel
    else if fDataFormula<>emptystr then
      Result:='('+fDataFormula+')'
    else
      Result:=Name
  else
  begin
    d:=GetDataSet;
    f:=GetField;
    if Assigned(d) and d.Active and not d.Eof then
      if f<>nil then
        Result:=SmartGetFieldDisplayText(f)
      else if fDataFormula<>emptystr then
        Result:=p.Parse(Self,fDataFormula)
      else
        Result:=emptystr
    else
      Result:=emptystr;
  end;
end;

function TRLCustomDBBarcode.InternalMakeCaption: string;
begin
  Result:=GetFieldText;
end;

function TRLCustomDBBarcode.GetFieldLabel: string;
var
  f:TField;
begin
  if (DataSet<>nil) and (fDataField<>'') then
    f:=DataSet.FindField(fDataField)
  else
    f:=nil;
  if f<>nil then
    Result:=f.DisplayLabel
  else
    Result:=fDataField;
end;

end.

