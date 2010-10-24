{$I RLReport.inc}

{ TODO : se houver uma macro não resolvida numa página, colocá-la na espera. }

{@unit RLMetaFile - Implementação das classes e rotinas para manipulação de coleções gráficas.
Portado para o Lazarus - Trabalho inicial de Isaac Trindade da Silva contato tioneobrasil@yahoo.com.br dicas4lazarus@yahoo.com.br
Lazarus Ported - initial work by Isaac 07/2009
}
unit RLMetaFile;
{$MODE DELPHI}{$H+}
interface

uses
  Classes, SysUtils, Contnrs, SyncObjs, Graphics, Dialogs,
  RLUtils, RLConsts, RLTypes,  Types;

const
  MetaOrientationPortrait =1;
  MetaOrientationLandscape=2;

  MetaTextAlignmentLeft   =1;
  MetaTextAlignmentRight  =2;
  MetaTextAlignmentCenter =3;
  MetaTextAlignmentJustify=4;
                
  MetaTextLayoutTop    =1;
  MetaTextLayoutBottom =2;
  MetaTextLayoutCenter =3;
  MetaTextLayoutJustify=4;

  MetaTextFlagAutoSize      =1;
  MetaTextFlagWordWrap      =2;
  MetaTextFlagIntegralHeight=4;
  MetaTextFlagContinuous    =8;

  MetaBrushStyleSolid     =1;
  MetaBrushStyleClear     =2;
  MetaBrushStyleHorizontal=3;
  MetaBrushStyleVertical  =4;
  MetaBrushStyleFDiagonal =5;
  MetaBrushStyleBDiagonal =6;
  MetaBrushStyleCross     =7;
  MetaBrushStyleDiagCross =8;

  MetaFontPitchDefault =1;
  MetaFontPitchVariable=2;
  MetaFontPitchFixed   =3;

  MetaFontStyleBold     =1;
  MetaFontStyleItalic   =2;
  MetaFontStyleUnderline=4;
  MetaFontStyleStrikeOut=8;

  MetaPenModeBlack      =1;
  MetaPenModeWhite      =2;
  MetaPenModeNop        =3;
  MetaPenModeNot        =4;
  MetaPenModeCopy       =5;
  MetaPenModeNotCopy    =6;
  MetaPenModeMergePenNot=7;
  MetaPenModeMaskPenNot =8;
  MetaPenModeMergeNotPen=9;
  MetaPenModeMaskNotPen =10;
  MetaPenModeMerge      =11;
  MetaPenModeNotMerge   =12;
  MetaPenModeMask       =13;
  MetaPenModeNotMask    =14;
  MetaPenModeXor        =15;
  MetaPenModeNotXor     =16;

  MetaPenStyleSolid      =1;
  MetaPenStyleDash       =2;
  MetaPenStyleDot        =3;
  MetaPenStyleDashDot    =4;
  MetaPenStyleDashDotDot =5;
  MetaPenStyleClear      =6;
  MetaPenStyleInsideFrame=7;

const
  MAXPAGECACHE=5;

type
  TRLGraphicObject=class;
  
  TRLMetaOrientation=byte;

  TRLMetaTextFlags=word;

  TRLMetaColor=packed record
    Red,Green,Blue:byte;
  end;

  TRLMetaTextAlignment=byte;
  TRLMetaTextLayout=byte;

  TRLMetaPenMode=byte;
  TRLMetaPenStyle=byte;
  TRLMetaBrushStyle=byte;
  TRLMetaFontCharset=byte;
  TRLMetaFontStyles=byte;
  TRLMetaFontPitch=byte;

  TRLMetaRect=packed record
    Left,Top,Right,Bottom:integer;
  end;

  TRLMetaPoint=packed record
    X,Y:integer;
  end;

  TRLMetaPointArray=packed array of TRLMetaPoint;

  TRLMetaFontDescriptor=record
    Name        :string;
    Styles      :string;
    Flags       :integer;
    FontBBox    :TRect;
    MissingWidth:integer;
    StemV       :integer;
    StemH       :integer;
    ItalicAngle :integer;
    CapHeight   :integer;
    XHeight     :integer;
    Ascent      :integer;
    Descent     :integer;
    Leading     :integer;
    MaxWidth    :integer;
    AvgWidth    :integer;
  end;
  
  TRLMetaFontMetrics=record
    TrueType      :boolean;
    BaseFont      :string;
    FirstChar     :integer;
    LastChar      :integer;
    Widths        :array[0..255] of integer;
    FontDescriptor:TRLMetaFontDescriptor;
  end;

  TRLMetaPen=class
  private
    fUser :TRLGraphicObject;
    fColor:TRLMetaColor;
    fMode :TRLMetaPenMode;
    fStyle:TRLMetaPenStyle;
    fWidth:integer;
  protected
    function    GetColor:TRLMetaColor;
    procedure   SetColor(const Value:TRLMetaColor);
    function    GetWidth:integer;
    procedure   SetWidth(Value:integer);
    function    GetMode:TRLMetaPenMode;
    procedure   SetMode(Value:TRLMetaPenMode);
    function    GetStyle:TRLMetaPenStyle;
    procedure   SetStyle(Value:TRLMetaPenStyle);
  public
    constructor Create(aUser:TRLGraphicObject);
    //
    procedure   SaveToStream(aStream:TStream);
    procedure   LoadFromStream(aStream:TStream);
    //
    procedure   Assign(aObject:TRLMetaPen);
    //
    procedure   Inflate(aFactor:double);
    //
    property    Color:TRLMetaColor    read GetColor write SetColor;
    property    Mode :TRLMetaPenMode  read GetMode  write SetMode;
    property    Style:TRLMetaPenStyle read GetStyle write SetStyle;
    property    Width:integer         read GetWidth write SetWidth;
  end;

  TRLMetaBrush=class
  private
    fUser :TRLGraphicObject;
    fColor:TRLMetaColor;
    fStyle:TRLMetaBrushStyle;
  protected
    function    GetStyle:TRLMetaBrushStyle;
    procedure   SetStyle(Value:TRLMetaBrushStyle);
    function    GetColor:TRLMetaColor;
    procedure   SetColor(const Value:TRLMetaColor);
  public
    constructor Create(aUser:TRLGraphicObject);
    //
    procedure   SaveToStream(aStream:TStream);
    procedure   LoadFromStream(aStream:TStream);
    //
    procedure   Assign(aObject:TRLMetaBrush);
    //
    property    Color:TRLMetaColor      read GetColor write SetColor;
    property    Style:TRLMetaBrushStyle read GetStyle write SetStyle;
  end;

  TRLMetaFont=class
  private
    fUser         :TRLGraphicObject;
    fPixelsPerInch:integer;
    fCharset      :TRLMetaFontCharset;
    fColor        :TRLMetaColor;
    fHeight       :integer;
    fNameId       :integer;
    fPitch        :TRLMetaFontPitch;
    fSize         :integer;
    fStyle        :TRLMetaFontStyles;
  protected
    function    GetName:string;
    procedure   SetName(const Value:string);
    function    GetCharset:TRLMetaFontCharset;
    procedure   SetCharset(Value:TRLMetaFontCharset);
    function    GetColor:TRLMetaColor;
    procedure   SetColor(const Value:TRLMetaColor);
    function    GetStyle:TRLMetaFontStyles;
    procedure   SetStyle(Value:TRLMetaFontStyles);
    function    GetSize:integer;
    procedure   SetSize(Value:integer);
    function    GetPixelsPerInch:integer;
    procedure   SetPixelsPerInch(Value:integer);
    function    GetPitch:TRLMetaFontPitch;
    procedure   SetPitch(Value:TRLMetaFontPitch);
    function    GetHeight:integer;
    procedure   SetHeight(Value:integer);
  public
    constructor Create(aUser:TRLGraphicObject);
    //
    procedure   SaveToStream(aStream:TStream);
    procedure   LoadFromStream(aStream:TStream);
    //
    procedure   Assign(aObject:TRLMetaFont);
    //
    property    PixelsPerInch:integer           read GetPixelsPerInch write SetPixelsPerInch;
    property    Charset      :TRLMetaFontCharset read GetCharset       write SetCharset;
    property    Color        :TRLMetaColor       read GetColor         write SetColor;
    property    Height       :integer           read GetHeight        write SetHeight;
    property    Name         :string            read GetName          write SetName;
    property    Pitch        :TRLMetaFontPitch   read GetPitch         write SetPitch;
    property    Size         :integer           read GetSize          write SetSize;
    property    Style        :TRLMetaFontStyles  read GetStyle         write SetStyle;
  end;

  TRLGraphicStorage=class;
  TRLGraphicSurface=class;
  TRLGraphicObjectClass=class of TRLGraphicObject;

  TInt64List=class
  private
    fInt64Array:array of Int64;
    fCapacity  :integer;
    fCount     :integer;
    function GetItems(i: integer): Int64;
    procedure SetItems(i: integer; const Value: Int64);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Add(Value:Int64):integer;
    property Items[i:integer]:Int64 read GetItems write SetItems; default;
    property Count:integer          read fCount;
  end;

  {@class TRLGraphicStorage - Coleção de páginas ou superfícies de desenho. }
  TRLGraphicStorage=class(TComponent)
  private
    // cache para páginas em memória
    fPageCache     :TObjectList;
    // endereço das páginas em disco (stream)
    fPageAllocation:TInt64List;
    // arquivo temporário para armazenamento das páginas
    fTempStream    :TStream;
    fTempFileName  :string;
    // versão do arquivo carregado que indica também o formato da gravação
    fFileVersion   :integer;
    // metasímbolos
    fMacros        :TStrings;
    // lista de referências à este objeto. quando não houver mais referências, o objeto é destruído
    fReferenceList :TList;
    //
    fUpdateCalls    :integer;
    fLock           :SyncObjs.TCriticalSection;
    fAboutToUpdate  :boolean;
    fLockIndent     :integer;
    fCanceled       :boolean;
    // guarda referência à página no cache em memória
    procedure   AddToCache(aSurface:TRLGraphicSurface);
    // retorna referência à página se ela estiver no cache em memória
    function    GetFromCache(aPageIndex:integer):TRLGraphicSurface;
    // atualiza pendências do cache em disco
    procedure   FlushCache;
    // instancia página e carrega do disco
    function    LoadPageFromDisk(aPageIndex:integer):TRLGraphicSurface;
    // retorna referência à página quer esteja em disco ou cachê
    function    GetPages(aPageIndex:integer):TRLGraphicSurface;
    // retorna a quantidade de páginas estocadas
    function    GetPageCount: integer;
    // força a criação do arquivo temporário
    procedure   TempStreamNeeded;
    // policia o número da versão para gravação
    procedure   SetFileVersion(aVersion:integer);
    procedure   Lock;
    procedure   Unlock;
    // getters e setters de símbolos especiais
    function    GetFirstPageNumber: integer;
    function    GetHeight: integer;
    function    GetLastPageNumber: integer;
    function    GetOrientation: TRLMetaOrientation;
    function    GetPaperHeight: double;
    function    GetPaperWidth: double;
    function    GetTitle: string;
    function    GetWidth: integer;
    function    GetOrientedHeight: integer;
    function    GetOrientedPaperHeight: double;
    function    GetOrientedPaperWidth: double;
    function    GetOrientedWidth: integer;
    procedure   SetFirstPageNumber(const Value: integer);
    procedure   SetLastPageNumber(const Value: integer);
    procedure   SetOrientation(const Value: TRLMetaOrientation);
    procedure   SetPaperHeight(const Value: double);
    procedure   SetPaperWidth(const Value: double);
    procedure   SetTitle(const Value: string);
    function    GetMacro(const Name: string): string;
    procedure   SetMacro(const Name, Value: string);
  protected
    procedure   Notification(aComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(aOwner:TComponent); override;
    destructor  Destroy; override;
    {@method Link - Cria uma referência para o componente.
     A instância é mantida até que não haja mais referências a ela. :/}
    procedure   Link(aComponent:TComponent);

    {@method Unlink - Retira referência para o componente.
     Quando não houver mais referências, a instância é automaticamente liberada. :/}
    procedure   Unlink(aComponent:TComponent=nil);

    {@method Add - Adiciona página à coleção. :/}
    procedure   Add(aSurface:TRLGraphicSurface);

    {@method New - Adiciona página à coleção. O tamanho da página pode ser informado em milímetros
     ou com uma constante fpXX da unit RLTypes.
     @links TRLPaperSize,TRLGraphicSurface. :}
    function New(PaperWidth,PaperHeight:double):TRLGraphicSurface; overload;
    function New(PaperSize:TRLPaperSize):TRLGraphicSurface; overload;
    {/@method}

    {@method StorePage - Atualiza dados da página em disco. :/}
    procedure   StorePage(aSurface:TRLGraphicSurface);
    {@method RetrievePage - Recupera a página do espaço temporário em disco. :/}
    procedure   RetrievePage(aSurface:TRLGraphicSurface);

    procedure   PrepareUpdate;
    procedure   BeginUpdate;
    procedure   EndUpdate;

    {@prop Busy - Indica se a coleção está sendo escrita no momento ou se já está
    pronta para ser impressa. @links BeginUpdate, EndUpdate. :/}
    function Busy:boolean;

    procedure Cancel;

    {@prop WaitPage - Retorna referência para uma página pelo índice. Se a página
    ainda não foi concluída, a thread é deixada em estado de espera. :/}
    function WaitPage(aPageIndex:integer):TRLGraphicSurface;

    {@method Clear - Libera todas as páginas da memória e do cachê. :/}
    procedure   Clear;

    {@method SaveToFile - Salva páginas para uma arquivo em disco. :/}
    procedure   SaveToFile(const aFileName:string);

    {@method LoadFromFile - Carrega páginas de um arquivo em disco. :/}
    procedure   LoadFromFile(const aFileName:string);

    {@method SaveToStream - Salva páginas em uma stream. :/}
    procedure   SaveToStream(aStream:TStream);

    {@method LoadFromStream - Carrega páginas de uma stream. :/}
    procedure   LoadFromStream(aStream:TStream);

    {@prop Pages - Retorna página pelo índice. :/}
    property    Pages[aPageIndex:integer]:TRLGraphicSurface read GetPages; default;

    {@prop PageCount - Retorna a quantidade páginas armazenadas. :/}
    property    PageCount:integer read GetPageCount;

    {@prop FileVersion - Indica versão do relatório carregado ou determina a versão do arquivo a ser gravado.
     Esta prop pode ser utilizada para converter arquivos de uma versão para outra, bastando para isso, carregar
     o arquivo, alterar a sua versão e salvá-lo novamente. :/}
    property    FileVersion:integer read fFileVersion write SetFileVersion;

    {@prop Macros - Lista de símbolos para tradução em tempo de visualização ou impressão. :/}
    property    Macros:TStrings read fMacros;

    {@prop FirstPageNumber - Numeração para a primeira página.
     Este número é normalmente 1, mas o relatório pode ser parte de uma encadernação maior e por isso ter uma
     numeração intercalada. :/}
    property    FirstPageNumber    :integer            read GetFirstPageNumber     write SetFirstPageNumber;

    {@prop LastPageNumber - Número da última página. :/}
    property    LastPageNumber     :integer            read GetLastPageNumber      write SetLastPageNumber;

    {@prop Title - Título do relatório. :/}
    property    Title              :string             read GetTitle               write SetTitle;

    {@prop Orientation - Orientação do papel. :/}
    property    Orientation        :TRLMetaOrientation read GetOrientation         write SetOrientation;

    {@prop PaperWidth - Largura do papel em milímetros. :/}
    property    PaperWidth         :double             read GetPaperWidth          write SetPaperWidth;

    {@prop PaperHeight - Altura do papel em milímetros. :/}
    property    PaperHeight        :double             read GetPaperHeight         write SetPaperHeight;

    {@prop OrientedPaperWidth - Largura do papel orientado para leitura em milímetros. :/}
    property    OrientedPaperWidth :double  read GetOrientedPaperWidth;

    {@prop OrientedPaperHeight - Altura do papel orientado para leitura em milímetros. :/}
    property    OrientedPaperHeight:double  read GetOrientedPaperHeight;

    {@prop OrientedWidth - Largura da superfície orientada para leitura em pixels. :/}
    property    OrientedWidth      :integer read GetOrientedWidth;

    {@prop OrientedHeight - Altura da superfície orientada para leitura em pixels. :/}
    property    OrientedHeight     :integer read GetOrientedHeight;

    {@prop Width - Largura da superfície em pixels. :/}
    property    Width              :integer read GetWidth;

    {@prop Height - Altura da superfície em pixels. :/}
    property    Height             :integer read GetHeight;

    property    Canceled:boolean read fCanceled;
  end;
  {/@class}

  {@class TRLGraphicSurface - Superfície de desenho.
   Assemelha-se ao TCanvas e, embora não haja qualquer relação hierárquica, contempla a maioria de seus métodos de
   desenho. }
  TRLGraphicSurface=class
  private
    // referência ao estoque. o estoque será avisado sempre que uma página for detruída para que seja excluída do cachê 
    fStorage    :TRLGraphicStorage;
    // índice da página
    fPageIndex  :integer;
    // lista de objetos gráficos
    fObjects    :TObjectList;
    // posição do cursor (caneta)
    fPenPos     :TPoint;
    fWritePos   :TPoint;
    fWordWrap   :boolean;
    // largura, altura e orientação
    fWidth      :integer;
    fHeight     :integer;
    // prop de desenho atuais
    fBrush      :TBrush;
    fFont       :TFont;
    fPen        :TPen;
    // margens para write e writeln
    fMargins    :TRect;
    // indica se algo foi desenhado
    fOpened     :boolean;
    fModified   :boolean;
    // coleção de fontes
    fFonts      :TStrings;
    // controle de clipping
    fClipStack  :TList;
    fClipRect   :TRect;
    // para livre uso
    fTag        :integer;
    // identificador de grupo e gerador
    fGeneratorId:integer;
    // metasímbolos
    fMacros        :TStrings;
    // retorna a quantidade de objetos incluídos
    function    GetObjectCount:integer;
    // referência ao objeto pelo índice
    function    GetObjects(aIndex:integer):TRLGraphicObject;
    // muda as props de desenho
    procedure   SetBrush(const Value:TBrush);
    procedure   SetFont(const Value:TFont);
    procedure   SetPen(const Value:TPen);
    // desenho a nível de pontos
    function    GetPixels(X,Y:integer):TColor;
    procedure   SetPixels(X,Y:integer; const Value:TColor);
    //
    procedure   SetStorage(aStorage:TRLGraphicStorage);
    // empilha o retângulo de corte
    procedure   PushClipRect(const aRect:TRect);
    // desempilha o retângulo de corte
    procedure   PopClipRect(var aRect:TRect);
    // persistência de símbolos
    function    GetOrientation: TRLMetaOrientation;
    procedure   SetOrientation(const Value: TRLMetaOrientation);
    function    GetPaperHeight: double;
    function    GetPaperWidth: double;
    procedure   SetPaperHeight(const Value: double);
    procedure   SetPaperWidth(const Value: double);
    function    GetOrientedPaperHeight: double;
    function    GetOrientedPaperWidth: double;
    function    GetOrientedHeight: integer;
    function    GetOrientedWidth: integer;
    procedure   BoundTextCursor;
  public
    constructor Create;
    destructor  Destroy; override;

    {@method SaveToFile - Salva os dados da página em um arquivo. :/}
    procedure   SaveToFile(const aFileName:string);

    {@method LoadFromFile - Restaura os dados da página de um arquivo. :/}
    procedure   LoadFromFile(const aFileName:string);

    {@method SaveToStream - Salva os dados da página em uma stream. :/}
    procedure   SaveToStream(aStream:TStream);

    {@method LoadFromStream - Carrega os dados da página de uma stream. :/}
    procedure   LoadFromStream(aStream:TStream);

    {@method FindFreeRow - Retorna a altura neutra mais próxima da coordenada informada, aonde nenhum texto é cortado. :/}
    function    FindFreeRow(aNearRow:integer; var aRow:integer):boolean;

    {@method TextWidth - Retorna a largura do texto de acordo com a fonte atual. :/}
    function    TextWidth(const aText:string):integer;
    
    {@method TextHeight - Retorna a altura do texto de acordo com a fonte atual. :/}
    function    TextHeight(const aText:string):integer;

    {@method MoveTo - Posiciona o cursor de desenho e escrita. :/}
    procedure   MoveTo(aX,aY:integer);

    {@method LineTo - Traça uma linha reta ligando a posição atual do cursor às coordenadas passadas. :/}
    procedure   LineTo(aX,aY:integer);
    
    {@method Rectangle - Desenha um retângulo. :}
    procedure   Rectangle(aLeft,aTop,aRight,aBottom:integer); overload;
    procedure   Rectangle(const aRect:TRect); overload;
    {/@method}

    {@method Ellipse - Desenha uma ellipse. :}
    procedure   Ellipse(aX1,aY1,aX2,aY2:integer); overload;
    procedure   Ellipse(const aRect:TRect); overload;
    {/@method}

    {@method Polygon - Desenha um polígono. :/}
    procedure   Polygon(const aPoints:array of TPoint);
    
    {@method Polyline - Desenha uma série de linhas ligando os pontos passados. :/}
    procedure   Polyline(const aPoints:array of TPoint);

    {@method Write - Escreve um texto na posição atual do cursor. :/}
    procedure   Write(const aText:string);

    {@method WriteLn - Escreve um texto na posição atual do cursor e salta para a linha seguinte. :/}
    procedure   WriteLn(const aText:string);

    {@method TextOut - Escreve um texto na posição informada. :}
    procedure   TextOut(aLeft,aTop:integer; const aText:string);
    procedure   TextOutEx(aLeft,aTop:integer; const aText:string; aTextFlags:TRLMetaTextFlags);
    {/@method}

    {@method TextRect - Escreve um texto delimitado pelo retângulo informado. :}
    procedure   TextRect(const aRect:TRect; aLeft,aTop:integer; const aText:string);
    procedure   TextRectEx(const aRect:TRect; aLeft,aTop:integer; const aText:string; aAlignment:TRLMetaTextAlignment; aLayout:TRLMetaTextLayout; aTextFlags:TRLMetaTextFlags);
    {/@method}

    {@method FillRect - Preenche um retângulo com os padrões definidos na prop Brush. :/}
    procedure   FillRect(const aRect:TRect);
    
    {@method Draw - Desenha a imagem nas coordenadas indicadas mantendo seu tamanho e proporção. :}
    procedure   Draw(aX,aY:integer; aGraphic:TGraphic; aParity:boolean=false); overload;
    procedure   Draw(aX,aY:integer; aSurface:TRLGraphicSurface); overload;
    {/@method}

    {@method StretchDraw - Desenha uma imagem alterando características de modo a preencher todo o retângulo. :}
    procedure   StretchDraw(const aRect:TRect; aGraphic:TGraphic; aParity:boolean=false); overload;
    procedure   StretchDraw(const aRect:TRect; aSurface:TRLGraphicSurface); overload;
    {/@method}

    {@method ScaleDraw - Desenha uma imagem contida num retângulo respeitando suas proporções. :}
    procedure   ScaleDraw(const aRect:TRect; aGraphic:TGraphic; aCenter:boolean); overload;
    procedure   ScaleDraw(const aRect:TRect; aSurface:TRLGraphicSurface; aCenter:boolean); overload;
    {/@method}

    {@method ClipDraw - Desenha um corte de uma imagem de modo a interceptar o retângulo. :}
    procedure   ClipDraw(const aRect:TRect; aGraphic:TGraphic; aCenter:boolean); overload;
    procedure   ClipDraw(const aRect:TRect; aSurface:TRLGraphicSurface; aCenter:boolean); overload;
    {/@method}

    {@method CopyRect - Copia os objetos que interceptam o retângulo para uma outra superfície. :}
    procedure   CopyRect(const aDest:TRect; aCanvas:TCanvas; const aSource:TRect); overload;
    procedure   CopyRect(const aDest:TRect; aSurface:TRLGraphicSurface; const aSource:TRect); overload;
    {/@method}

    {@method SetClipRect - Determina um novo retângulo de corte para desenho e retorna a definição antiga. :/}
    procedure   SetClipRect(const aRect:TRect);

    {@method ResetClipRect - Anula o retângulo de corte para desenho. :/}
    procedure   ResetClipRect;

    {@method Open - Inicializa a superfície. :/}
    procedure   Open;

    {@method Close - Finaliza a superfície e apaga tudo o que foi feito. :/}
    procedure   Close;
    
    {@method Clear - Libera todos os objetos e fontes da página e reposiciona a caneta. :/}
    procedure   Clear;

    {@method PaintTo - Desenha a superfície em um Canvas com fator de escala definido pelas relações entre o retângulo
     passado e as dimensões da superfície. :/}
    procedure   PaintTo(aCanvas:TCanvas; aRect:TRect);

    {@method PageIndex - Retorna o índice da página na lista. :/}
    property    PageIndex:integer read fPageIndex;

    {@prop Opened - Indica se a superfície já foi aberta. :/}
    property    Opened   :boolean read fOpened;

    {@prop Modified - Indica se a superfície foi modificada. :/}
    property    Modified:boolean read fModified write fModified;

    {@prop Objects - Vetor de objetos da superfície. :/}
    property    Objects[aIndex:integer]:TRLGraphicObject read GetObjects;

    {@prop ObjectCount - Quantidade de objetos na superfície. :/}
    property    ObjectCount:integer read GetObjectCount;

    {@prop Brush - Padrão utilizado para preenchimentos. :/}
    property    Brush  :TBrush  read fBrush   write SetBrush;

    {@prop Pen - Padrão utilizado para linhas. :/}
    property    Pen    :TPen    read fPen     write SetPen;

    {@prop Font - Fonte padrão para escrita. :/}
    property    Font   :TFont   read fFont    write SetFont;

    {@prop Pixels - Matriz de pontos. :/}
    property    Pixels[X,Y:integer]:TColor read GetPixels write SetPixels;

    {@prop Width - Largura da superfície em pixels. :/}
    property    Width  :integer read fWidth   write fWidth;

    {@prop Height - Altura da superfície em pixels. :/}
    property    Height :integer read fHeight  write fHeight;

    {@prop Orientation - Orientação da superfície. :/}
    property    Orientation:TRLMetaOrientation read GetOrientation write SetOrientation;

    {@prop PaperWidth - Largura do papel em milímetros. :/}
    property    PaperWidth :double read GetPaperWidth  write SetPaperWidth;

    {@prop PaperHeight - Altura do papel em milímetros. :/}
    property    PaperHeight:double read GetPaperHeight write SetPaperHeight;

    {@prop OrientedPaperWidth - Largura do papel orientado para leitura em milímetros. :/}
    property    OrientedPaperWidth :double  read GetOrientedPaperWidth;

    {@prop OrientedPaperHeight - Altura do papel orientado para leitura em milímetros. :/}
    property    OrientedPaperHeight:double  read GetOrientedPaperHeight;

    {@prop OrientedWidth - Largura da superfície orientada para leitura em pixels. :/}
    property    OrientedWidth :integer read GetOrientedWidth;

    {@prop OrientedHeight - Altura da superfície orientada para leitura em pixels. :/}
    property    OrientedHeight:integer read GetOrientedHeight;

    {@prop PenPos - Posição atual do cursor gráfico. :/}
    property    PenPos :TPoint  read fPenPos  write fPenPos;

    {@prop WritePos - Posição atual do cursor para escrita de texto. @links Writeln, Write. :/}
    property    WritePos:TPoint  read fWritePos  write fWritePos;

    {@prop WordWrap - Determina a quebra automatica de linhas que não caibam dentro
    das margens. @links Writeln, Write, Margins. :/}
    property    WordWrap:boolean read fWordWrap  write fWordWrap;

    {@prop Margins - Margens de texto para uso com os métodos: Write e WriteLn. :/}
    property    Margins:TRect   read fMargins write fMargins;

    {@prop ClipRect - Retângulo de corte atual. :/}
    property    ClipRect:TRect  read fClipRect;

    {@prop Tag - Inteiro associado à superfície.
     Não tem significado para o sistema e pode ser livremente utilizado pelo usuário.
     Nota: Esta prop não é armazenada em disco. :/}
    property    Tag    :integer read fTag     write fTag;

    {@prop Fonts - Lista de fontes utilizadas. :/}
    property    Fonts:TStrings read fFonts;

    {@prop GeneratorId - Identifica o objeto gerador para os próximos elementos gráficos. :/}
    property    GeneratorId:integer read fGeneratorId write fGeneratorId;

    {@prop Storage - Referência para o estoque ao qual pertence à superfície gráfica. :/}
    property    Storage:TRLGraphicStorage read fStorage;

    {@prop Macros - Lista de símbolos para tradução em tempo de visualização ou impressão. :/}
    property    Macros:TStrings read fMacros;
  end;
  {/@class}

  {@class TRLGraphicObject - Objeto primitivo de desenho. }
  TRLGraphicObject=class
  private
    fSurface       :TRLGraphicSurface;
    //
    fBoundsRect    :TRLMetaRect;
    fGroupId       :integer;
    fGeneratorId   :integer;
    fTag           :integer;
  public
    constructor Create(aSurface:TRLGraphicSurface); virtual;
    destructor  Destroy; override;

    {@method SaveToStream - Salva os dados do objeto em uma stream. :/}
    procedure   SaveToStream(aStream:TStream); dynamic;

    {@method LoadFromStream - Carrega os dados do objeto de uma stream. :/}
    procedure   LoadFromStream(aStream:TStream); dynamic;

    {@method Clone - Instancia um novo objeto com características semelhantes. :/}
    function    Clone(aSurface:TRLGraphicSurface):TRLGraphicObject;

    {@method Assign - Assume as características de um outro objeto. :/}
    procedure   Assign(aObject:TRLGraphicObject); dynamic;
    
    {@method Offset - Desloca as coordenadas do objeto. :/}
    procedure   Offset(aXDesloc,aYDesloc:integer); dynamic;

    {@method Inflate - Redimensiona o controle de acordo com os fatores passados. :/}
    procedure   Inflate(aXFactor,aYFactor:double); dynamic;

    {@prop BoundsRect - Dimensões do objeto. :/}
    property    BoundsRect:TRLMetaRect read fBoundsRect write fBoundsRect;

    {@prop GroupId - Índice de grupo. Os elementos gráficos gerados na mesma operação têm o mesmo GroupId. :/}
    property    GroupId:integer read fGroupId write fGroupId;

    {@prop GeneratorId - Identifica o objeto gerador do elemento gráfico. :/}
    property    GeneratorId:integer read fGeneratorId write fGeneratorId;

    {@prop Tag - Inteiro associado ao objeto.
     Não tem significado para o sistema e pode ser livremente utilizado pelo usuário.
     Nota: Esta prop não é armazenada em disco. :/}
    property    Tag       :integer     read fTag        write fTag;

    {@prop Surface - Referência para a superfície gráfica à qual pertence o objeto. :/}
    property    Surface   :TRLGraphicSurface read fSurface;
  end;
  {/@class}

  { TRLPixelObject }

  TRLPixelObject=class(TRLGraphicObject)
  private
    fColor:TRLMetaColor;
  public
    constructor Create(aSurface:TRLGraphicSurface); override;
    //
    procedure   SaveToStream(aStream:TStream); override;
    procedure   LoadFromStream(aStream:TStream); override;
    //
    procedure   Assign(aObject: TRLGraphicObject); override;
    //
    property    Color:TRLMetaColor read fColor write fColor;
  end;

  { TRLLineObject }

  TRLLineObject=class(TRLGraphicObject)
  private
    fFromPoint:TRLMetaPoint;
    fToPoint  :TRLMetaPoint;
    fPen      :TRLMetaPen;
    fBrush    :TRLMetaBrush;
    //
    procedure   SetPen(Value:TRLMetaPen);
    procedure   SetBrush(Value:TRLMetaBrush);
  public
    constructor Create(aSurface:TRLGraphicSurface); override;
    destructor  Destroy; override;
    //
    procedure   SaveToStream(aStream:TStream); override;
    procedure   LoadFromStream(aStream:TStream); override;
    //
    procedure   Assign(aObject: TRLGraphicObject); override;
    procedure   Offset(aXDesloc, aYDesloc: integer); override;
    procedure   Inflate(aXFactor,aYFactor:double); override;
    //
    property    FromPoint:TRLMetaPoint read fFromPoint write fFromPoint;
    property    ToPoint  :TRLMetaPoint read fToPoint   write fToPoint;
    property    Pen      :TRLMetaPen   read fPen       write SetPen;
    property    Brush    :TRLMetaBrush read fBrush     write SetBrush;
  end;

  { TRLRectangleObject }

  TRLRectangleObject=class(TRLGraphicObject)
  private
    fPen  :TRLMetaPen;
    fBrush:TRLMetaBrush;
    //
    procedure   SetPen(Value:TRLMetaPen);
    procedure   SetBrush(Value:TRLMetaBrush);
  public
    constructor Create(aSurface:TRLGraphicSurface); override;
    destructor  Destroy; override;
    //
    procedure   SaveToStream(aStream:TStream); override;
    procedure   LoadFromStream(aStream:TStream); override;
    //
    procedure   Assign(aObject: TRLGraphicObject); override;
    procedure   Inflate(aXFactor,aYFactor:double); override;
    //
    property    Pen  :TRLMetaPen   read fPen   write SetPen;
    property    Brush:TRLMetaBrush read fBrush write SetBrush;
  end;

  { TRLTextObject }

  TRLTextObject=class(TRLGraphicObject)
  private
    fBrush    :TRLMetaBrush;
    fFont     :TRLMetaFont;
    fText     :string;
    fOrigin   :TRLMetaPoint;
    fAlignment:TRLMetaTextAlignment;
    fLayout   :TRLMetaTextLayout;
    fTextFlags:TRLMetaTextFlags;
    //
    procedure   TranslateMacros(var aText:string);
    //
    procedure   SetBrush(Value:TRLMetaBrush);
    procedure   SetFont(Value:TRLMetaFont);
    function    GetDisplayText:string;
  public
    constructor Create(aSurface:TRLGraphicSurface); override;
    destructor  Destroy; override;
    //
    procedure   SaveToStream(aStream:TStream); override;
    procedure   LoadFromStream(aStream:TStream); override;
    //
    procedure   Assign(aObject: TRLGraphicObject); override;
    procedure   Offset(aXDesloc, aYDesloc: integer); override;
    procedure   Inflate(aXFactor,aYFactor:double); override;
    //                  
    property    Alignment:TRLMetaTextAlignment read fAlignment  write fAlignment;
    property    Brush    :TRLMetaBrush         read fBrush      write SetBrush;
    property    Font     :TRLMetaFont          read fFont       write SetFont;
    property    Layout   :TRLMetaTextLayout    read fLayout     write fLayout;
    property    Origin   :TRLMetaPoint         read fOrigin     write fOrigin;
    property    Text     :string               read fText       write fText;
    property    TextFlags:TRLMetaTextFlags     read fTextFlags  write fTextFlags;
    //
    property    DisplayText:string read GetDisplayText;
  end;

  { TRLFillRectObject }

  TRLFillRectObject=class(TRLGraphicObject)
  private
    fBrush:TRLMetaBrush;
    //
    procedure   SetBrush(Value:TRLMetaBrush);
  public
    constructor Create(aSurface:TRLGraphicSurface); override;
    destructor  Destroy; override;
    //
    procedure   SaveToStream(aStream:TStream); override;
    procedure   LoadFromStream(aStream:TStream); override;
    //
    procedure   Assign(aObject: TRLGraphicObject); override;
    //
    property    Brush:TRLMetaBrush read fBrush write SetBrush;
  end;

  { TRLEllipseObject }

  TRLEllipseObject=class(TRLGraphicObject)
  private
    fPen  :TRLMetaPen;
    fBrush:TRLMetaBrush;
    //
    procedure   SetPen(Value:TRLMetaPen);
    procedure   SetBrush(Value:TRLMetaBrush);
  public
    constructor Create(aSurface:TRLGraphicSurface); override;
    destructor  Destroy; override;
    //
    procedure   SaveToStream(aStream:TStream); override;
    procedure   LoadFromStream(aStream:TStream); override;
    //
    procedure   Assign(aObject: TRLGraphicObject); override;
    procedure   Inflate(aXFactor,aYFactor:double); override;
    //
    property    Pen  :TRLMetaPen   read fPen   write SetPen;
    property    Brush:TRLMetaBrush read fBrush write SetBrush;
  end;

  { TRLPolygonObject }

  TRLPolygonObject=class(TRLGraphicObject)
  private
    fPen   :TRLMetaPen;
    fBrush :TRLMetaBrush;
    fPoints:TRLMetaPointArray;
    //
    procedure   SetPen(Value:TRLMetaPen);
    procedure   SetBrush(Value:TRLMetaBrush);
  public
    constructor Create(aSurface:TRLGraphicSurface); override;
    destructor  Destroy; override;
    //
    procedure   SaveToStream(aStream:TStream); override;
    procedure   LoadFromStream(aStream:TStream); override;
    //
    procedure   Assign(aObject: TRLGraphicObject); override;
    procedure   Offset(aXDesloc, aYDesloc: integer); override;
    procedure   Inflate(aXFactor,aYFactor:double); override;
    //
    property    Pen   :TRLMetaPen      read fPen    write SetPen;
    property    Brush :TRLMetaBrush    read fBrush  write SetBrush;
    property    Points:TRLMetaPointArray read fPoints write fPoints;
  end;

  { TRLPolylineObject }

  TRLPolylineObject=class(TRLGraphicObject)
  private
    fPen   :TRLMetaPen;
    fPoints:TRLMetaPointArray;
    //
    procedure   SetPen(Value:TRLMetaPen);
  public
    constructor Create(aSurface:TRLGraphicSurface); override;
    destructor  Destroy; override;
    //
    procedure   SaveToStream(aStream:TStream); override;
    procedure   LoadFromStream(aStream:TStream); override;
    //
    procedure   Assign(aObject: TRLGraphicObject); override;
    procedure   Offset(aXDesloc, aYDesloc: integer); override;
    procedure   Inflate(aXFactor,aYFactor:double); override;
    //
    property    Pen   :TRLMetaPen      read fPen    write SetPen;
    property    Points:TRLMetaPointArray read fPoints write fPoints;
  end;

  { TRLImageObject }

  TRLImageObject=class(TRLGraphicObject)
  private
    fData  :string;
    fParity:boolean;
    //
  public
    constructor Create(aSurface:TRLGraphicSurface); override;
    //
    procedure   SaveToStream(aStream:TStream); override;
    procedure   LoadFromStream(aStream:TStream); override;
    //
    procedure   Assign(aObject:TRLGraphicObject); override;
    //
    property    Data  :string  read fData   write fData;
    property    Parity:boolean read fParity write fParity;
  end;

  { TRLSetClipRectObject }

  TRLSetClipRectObject=class(TRLGraphicObject)
  public
  end;

  { TRLResetClipRectObject }

  TRLResetClipRectObject=class(TRLGraphicObject)
  public
  end;

function  GetPointsBounds(const aPoints:TRLMetaPointArray):TRect;
function  FloatToPtStr(f:double):string;
function  PtStrToFloat(const s:string; def:double=0):double;
function  ClipGraphic(aGraphic:TGraphic; var aRect:TRect; const aCenter:boolean):TBitmap;
function  ClipSurface(aSurface:TRLGraphicSurface; var aRect:TRect; const aCenter:boolean):TRLGraphicSurface;

function  MetaPoint(X,Y:integer):TRLMetaPoint;
function  MetaRect(aLeft,aTop,aRight,aBottom:integer):TRLMetaRect;
function  MetaColor(aRed,aGreen,aBlue:byte):TRLMetaColor;

{@function NewGroupId - Cria um identificador para um novo grupo de elementos gráficos.
 @links TRLGraphicObject.GroupId, TRLGraphicSurface.GeneratorId, TRLGraphicObject.GeneratorId. :/}
function NewGroupId:integer;

{/@unit}

implementation

uses RLMetaVCL, Math, LCLProc, LCLIntf;

{ UTILS }

var
  CurrentGroupId:integer=0;

function NewGroupId:integer;
begin
  Inc(CurrentGroupId);
  Result:=CurrentGroupId;
end;

// retorna dimensões de uma coleção de pontos
function GetPointsBounds(const aPoints:TRLMetaPointArray):TRect;
var
  i:integer;
  p:TRLMetaPoint;
begin
  for i:=0 to High(aPoints) do
  begin
    p:=aPoints[i];
    if i=0 then
    begin
      Result.Left  :=p.X;
      Result.Top   :=p.Y;
      Result.Right :=p.X;
      Result.Bottom:=p.Y;
    end
    else
    begin
      Result.Left  :=Min(p.X,Result.Left);
      Result.Top   :=Min(p.Y,Result.Top);
      Result.Right :=Max(p.X,Result.Right);
      Result.Bottom:=Max(p.Y,Result.Bottom);
    end;
  end;
  Dec(Result.Left);
  Dec(Result.Top);
  Inc(Result.Right);
  Inc(Result.Bottom);
end;

// de float para string com ponto como separador decimal
function FloatToPtStr(f:double):string;
begin
  Str(f:0:4,Result);
end;

// de string com ponto como separador decimal para float
function PtStrToFloat(const s:string; def:double=0):double;
var
  e:integer;
begin
  Val(s,Result,e);
  if e<>0 then
    Result:=def;
end;

// retorna um bitmap a partir de um pedaço recortado do gráfico aGraphic que caiba em aRect
function ClipGraphic(aGraphic:TGraphic; var aRect:TRect; const aCenter:boolean):TBitmap;
var
  graphicrect:TRect;
begin
  // cria um retângulo com o tamanho natural do gráfico na posição de corte
  graphicrect:=Rect(aRect.Left,aRect.Top,aRect.Left+aGraphic.Width,aRect.Top+aGraphic.Height);
  // centraliza os dois retângulos
  if aCenter then
    OffsetRect(graphicrect,((aRect.Right-aRect.Left)-(graphicrect.Right-graphicrect.Left)) div 2,
                           ((aRect.Bottom-aRect.Top)-(graphicrect.Bottom-graphicrect.Top)) div 2);
  // faz a interseção dos dois retângulos em aRect
  if IntersectRect(aRect,aRect,graphicrect) then
  begin
    // projeta um bitmap do tamanho de aRect e de qualidade compatível com aGraphic
    Result:=NewBitmap(aRect.Right-aRect.Left,aRect.Bottom-aRect.Top);
    // transfere imagem para o novo bitmap
    Result.Canvas.Draw(graphicrect.Left-aRect.Left,graphicrect.Top-aRect.Top,aGraphic);
  end
  // se não houver interseção...
  else
    Result:=nil;
end;

// retorna um bitmap a partir de um pedaço recortado do gráfico aGraphic que caiba em aRect
function ClipSurface(aSurface:TRLGraphicSurface; var aRect:TRect; const aCenter:boolean):TRLGraphicSurface;
var
  graphicrect:TRect;
begin
  // cria um retângulo com o tamanho natural do gráfico na posição de corte
  graphicrect:=Rect(aRect.Left,aRect.Top,aRect.Left+aSurface.Width,aRect.Top+aSurface.Height);
  // centraliza os dois retângulos
  if aCenter then
    OffsetRect(graphicrect,((aRect.Right-aRect.Left)-(graphicrect.Right-graphicrect.Left)) div 2,
                           ((aRect.Bottom-aRect.Top)-(graphicrect.Bottom-graphicrect.Top)) div 2);
  // faz a interseção dos dois retângulos em aRect
  if IntersectRect(aRect,aRect,graphicrect) then
  begin
    // projeta um bitmap do tamanho de aRect e de qualidade compatível com aGraphic
    Result:=TRLGraphicSurface.Create;
    Result.Width :=aRect.Right-aRect.Left;
    Result.Height:=aRect.Bottom-aRect.Top;
    // transfere imagem para o novo bitmap
    Result.Draw(graphicrect.Left-aRect.Left,graphicrect.Top-aRect.Top,aSurface);
  end
  // se não houver interseção...
  else
    Result:=nil;
end;

function MetaPoint(X,Y:integer):TRLMetaPoint;
begin
  Result.X:=X;
  Result.Y:=Y;
end;

function MetaRect(aLeft,aTop,aRight,aBottom:integer):TRLMetaRect;
begin
  Result.Left  :=aLeft;
  Result.Top   :=aTop;
  Result.Right :=aRight;
  Result.Bottom:=aBottom;
end;

function MetaColor(aRed,aGreen,aBlue:byte):TRLMetaColor;
begin
  Result.Red  :=aRed;
  Result.Green:=aGreen;
  Result.Blue :=aBlue;
end;

{ Compatibility }

const
  // TGraphicKind
  gkPixel      =0;
  gkLine       =1;
  gkRectangle  =2;
  gkTextOut    =3;
  gkTextRect   =4;
  gkFillRect   =5;
  gkStretchDraw=6;
  gkDraw       =7;
  gkEllipse    =8;
  gkPolygon    =9;
  gkPolyline   =10;
  gkCutBegin   =11;
  gkCutEnd     =12;
  // TImageKind
  ikBitmap  =0;
  ikJPeg    =1;
  ikIcon    =2;
  ikMetafile=3;
type
  TGraphicKind      =byte;
  TImageKind        =byte;
  TTextAlignmentType=byte;
  TPenRecord=record
    Color:TColor;
    Mode :TPenMode;
    Style:TPenStyle;
    Width:integer;
  end;
  TBrushRecord=record
    Color:TColor;
    Style:TBrushStyle;
  end;
  TFontRecord=record
    Color        :TColor;
    Height       :integer;
    Pitch        :TFontPitch;
    PixelsPerInch:integer;
    Size         :integer;
    Style        :TFontStyles;
    Charset      :TFontCharset;
    Angle        :double;
    NameId       :integer;
  end;
  TGraphicFileRecord=record
    X1,Y1,X2,Y2:integer;
    X,Y        :integer;
    Tag        :integer;
    Kind       :TGraphicKind;
    Color      :TColor;
    HasPen     :boolean;
    Pen        :TPenRecord;
    HasBrush   :boolean;
    Brush      :TBrushRecord;
    HasFont    :boolean;
    Font       :TFontRecord;
    Text       :integer;
    Alignment  :TTextAlignmentType;
    AutoSize   :boolean;
  end;
  
  TPointArray=array of TPoint;

procedure UpgradePage(aStorage:TRLGraphicStorage; aInput,aOutput:TStream);
var
  surface:TRLGraphicSurface;
  texts  :TStringList;
  count  :integer;
  len    :integer;
  i      :integer;
  s      :string;
  rec    :TGraphicFileRecord;
  pgraph :TGraphic;
  cutlist:array of TRect;
  cutrect:TRect;
  cutlen :integer;
  cutsize:integer;
  function StrToGraphic(const aStr:string; aImageKind:TImageKind):TGraphic;
  var
    s:TStringStream;
  begin
    s:=TStringStream.Create(aStr);
    try
      case aImageKind of
        ikBitmap: Result:=NewBitmap;
        ikIcon  : Result:=TIcon.Create;
      else
        Result:=nil;
      end;
      if Assigned(Result) then
      begin
        s.Position:=0;
        Result.LoadFromStream(s);
      end;
    finally
      s.free;
    end;
  end;
  function StrToPoints(const aStr:string):TPointArray;
  var
    q,i:integer;
  begin
    q:=Length(aStr) div SizeOf(TPoint);
    SetLength(Result,q);
    for i:=0 to q-1 do
      Move(aStr[i*SizeOf(TPoint)+1],Result[i],SizeOf(TPoint));
  end;
begin
  cutlen :=0;
  cutsize:=0;
  //
  surface:=TRLGraphicSurface.Create;
  try
    aInput.Read(surface.fWidth ,SizeOf(surface.fWidth));
    aInput.Read(surface.fHeight,SizeOf(surface.fHeight));
    surface.Orientation:=aStorage.Orientation;
    surface.PaperHeight:=aStorage.PaperHeight;
    surface.PaperWidth :=aStorage.PaperWidth;
    //
    cutrect:=Rect(0,0,surface.fWidth,surface.fHeight);
    // strings
    texts:=TStringList.Create;
    try
      aInput.Read(count,SizeOf(count));
      for i:=0 to count-1 do
      begin
        aInput.Read(len,SizeOf(len));
        SetLength(s,len);
        aInput.Read(s[1],len);
        texts.Add(s);
      end;
      // objects
      aInput.Read(count,SizeOf(count));
      for i:=1 to count do
      begin
        aInput.Read(rec,SizeOf(rec));

        case rec.Kind of
          gkPixel      : with TRLPixelObject.Create(surface) do
                         begin
                           BoundsRect:=ToMetaRect(Rect(rec.X1,rec.Y1,rec.X2,rec.Y2));
                           Color     :=ToMetaColor(rec.Color);
                         end;
          gkLine       : with TRLLineObject.Create(surface) do
                         begin
                           FromPoint :=ToMetaPoint(Point(rec.X1,rec.Y1));
                           ToPoint   :=ToMetaPoint(Point(rec.X2,rec.Y2));
                           BoundsRect:=ToMetaRect(Rect(Min(FromPoint.X,ToPoint.X),
                                                     Min(FromPoint.Y,ToPoint.Y),
                                                     Max(FromPoint.X,ToPoint.X),
                                                     Max(FromPoint.Y,ToPoint.Y)));
                           Pen.Color :=ToMetaColor(rec.Pen.Color);
                           Pen.Mode  :=ToMetaPenMode(rec.Pen.Mode);
                           Pen.Style :=ToMetaPenStyle(rec.Pen.Style);
                           Pen.Width :=rec.Pen.Width;
                         end;
          gkRectangle  : with TRLRectangleObject.Create(surface) do
                         begin
                           BoundsRect :=ToMetaRect(Rect(rec.X1,rec.Y1,rec.X2,rec.Y2));
                           Pen.Color  :=ToMetaColor(rec.Pen.Color);
                           Pen.Mode   :=ToMetaPenMode(rec.Pen.Mode);
                           Pen.Style  :=ToMetaPenStyle(rec.Pen.Style);
                           Pen.Width  :=rec.Pen.Width;
                           Brush.Color:=ToMetaColor(rec.Brush.Color);
                           Brush.Style:=ToMetaBrushStyle(rec.Brush.Style);
                         end;
          gkTextOut,
          gkTextRect   : with TRLTextObject.Create(surface) do
                         begin
                           BoundsRect        :=ToMetaRect(Rect(rec.X1,rec.Y1,rec.X2,rec.Y2));
                           Text              :=texts[rec.Text];
                           Origin            :=ToMetaPoint(Point(rec.X,rec.Y));
                           Alignment         :=rec.Alignment+1;
                           Layout            :=MetaTextLayoutTop;
                           if rec.AutoSize or (rec.Kind=gkTextOut) then
                             TextFlags:=TextFlags or MetaTextFlagAutoSize;
                           Brush.Color       :=ToMetaColor(rec.Brush.Color);
                           Brush.Style       :=ToMetaBrushStyle(rec.Brush.Style);
                           Font.PixelsPerInch:=ScreenPPI; //rec.Font.PixelsPerInch;
                           Font.Charset      :=ToMetaFontCharset(rec.Font.Charset);
                           Font.Color        :=ToMetaColor(rec.Font.Color);
                           Font.Height       :=rec.Font.Height;
                           Font.Name         :=texts[rec.Font.NameId];
                           Font.Pitch        :=ToMetaFontPitch(rec.Font.Pitch);
                           Font.Size         :=-Round(Font.Height*72/Font.PixelsPerInch);
                           Font.Style        :=ToMetaFontStyles(rec.Font.Style);
                         end;
          gkFillRect   : with TRLFillRectObject.Create(surface) do
                         begin
                           BoundsRect :=ToMetaRect(Rect(rec.X1,rec.Y1,rec.X2,rec.Y2));
                           Brush.Color:=ToMetaColor(rec.Brush.Color);
                           Brush.Style:=ToMetaBrushStyle(rec.Brush.Style);
                         end;
          gkStretchDraw: with TRLImageObject.Create(surface) do
                         begin
                           BoundsRect:=ToMetaRect(Rect(rec.X1,rec.Y1,rec.X2,rec.Y2));
                           Parity    :=false;
                           pgraph:=StrToGraphic(texts[rec.Text],rec.Tag);
                           try
                             Data:=ToMetaGraphic(pgraph);
                           finally
                             pgraph.free;
                           end;
                         end;
          gkDraw       : with TRLImageObject.Create(surface) do
                         begin
                           BoundsRect:=ToMetaRect(Rect(rec.X1,rec.Y1,rec.X2,rec.Y2));
                           Parity    :=false;
                           pgraph:=StrToGraphic(texts[rec.Text],rec.Tag);
                           try
                             Data:=ToMetaGraphic(pgraph);
                           finally
                             pgraph.free;
                           end;
                         end;
          gkEllipse    : with TRLEllipseObject.Create(surface) do
                         begin
                           BoundsRect :=ToMetaRect(Rect(rec.X1,rec.Y1,rec.X2,rec.Y2));
                           Pen.Color  :=ToMetaColor(rec.Pen.Color);
                           Pen.Mode   :=ToMetaPenMode(rec.Pen.Mode);
                           Pen.Style  :=ToMetaPenStyle(rec.Pen.Style);
                           Pen.Width  :=rec.Pen.Width;
                           Brush.Color:=ToMetaColor(rec.Brush.Color);
                           Brush.Style:=ToMetaBrushStyle(rec.Brush.Style);
                         end;
          gkPolygon    : with TRLPolygonObject.Create(surface) do
                         begin
                           Points     :=ToMetaPointArray(StrToPoints(texts[rec.Text]));
                           BoundsRect :=ToMetaRect(Rect(rec.X1,rec.Y1,rec.X2,rec.Y2));
                           Pen.Color  :=ToMetaColor(rec.Pen.Color);
                           Pen.Mode   :=ToMetaPenMode(rec.Pen.Mode);
                           Pen.Style  :=ToMetaPenStyle(rec.Pen.Style);
                           Pen.Width  :=rec.Pen.Width;
                           Brush.Color:=ToMetaColor(rec.Brush.Color);
                           Brush.Style:=ToMetaBrushStyle(rec.Brush.Style);
                         end;
          gkPolyline   : with TRLPolylineObject.Create(surface) do
                         begin
                           Points      :=ToMetaPointArray(StrToPoints(texts[rec.Text]));
                           BoundsRect  :=ToMetaRect(Rect(rec.X1,rec.Y1,rec.X2,rec.Y2));
                           Pen.Color   :=ToMetaColor(rec.Pen.Color);
                           Pen.Mode    :=ToMetaPenMode(rec.Pen.Mode);
                           Pen.Style   :=ToMetaPenStyle(rec.Pen.Style);
                           Pen.Width   :=rec.Pen.Width;
                         end;
          gkCutBegin   : with TRLSetClipRectObject.Create(surface) do
                         begin
                           Inc(cutlen);
                           if cutlen>cutsize then
                           begin
                             Inc(cutsize,1024);
                             SetLength(cutlist,cutsize);
                           end;
                           cutlist[cutlen-1]:=cutrect;
                           cutrect          :=Rect(rec.X1,rec.Y1,rec.X2,rec.Y2);
                           BoundsRect       :=ToMetaRect(cutrect);
                         end;
          gkCutEnd     : with TRLResetClipRectObject.Create(surface) do
                         begin
                           cutrect:=cutlist[cutlen-1];
                           Dec(cutlen);
                           BoundsRect:=ToMetaRect(cutrect);
                         end;
        end;
      end;
    finally
      texts.free;
    end;
    //
    surface.SaveToStream(aOutput);
  finally
    surface.free;
  end;
end;

procedure DowngradePage(aInput,aOutput:TStream);
var
  surface:TRLGraphicSurface;
  pen    :TRLMetaPen;
  brush  :TRLMetaBrush;
  font   :^TRLMetaFont;
  rec    :^TGraphicFileRecord;
  textid :integer;
  fontid :integer;
  obj    :TRLGraphicObject;
  texts  :TStringList;
  objcs  :TList;
  count  :integer;
  len    :integer;
  s      :string;
  i      :integer;
begin
  surface:=TRLGraphicSurface.Create;
  try
    surface.LoadFromStream(aInput);
    //
    texts:=TStringList.Create;
    objcs:=TList.Create;
    try
      for i:=0 to surface.ObjectCount-1 do
      begin
        obj:=surface.Objects[i];
        textid:=-1;
        fontid:=-1;

        new(rec);

        if obj is TRLTextObject then
        begin
          s:=TRLTextObject(obj).Text;
          textid:=texts.Add(s);
          s:=TRLTextObject(obj).Font.GetName;
          fontid:=texts.indexof(s);
          if fontid=-1 then
            fontid:=texts.Add(s);
        end
        else if obj is TRLImageObject then
        begin
          s:=TRLImageObject(obj).Data;
          delete(s,1,3); // retira prefixo
          textid:=texts.Add(s);
        end;

        if obj is TRLPixelObject then
          rec^.Kind:=gkPixel
        else if obj is TRLLineObject then
          rec^.Kind:=gkLine
        else if obj is TRLRectangleObject then
          rec^.Kind:=gkRectangle
        else if obj is TRLTextObject then
          if (TRLTextObject(obj).TextFlags and MetaTextFlagAutoSize)=MetaTextFlagAutoSize then
            rec^.Kind:=gkTextOut
          else
            rec^.Kind:=gkTextRect
        else if obj is TRLFillRectObject then
          rec^.Kind:=gkFillRect
        else if obj is TRLImageObject then
          rec^.Kind:=gkStretchDraw
        else if obj is TRLEllipseObject then
          rec^.Kind:=gkEllipse
        else if obj is TRLPolygonObject then
          rec^.Kind:=gkPolygon
        else if obj is TRLPolylineObject then
          rec^.Kind:=gkPolyline
        else if obj is TRLSetClipRectObject then
          rec^.Kind:=gkCutBegin
        else if obj is TRLResetClipRectObject then
          rec^.Kind:=gkCutEnd;

        if obj is TRLLineObject then
        begin
          rec^.X1:=TRLLineObject(obj).FromPoint.X;
          rec^.Y1:=TRLLineObject(obj).FromPoint.Y;
          rec^.X2:=TRLLineObject(obj).ToPoint.X;
          rec^.Y2:=TRLLineObject(obj).ToPoint.Y;
        end
        else
        begin
          rec^.X1:=TRLLineObject(obj).BoundsRect.Left;
          rec^.Y1:=TRLLineObject(obj).BoundsRect.Top;
          rec^.X2:=TRLLineObject(obj).BoundsRect.Right;
          rec^.Y2:=TRLLineObject(obj).BoundsRect.Bottom;
        end;
        if obj is TRLTextObject then
        begin
          rec^.X:=TRLTextObject(obj).Origin.X;
          rec^.Y:=TRLTextObject(obj).Origin.Y;
        end
        else
        begin
          rec^.X:=0;
          rec^.Y:=0;
        end;
        if obj is TRLImageObject then
        begin
          s:=TRLImageObject(obj).Data;
          if copy(s,1,3)='BMP' then
            rec^.Tag:=ord(ikBitmap)
          else if copy(s,1,3)='ICO' then
            rec^.Tag:=ord(ikIcon);
        end;
        rec^.Color:=0; // not used
        if obj is TRLTextObject then
        begin
          rec^.Alignment:=TRLTextObject(obj).Alignment-1;
          rec^.AutoSize :=((TRLTextObject(obj).TextFlags and MetaTextFlagAutoSize)=MetaTextFlagAutoSize);
        end;

        if obj is TRLLineObject then
          pen:=TRLLineObject(obj).Pen
        else if obj is TRLRectangleObject then
          pen:=TRLRectangleObject(obj).Pen
        else if obj is TRLEllipseObject then
          pen:=TRLEllipseObject(obj).Pen
        else if obj is TRLPolygonObject then
          pen:=TRLPolygonObject(obj).Pen
        else if obj is TRLPolylineObject then
          pen:=TRLPolylineObject(obj).Pen
        else
          pen:=nil;

        rec^.HasPen:=(pen<>nil);
        if rec^.HasPen then
        begin
          rec^.Pen.Color:=FromMetaColor(pen.Color);
          rec^.Pen.Mode :=FromMetaPenMode(pen.Mode);
          rec^.Pen.Style:=FromMetaPenStyle(pen.Style);
          rec^.Pen.Width:=pen.Width;
        end;

        if obj is TRLRectangleObject then
          brush:=TRLRectangleObject(obj).Brush
        else if obj is TRLTextObject then
          brush:=TRLTextObject(obj).Brush
        else if obj is TRLFillRectObject then
          brush:=TRLFillRectObject(obj).Brush
        else if obj is TRLEllipseObject then
          brush:=TRLEllipseObject(obj).Brush
        else if obj is TRLPolygonObject then
          brush:=TRLPolygonObject(obj).Brush
        else
          brush:=nil;

        rec^.HasBrush:=(brush<>nil);
        if rec^.HasBrush then
        begin
          rec^.Brush.Color:=FromMetaColor(brush.Color);
          rec^.Brush.Style:=FromMetaBrushStyle(brush.Style);
        end;

        if obj is TRLTextObject then
          font:=@TRLTextObject(obj).Font
        else
          font:=nil;
        rec^.HasFont:=(font<>nil);
        if rec^.HasFont then
        begin
          rec^.Font.NameId :=fontid;
          rec^.Font.Charset:=FromMetaFontCharset(font^.Charset);
          rec^.Font.Pitch  :=FromMetaFontPitch(font^.Pitch);
          rec^.Font.Height :=font^.Height;
          rec^.Font.Style  :=FromMetaFontStyles(font^.Style);
          rec^.Font.Color  :=FromMetaColor(font^.Color);
        end;

        rec^.Text:=textid;

        objcs.Add(rec);
      end;
      //
      aOutput.Write(surface.fWidth ,SizeOf(surface.fWidth));
      aOutput.Write(surface.fHeight,SizeOf(surface.fHeight));
      count:=texts.Count;
      aOutput.Write(count,SizeOf(count));
      for i:=0 to count-1 do
      begin
        s:=texts[i];
        len:=Length(s);
        aOutput.Write(len,SizeOf(len));
        aOutput.Write(s[1],len);
      end;
      count:=objcs.Count;
      aOutput.Write(count,SizeOf(count));
      for i:=0 to count-1 do
      begin
        rec:=objcs[i];
        aOutput.Write(rec^,SizeOf(TGraphicFileRecord));
        dispose(rec);
      end;
    finally
      texts.free;
      objcs.free;
    end;
  finally
    surface.free;
  end;
end;

{ TRLGraphicStorage }

constructor TRLGraphicStorage.Create(aOwner:TComponent); 
begin
  fPageCache      :=nil;
  fPageAllocation :=nil;
  fTempStream     :=nil;
  fTempFileName   :=emptystr;
  fFileVersion    :=4;
  fMacros         :=nil;
  fReferenceList  :=nil;
  fUpdateCalls    :=0;
  fLock           :=nil;
  fAboutToUpdate  :=False;
  fLockIndent     :=0;
  fCanceled       :=False;
  //
  fPageCache     :=TObjectList.Create;
  fPageAllocation:=TInt64List.Create;
  fMacros        :=TStringList.Create;
  fReferenceList :=TList.Create;
  fLock           :=TCriticalSection.Create;
  //
  inherited;
end;

destructor TRLGraphicStorage.Destroy;
begin
  //
  FreeObj(fLock);
  FreeObj(fReferenceList);
  FreeObj(fPageCache);
  FreeObj(fPageAllocation);
  FreeObj(fMacros);
  if Assigned(fTempStream) then
  begin
    fTempStream.Destroy;
    SysUtils.DeleteFile(fTempFileName);
    UnregisterTempFile(fTempFileName);
  end;
  inherited;
end;

procedure TRLGraphicStorage.Notification(aComponent:TComponent; Operation:TOperation);
begin
  inherited;
  //
  if Operation=opRemove then
    Unlink(aComponent);
end;

procedure TRLGraphicStorage.Link(aComponent:TComponent);
begin
  if (aComponent<>Owner) and (fReferenceList.IndexOf(aComponent)=-1) then
  begin
    fReferenceList.Add(aComponent);
    aComponent.FreeNotification(Self);
  end;
end;

procedure TRLGraphicStorage.Unlink(aComponent:TComponent=nil);
var
  i:integer;
begin
  if csDestroying in ComponentState then
    Exit;
  i:=fReferenceList.IndexOf(aComponent);
  if i<>-1 then
  begin
    fReferenceList.Delete(i);
    if not Assigned(Self.Owner) and (fReferenceList.Count=0) then
      Self.Free;
  end;
end;

procedure TRLGraphicStorage.Lock;
begin
  fLock.Enter;
  Inc(fLockIndent);
end;

procedure TRLGraphicStorage.Unlock;
begin
  Dec(fLockIndent);
  fLock.Leave;
end;

procedure TRLGraphicStorage.StorePage(aSurface:TRLGraphicSurface);
var
  size:integer;
  datapos,beginpos,endpos:int64;
begin
  Lock;
  try
    TempStreamNeeded;
    fTempStream.Position:=fTempStream.Size;
    // guarda a posição de gravação e reserva espaço para o tamanho
    beginpos:=fTempStream.Position;
    size    :=0;
    fTempStream.Write(size,SizeOf(size));
    datapos :=fTempStream.Position;
    // atualiza a lista de páginas atribuindo o novo offset
    if aSurface.fPageIndex=-1 then
      aSurface.fPageIndex:=fPageAllocation.Add(beginpos)
    else
      fPageAllocation[aSurface.fPageIndex]:=beginpos;
    // salva a página em disco
    aSurface.SaveToStream(fTempStream);
    // atualiza o tamanho no início da gravação e retorna o cursor para o fim do arquivo
    endpos:=fTempStream.Position;
    size  :=endpos-datapos;
    fTempStream.Position:=beginpos;
    fTempStream.Write(size,SizeOf(size));
    fTempStream.Position:=endpos;
    aSurface.Modified:=False;
  finally
    Unlock;
  end;
end;

procedure TRLGraphicStorage.PrepareUpdate;
begin
  Lock;
  try
    fAboutToUpdate:=True;
    fCanceled     :=False; 
  finally
    Unlock;
  end;
end;

procedure TRLGraphicStorage.BeginUpdate;
begin
  Lock;
  try
    Inc(fUpdateCalls);
    fAboutToUpdate:=False;
  finally
    Unlock;
  end;
end;

procedure TRLGraphicStorage.EndUpdate;
begin
  Lock;
  try
    Dec(fUpdateCalls);
  finally
    Unlock;
  end;
end;

function TRLGraphicStorage.Busy: boolean;
begin
  Lock;
  try
    Result:=(fUpdateCalls>0) or fAboutToUpdate;
  finally
    Unlock;
  end;
end;

procedure TRLGraphicStorage.Cancel;
begin
  Lock;
  try
    if Busy then
      fCanceled:=True; 
  finally
    Unlock;
  end;
end;

function TRLGraphicStorage.WaitPage(aPageIndex:integer):TRLGraphicSurface;
label
  TryAgain;
var
  MustWait:boolean;
begin
  TryAgain:
  Lock;
  try
    Result:=GetPages(aPageIndex);
    // espera se ainda estiver em processamento e se a preparação não tiver sido cancelada
    MustWait:=(Result=nil) and Busy and not Canceled;
  finally
    Unlock;
  end;
  if MustWait then
  begin
    // dedica um tempo ao processamento paralelo enquanto aguarda a página ficar pronta
{$ifdef DELPHI5}
    Sleep(100);
{$else}
{$ifdef DELPHI6}
    CheckSynchronize;
{$else}
{$ifdef CPP6}
    CheckSynchronize;
{$else}
{$ifdef KYLIX1}
    CheckSynchronize;
{$else}
{$ifdef KYLIX2}
    CheckSynchronize;
{$else}
    CheckSynchronize(100);
{$endif}
{$endif}
{$endif}
{$endif}
{$endif}
    goto TryAgain;
  end;
end;

procedure TRLGraphicStorage.RetrievePage(aSurface:TRLGraphicSurface);
var
  size:integer;
begin
  Lock;
  try
    fTempStream.Position:=fPageAllocation[aSurface.fPageIndex];
    fTempStream.Read(size,SizeOf(size));
    aSurface.LoadFromStream(fTempStream);
  finally
    Unlock;
  end;
end;

procedure TRLGraphicStorage.Add(aSurface:TRLGraphicSurface);
begin
  Lock;
  try
    aSurface.SetStorage(Self);
    StorePage(aSurface);
    AddToCache(aSurface);
  finally
    Unlock;
  end;
end;

function TRLGraphicStorage.New(PaperSize:TRLPaperSize):TRLGraphicSurface;
var
  Info:TRLPaperInfo;
begin
  Lock;
  try
    Info  :=PaperInfo[PaperSize];
    Result:=New(Info.Width,Info.Height);
  finally
    Unlock;
  end;
end;

function TRLGraphicStorage.New(PaperWidth,PaperHeight:double):TRLGraphicSurface;
begin
  // notifica antes, para a página anterior
  Lock;
  try
    Result:=TRLGraphicSurface.Create;
    Result.Width      :=Round(ScreenPPI*PaperWidth/InchAsMM);
    Result.Height     :=Round(ScreenPPI*PaperHeight/InchAsMM);
    Result.Orientation:=MetaOrientationPortrait;
    Result.PaperWidth :=PaperWidth;
    Result.PaperHeight:=PaperHeight;
    Result.Open;
    Result.SetStorage(Self);
    StorePage(Result);
    AddToCache(Result);
  finally
    Unlock;
  end;
end;

procedure TRLGraphicStorage.Clear;
begin
  Lock;
  try
    if Assigned(fTempStream) then
      fTempStream.Size:=0;
    fPageAllocation.Clear;
    fPageCache.Clear;
    fMacros.Clear;
  finally
    Unlock;
  end;
end;

procedure TRLGraphicStorage.AddToCache(aSurface:TRLGraphicSurface);
var
  s:TRLGraphicSurface;
begin
  Lock;
  try
    // limite de dez páginas em cachê
    if fPageCache.Count>=MAXPAGECACHE then
    begin
      s:=TRLGraphicSurface(fPageCache[0]);
      if s.Modified then
        StorePage(s);
      fPageCache.Remove(s);
    end;
    fPageCache.Add(aSurface);
  finally
    Unlock;
  end;
end;

function TRLGraphicStorage.GetFromCache(aPageIndex:integer):TRLGraphicSurface;
var
  i:integer;
begin
  Lock;
  try
    Result:=nil;
    if (aPageIndex>=0) and Assigned(fPageCache) then
    begin
      i:=0;
      while (i<fPageCache.Count) and (TRLGraphicSurface(fPageCache[i]).PageIndex<>aPageIndex) do
        Inc(i);
      if i<fPageCache.Count then
        Result:=TRLGraphicSurface(fPageCache[i]);
    end;
  finally
    Unlock;
  end;
end;

procedure TRLGraphicStorage.FlushCache;
var
  s:TRLGraphicSurface;
  i:integer;
begin
  Lock;
  try
    for i:=0 to fPageCache.Count-1 do
    begin
      s:=TRLGraphicSurface(fPageCache[i]);
      if s.Modified then
        StorePage(s);
    end;
  finally
    Unlock;
  end;
end;

function TRLGraphicStorage.LoadPageFromDisk(aPageIndex:integer):TRLGraphicSurface;
begin
  Lock;
  try
    if (aPageIndex>=0) and (aPageIndex<fPageAllocation.Count) then
    begin
      Result:=TRLGraphicSurface.Create;
      try
        Result.SetStorage(Self);
        Result.fPageIndex:=aPageIndex;
        RetrievePage(Result);
      except
        Result.free;
        Result:=nil;
      end;
    end
    else
      Result:=nil;
  finally
    Unlock;
  end;
end;

function TRLGraphicStorage.GetPages(aPageIndex:integer):TRLGraphicSurface;
begin
  Lock;
  try
    Result:=GetFromCache(aPageIndex);
    if Result=nil then
    begin
      Result:=LoadPageFromDisk(aPageIndex);
      if Result<>nil then
        AddToCache(Result);
    end;
  finally
    Unlock;
  end;
end;

function TRLGraphicStorage.GetPageCount: integer;
begin
  Lock;
  try
    Result:=fPageAllocation.Count;
  finally
    Unlock;
  end;
end;

procedure TRLGraphicStorage.TempStreamNeeded;
begin
  Lock;
  try
    if not Assigned(fTempStream) then
    begin
      fTempFileName:=GetTempFileName;
      RegisterTempFile(fTempFileName);
      fTempStream  :=TFileStream.Create(fTempFileName,fmCreate);
    end;
  finally
    Unlock;
  end;
end;

procedure TRLGraphicStorage.SaveToFile(const aFileName: string);
var
  s:TFileStream;
begin
  s:=TFileStream.Create(aFileName,fmCreate);
  try
    SaveToStream(s);
  finally
    s.free;
  end;
end;

procedure TRLGraphicStorage.LoadFromFile(const aFileName: string);
var
  s:TFileStream;
begin
  s:=TFileStream.Create(aFileName,fmOpenRead+fmShareDenyWrite);
  try
    LoadFromStream(s);
  finally
    s.free;
  end;
end;

const
  MaxFileHeader     =20;
  FileHeaderVersion1='Fortes Metafile'#26;
  FileHeaderVersion2='RPF2'#26;
  FileHeaderVersion3='RLGraphicStorage3'#26;
  FileHeaderVersion4='RPF4'#26;

procedure TRLGraphicStorage.SaveToStream(aStream: TStream);
  procedure SaveHeaderToStream(aStream: TStream);
  var
    data:string;
  begin
    case fFileVersion of
      1: data:=FileHeaderVersion1;
      2: data:=FileHeaderVersion2;
      3: data:=FileHeaderVersion3;
      4: data:=FileHeaderVersion4;
    else
      raise Exception.Create(Ls_File_version + '"'+IntToStr(fFileVersion)+'"!');
    end;
    aStream.Write(data[1],Length(data));
  end;
  procedure SaveMacrosToStream(aStream: TStream);
  var
    count,len,i,p:integer;
    ln,name,value:string;
  begin
    // grava a quantidade de macros
    count:=fMacros.Count;
    aStream.Write(count,SizeOf(count));
    // grava símbolos
    for i:=0 to count-1 do
    begin
      ln:=fMacros[i];
      // downgrade
      p:=Pos('=',ln);
      if p<>0 then
      begin
        name :=Trim(Copy(ln,1,p-1));
        value:=Trim(Copy(ln,p+1,Length(ln)));
        if (fFileVersion<3) and SameText(name,'Orientation') then
          ln:=name+'='+IntToStr(StrToIntDef(value,1)-1);
      end;
      // grava length + nome
      len:=Length(ln);
      aStream.Write(len,SizeOf(len));
      aStream.Write(ln[1],len);
    end;
  end;
  function SavePageToStream(aStream:TStream; aPageIndex:integer):Int64;
  var
    size:integer;
  begin
    // lê o tamanho da página armazenada
    fTempStream.Position:=fPageAllocation[aPageIndex];
    fTempStream.Read(size,SizeOf(size));
    // guarda posição inicial de gravação da nova stream
    Result:=aStream.Position;
    // versões >2 gravam o tamanho da página
    if fFileVersion>2 then
      aStream.Write(size,SizeOf(size));
    // grava página no novo stream
    if fFileVersion>2 then
      aStream.CopyFrom(fTempStream,size)
    else
      DowngradePage(fTempStream,aStream);  
  end;
  procedure SavePagesToStream(aStream: TStream);
  var
    count,i,aux32:integer;
    page0,savedpos,pagetbl:Int64;
    offsets:array of Int64;
  begin
    // grava a quantidade de páginas
    count:=fPageAllocation.Count;
    aStream.Write(count,SizeOf(count));
    // guarda posição inicial de gravação da tabela de páginas
    pagetbl:=aStream.Position;
    // reserva espaço para os offsets
    SetLength(offsets,count);
    for i:=0 to count-1 do
      if fFileVersion<4 then
      begin
        aux32:=offsets[i];
        aStream.Write(aux32,SizeOf(aux32));
      end
      else  
        aStream.Write(offsets[i],SizeOf(offsets[i]));
    // grava páginas e memoriza os offsets
    page0:=aStream.Position;
    for i:=0 to fPageAllocation.Count-1 do
      offsets[i]:=SavePageToStream(aStream,i);
    // guarda posição atual, grava offsets e restaura posição
    savedpos:=aStream.Position;
    aStream.Position:=pagetbl;
    for i:=0 to count-1 do
    begin
      // nas versões <=2 o offsets da primeira página era 0 
      if fFileVersion<=2 then
        Dec(offsets[i],page0);
      if fFileVersion<4 then
      begin
        aux32:=offsets[i];
        aStream.Write(aux32,SizeOf(aux32));
      end
      else
        aStream.Write(offsets[i],SizeOf(offsets[i]));
    end;
    aStream.Position:=savedpos;
  end;
begin
  Lock;
  try
    FlushCache;
    SaveHeaderToStream(aStream);
    if fFileVersion>=2 then
      SaveMacrosToStream(aStream);
    SavePagesToStream(aStream);
  finally
    Unlock;
  end;
end;

procedure TRLGraphicStorage.LoadFromStream(aStream:TStream);
  procedure LoadHeaderFromStream(aStream:TStream);
  var
    data:string;
    ch  :char;
    i   :integer;
  begin
    SetLength(data,MaxFileHeader);
    i:=0;
    while (i<MaxFileHeader) and (aStream.Read(ch,1)=1) do
    begin
      Inc(i);
      data[i]:=ch;
      if ch=#26 then
        break;
    end;
    SetLength(data,i);
    if data=FileHeaderVersion1 then
      fFileVersion:=1
    else if data=FileHeaderVersion2 then
      fFileVersion:=2
    else if data=FileHeaderVersion3 then
      fFileVersion:=3
    else if data=FileHeaderVersion4 then
      fFileVersion:=4
    else
      raise Exception.Create(Ls_File_version + '"'+data+'"!');
  end;
  procedure LoadMacrosFromStream(aStream:TStream);
  var
    count,len,i,p:integer;
    ln,name,value:string;
  begin
    aStream.Read(count,SizeOf(count));
    // grava símbolos e seus valores
    for i:=0 to count-1 do
    begin
      // lê length + nome
      aStream.Read(len,SizeOf(len));
      SetLength(ln,len);
      aStream.Read(ln[1],len);
      // upgrade
      p:=Pos('=',ln);
      if p<>0 then
      begin
        name :=Trim(Copy(ln,1,p-1));
        value:=Trim(Copy(ln,p+1,Length(ln)));
        if (fFileVersion<3) and SameText(name,'Orientation') then
          ln:=name+'='+IntToStr(StrToIntDef(value,0)+1);
      end;
      //
      fMacros.Add(ln);
    end;
  end;
  procedure LoadPageTableFromStream(aStream:TStream);
  var
    count,i,aux32:integer;
    offset,page0:Int64;
  begin
    aStream.Read(count,SizeOf(count));
    // lê offsets
    for i:=0 to count-1 do
    begin
      if fFileVersion<4 then
      begin
        aStream.Read(aux32,SizeOf(aux32));
        offset:=aux32;
      end
      else
        aStream.Read(offset,SizeOf(offset));
      fPageAllocation.Add(offset);
    end;
    // nas versões <=2 o offsets da primeira página era 0
    if fFileVersion<=2 then
    begin
      page0:=aStream.Position;
      for i:=0 to count-1 do
        fPageAllocation[i]:=fPageAllocation[i]+page0;
    end;
  end;
  procedure LoadPageFromStream(aStream:TStream; aPageIndex:integer);
  var
    size:integer;
    sizeat,beginat,endat:Int64;
  begin
    // lê o tamanho da página armazenada
    aStream.Position:=fPageAllocation[aPageIndex];
    // versões >2 indicam o tamanho da página
    if fFileVersion>2 then
      aStream.Read(size,SizeOf(size))
    else
      size:=0;
    // atualiza tabela de páginas
    fPageAllocation[aPageIndex]:=fTempStream.Position;
    // grava o tamanho no temp stream
    sizeat:=fTempStream.Position;
    fTempStream.Write(size,SizeOf(size));
    // grava página no stream de trabalho
    if fFileVersion>2 then
      fTempStream.CopyFrom(aStream,size)
    else
    begin
      beginat:=fTempStream.Position;
      UpgradePage(Self,aStream,fTempStream);
      endat  :=fTempStream.Position;
      fTempStream.Position:=sizeat;
      size:=endat-beginat;
      fTempStream.Write(size,SizeOf(size));
      fTempStream.Position:=endat;
    end;
  end;
  procedure LoadPagesFromStream(aStream: TStream);
  var
    i:integer;
  begin
    for i:=0 to fPageAllocation.Count-1 do
      LoadPageFromStream(aStream,i);
  end;
begin
  Lock;
  try
    Clear;
    LoadHeaderFromStream(aStream);
    if fFileVersion>=2 then
      LoadMacrosFromStream(aStream);
    LoadPageTableFromStream(aStream);
    TempStreamNeeded;
    LoadPagesFromStream(aStream);
  finally
    Unlock;
  end;
end;

procedure TRLGraphicStorage.SetFileVersion(aVersion:integer);
begin
  if (aVersion<1) or (aVersion>4) then
    raise Exception.Create(Ls_File_version);
  fFileVersion:=aVersion;  
end;

function TRLGraphicStorage.GetMacro(const Name:string):string;
begin
  Lock;
  try
    Result:=fMacros.Values[Name];
  finally
    Unlock;
  end;
end;

procedure TRLGraphicStorage.SetMacro(const Name,Value:string);
begin
  Lock;
  try
    fMacros.Values[Name]:=Value;
  finally
    Unlock;
  end;
end;

function TRLGraphicStorage.GetFirstPageNumber: integer;
begin
  Result:=StrToIntDef(GetMacro('FirstPageNumber'),0);
end;

procedure TRLGraphicStorage.SetFirstPageNumber(const Value: integer);
begin
  SetMacro('FirstPageNumber',IntToStr(Value));
end;

function TRLGraphicStorage.GetLastPageNumber: integer;
begin
  Result:=StrToIntDef(GetMacro('LastPageNumber'),0);
end;

procedure TRLGraphicStorage.SetLastPageNumber(const Value: integer);
begin
  SetMacro('LastPageNumber',IntToStr(Value));
end;

function TRLGraphicStorage.GetOrientation: TRLMetaOrientation;
begin
  Result:=StrToIntDef(GetMacro('Orientation'),MetaOrientationPortrait);
end;

procedure TRLGraphicStorage.SetOrientation(const Value: TRLMetaOrientation);
begin
  SetMacro('Orientation',IntToStr(Value));
end;

function TRLGraphicStorage.GetPaperHeight: double;
begin
  Result:=PtStrToFloat(GetMacro('PaperHeight'),0);
end;

procedure TRLGraphicStorage.SetPaperHeight(const Value: double);
begin
  SetMacro('PaperHeight',FloatToPtStr(Value));
end;

function TRLGraphicStorage.GetPaperWidth: double;
begin
  Result:=PtStrToFloat(GetMacro('PaperWidth'),0);
end;

procedure TRLGraphicStorage.SetPaperWidth(const Value: double);
begin
  SetMacro('PaperWidth',FloatToPtStr(Value));
end;

function TRLGraphicStorage.GetTitle: string;
begin
  Result:=GetMacro('Title');
end;

procedure TRLGraphicStorage.SetTitle(const Value: string);
begin
  SetMacro('Title',Value);
end;

function TRLGraphicStorage.GetHeight: integer;
begin
  Result:=Round(PaperHeight*MMAsPixels);
end;

function TRLGraphicStorage.GetWidth: integer;
begin
  Result:=Round(PaperWidth*MMAsPixels);
end;

function TRLGraphicStorage.GetOrientedPaperHeight: double;
begin
  if Orientation=MetaOrientationPortrait then
    Result:=PaperHeight
  else
    Result:=PaperWidth;
end;

function TRLGraphicStorage.GetOrientedPaperWidth: double;
begin
  if Orientation=MetaOrientationPortrait then
    Result:=PaperWidth
  else
    Result:=PaperHeight;
end;

function TRLGraphicStorage.GetOrientedHeight: integer;
begin
  Result:=Round(OrientedPaperHeight*MMAsPixels);
end;

function TRLGraphicStorage.GetOrientedWidth: integer;
begin
  Result:=Round(OrientedPaperWidth*MMAsPixels);
end;

{ TRLGraphicSurface }

constructor TRLGraphicSurface.Create;
begin
  fStorage    :=nil;
  fPageIndex  :=-1;
  fObjects    :=nil;
  fPenPos     :=Point(0,0);
  fWritePos   :=Point(0,0);
  fWordWrap   :=True;
  fWidth      :=0;
  fHeight     :=0;
  fBrush      :=nil;
  fFont       :=nil;
  fPen        :=nil;
  fMargins    :=Rect(0,0,0,0);
  fOpened     :=false;
  fModified   :=false;
  fFonts      :=nil;
  fClipStack  :=nil;
  fGeneratorId:=0;
  fMacros     :=nil;
  //
  fBrush:=TBrush.Create;
  fBrush.Color:=clWhite;
  fPen:=TPen.Create;
  fPen.Color:=clBlack;
  fFont:=TFont.Create;
  fFont.Color:=clBlack;
  //
  fObjects  :=TObjectList.Create;
  fFonts    :=TStringList.Create;
  fClipStack:=TList.Create;
  fMacros   :=TStringList.Create;
  //
  inherited Create;
end;

destructor TRLGraphicSurface.Destroy;
begin
  //
  SetStorage(nil);
  FreeObj(fObjects);
  FreeObj(fBrush);
  FreeObj(fPen);
  FreeObj(fFont);
  FreeObj(fFonts);
  FreeObj(fClipStack);
  FreeObj(fMacros);
  inherited;
end;

procedure TRLGraphicSurface.SaveToFile(const aFileName:string);
var
  s:TFileStream;
begin
  s:=TFileStream.Create(aFileName,fmCreate);
  try
    SaveToStream(s);
  finally
    s.free;
  end;
end;

procedure TRLGraphicSurface.LoadFromFile(const aFileName:string);
var
  s:TFileStream;
begin
  s:=TFileStream.Create(aFileName,fmOpenRead+fmShareDenyWrite);
  try
    LoadFromStream(s);
  finally
    s.free;
  end;
end;

type
  TGraphicObjectKind=byte; 

const
  ObjectKindPixel        =1;
  ObjectKindLine         =2;
  ObjectKindRectangle    =3;
  ObjectKindText         =4;
  ObjectKindFillRect     =5;
  ObjectKindEllipse      =6;
  ObjectKindPolygon      =7;
  ObjectKindPolyline     =8;
  ObjectKindImage        =9;
  ObjectKindSetClipRect  =10;
  ObjectKindResetClipRect=11;

function GraphicObjectKind(aGraphicObject:TRLGraphicObject):TGraphicObjectKind;
begin
  if aGraphicObject is TRLPixelObject then
    Result:=ObjectKindPixel
  else if aGraphicObject is TRLLineObject then
    Result:=ObjectKindLine
  else if aGraphicObject is TRLRectangleObject then
    Result:=ObjectKindRectangle
  else if aGraphicObject is TRLTextObject then
    Result:=ObjectKindText
  else if aGraphicObject is TRLFillRectObject then
    Result:=ObjectKindFillRect
  else if aGraphicObject is TRLEllipseObject then
    Result:=ObjectKindEllipse
  else if aGraphicObject is TRLPolygonObject then
    Result:=ObjectKindPolygon
  else if aGraphicObject is TRLPolylineObject then
    Result:=ObjectKindPolyline
  else if aGraphicObject is TRLImageObject then
    Result:=ObjectKindImage
  else if aGraphicObject is TRLSetClipRectObject then
    Result:=ObjectKindSetClipRect
  else if aGraphicObject is TRLResetClipRectObject then
    Result:=ObjectKindResetClipRect
  else
    Result:=0;  
end;

const
  MaxSurfaceHeader=20;
  SurfaceHeaderStr='RLGraphicSurface3'#26;
  
procedure TRLGraphicSurface.SaveToStream(aStream:TStream);
  procedure SaveHeaderToStream(aStream:TStream);
  var
    data:string;
  begin
    data:=SurfaceHeaderStr;
    aStream.Write(data[1],Length(data));
  end;
  procedure SaveBoundsToStream(aStream: TStream);
  begin
    aStream.Write(fWidth ,SizeOf(fWidth));
    aStream.Write(fHeight,SizeOf(fHeight));
  end;
  procedure SaveMacrosToStream(aStream: TStream);
  var
    count,len,i:integer;
    ln:string;
  begin
    // grava a quantidade de macros
    count:=fMacros.Count;
    aStream.Write(count,SizeOf(count));
    // grava símbolos
    for i:=0 to count-1 do
    begin
      ln :=fMacros[i];
      len:=Length(ln);
      aStream.Write(len,SizeOf(len));
      aStream.Write(ln[1],len);
    end;
  end;
  procedure SaveFontsToStream(aStream:TStream);
  var
    count,len,i:integer;
    name:string;
  begin
    count:=fFonts.Count;
    aStream.Write(count,SizeOf(count));
    // grava nomes das fontes
    for i:=0 to count-1 do
    begin
      name:=fFonts[i];
      len :=Length(name);
      // grava length + nome
      aStream.Write(len,SizeOf(len));
      aStream.Write(name[1],len);
    end;
  end;
  procedure SaveObjectToStream(aStream:TStream; aObject:TRLGraphicObject);
  var
    kind      :TGraphicObjectKind;
    size      :integer;
    sizeoffset:Int64;
    dataoffset:Int64;
    endpos    :Int64;
  begin
    // grava tipo
    kind:=GraphicObjectKind(aObject);
    aStream.Write(kind,SizeOf(kind));
    // reserva tamanho
    sizeoffset:=aStream.Position;
    size:=0;
    aStream.Write(size,SizeOf(size));
    // grava objeto
    dataoffset:=aStream.Position;
    aObject.SaveToStream(aStream);
    // ajusta tamanho
    endpos:=aStream.Position;
    size  :=endpos-dataoffset;
    aStream.Position:=sizeoffset;
    aStream.Write(size,SizeOf(size));
    // restaura eof
    aStream.Position:=endpos;
  end;
  procedure SaveObjectsToStream(aStream:TStream);
  var
    count,i:integer;
  begin
    count:=ObjectCount;
    aStream.Write(count,SizeOf(count));
    // grava dados dos objetos
    for i:=0 to count-1 do
      SaveObjectToStream(aStream,Objects[i]);
  end;
begin
  SaveHeaderToStream(aStream);
  SaveBoundsToStream(aStream);
  SaveMacrosToStream(aStream);
  SaveFontsToStream(aStream);
  SaveObjectsToStream(aStream);
end;

function GraphicObjectClass(aGraphicObjectKind:TGraphicObjectKind):TRLGraphicObjectClass;
begin
  case aGraphicObjectKind of
    ObjectKindPixel        : Result:=TRLPixelObject;
    ObjectKindLine         : Result:=TRLLineObject;
    ObjectKindRectangle    : Result:=TRLRectangleObject;
    ObjectKindText         : Result:=TRLTextObject;
    ObjectKindFillRect     : Result:=TRLFillRectObject;
    ObjectKindEllipse      : Result:=TRLEllipseObject;
    ObjectKindPolygon      : Result:=TRLPolygonObject;
    ObjectKindPolyline     : Result:=TRLPolylineObject;
    ObjectKindImage        : Result:=TRLImageObject;
    ObjectKindSetClipRect  : Result:=TRLSetClipRectObject;
    ObjectKindResetClipRect: Result:=TRLResetClipRectObject;
  else
    Result:=nil;
  end;
end;

procedure TRLGraphicSurface.LoadFromStream(aStream:TStream);
  procedure LoadHeaderFromStream(aStream:TStream);
  var
    data:string;
    ch  :char;
    i   :integer;
  begin
    SetLength(data,MaxSurfaceHeader);
    i:=0;
    while (i<MaxSurfaceHeader) and (aStream.Read(ch,1)=1) do
    begin
      Inc(i);
      data[i]:=ch;
      if ch=#26 then
        break;
    end;
    SetLength(data,i);
    if data<>SurfaceHeaderStr then
      raise Exception.Create(Ls_File_corrupted);
  end;
  procedure LoadBoundsFromStream(aStream: TStream);
  begin
    aStream.Read(fWidth ,SizeOf(fWidth));
    aStream.Read(fHeight,SizeOf(fHeight));
  end;
  procedure LoadMacrosFromStream(aStream:TStream);
  var
    count,len,i:integer;
    ln:string;
  begin
    aStream.Read(count,SizeOf(count));
    // grava símbolos e seus valores
    for i:=0 to count-1 do
    begin
      // lê length + nome
      aStream.Read(len,SizeOf(len));
      SetLength(ln,len);
      aStream.Read(ln[1],len);
      //
      fMacros.Add(ln);
    end;
  end;
  procedure LoadFontsFromStream(aStream:TStream);
  var
    count,len,i:integer;
    name:string;
  begin
    aStream.Read(count,SizeOf(count));
    // carrega nomes das fontes
    for i:=0 to count-1 do
    begin
      aStream.Read(len,SizeOf(len));
      SetLength(name,len);
      aStream.Read(name[1],len);
      fFonts.Add(name);
    end;
  end;
  procedure LoadObjectsFromStream(aStream:TStream);
  var
    count  :integer;
    size   :integer;
    kind   :TGraphicObjectKind;
    creator:TRLGraphicObjectClass;
    i      :integer;
  begin
    aStream.Read(count,SizeOf(count));
    for i:=0 to count-1 do
    begin
      aStream.Read(kind,SizeOf(kind));
      aStream.Read(size,SizeOf(size));
      creator:=GraphicObjectClass(kind);
      // se a classe não for conhecida, salta o segmento
      if creator<>nil then
        creator.Create(Self).LoadFromStream(aStream)
      else
        aStream.Position:=aStream.Position+size;  
    end;
  end;
begin
  Clear;
  LoadHeaderFromStream(aStream);
  LoadBoundsFromStream(aStream);
  LoadMacrosFromStream(aStream);
  LoadFontsFromStream(aStream);
  LoadObjectsFromStream(aStream);
  fModified:=False;
end;

function TRLGraphicSurface.GetObjectCount:integer;
begin
  Result:=fObjects.Count;
end;

function TRLGraphicSurface.GetObjects(aIndex:integer):TRLGraphicObject;
begin
  Result:=TRLGraphicObject(fObjects[aIndex]);
end;

function TRLGraphicSurface.FindFreeRow(aNearRow:integer; var aRow:integer):boolean;
var
  i:integer;
  g:TRLGraphicObject;
  b:boolean;
begin
  aRow:=aNearRow;
  repeat
    b:=false;
    for i:=0 to ObjectCount-1 do
    begin
      g:=Objects[i];
      if (g is TRLTextObject) and ((TRLTextObject(g).TextFlags and MetaTextFlagIntegralHeight)=MetaTextFlagIntegralHeight) then
        if (aRow>g.BoundsRect.Top) and (aRow<g.BoundsRect.Bottom) then
        begin
          aRow:=g.BoundsRect.Top;
          b   :=true;
        end;
    end;
  until not b or (aRow<=0);
  Result:=(aRow>0);
end;

procedure TRLGraphicSurface.Open;
begin
  if not fOpened then
  begin
    fOpened     :=true;
    fPenPos     :=Point(fMargins.Left,fMargins.Top);
    fWritePos   :=fPenPos;
    fGeneratorId:=0;
    fClipRect   :=Rect(0,0,fWidth,fHeight);
    fClipStack.Clear;
  end;
end;

procedure TRLGraphicSurface.Close;
begin
  if fOpened then
    fOpened:=false;
end;

procedure TRLGraphicSurface.Clear;
begin
  fObjects.Clear;
  fFonts.Clear;
  fMacros.Clear;
  //
  fPenPos     :=Point(fMargins.Left,fMargins.Top);
  fWritePos   :=fPenPos;
  fModified   :=True;
  fGeneratorId:=0;
end;

procedure TRLGraphicSurface.Ellipse(const aRect:TRect);
var
  obj:TRLEllipseObject;
begin
  Open;
  fModified:=true;
  obj:=TRLEllipseObject.Create(Self);
  obj.BoundsRect:=ToMetaRect(aRect);
  ToMetaPen(Self.Pen,obj.Pen);
  ToMetaBrush(Self.Brush,obj.Brush);
end;

procedure TRLGraphicSurface.Ellipse(aX1,aY1,aX2,aY2:integer);
begin
  Ellipse(Rect(aX1,aY1,aX2,aY2));
end;

procedure TRLGraphicSurface.FillRect(const aRect:TRect);
var
  obj:TRLFillRectObject;
begin
  Open;
  fModified:=true;
  obj:=TRLFillRectObject.Create(Self);
  obj.BoundsRect:=ToMetaRect(aRect);
  ToMetaBrush(Self.Brush,obj.Brush);
end;

procedure TRLGraphicSurface.MoveTo(aX,aY:integer);
begin
  Open;
  fModified:=true;
  fPenPos:=Point(aX,aY);
end;

procedure TRLGraphicSurface.LineTo(aX,aY:integer);
var
  obj:TRLLineObject;
begin
  Open;
  fModified:=true;
  obj:=TRLLineObject.Create(Self);
  obj.FromPoint :=ToMetaPoint(fPenPos);
  fPenPos       :=Point(aX,aY);
  obj.ToPoint   :=ToMetaPoint(fPenPos);
  obj.BoundsRect:=ToMetaRect(Rect(Min(obj.FromPoint.X,obj.ToPoint.X)-1,
                                  Min(obj.FromPoint.Y,obj.ToPoint.Y)-1,
                                  Max(obj.FromPoint.X,obj.ToPoint.X)+1,
                                  Max(obj.FromPoint.Y,obj.ToPoint.Y)+1));
  ToMetaPen(Self.Pen,obj.Pen);
  ToMetaBrush(Self.Brush,obj.Brush);
end;

procedure TRLGraphicSurface.Polygon(const aPoints:array of TPoint);
var
  obj:TRLPolygonObject;
begin
  Open;
  fModified:=true;
  obj:=TRLPolygonObject.Create(Self);
  obj.Points    :=ToMetaPointArray(aPoints);
  obj.BoundsRect:=ToMetaRect(GetPointsBounds(obj.Points));
  ToMetaPen(Self.Pen,obj.Pen);
  ToMetaBrush(Self.Brush,obj.Brush);
end;

procedure TRLGraphicSurface.Polyline(const aPoints:array of TPoint);
var
  obj:TRLPolylineObject;
begin
  Open;
  fModified:=true;
  obj:=TRLPolylineObject.Create(Self);
  obj.Points    :=ToMetaPointArray(aPoints);
  obj.BoundsRect:=ToMetaRect(GetPointsBounds(obj.Points));
  ToMetaPen(Self.Pen,obj.Pen);
end;

procedure TRLGraphicSurface.Rectangle(aLeft,aTop,aRight,aBottom:integer);
var
  obj:TRLRectangleObject;
begin
  Open;
  fModified:=true;
  obj:=TRLRectangleObject.Create(Self);
  obj.BoundsRect:=ToMetaRect(Rect(aLeft,aTop,aRight,aBottom));
  ToMetaPen(Self.Pen,obj.Pen);
  ToMetaBrush(Self.Brush,obj.Brush);
end;

procedure TRLGraphicSurface.Rectangle(const aRect:TRect);
begin
  with aRect do
    Rectangle(Left,Top,Right,Bottom);
end;

procedure TRLGraphicSurface.SetClipRect(const aRect:TRect);
var
  obj:TRLSetClipRectObject;
begin
  Open;
  fModified:=true;
  obj:=TRLSetClipRectObject.Create(Self);
  obj.BoundsRect:=ToMetaRect(aRect);
  PushClipRect(fClipRect);
  fClipRect:=aRect;
end;

procedure TRLGraphicSurface.ResetClipRect;
var
  obj:TRLResetClipRectObject;
begin
  Open;
  fModified:=true;
  obj:=TRLResetClipRectObject.Create(Self);
  obj.BoundsRect:=ToMetaRect(fClipRect);
  PopClipRect(fClipRect);
end;

procedure TRLGraphicSurface.PaintTo(aCanvas:TCanvas; aRect:TRect);
  function TransformObjBounds(aObj:TRLGraphicObject; aXFactor,aYFactor:double; aXDesloc,aYDesloc:integer):TRect;
  begin
    Result.Left  :=aXDesloc+Round(aObj.fBoundsRect.Left*aXFactor);
    Result.Top   :=aYDesloc+Round(aObj.fBoundsRect.Top *aYFactor);
    Result.Right :=Max(aXDesloc+Round(aObj.fBoundsRect.Right *aXFactor),Result.Left+1);
    Result.Bottom:=Max(aYDesloc+Round(aObj.fBoundsRect.Bottom*aYFactor),Result.Top +1);
  end;
  procedure PaintPixelObject(aObj:TRLPixelObject; aCanvas:TCanvas; aXFactor,aYFactor:double; aXDesloc,aYDesloc:integer);
  begin
    aCanvas.Brush.Style:=bsSolid;
    aCanvas.Brush.Color:=FromMetaColor(aObj.fColor);
    aCanvas.FillRect(TransformObjBounds(aObj,aXFactor,aYFactor,aXDesloc,aYDesloc));
  end;
  procedure PaintLineObject(aObj:TRLLineObject; aCanvas:TCanvas; aXFactor,aYFactor:double; aXDesloc,aYDesloc:integer);
  var
    p1,p2:TPoint;
  begin
    p1.X:=aXDesloc+Round(aObj.fFromPoint.X*aXFactor);
    p1.Y:=aYDesloc+Round(aObj.fFromPoint.Y*aYFactor);
    p2.X:=aXDesloc+Round(aObj.fToPoint.X  *aXFactor);
    p2.Y:=aYDesloc+Round(aObj.fToPoint.Y  *aYFactor);
    FromMetaPen(aObj.fPen,aCanvas.Pen);
    PenInflate(aCanvas.Pen,aXFactor);
    FromMetaBrush(aObj.fBrush,aCanvas.Brush);
    aCanvas.MoveTo(p1.X,p1.Y);
    CanvasLineToEx(aCanvas,p2.X,p2.Y);
  end;
  procedure PaintRectangleObject(aObj:TRLRectangleObject; aCanvas:TCanvas; aXFactor,aYFactor:double; aXDesloc,aYDesloc:integer);
  begin
    FromMetaPen(aObj.fPen,aCanvas.Pen);
    PenInflate(aCanvas.Pen,aXFactor);
    FromMetaBrush(aObj.fBrush,aCanvas.Brush);
    aCanvas.Rectangle(TransformObjBounds(aObj,aXFactor,aYFactor,aXDesloc,aYDesloc));
  end;
  procedure PaintTextObject(aObj:TRLTextObject; aCanvas:TCanvas; aXFactor,aYFactor:double; aXDesloc,aYDesloc:integer);
  var
    o:TPoint;
    r:TRect;
  begin
    o.X:=aXDesloc+Round(aObj.fOrigin.X*aXFactor);
    o.Y:=aYDesloc+Round(aObj.fOrigin.Y*aYFactor);
    r  :=TransformObjBounds(aObj,aXFactor,aYFactor,aXDesloc,aYDesloc);
    FromMetaBrush(aObj.fBrush,aCanvas.Brush);
    FromMetaFont(aObj.fFont,aCanvas.Font,aYFactor);
    CanvasTextRectEx(aCanvas,r,o.X,o.Y,aObj.DisplayText,aObj.fAlignment,aObj.fLayout,aObj.fTextFlags);
  end;
  procedure PaintContinuousTextObjects(aList:TList; aCanvas:TCanvas; aXFactor,aYFactor:double; aXDesloc,aYDesloc:integer);
    procedure SetObj(tx:TRLTextObject; var r:TRect; var o:TPoint; var w:integer);
    begin
      r  :=TransformObjBounds(tx,aXFactor,aYFactor,aXDesloc,aYDesloc);
      o.X:=aXDesloc+Round(tx.fOrigin.X*aXFactor);
      o.Y:=aYDesloc+Round(tx.fOrigin.Y*aYFactor);
      FromMetaBrush(tx.fBrush,aCanvas.Brush);
      FromMetaFont(tx.fFont,aCanvas.Font,aYFactor);
      w:=CanvasTextWidth(aCanvas,tx.Text);
      r.Right:=r.Left+w;
    end;
  var
    tx0:TRLTextObject;
    txN:TRLTextObject;
    txi:TRLTextObject;
    r0 :TRect;
    rN :TRect;
    r  :TRect;
    o  :TPoint;
    i,w:integer;
    tw :integer;
    l  :integer;
  begin
    tx0:=TRLTextObject(aList[0]);
    txN:=TRLTextObject(aList[aList.Count-1]);
    r0 :=TransformObjBounds(tx0,aXFactor,aYFactor,aXDesloc,aYDesloc);
    rN :=TransformObjBounds(txN,aXFactor,aYFactor,aXDesloc,aYDesloc);
    tw :=0;
    for i:=0 to aList.Count-1 do
    begin
      txi:=TRLTextObject(aList[i]);
      FromMetaFont(txi.fFont,aCanvas.Font,aYFactor);
      w:=CanvasTextWidth(aCanvas,txi.Text);
      Inc(tw,w);
    end;
    case tx0.Alignment of
      MetaTextAlignmentLeft,MetaTextAlignmentCenter:
      begin
        l:=r0.Left;
        for i:=0 to aList.Count-1 do
        begin
          txi:=TRLTextObject(aList[i]);
          SetObj(txi,r,o,w);
          Inc(o.X,l-r.Left);
          OffsetRect(r,l-r.Left,0);
          CanvasTextRectEx(aCanvas,r,o.X,o.Y,txi.DisplayText,txi.fAlignment,txi.fLayout,txi.fTextFlags);
          l:=r.Right;
        end;
      end;
      MetaTextAlignmentRight:
      begin
        l:=rN.Right;
        for i:=aList.Count-1 downto 0 do
        begin
          txi:=TRLTextObject(aList[i]);
          SetObj(txi,r,o,w);
          Inc(o.X,l-r.Right);
          OffsetRect(r,l-r.Right,0);
          CanvasTextRectEx(aCanvas,r,o.X,o.Y,txi.DisplayText,txi.fAlignment,txi.fLayout,txi.fTextFlags);
          l:=r.Left;
        end;
      end;
      MetaTextAlignmentJustify:
      begin
        l:=r0.Left;
        for i:=0 to aList.Count-1 do
        begin
          txi:=TRLTextObject(aList[i]);
          SetObj(txi,r,o,w);
          if i>0 then
            if i=aList.Count-1 then
              l:=Max(l,rN.Right-w)
            else
              l:=Max(l,(tw-(rN.Right-l)) div (aList.Count-1-i));
          Inc(o.X,l-r.Left);
          OffsetRect(r,l-r.Left,0);
          CanvasTextRectEx(aCanvas,r,o.X,o.Y,txi.DisplayText,txi.fAlignment,txi.fLayout,txi.fTextFlags);
          l:=r.Right;
        end;
      end;
    end;
  end;
  procedure PaintFillRectObject(aObj:TRLFillRectObject; aCanvas:TCanvas; aXFactor,aYFactor:double; aXDesloc,aYDesloc:integer);
  begin
    FromMetaBrush(aObj.fBrush,aCanvas.Brush);
    aCanvas.FillRect(TransformObjBounds(aObj,aXFactor,aYFactor,aXDesloc,aYDesloc));
  end;
  procedure PaintEllipseObject(aObj:TRLEllipseObject; aCanvas:TCanvas; aXFactor,aYFactor:double; aXDesloc,aYDesloc:integer);
  begin
    FromMetaPen(aObj.fPen,aCanvas.Pen);
    PenInflate(aCanvas.Pen,aXFactor);
    FromMetaBrush(aObj.fBrush,aCanvas.Brush);
    aCanvas.Ellipse(TransformObjBounds(aObj,aXFactor,aYFactor,aXDesloc,aYDesloc));
  end;
  procedure PaintPolygonObject(aObj:TRLPolygonObject; aCanvas:TCanvas; aXFactor,aYFactor:double; aXDesloc,aYDesloc:integer);
  var
    p:TPointArray;
    i:integer;
  begin
    SetLength(p,High(aObj.fPoints)+1);
    for i:=0 to High(p) do
    begin
      p[i].X:=aXDesloc+Round(aObj.fPoints[i].X*aXFactor);
      p[i].Y:=aYDesloc+Round(aObj.fPoints[i].Y*aYFactor);
    end;
    FromMetaPen(aObj.fPen,aCanvas.Pen);
    PenInflate(aCanvas.Pen,aXFactor);
    FromMetaBrush(aObj.fBrush,aCanvas.Brush);
    aCanvas.Polygon(p);
  end;
  procedure PaintPolylineObject(aObj:TRLPolylineObject; aCanvas:TCanvas; aXFactor,aYFactor:double; aXDesloc,aYDesloc:integer);
  var
    p:TPointArray;
    i:integer;
  begin
    SetLength(p,High(aObj.fPoints)+1);
    for i:=0 to High(p) do
    begin
      p[i].X:=aXDesloc+Round(aObj.fPoints[i].X*aXFactor);
      p[i].Y:=aYDesloc+Round(aObj.fPoints[i].Y*aYFactor);
    end;
    FromMetaPen(aObj.fPen,aCanvas.Pen);
    PenInflate(aCanvas.Pen,aXFactor);
    aCanvas.Brush.Style:=bsClear;
    aCanvas.Polyline(p);
  end;
  procedure PaintImageObject(aObj:TRLImageObject; aCanvas:TCanvas; aXFactor,aYFactor:double; aXDesloc,aYDesloc:integer);
  begin
    CanvasStretchDraw(aCanvas,TransformObjBounds(aObj,aXFactor,aYFactor,aXDesloc,aYDesloc),aObj.Data,aObj.Parity);
  end;
  procedure PaintSetClipRectObject(aObj:TRLSetClipRectObject; aCanvas:TCanvas; aXFactor,aYFactor:double; aXDesloc,aYDesloc:integer);
  begin
    aObj.fSurface.PushClipRect(aObj.fSurface.fClipRect);
    aObj.fSurface.fClipRect:=TransformObjBounds(aObj,aXFactor,aYFactor,aXDesloc,aYDesloc);
    CanvasSetClipRect(aCanvas,aObj.fSurface.fClipRect);
  end;
  procedure PaintResetClipRectObject(aObj:TRLResetClipRectObject; aCanvas: TCanvas; aXFactor, aYFactor: double; aXDesloc, aYDesloc: integer);
  begin
    aObj.fSurface.PopClipRect(aObj.fSurface.fClipRect);
    CanvasSetClipRect(aCanvas,aObj.fSurface.fClipRect);
  end;
var
  xfactor,yfactor:double;
  contlist:TList;
  obj:TRLGraphicObject;
  i:integer;
begin
  if fWidth=0 then
    xfactor:=1
  else
    xfactor:=(aRect.Right-aRect.Left)/fWidth;
  if fHeight=0 then
    yfactor:=1
  else
    yfactor:=(aRect.Bottom-aRect.Top)/fHeight;
  //
  fClipStack.Clear;
  try
    fClipRect:=aRect;
    CanvasStart(aCanvas);
    try
      CanvasSetClipRect(aCanvas,fClipRect);
      try
        i:=0;
        while i<ObjectCount do
        begin
          obj:=Objects[i];
          if obj is TRLPixelObject then
            PaintPixelObject(TRLPixelObject(obj),aCanvas,xfactor,yfactor,aRect.Left,aRect.Top)
          else if obj is TRLLineObject then
            PaintLineObject(TRLLineObject(obj),aCanvas,xfactor,yfactor,aRect.Left,aRect.Top)
          else if obj is TRLRectangleObject then
            PaintRectangleObject(TRLRectangleObject(obj),aCanvas,xfactor,yfactor,aRect.Left,aRect.Top)
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
                  if i>ObjectCount-1 then
                    Break;
                  obj:=TRLGraphicObject(Objects[i]);
                until False;
                PaintContinuousTextObjects(contlist,aCanvas,xfactor,yfactor,aRect.Left,aRect.Top);
              finally
                contlist.Free;
              end;
            end
            else
              PaintTextObject(TRLTextObject(obj),aCanvas,xfactor,yfactor,aRect.Left,aRect.Top)
          else if obj is TRLFillRectObject then
            PaintFillRectObject(TRLFillRectObject(obj),aCanvas,xfactor,yfactor,aRect.Left,aRect.Top)
          else if obj is TRLEllipseObject then
            PaintEllipseObject(TRLEllipseObject(obj),aCanvas,xfactor,yfactor,aRect.Left,aRect.Top)
          else if obj is TRLPolygonObject then
            PaintPolygonObject(TRLPolygonObject(obj),aCanvas,xfactor,yfactor,aRect.Left,aRect.Top)
          else if obj is TRLPolylineObject then
            PaintPolylineObject(TRLPolylineObject(obj),aCanvas,xfactor,yfactor,aRect.Left,aRect.Top)
          else if obj is TRLImageObject then
            PaintImageObject(TRLImageObject(obj),aCanvas,xfactor,yfactor,aRect.Left,aRect.Top)
          else if obj is TRLSetClipRectObject then
            PaintSetClipRectObject(TRLSetClipRectObject(obj),aCanvas,xfactor,yfactor,aRect.Left,aRect.Top)
          else if obj is TRLResetClipRectObject then
            PaintResetClipRectObject(TRLResetClipRectObject(obj),aCanvas,xfactor,yfactor,aRect.Left,aRect.Top);
          Inc(i);  
        end;
      finally
        CanvasResetClipRect(aCanvas);
      end;
    finally
      CanvasStop(aCanvas);
    end;
  finally
    while fClipStack.Count>0 do
      PopClipRect(fClipRect);
  end;
end;

procedure TRLGraphicSurface.CopyRect(const aDest:TRect; aCanvas:TCanvas; const aSource:TRect);
var
  b:TBitmap;
begin
  b:=AuxBitmapNeeded(aSource.Right-aSource.Left,aSource.Bottom-aSource.Top);
  b.Canvas.CopyRect(Rect(0,0,b.Width,b.Height),aCanvas,aSource);
  StretchDraw(aDest,b);
end;

procedure TRLGraphicSurface.CopyRect(const aDest:TRect; aSurface:TRLGraphicSurface; const aSource:TRect);
var
  xfactor,yfactor:double;
  xdesloc,ydesloc:integer;
  obj,clone:TRLGraphicObject;
  p:TRLMetaRect;
  r:TRect;
  i:integer;
begin
  Open;
  fModified:=true;
  xfactor:=(aDest.Right-aDest.Left)/(aSource.Right-aSource.Left);
  yfactor:=(aDest.Bottom-aDest.Top)/(aSource.Bottom-aSource.Top);
  xdesloc:=aDest.Left-Round(aSource.Left*xfactor);
  ydesloc:=aDest.Top-Round(aSource.Top*yfactor);
  //
  SetClipRect(aDest);
  try
    for i:=0 to aSurface.ObjectCount-1 do
    begin
      obj:=aSurface.Objects[i];
      p:=obj.fBoundsRect;
      r.Left  :=p.Left;
      r.Top   :=p.Top;
      r.Right :=p.Right;
      r.Bottom:=p.Bottom;
      if IntersectRect(r,aSource,r) then
      begin
        clone:=obj.Clone(Self);
        clone.Inflate(xfactor,yfactor);
        clone.Offset(xdesloc,ydesloc);
      end;
    end;
  finally
    ResetClipRect;
  end;
end;

procedure TRLGraphicSurface.SetFont(const Value:TFont);
begin
  fFont.Assign(Value);
end;

procedure TRLGraphicSurface.SetPen(const Value:TPen);
begin
  fPen.Assign(Value);
end;

procedure TRLGraphicSurface.SetBrush(const Value:TBrush);
begin
  fBrush.Assign(Value);
end;

function TRLGraphicSurface.GetPixels(X,Y:integer):TColor;
begin
  Result:=Self.Brush.Color;
end;

procedure TRLGraphicSurface.SetPixels(X,Y:integer; const Value:TColor);
var
  obj:TRLPixelObject;
begin
  Open;
  fModified:=true;
  obj:=TRLPixelObject.Create(Self);
  obj.BoundsRect:=ToMetaRect(Rect(X,Y,X+1,Y+1));
  obj.Color     :=ToMetaColor(Value);
end;

procedure TRLGraphicSurface.SetStorage(aStorage: TRLGraphicStorage);
begin
  if fStorage<>aStorage then
  begin
    if Assigned(fStorage) then
      fStorage.fPageCache.Extract(Self);
    fStorage:=aStorage;
  end;  
end;

function TRLGraphicSurface.TextWidth(const aText:string):integer;
Var
B: Tbitmap;
begin
  B:= AuxBitmapNeeded;
  B.Canvas.Font.Assign(fFont);
  Result:=B.Canvas.TextWidth(aText);
  FreeAndNil(B);
end;


function TRLGraphicSurface.TextHeight(const aText:string):integer;
Var
B: Tbitmap;
begin
  B:= AuxBitmapNeeded;
  B.Canvas.Font.Assign(fFont);
  Result:=B.Canvas.TextHeight(aText);
  FreeAndNil(B);
end;


procedure TRLGraphicSurface.TextOut(aLeft,aTop:integer; const aText:string);
begin
  TextOutEx(aLeft,aTop,aText,MetaTextFlagAutoSize or MetaTextFlagIntegralHeight);
end;

procedure TRLGraphicSurface.TextOutEx(aLeft,aTop:integer; const aText:string; aTextFlags:TRLMetaTextFlags);
var
  obj:TRLTextObject;
begin
  Open;
  fModified:=true;
  obj:=TRLTextObject.Create(Self);
  obj.BoundsRect:=ToMetaRect(Rect(aLeft,aTop,aLeft+TextWidth(aText),aTop+TextHeight(aText)));
  obj.Text      :=aText;
  obj.Origin    :=ToMetaPoint(Point(aLeft,aTop));
  obj.Alignment :=MetaTextAlignmentLeft;
  obj.Layout    :=MetaTextLayoutTop;
  obj.TextFlags :=aTextFlags;
  ToMetaBrush(Self.Brush,obj.Brush);
  ToMetaFont(Self.Font,obj.Font);
end;

procedure TRLGraphicSurface.TextRect(const aRect:TRect; aLeft,aTop:integer; const aText:string);
begin
  TextRectEx(aRect,aLeft,aTop,aText,MetaTextAlignmentLeft,MetaTextLayoutTop,MetaTextFlagIntegralHeight);
end;

procedure TRLGraphicSurface.TextRectEx(const aRect:TRect; aLeft,aTop:integer; const aText:string; aAlignment:TRLMetaTextAlignment; aLayout:TRLMetaTextLayout; aTextFlags:TRLMetaTextFlags);
var
  obj:TRLTextObject;
begin
  Open;
  fModified:=true;
  obj:=TRLTextObject.Create(Self);
  obj.BoundsRect:=ToMetaRect(aRect);
  obj.Text      :=aText;
  obj.Origin    :=ToMetaPoint(Point(aLeft,aTop));
  obj.Alignment :=aAlignment;
  obj.Layout    :=aLayout;
  obj.TextFlags :=aTextFlags;
  ToMetaBrush(Self.Brush,obj.Brush);
  ToMetaFont(Self.Font,obj.Font);
end;

procedure TRLGraphicSurface.BoundTextCursor;
begin
  if fWritePos.x<fMargins.Left then
    fWritePos.x:=fMargins.Left;
  if fWritePos.y<fMargins.Top then
    fWritePos.y:=fMargins.Top;
end;

procedure TRLGraphicSurface.Write(const aText:string);
begin
  BoundTextCursor;
  TextOut(fWritePos.x,fWritePos.y,aText);
  Inc(fWritePos.x,TextWidth(aText));
end;

procedure TRLGraphicSurface.WriteLn(const aText:string);
begin
  Write(aText);
  Inc(fWritePos.y,TextHeight('H'));
  fWritePos.x:=fMargins.Left;
end;

procedure TRLGraphicSurface.Draw(aX,aY:integer; aGraphic:TGraphic; aParity:boolean=false);
var
  obj:TRLImageObject;
begin
  Open;
  fModified:=True;
  obj:=TRLImageObject.Create(Self);
  obj.BoundsRect:=ToMetaRect(Rect(aX,aY,aX+aGraphic.Width,aY+aGraphic.Height));
  obj.Data      :=ToMetaGraphic(aGraphic);
  obj.Parity    :=aParity;
end;

procedure TRLGraphicSurface.Draw(aX,aY:integer; aSurface:TRLGraphicSurface);
var
  i:integer;
begin
  Open;
  fModified:=true;
  for i:=0 to aSurface.ObjectCount-1 do
    aSurface.Objects[i].Clone(Self).Offset(aX,aY);
end;

procedure TRLGraphicSurface.StretchDraw(const aRect:TRect; aGraphic:TGraphic; aParity:boolean=false);
var
  obj:TRLImageObject;
begin
  Open;
  fModified:=true;
  obj:=TRLImageObject.Create(Self);
  obj.BoundsRect:=ToMetaRect(aRect);
  obj.Data      :=ToMetaGraphic(aGraphic);
  obj.Parity    :=aParity;
end;

procedure TRLGraphicSurface.StretchDraw(const aRect:TRect; aSurface:TRLGraphicSurface);
begin
  CopyRect(aRect,aSurface,Rect(0,0,aSurface.Width,aSurface.Height));
end;

procedure TRLGraphicSurface.ScaleDraw(const aRect:TRect; aGraphic:TGraphic; aCenter:boolean);
var
  scaledrect:TRect;
begin
  scaledrect:=ScaleRect(Rect(0,0,aGraphic.Width,aGraphic.Height),aRect,aCenter);
  StretchDraw(scaledrect,aGraphic);
end;

procedure TRLGraphicSurface.ScaleDraw(const aRect:TRect; aSurface:TRLGraphicSurface; aCenter:boolean);
var
  scaledrect:TRect;
begin
  scaledrect:=ScaleRect(Rect(0,0,aSurface.Width,aSurface.Height),aRect,aCenter);
  StretchDraw(scaledrect,aSurface);
end;

procedure TRLGraphicSurface.ClipDraw(const aRect:TRect; aGraphic:TGraphic; aCenter:boolean);
var
  b:TBitmap;
  r:TRect;
begin
  r:=aRect;
  b:=ClipGraphic(aGraphic,r,aCenter);
  if Assigned(b) then
    try
      StretchDraw(r,b);
    finally
      b.free;
    end;
end;

procedure TRLGraphicSurface.ClipDraw(const aRect:TRect; aSurface:TRLGraphicSurface; aCenter:boolean);
var
  b:TRLGraphicSurface;
  r:TRect;
begin
  r:=aRect;
  b:=ClipSurface(aSurface,r,aCenter);
  if Assigned(b) then
    try
      StretchDraw(r,b);
    finally
      b.free;
    end;
end;

procedure TRLGraphicSurface.PushClipRect(const aRect:TRect);
var
  p:PRect;
begin
  New(p);
  p^:=aRect;
  fClipStack.Insert(0,p);
end;

procedure TRLGraphicSurface.PopClipRect(var aRect:TRect);
var
  p:PRect;
begin
  p:=fClipStack[0];
  aRect:=p^;
  Dispose(p);
  fClipStack.Delete(0);
end;

function TRLGraphicSurface.GetOrientation: TRLMetaOrientation;
begin
  Result:=StrToIntDef(fMacros.Values['Orientation'],MetaOrientationPortrait);
end;

procedure TRLGraphicSurface.SetOrientation(const Value: TRLMetaOrientation);
begin
  fMacros.Values['Orientation']:=IntToStr(Value);
end;

function TRLGraphicSurface.GetPaperHeight: double;
begin
  Result:=PtStrToFloat(fMacros.Values['PaperHeight'],0);
end;

procedure TRLGraphicSurface.SetPaperHeight(const Value: double);
begin
  fMacros.Values['PaperHeight']:=FloatToPtStr(Value);
end;

function TRLGraphicSurface.GetPaperWidth: double;
begin
  Result:=PtStrToFloat(fMacros.Values['PaperWidth'],0);
end;

procedure TRLGraphicSurface.SetPaperWidth(const Value: double);
begin
  fMacros.Values['PaperWidth']:=FloatToPtStr(Value);
end;

function TRLGraphicSurface.GetOrientedPaperHeight: double;
begin
  if Orientation=MetaOrientationPortrait then
    Result:=PaperHeight
  else
    Result:=PaperWidth;
end;

function TRLGraphicSurface.GetOrientedPaperWidth: double;
begin
  if Orientation=MetaOrientationPortrait then
    Result:=PaperWidth
  else
    Result:=PaperHeight;
end;

function TRLGraphicSurface.GetOrientedHeight: integer;
begin
  if Orientation=MetaOrientationPortrait then
    Result:=fHeight
  else
    Result:=fWidth;
end;

function TRLGraphicSurface.GetOrientedWidth: integer;
begin
  if Orientation=MetaOrientationPortrait then
    Result:=fWidth
  else
    Result:=fHeight;
end;

{ TRLMetaPen }

constructor TRLMetaPen.Create(aUser:TRLGraphicObject);
begin
  fUser  :=aUser;
  fColor :=MetaColor(0,0,0);
  fMode  :=MetaPenModeCopy;
  fStyle :=MetaPenStyleSolid;
  fWidth :=0;
  //
  inherited Create;
end;

procedure TRLMetaPen.SaveToStream(aStream:TStream);
begin
  aStream.Write(fColor,SizeOf(fColor));
  aStream.Write(fMode ,SizeOf(fMode));
  aStream.Write(fStyle,SizeOf(fStyle));
  aStream.Write(fWidth,SizeOf(fWidth));
end;

procedure TRLMetaPen.LoadFromStream(aStream:TStream);
begin
  aStream.Read(fColor,SizeOf(fColor));
  aStream.Read(fMode ,SizeOf(fMode));
  aStream.Read(fStyle,SizeOf(fStyle));
  aStream.Read(fWidth,SizeOf(fWidth));
end;

procedure TRLMetaPen.Assign(aObject:TRLMetaPen);
begin
  Color:=aObject.Color;
  Mode :=aObject.Mode;
  Style:=aObject.Style;
  Width:=aObject.Width;
end;

procedure TRLMetaPen.Inflate(aFactor: double);
begin
  if Width<>0 then
    Width:=Max(1,Round(Width*aFactor));
end;

function TRLMetaPen.GetColor: TRLMetaColor;
begin
  Result:=fColor;
end;

function TRLMetaPen.GetMode: TRLMetaPenMode;
begin
  Result:=fMode;
end;

function TRLMetaPen.GetStyle: TRLMetaPenStyle;
begin
  Result:=fStyle;
end;

function TRLMetaPen.GetWidth: integer;
begin
  Result:=fWidth;
end;

procedure TRLMetaPen.SetColor(const Value: TRLMetaColor);
begin
  fColor:=Value;
end;

procedure TRLMetaPen.SetMode(Value: TRLMetaPenMode);
begin
  fMode:=Value;
end;

procedure TRLMetaPen.SetStyle(Value: TRLMetaPenStyle);
begin
  fStyle:=Value;
end;

procedure TRLMetaPen.SetWidth(Value: integer);
begin
  fWidth:=Value;
end;

{ TRLMetaBrush }

constructor TRLMetaBrush.Create(aUser:TRLGraphicObject);
begin
  fUser :=aUser;
  fColor:=MetaColor(0,0,0);
  fStyle:=MetaBrushStyleSolid;
  //
  inherited Create;
end;

procedure TRLMetaBrush.SaveToStream(aStream:TStream);
begin
  aStream.Write(fColor,SizeOf(fColor));
  aStream.Write(fStyle,SizeOf(fStyle));
end;

procedure TRLMetaBrush.LoadFromStream(aStream:TStream);
begin
  aStream.Read(fColor,SizeOf(fColor));
  aStream.Read(fStyle,SizeOf(fStyle));
end;

procedure TRLMetaBrush.Assign(aObject:TRLMetaBrush);
begin
  Color:=aObject.Color;
  Style:=aObject.Style;
end;

function TRLMetaBrush.GetColor: TRLMetaColor;
begin
  Result:=fColor;
end;

function TRLMetaBrush.GetStyle: TRLMetaBrushStyle;
begin
  Result:=fStyle;
end;

procedure TRLMetaBrush.SetColor(const Value: TRLMetaColor);
begin
  fColor:=Value;
end;

procedure TRLMetaBrush.SetStyle(Value: TRLMetaBrushStyle);
begin
  fStyle:=Value;
end;

{ TRLMetaFont }

constructor TRLMetaFont.Create(aUser:TRLGraphicObject);
begin
  fUser         :=aUser;
  fPixelsPerInch:=72;
  fCharset      :=0;
  fColor        :=MetaColor(0,0,0);
  fHeight       :=0;
  fNameId       :=0;
  fPitch        :=MetaFontPitchDefault;
  fSize         :=0;
  fStyle        :=0;
  //
  inherited Create;
end;

procedure TRLMetaFont.SaveToStream(aStream:TStream);
begin
  aStream.Write(fPixelsPerInch,SizeOf(fPixelsPerInch));
  aStream.Write(fCharset      ,SizeOf(fCharset));
  aStream.Write(fColor        ,SizeOf(fColor));
  aStream.Write(fHeight       ,SizeOf(fHeight));
  aStream.Write(fNameId       ,SizeOf(fNameId));
  aStream.Write(fPitch        ,SizeOf(fPitch));
  aStream.Write(fSize         ,SizeOf(fSize));
  aStream.Write(fStyle        ,SizeOf(fStyle));
end;

procedure TRLMetaFont.LoadFromStream(aStream:TStream);
begin
  aStream.Read(fPixelsPerInch,SizeOf(fPixelsPerInch));
  aStream.Read(fCharset      ,SizeOf(fCharset));
  aStream.Read(fColor        ,SizeOf(fColor));
  aStream.Read(fHeight       ,SizeOf(fHeight));
  aStream.Read(fNameId       ,SizeOf(fNameId));
  aStream.Read(fPitch        ,SizeOf(fPitch));
  aStream.Read(fSize         ,SizeOf(fSize));
  aStream.Read(fStyle        ,SizeOf(fStyle));
end;

procedure TRLMetaFont.Assign(aObject:TRLMetaFont);
begin
  PixelsPerInch:=aObject.PixelsPerInch;
  Charset      :=aObject.Charset;
  Color        :=aObject.Color;
  Height       :=aObject.Height;
  Name         :=aObject.Name;
  Pitch        :=aObject.Pitch;
  Size         :=aObject.Size;
  Style        :=aObject.Style;
end;

function TRLMetaFont.GetName:string;
begin
  Result:=fUser.fSurface.fFonts[fNameId];
end;

procedure TRLMetaFont.SetName(const Value:string);
begin
  fNameId:=fUser.fSurface.fFonts.IndexOf(Value);
  if fNameId=-1 then
    fNameId:=fUser.fSurface.fFonts.Add(Value);
end;

function TRLMetaFont.GetCharset: TRLMetaFontCharset;
begin
  Result:=fCharset;
end;

function TRLMetaFont.GetColor: TRLMetaColor;
begin
  Result:=fColor;
end;

function TRLMetaFont.GetHeight: integer;
begin
  Result:=fHeight;
end;

function TRLMetaFont.GetPitch: TRLMetaFontPitch;
begin
  Result:=fPitch;
end;

function TRLMetaFont.GetPixelsPerInch: integer;
begin
  Result:=fPixelsPerInch;
end;

function TRLMetaFont.GetSize: integer;
begin
  Result:=fSize;
end;

function TRLMetaFont.GetStyle: TRLMetaFontStyles;
begin
  Result:=fStyle;
end;

procedure TRLMetaFont.SetCharset(Value: TRLMetaFontCharset);
begin
  fCharset:=Value;
end;

procedure TRLMetaFont.SetColor(const Value: TRLMetaColor);
begin
  fColor:=Value;
end;

procedure TRLMetaFont.SetHeight(Value: integer);
begin
  fHeight:=Value;
end;

procedure TRLMetaFont.SetPitch(Value: TRLMetaFontPitch);
begin
  fPitch:=Value;
end;

procedure TRLMetaFont.SetPixelsPerInch(Value: integer);
begin
  fPixelsPerInch:=Value;
end;

procedure TRLMetaFont.SetSize(Value: integer);
begin
  fSize:=Value;
end;

procedure TRLMetaFont.SetStyle(Value: TRLMetaFontStyles);
begin
  fStyle:=Value;
end;

{ TRLGraphicObject }

constructor TRLGraphicObject.Create(aSurface:TRLGraphicSurface);
begin
  fSurface       :=aSurface;
  fBoundsRect    :=ToMetaRect(Rect(0,0,0,0));
  fGroupId       :=CurrentGroupId;
  fGeneratorId   :=0;
  fTag           :=0;
  //
  inherited Create;
  //
  fSurface.fObjects.Add(Self);
end;

destructor TRLGraphicObject.Destroy;
begin
  fSurface.fObjects.Extract(Self);
  //
  inherited;
end;

procedure TRLGraphicObject.SaveToStream(aStream:TStream);
begin
  aStream.Write(fBoundsRect,SizeOf(fBoundsRect));
  aStream.Write(fGroupId,SizeOf(fGroupId));
  aStream.Write(fGeneratorId,SizeOf(fGeneratorId));
end;

procedure TRLGraphicObject.LoadFromStream(aStream:TStream);
begin
  aStream.Read(fBoundsRect,SizeOf(fBoundsRect));
  aStream.Read(fGroupId,SizeOf(fGroupId));
  aStream.Read(fGeneratorId,SizeOf(fGeneratorId));
end;

function TRLGraphicObject.Clone(aSurface: TRLGraphicSurface): TRLGraphicObject;
begin
  Result:=TRLGraphicObjectClass(Self.ClassType).Create(aSurface);
  Result.Assign(Self);
end;

procedure TRLGraphicObject.Assign(aObject: TRLGraphicObject); 
begin
  BoundsRect :=aObject.BoundsRect;
  GroupId    :=aObject.GroupId;
  GeneratorId:=aObject.GeneratorId;
end;

procedure TRLGraphicObject.Offset(aXDesloc,aYDesloc: integer);
begin
  Inc(fBoundsRect.Left  ,aXDesloc);
  Inc(fBoundsRect.Top   ,aYDesloc);
  Inc(fBoundsRect.Right ,aXDesloc);
  Inc(fBoundsRect.Bottom,aYDesloc);
end;

procedure TRLGraphicObject.Inflate(aXFactor, aYFactor: double);
begin
  fBoundsRect.Left  :=Round(fBoundsRect.Left  *aXFactor);
  fBoundsRect.Top   :=Round(fBoundsRect.Top   *aYFactor);
  fBoundsRect.Right :=Round(fBoundsRect.Right *aXFactor);
  fBoundsRect.Bottom:=Round(fBoundsRect.Bottom*aYFactor);
end;

{ TRLPixelObject }

constructor TRLPixelObject.Create(aSurface:TRLGraphicSurface);
begin
  fColor:=ToMetaColor(clBlack);
  //
  inherited;
end;

procedure TRLPixelObject.SaveToStream(aStream:TStream);
begin
  inherited;
  //
  aStream.Write(fColor,SizeOf(fColor));
end;

procedure TRLPixelObject.LoadFromStream(aStream:TStream);
begin
  inherited;
  //
  aStream.Read(fColor,SizeOf(fColor));
end;

procedure TRLPixelObject.Assign(aObject: TRLGraphicObject); 
begin
  inherited Assign(aObject); 
  //
  Color:=TRLPixelObject(aObject).Color;
end;

{ TRLLineObject }

constructor TRLLineObject.Create(aSurface:TRLGraphicSurface);
begin
  fFromPoint:=MetaPoint(0,0);
  fToPoint  :=MetaPoint(0,0);
  fPen      :=nil;
  fBrush    :=nil;
  //
  fPen  :=TRLMetaPen.Create(Self);
  fBrush:=TRLMetaBrush.Create(Self);
  //
  inherited;
end;

destructor TRLLineObject.Destroy;
begin
  inherited;
  //
  FreeObj(fPen);
  FreeObj(fBrush);
end;

procedure TRLLineObject.SaveToStream(aStream:TStream);
begin
  inherited;
  //
  aStream.Write(fFromPoint,SizeOf(fFromPoint));
  aStream.Write(fToPoint,SizeOf(fToPoint));
  fPen.SaveToStream(aStream);
  fBrush.SaveToStream(aStream);
end;

procedure TRLLineObject.LoadFromStream(aStream:TStream);
begin
  inherited;
  //
  aStream.Read(fFromPoint,SizeOf(fFromPoint));
  aStream.Read(fToPoint,SizeOf(fToPoint));
  fPen.LoadFromStream(aStream);
  fBrush.LoadFromStream(aStream);
end;

procedure TRLLineObject.Assign(aObject: TRLGraphicObject); 
begin
  inherited Assign(aObject);
  //
  FromPoint:=TRLLineObject(aObject).FromPoint;
  ToPoint  :=TRLLineObject(aObject).ToPoint;
  Pen      :=TRLLineObject(aObject).Pen;
  Brush    :=TRLLineObject(aObject).Brush;
end;

procedure TRLLineObject.Offset(aXDesloc,aYDesloc: integer); 
begin
  inherited Offset(aXDesloc,aYDesloc);
  //
  Inc(fFromPoint.X,aXDesloc);
  Inc(fFromPoint.Y,aYDesloc);
  Inc(fToPoint.X  ,aXDesloc);
  Inc(fToPoint.Y  ,aYDesloc);
end;

procedure TRLLineObject.Inflate(aXFactor, aYFactor: double);
begin
  inherited Inflate(aXFactor,aYFactor);
  //
  fFromPoint.X:=Round(fFromPoint.X*aXFactor);
  fFromPoint.Y:=Round(fFromPoint.Y*aYFactor);
  fToPoint.X  :=Round(fToPoint.X  *aXFactor);
  fToPoint.Y  :=Round(fToPoint.Y  *aYFactor);
  fPen.Inflate(aXFactor);
end;

procedure TRLLineObject.SetPen(Value:TRLMetaPen);
begin
  fPen.Assign(Value);
end;

procedure TRLLineObject.SetBrush(Value:TRLMetaBrush);
begin
  fBrush.Assign(Value);
end;

{ TRLRectangleObject }

constructor TRLRectangleObject.Create(aSurface:TRLGraphicSurface);
begin
  fPen  :=nil;
  fBrush:=nil;
  //
  fPen  :=TRLMetaPen.Create(Self);
  fBrush:=TRLMetaBrush.Create(Self);
  //
  inherited;
end;

destructor TRLRectangleObject.Destroy;
begin
  inherited;
  //
  FreeObj(fPen);
  FreeObj(fBrush);
end;

procedure TRLRectangleObject.SaveToStream(aStream: TStream);
begin
  inherited;
  //
  fPen.SaveToStream(aStream);
  fBrush.SaveToStream(aStream);
end;

procedure TRLRectangleObject.LoadFromStream(aStream: TStream);
begin
  inherited;
  //
  fPen.LoadFromStream(aStream);
  fBrush.LoadFromStream(aStream);
end;

procedure TRLRectangleObject.Assign(aObject: TRLGraphicObject);
begin
  inherited Assign(aObject);
  //
  Pen  :=TRLRectangleObject(aObject).Pen;
  Brush:=TRLRectangleObject(aObject).Brush;
end;

procedure TRLRectangleObject.Inflate(aXFactor, aYFactor: double);
begin
  inherited Inflate(aXFactor,aYFactor);
  //
  fPen.Inflate(aXFactor);
end;

procedure TRLRectangleObject.SetPen(Value:TRLMetaPen);
begin
  fPen.Assign(Value);
end;

procedure TRLRectangleObject.SetBrush(Value:TRLMetaBrush);
begin
  fBrush.Assign(Value);
end;

{ TRLTextObject }

constructor TRLTextObject.Create(aSurface:TRLGraphicSurface);
begin
  fAlignment:=MetaTextAlignmentLeft;
  fBrush    :=nil;
  fFont     :=nil;
  fLayout   :=MetaTextLayoutTop;
  fOrigin   :=MetaPoint(0,0);
  fText     :=emptystr;
  fTextFlags:=MetaTextFlagAutoSize or MetaTextFlagIntegralHeight;
  //
  fBrush:=TRLMetaBrush.Create(Self);
  fFont :=TRLMetaFont.Create(Self);
  //
  inherited;
end;

destructor TRLTextObject.Destroy;
begin
  inherited;
  //
  FreeObj(fBrush);
  FreeObj(fFont);
end;

procedure TRLTextObject.SaveToStream(aStream: TStream);
var
  len:integer;
begin
  inherited;
  //
  aStream.Write(fAlignment,SizeOf(fAlignment));
  aStream.Write(fLayout,SizeOf(fLayout));
  aStream.Write(fOrigin,SizeOf(fOrigin));
  aStream.Write(fTextFlags,SizeOf(fTextFlags));
  //
  len:=Length(fText);
  aStream.Write(len,SizeOf(len));
  if len>0 then
    aStream.Write(fText[1],len);
  //
  fBrush.SaveToStream(aStream);
  fFont.SaveToStream(aStream);
end;

procedure TRLTextObject.LoadFromStream(aStream: TStream);
var
  len:integer;
begin
  inherited;
  //
  aStream.Read(fAlignment,SizeOf(fAlignment));
  aStream.Read(fLayout,SizeOf(fLayout));
  aStream.Read(fOrigin,SizeOf(fOrigin));
  aStream.Read(fTextFlags,SizeOf(fTextFlags));
  //
  aStream.Read(len,SizeOf(len));
  SetLength(fText,len);
  if len>0 then
    aStream.Read(fText[1],len);
  //
  fBrush.LoadFromStream(aStream);
  fFont.LoadFromStream(aStream);
end;

// processa macros
procedure TRLTextObject.TranslateMacros(var aText:string);
var
  keyword,keyvalue:string;
  macros1,macros2:TStrings;
  i,m:integer;
begin
  macros1:=fSurface.Macros;
  if Assigned(fSurface.fStorage) then
    macros2:=fSurface.fStorage.Macros
  else
    macros2:=nil;
  i:=1;
  while i<=Length(aText) do
    if aText[i]='{' then
    begin
      m:=i;
      while (i<=Length(aText)) and (aText[i]<>'}') do
        Inc(i);
      if i<=Length(aText) then
      begin
        keyword:=Copy(aText,m+1,i-(m+1));
        if macros1.IndexOfName(keyword)<>-1 then
          keyvalue:=macros1.Values[keyword]
        else if Assigned(macros2) and (macros2.IndexOfName(keyword)<>-1) then
          keyvalue:=macros2.Values[keyword]
        else
          continue;  
        Delete(aText,m,i-m+1);
        Insert(keyvalue,aText,m);
        i:=m+Length(keyvalue);
      end;
    end
    else
      Inc(i);
end;

procedure TRLTextObject.Assign(aObject: TRLGraphicObject); 
begin
  inherited Assign(aObject);
  //
  Alignment:=TRLTextObject(aObject).Alignment;
  TextFlags:=TRLTextObject(aObject).TextFlags;
  Brush    :=TRLTextObject(aObject).Brush;
  Font     :=TRLTextObject(aObject).Font;
  Layout   :=TRLTextObject(aObject).Layout;
  Origin   :=TRLTextObject(aObject).Origin;
  Text     :=TRLTextObject(aObject).Text;
end;

procedure TRLTextObject.Offset(aXDesloc,aYDesloc: integer); 
begin
  inherited Offset(aXDesloc,aYDesloc);
  //
  Inc(fOrigin.X,aXDesloc);
  Inc(fOrigin.Y,aYDesloc);
end;

procedure TRLTextObject.Inflate(aXFactor, aYFactor: double);
begin
  inherited Inflate(aXFactor,aYFactor);
  //
  fOrigin.X :=Round(fOrigin.X*aXFactor);
  fOrigin.Y :=Round(fOrigin.Y*aYFactor);
  fFont.Size:=Round(fFont.Size*aYFactor);
end;

procedure TRLTextObject.SetBrush(Value:TRLMetaBrush);
begin
  fBrush.Assign(Value);
end;

procedure TRLTextObject.SetFont(Value:TRLMetaFont);
begin
  fFont.Assign(Value);
end;

function TRLTextObject.GetDisplayText:string;
begin
  Result:=fText;
  TranslateMacros(Result);
end;

{ TRLFillRectObject }

constructor TRLFillRectObject.Create(aSurface:TRLGraphicSurface);
begin
  fBrush:=nil;
  //
  fBrush:=TRLMetaBrush.Create(Self);
  //
  inherited;
end;

destructor TRLFillRectObject.Destroy;
begin
  inherited;
  //
  FreeObj(fBrush);
end;

procedure TRLFillRectObject.SaveToStream(aStream: TStream);
begin
  inherited;
  //
  fBrush.SaveToStream(aStream);
end;

procedure TRLFillRectObject.LoadFromStream(aStream: TStream);
begin
  inherited;
  //
  fBrush.LoadFromStream(aStream);
end;

procedure TRLFillRectObject.Assign(aObject: TRLGraphicObject); 
begin
  inherited Assign(aObject);
  //
  Brush:=TRLFillRectObject(aObject).Brush;
end;

procedure TRLFillRectObject.SetBrush(Value:TRLMetaBrush);
begin
  fBrush.Assign(Value);
end;

{ TRLEllipseObject }

constructor TRLEllipseObject.Create(aSurface:TRLGraphicSurface);
begin
  fPen  :=nil;
  fBrush:=nil;
  //
  fPen  :=TRLMetaPen.Create(Self);
  fBrush:=TRLMetaBrush.Create(Self);
  //
  inherited;
end;

destructor TRLEllipseObject.Destroy;
begin
  inherited;
  //
  FreeObj(fPen);
  FreeObj(fBrush);
end;

procedure TRLEllipseObject.SaveToStream(aStream: TStream);
begin
  inherited;
  //
  fPen.SaveToStream(aStream);
  fBrush.SaveToStream(aStream);
end;

procedure TRLEllipseObject.LoadFromStream(aStream: TStream);
begin
  inherited;
  //
  fPen.LoadFromStream(aStream);
  fBrush.LoadFromStream(aStream);
end;

procedure TRLEllipseObject.Assign(aObject: TRLGraphicObject); 
begin
  inherited Assign(aObject);
  //
  Pen  :=TRLEllipseObject(aObject).Pen;
  Brush:=TRLEllipseObject(aObject).Brush;
end;

procedure TRLEllipseObject.Inflate(aXFactor, aYFactor: double);
begin
  inherited Inflate(aXFactor,aYFactor);
  //
  fPen.Inflate(aXFactor);
end;

procedure TRLEllipseObject.SetPen(Value:TRLMetaPen);
begin
  fPen.Assign(Value);
end;

procedure TRLEllipseObject.SetBrush(Value:TRLMetaBrush);
begin
  fBrush.Assign(Value);
end;

{ TRLPolygonObject }

constructor TRLPolygonObject.Create(aSurface:TRLGraphicSurface);
begin
  fPen  :=nil;
  fBrush:=nil;
  SetLength(fPoints,0);
  //
  fPen  :=TRLMetaPen.Create(Self);
  fBrush:=TRLMetaBrush.Create(Self);
  //
  inherited;
end;

destructor TRLPolygonObject.Destroy;
begin
  inherited;
  //
  FreeObj(fPen);
  FreeObj(fBrush);
end;

procedure TRLPolygonObject.SaveToStream(aStream: TStream);
var
  i,count:integer;
begin
  inherited;
  //
  fPen.SaveToStream(aStream);
  fBrush.SaveToStream(aStream);
  count:=High(fPoints)+1;
  aStream.Write(count,SizeOf(count));
  for i:=0 to count-1 do
    aStream.Write(fPoints[i],SizeOf(fPoints[i]));
end;

procedure TRLPolygonObject.LoadFromStream(aStream: TStream);
var
  i,count:integer;
begin
  inherited;
  //
  fPen.LoadFromStream(aStream);
  fBrush.LoadFromStream(aStream);
  aStream.Read(count,SizeOf(count));
  SetLength(fPoints,count);
  for i:=0 to count-1 do
    aStream.Read(fPoints[i],SizeOf(fPoints[i]));
end;

procedure TRLPolygonObject.Assign(aObject: TRLGraphicObject); 
begin
  inherited Assign(aObject);
  //
  Pen   :=TRLPolygonObject(aObject).Pen;
  Brush :=TRLPolygonObject(aObject).Brush;
  Points:=TRLPolygonObject(aObject).Points;
end;

procedure TRLPolygonObject.Offset(aXDesloc,aYDesloc: integer);
var
  i:integer;
begin
  inherited Offset(aXDesloc,aYDesloc);
  //
  for i:=0 to High(fPoints) do
  begin
    Inc(fPoints[i].X,aXDesloc);
    Inc(fPoints[i].Y,aYDesloc);
  end;
end;

procedure TRLPolygonObject.Inflate(aXFactor, aYFactor: double);
var
  i:integer;
begin
  inherited Inflate(aXFactor,aYFactor);
  //
  for i:=0 to High(fPoints) do
  begin
    fPoints[i].X:=Round(fPoints[i].X*aXFactor);
    fPoints[i].Y:=Round(fPoints[i].Y*aYFactor);
  end;
  fPen.Inflate(aXFactor);
end;

procedure TRLPolygonObject.SetPen(Value:TRLMetaPen);
begin
  fPen.Assign(Value);
end;

procedure TRLPolygonObject.SetBrush(Value:TRLMetaBrush);
begin
  fBrush.Assign(Value);
end;

{ TRLPolylineObject }

constructor TRLPolylineObject.Create(aSurface:TRLGraphicSurface);
begin
  fPen:=nil;
  SetLength(fPoints,0);
  //
  fPen:=TRLMetaPen.Create(Self);
  //
  inherited;
end;

destructor TRLPolylineObject.Destroy;
begin
  inherited;
  //
  FreeObj(fPen);
end;

procedure TRLPolylineObject.SaveToStream(aStream: TStream);
var
  i,count:integer;
begin
  inherited;
  //
  fPen.SaveToStream(aStream);
  count:=High(fPoints)+1;
  aStream.Write(count,SizeOf(count));
  for i:=0 to count-1 do
    aStream.Write(fPoints[i],SizeOf(fPoints[i]));
end;

procedure TRLPolylineObject.LoadFromStream(aStream: TStream);
var
  i,count:integer;
begin
  inherited;
  //
  fPen.LoadFromStream(aStream);
  aStream.Read(count,SizeOf(count));
  SetLength(fPoints,count);
  for i:=0 to count-1 do
    aStream.Read(fPoints[i],SizeOf(fPoints[i]));
end;

procedure TRLPolylineObject.Assign(aObject: TRLGraphicObject); 
begin
  inherited Assign(aObject);
  //
  Pen   :=TRLPolylineObject(aObject).Pen;
  Points:=TRLPolylineObject(aObject).Points;
end;

procedure TRLPolylineObject.Offset(aXDesloc,aYDesloc: integer);
var
  i:integer;
begin
  inherited Offset(aXDesloc,aYDesloc);
  //
  for i:=0 to High(fPoints) do
  begin
    Inc(fPoints[i].X,aXDesloc);
    Inc(fPoints[i].Y,aYDesloc);
  end;
end;

procedure TRLPolylineObject.Inflate(aXFactor,aYFactor:double);
var
  i:integer;
begin
  inherited Inflate(aXFactor,aYFactor);
  //
  for i:=0 to High(fPoints) do
  begin
    fPoints[i].X:=Round(fPoints[i].X*aXFactor);
    fPoints[i].Y:=Round(fPoints[i].Y*aYFactor);
  end;
  fPen.Inflate(aXFactor);
end;

procedure TRLPolylineObject.SetPen(Value:TRLMetaPen);
begin
  fPen.Assign(Value);
end;

{ TRLImageObject }

constructor TRLImageObject.Create(aSurface:TRLGraphicSurface);
begin
  fData  :=emptystr;
  fParity:=false;
  //
  inherited;
end;

procedure TRLImageObject.SaveToStream(aStream: TStream);
var
  len:integer;
begin
  inherited;
  //
  len:=Length(fData);
  aStream.Write(len,SizeOf(len));
  if len>0 then
    aStream.Write(fData[1],len);
  //
  aStream.Write(fParity,SizeOf(fParity));
end;

procedure TRLImageObject.LoadFromStream(aStream: TStream);
var
  len:integer;
begin
  inherited;
  //
  aStream.Read(len,SizeOf(len));
  SetLength(fData,len);
  if len>0 then
    aStream.Read(fData[1],len);
  //
  aStream.Read(fParity,SizeOf(fParity));
end;

procedure TRLImageObject.Assign(aObject: TRLGraphicObject);
begin
  inherited Assign(aObject);
  //
  Data  :=TRLImageObject(aObject).Data;
  Parity:=TRLImageObject(aObject).Parity;
end;

{ TInt64List }

constructor TInt64List.Create;
begin
  inherited Create;
  fCapacity:=0;
  fCount   :=0;
  SetLength(fInt64Array,fCapacity);
end;

destructor TInt64List.Destroy;
begin
  inherited;
  SetLength(fInt64Array,0);
end;

function TInt64List.Add(Value: Int64): integer;
const
  Delta=1024;
begin
  Inc(fCount);
  if fCount>fCapacity then
  begin
    Inc(fCapacity,fCapacity*2+Delta);
    SetLength(fInt64Array,fCapacity);
  end;
  Result:=fCount-1;
  fInt64Array[Result]:=Value;
end;

procedure TInt64List.Clear;
begin
  fCount:=0;
end;

function TInt64List.GetItems(i: integer): Int64;
begin
  if (i<0) or (i>fCount-1) then
    raise Exception.Create('SListIndexError');
  Result:=fInt64Array[i];
end;

procedure TInt64List.SetItems(i: integer; const Value: Int64);
begin
  if (i<0) or (i>fCount-1) then
    raise Exception.Create('SListIndexError');
  fInt64Array[i]:=Value;
end;

end.

