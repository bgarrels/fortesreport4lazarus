{$I RLReport.inc}

{@unit RLFilters - Implementa��o do filtro padr�o de impress�o e classes abstratas para filtros de salvamento e filtros de impress�o.
Portado para o Lazarus - Trabalho inicial de Isaac Trindade da Silva contato tioneobrasil@yahoo.com.br dicas4lazarus@yahoo.com.br
Lazarus Ported - initial work by Isaac 07/2009
}
unit RLFilters;
{$MODE DELPHI}{$H+}
interface

uses
  Classes, SysUtils, Types,
  RLMetaFile, RLConsts, RLTypes, RLUtils, RLFeedBack, RLPrinters;

type

  TRLCustomFilter=class;
  TRLCustomPrintFilter=class;
  TRLCustomSaveFilter=class;

  TRLFilterStyle=(fsEmulateCopies,fsSetupDialog);
  TRLFilterStyles=set of TRLFilterStyle;

  TRLFilterAddVerbProc=procedure(const Caption:string; Verb:integer) of object;

  { TRLCustomFilter }

  {@class TRLCustomFilter - Classe abstrata ancestral para todos os filtros de salvamento e filtros de impress�o.
   @links TRLHTMLFilter, TRLPDFFilter, TRLRichFilter, TRLDraftFilter. }
  TRLCustomFilter=class(TComponent)
  private
    fDisplayName     :string;
    fPages           :TRLGraphicStorage;
    fProgress        :TfrmRLFeedBack;
    fShowProgress    :boolean;
    fCanceled        :boolean;
    fFilterStyle     :TRLFilterStyles;
    fFirstPage       :integer;
    fLastPage        :integer;
    fCopies          :integer;
    fBackgroundMode  :boolean;
    //
    fFilteringFrom   :integer;
    fFilteringTo     :integer;
    fFilteringCopies :integer;
    fFilteringPageNo :integer;
    fFilteringCopyNo :integer;
    fShowFilterDialog:boolean;
    //
    procedure SetPages(const Value:TRLGraphicStorage);
    //
    function IsDisplayName:boolean;
    procedure ProgressCreate;
    procedure ProgressDestroy;
    procedure ProgressPhase;
    procedure ProgressSetMax;
    procedure ProgressStepIt;
    function CanShowProgress:boolean;
    procedure InternalRun;
  protected
    { Protected declarations }
    procedure GetPageVerbs(Page:TRLGraphicStorage; Add:TRLFilterAddVerbProc); virtual;
    procedure ExecutePageVerbs(Page:TRLGraphicStorage; Verb:integer); virtual;
    //
    procedure InternalBeginDoc; virtual; abstract;
    procedure InternalEndDoc; virtual; abstract;
    procedure InternalNewPage; virtual; abstract;
    procedure InternalDrawPage(aPage:TRLGraphicSurface); virtual; abstract;
    //
    procedure Notification(aComponent:TComponent; Operation:TOperation); override;
  public
    { Public declarations }
    constructor Create(aOwner:TComponent); override;
    destructor  Destroy; override;

    {@method GetDisplayLabel - Devolve o nome do filtro para exibi��o em caixas de sele��o. :/}
    function GetDisplayLabel:string; virtual;

    {@method GetDisplayName - Devolve o nome do filtro para exibi��o em tempo de design. :/}
    function GetDisplayName:string;

    {@method BeginDoc - Inicializa o processo de filtragem. :/}
    procedure BeginDoc;

    {@method EndDoc - Finaliza o processo de filtragem. :/}
    procedure EndDoc;

    {@method NewPage - Adiciona uma nova p�gina. :/}
    procedure NewPage;

    {@method DrawPage - Desenha o conte�do da superf�cie na p�gina corrente. :/}
    procedure DrawPage(aPage:TRLGraphicSurface);

    {@method Run - Processa as p�ginas atrav�s do filtro.
     A lista de p�ginas aPages pode ser obtida na prop Pages de um TRLReport ap�s a prepara��o do relat�rio, ou de
     modo avulso criando uma instancia do TRLGraphicStorage e carregando um relat�rio do disco.
     Os par�metros aFirstPage e aLastPage s�o opcionais e indicam o intervalo de p�ginas a processar.
     @links TRLGraphicStorage, TRLCustomReport.Pages. :/}
    procedure Run;

    procedure ExecuteDialog; virtual;
    property FilterStyle:TRLFilterStyles read fFilterStyle write fFilterStyle;
    {@prop DisplayName - Retorna o nome para exibi��o em caixas de sele��o. :/}
    property DisplayName:string read GetDisplayName write fDisplayName stored IsDisplayName;

    {@prop Pages - Refer�ncia � cole��o de p�ginas a filtrar. :/}
    property Pages:TRLGraphicStorage read fPages write SetPages;
    property FirstPage:integer read fFirstPage write fFirstPage;
    property LastPage :integer read fLastPage  write fLastPage;
    property Copies   :integer read fCopies    write fCopies;

    {@prop BackgroundMode - Determina a filtragem de p�ginas em segundo plano. :/}
    property BackgroundMode:boolean read fBackgroundMode write fBackgroundMode;

    {@prop Canceled - Indica se o processo foi interrompido pelo usu�rio. :/}
    property Canceled:boolean read fCanceled write fCanceled;

    {@prop ShowProgress - Mostra barra de progresso do salvamento ou impress�o. :/}
    property ShowProgress:boolean read fShowProgress write fShowProgress default True;

    {@prop ShowFilterDialog - Oferece op��es de configura��o do filtro para o usu�rio final. :/}
    property ShowFilterDialog:boolean read fShowFilterDialog write fShowFilterDialog default True;
  end;
  {/@class}
  

  { TRLCustomPrintFilter }

  {@class TRLCustomPrintFilter - Classe base para filtros de impress�o.
   @links TRLDraftFilter. }
  TRLCustomPrintFilter=class(TRLCustomFilter)
  public
  end;
  {/@class}
  

  { TRLCustomSaveFilter }

  {@class TRLCustomSaveFilter - Classe base para filtros de salvamento.
   @links TRLHTMLFilter, TRLPDFFilter, TRLRichFilter. }
  TRLCustomSaveFilter=class(TRLCustomFilter)
  private
    fFileName  :string;
    fDefaultExt:string;
  public
    constructor Create(aOwner:TComponent); override;

    {@prop FileName - Nome do arquivo destino para o filtro de salvamento. :/}
    property FileName:string read fFileName write fFileName;

    {@prop DefaultExt - Extens�o padr�o para o arquivo destino. :/}
    property DefaultExt:string read fDefaultExt write fDefaultExt;
  end;
  {/@class}
  
{@var ActiveFilters - Lista de filtros ativos.
 Esta lista cont�m refer�ncias a todos os filtros de impress�o e filtros de salvamento j� instanciados.
 @links TRLCustomFilter, SelectedFilter. :/}
var ActiveFilters:TList=nil;

{@func SaveFilterByFileName - Retorna uma refer�ncia para um filtro de salvamento j� instanciado baseado na extens�o
 do nome de arquivo informado.
 @links TRLCustomSaveFilter. :/}
function SaveFilterByFileName(const aFileName:string):TRLCustomSaveFilter;

{/@unit}

implementation

uses
  RLSpoolFilter;

function SaveFilterByFileName(const aFileName:string):TRLCustomSaveFilter;
var
  f:TRLCustomSaveFilter;
  e1,e2:string;
  i:integer;
begin
  Result:=nil;
  e1:=FormatFileExt(ExtractFileExt(aFileName));
  for i:=0 to ActiveFilters.Count-1 do
    if TObject(ActiveFilters[i]) is TRLCustomSaveFilter then
    begin
      f :=TRLCustomSaveFilter(ActiveFilters[i]);
      e2:=FormatFileExt(f.DefaultExt);
      if AnsiSameText(e1,e2) then
      begin
        Result:=f;
        break;
      end;
    end;
end;

{ TRLCustomFilter }

constructor TRLCustomFilter.Create(aOwner: TComponent);
begin
  fDisplayName     :=emptystr;
  fPages           :=nil;
  fProgress        :=nil;
  fShowProgress    :=True;
  fShowFilterDialog:=True;
  fCanceled        :=False;
  fFilterStyle     :=[];
  fFirstPage       :=1;
  fLastPage        :=LastPage;
  fCopies          :=1;
  fBackgroundMode  :=False;
  //
  fFilteringFrom   :=0;
  fFilteringTo     :=0;
  fFilteringCopies :=0;
  fFilteringPageNo :=0;
  fFilteringCopyNo :=0;
  //
  inherited;
  //
  ActiveFilters.Add(Self);
end;

destructor TRLCustomFilter.Destroy;
begin
  ActiveFilters.Extract(Self);
  if Assigned(fPages) then
    fPages.Unlink(Self);
  FreeObj(fProgress);
  //
  inherited;
end;

procedure TRLCustomFilter.Notification(aComponent: TComponent; Operation: TOperation);
begin
  inherited;
  //
  if Operation=opRemove then
    if aComponent=fPages then
      fPages:=nil;
end;

procedure TRLCustomFilter.BeginDoc;
begin
  InternalBeginDoc;
end;

procedure TRLCustomFilter.EndDoc;
begin
  InternalEndDoc;
end;

procedure TRLCustomFilter.NewPage;
begin
  InternalNewPage;
end;

procedure TRLCustomFilter.DrawPage(aPage: TRLGraphicSurface);
begin
  InternalDrawPage(aPage);
end;

procedure TRLCustomFilter.Run;
begin
  if BackgroundMode then
    ThreadIt(InternalRun).Resume
  else
    InternalRun;  
end;

procedure TRLCustomFilter.InternalRun;
var
  MustEject:boolean;
  PageObj  :TRLGraphicSurface;
begin
  fCanceled    :=False;
  fFilteringFrom:=FirstPage;
  fFilteringTo  :=LastPage;
  if fsEmulateCopies in fFilterStyle then
    fFilteringCopies:=Self.Copies
  else
    fFilteringCopies:=1;
  //
  if CanShowProgress then
    ProgressCreate;
  try
    if CanShowProgress then
      ProgressSetMax;
    BeginDoc;
    try
      MustEject:=False;
      fFilteringCopyNo:=1;
      repeat
        fFilteringPageNo:=fFilteringFrom;
        repeat
          // termina se ultrapassar a p�gina final
          if (fFilteringTo>=fFilteringFrom) and (fFilteringPageNo>fFilteringTo) then
            Break;
          // cria uma nova folha apenas ap�s a primeira   
          if MustEject then
            NewPage;
          MustEject:=True;
          // espera a p�gina ficar dispon�vel e devolve a refer�ncia � ela, no caso de impress�o imediata
          PageObj:=Pages.WaitPage(fFilteringPageNo-1);
          if not Assigned(PageObj) then
            Exit;
          // filtra a p�gina  
          DrawPage(PageObj);
          // progresso por p�gina e check de interrup��o
          if CanShowProgress then
            ProgressPhase;
          if CanShowProgress then
            ProgressStepIt;
          if Pages.Canceled then
            Abort;
          if Self.Canceled then
            Abort;
          Inc(fFilteringPageNo);  
        until False;
        Inc(fFilteringCopyNo);
      until fFilteringCopyNo>fFilteringCopies;
    finally
      EndDoc;
    end;
  finally
    Pages.Cancel;  
    if CanShowProgress then
      ProgressDestroy;
  end;
end;

function TRLCustomFilter.GetDisplayName:string;
begin
  if fDisplayName=emptystr then
    Result:=Name
  else
    Result:=fDisplayName;
end;

function TRLCustomFilter.GetDisplayLabel: string;
begin
  Result:=GetDisplayName;
end;

function TRLCustomFilter.IsDisplayName:boolean;
begin
  Result:=(GetDisplayName<>Name);
end;

procedure TRLCustomFilter.ProgressCreate;
var
  s:string;
begin
  if Self is TRLCustomPrintFilter then
    s:=LS_PrintingInProgressStr
  else
    s:=LS_FilterInProgressStr;
  fProgress:=TfrmRLFeedBack.Create(s);
  fProgress.Show;
  fProgress.SetFocus;
end;

procedure TRLCustomFilter.ProgressDestroy;
begin
  FreeObj(fProgress);
end;

procedure TRLCustomFilter.ProgressPhase;
var
  s:string;
begin
  s:=IntToStr(fFilteringPageNo);
  if fFilteringCopies>1 then
    s:=IntToStr(fFilteringCopyNo)+' - '+s;
  fProgress.SetPhase(LS_PageStr+' '+s);
end;

procedure TRLCustomFilter.ProgressSetMax;
begin
  if fFilteringTo>=fFilteringFrom then
    fProgress.SetMax((fFilteringTo-fFilteringFrom+1)*fFilteringCopies)
  else
    fProgress.SetMax(fFilteringCopies);
end;

procedure TRLCustomFilter.ProgressStepIt;
begin
  fProgress.StepIt;
  if fProgress.Canceled then
    Abort;
end;

procedure TRLCustomFilter.SetPages(const Value:TRLGraphicStorage);
begin
  if Assigned(fPages) then
  begin
    fPages.RemoveFreeNotification(Self);
    fPages.Unlink(Self);
  end;
  fPages:=Value;
  if Assigned(fPages) then
  begin
    fPages.FreeNotification(Self);
    fPages.Link(Self);
  end;  
end;

function TRLCustomFilter.CanShowProgress: boolean;
begin
  Result:=ShowProgress and IsMainThread;
end;

procedure TRLCustomFilter.ExecutePageVerbs(Page: TRLGraphicStorage;
  Verb: integer);
begin
end;

procedure TRLCustomFilter.GetPageVerbs(Page: TRLGraphicStorage;
  Add: TRLFilterAddVerbProc);
begin
end;

procedure TRLCustomFilter.ExecuteDialog;
begin
end;

{ TRLCustomSaveFilter }

constructor TRLCustomSaveFilter.Create(aOwner: TComponent);
begin
  fFileName  :=emptystr;
  fDefaultExt:=emptystr;
  //
  inherited;
end;

initialization
  // filter instance list
  ActiveFilters:=TList.Create;

finalization
  ActiveFilters.free;

end.

