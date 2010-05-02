{$I RLReport.inc}

{@unit RLFilters - Implementação do filtro padrão de impressão e classes abstratas para filtros de salvamento e filtros de impressão.
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

  {@class TRLCustomFilter - Classe abstrata ancestral para todos os filtros de salvamento e filtros de impressão.
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

    {@method GetDisplayLabel - Devolve o nome do filtro para exibição em caixas de seleção. :/}
    function GetDisplayLabel:string; virtual;

    {@method GetDisplayName - Devolve o nome do filtro para exibição em tempo de design. :/}
    function GetDisplayName:string;

    {@method BeginDoc - Inicializa o processo de filtragem. :/}
    procedure BeginDoc;

    {@method EndDoc - Finaliza o processo de filtragem. :/}
    procedure EndDoc;

    {@method NewPage - Adiciona uma nova página. :/}
    procedure NewPage;

    {@method DrawPage - Desenha o conteúdo da superfície na página corrente. :/}
    procedure DrawPage(aPage:TRLGraphicSurface);

    {@method Run - Processa as páginas através do filtro.
     A lista de páginas aPages pode ser obtida na prop Pages de um TRLReport após a preparação do relatório, ou de
     modo avulso criando uma instancia do TRLGraphicStorage e carregando um relatório do disco.
     Os parâmetros aFirstPage e aLastPage são opcionais e indicam o intervalo de páginas a processar.
     @links TRLGraphicStorage, TRLCustomReport.Pages. :/}
    procedure Run;

    procedure ExecuteDialog; virtual;
    property FilterStyle:TRLFilterStyles read fFilterStyle write fFilterStyle;
    {@prop DisplayName - Retorna o nome para exibição em caixas de seleção. :/}
    property DisplayName:string read GetDisplayName write fDisplayName stored IsDisplayName;

    {@prop Pages - Referência à coleção de páginas a filtrar. :/}
    property Pages:TRLGraphicStorage read fPages write SetPages;
    property FirstPage:integer read fFirstPage write fFirstPage;
    property LastPage :integer read fLastPage  write fLastPage;
    property Copies   :integer read fCopies    write fCopies;

    {@prop BackgroundMode - Determina a filtragem de páginas em segundo plano. :/}
    property BackgroundMode:boolean read fBackgroundMode write fBackgroundMode;

    {@prop Canceled - Indica se o processo foi interrompido pelo usuário. :/}
    property Canceled:boolean read fCanceled write fCanceled;

    {@prop ShowProgress - Mostra barra de progresso do salvamento ou impressão. :/}
    property ShowProgress:boolean read fShowProgress write fShowProgress default True;

    {@prop ShowFilterDialog - Oferece opções de configuração do filtro para o usuário final. :/}
    property ShowFilterDialog:boolean read fShowFilterDialog write fShowFilterDialog default True;
  end;
  {/@class}
  

  { TRLCustomPrintFilter }

  {@class TRLCustomPrintFilter - Classe base para filtros de impressão.
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

    {@prop DefaultExt - Extensão padrão para o arquivo destino. :/}
    property DefaultExt:string read fDefaultExt write fDefaultExt;
  end;
  {/@class}
  
{@var ActiveFilters - Lista de filtros ativos.
 Esta lista contém referências a todos os filtros de impressão e filtros de salvamento já instanciados.
 @links TRLCustomFilter, SelectedFilter. :/}
var ActiveFilters:TList=nil;

{@func SaveFilterByFileName - Retorna uma referência para um filtro de salvamento já instanciado baseado na extensão
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
          // termina se ultrapassar a página final
          if (fFilteringTo>=fFilteringFrom) and (fFilteringPageNo>fFilteringTo) then
            Break;
          // cria uma nova folha apenas após a primeira   
          if MustEject then
            NewPage;
          MustEject:=True;
          // espera a página ficar disponível e devolve a referência à ela, no caso de impressão imediata
          PageObj:=Pages.WaitPage(fFilteringPageNo-1);
          if not Assigned(PageObj) then
            Exit;
          // filtra a página  
          DrawPage(PageObj);
          // progresso por página e check de interrupção
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

