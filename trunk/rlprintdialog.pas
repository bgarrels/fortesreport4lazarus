{$I RLReport.inc}

{@unit RLPrintDialog - Implementa��o do di�logo de impress�o e sua classe de setup.
Portado para o Lazarus - Trabalho inicial de Isaac Trindade da Silva contato tioneobrasil@yahoo.com.br dicas4lazarus@yahoo.com.br
Lazarus Ported - initial work by Isaac 07/2009
}
unit RLPrintDialog;
{$MODE DELPHI}{$H+}
interface

uses
  LCLIntf, LCLType, Classes, SysUtils, LResources,
  LMessages, Graphics, Controls, Forms, Dialogs, StdCtrls,
  RLFilters, RLConsts, RLPrinters, RLTypes, RLSpoolFilter;

type
  {@type TRLPrintDialogOptions - Op��es de configura��o do di�logo de impress�o.
  Pode ser um conjunto dos seguintes valores:
  rpoPrintToFile - Mostrar a op��o de impress�o em arquivo;
  rpoPageNums - Mostrar a sele��o de p�ginas por intervalo;
  rpoSelection - Mostrar a sele��o de p�ginas por n�meros espec�ficos;
  rpoWarning - Exibir mensagens de advert�ncia;
  rpoHelp - Utilizar ajuda do sistema;
  rpoDisablePrintToFile - Desabilitar op��o de impress�o em arquivo;
  rpoDisableCopies - Desabilitar a op��o de n�mero de c�pias;
  rpoDisableBackgroundMode - Desabilitar a op��o de impress�o em segundo plano.
  rpoprintdirect - Ao sair habilita impriss�o imediata
  :}
  TRLPrintDialogOption =(rpoPrintToFile,rpoPageNums,rpoSelection,rpoWarning,rpoHelp,
    rpoDisablePrintToFile,rpoDisableCopies,rpoDisableBackgroundMode, rpoprintdirect);
  TRLPrintDialogOptions=set of TRLPrintDialogOption;
  {/@type}

  {@type TRLPrintRange - Op��es para o modo sele��o de p�ginas.
  Pode ser um dos seguintes valores:
  rprAllPages - Imprimir todas as p�ginas;
  rprSelection - Imprimir as p�ginas de n�meros indicados;
  rprPageNums - Imprimir o intervalo de p�ginas indicado.
  :/}
  TRLPrintRange=(rprAllPages,rprSelection,rprPageNums);

const
  DefaultPrintOptions=[rpoPrintToFile,rpoPageNums,rpoSelection,rpoWarning,rpoDisablePrintToFile];

type

  { TRLPrintDialog }

  TRLPrintDialog = class(TForm)
    SpeedButtonSetup: TButton;
    ButtonImprimir: TButton;
    ButtonAplicar: TButton;
    GroupBoxPrinter: TGroupBox;
    ComboBoxPrinterNames: TComboBox;
    LabelPrinterName: TLabel;
    GroupBoxPages: TGroupBox;
    GroupBoxCopies: TGroupBox;
    ButtonCancel: TButton;
    RadioButtonPagesAll: TRadioButton;
    RadioButtonPagesInterval: TRadioButton;
    RadioButtonPagesSelect: TRadioButton;
    LabelFromPage: TLabel;
    EditFromPage: TEdit;
    LabelToPage: TLabel;
    EditToPage: TEdit;
    LabelCopies: TLabel;
    EditCopies: TEdit;
    CheckBoxPrintToFile: TCheckBox;
    ComboBoxFilters: TComboBox;
    LabelFilterName: TLabel;
    CheckBoxBackgroundMode: TCheckBox;
    ButtonOptions: TButton;
    procedure EditFromToPageChange(Sender: TObject);
    procedure ComboBoxFiltersChange(Sender: TObject);
    procedure ButtonOptionsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SpeedButtonSetupClick(Sender: TObject);
    procedure ButtonImprimirClick(Sender: TObject);
  private
    { Private declarations }
    procedure LoadEditors;
    procedure SaveEditors;
    procedure LoadPrinterList;
    procedure LoadFilterList;
  protected
    { Protected declarations }
  public
    function Execute:boolean;
  end;

  {@class TRLPrintDialogSetup - Op��es do di�logo de impress�o.
   O di�logo de impress�o obedecer� as configura��es da inst�ncia deste componente. Com ele, �
   poss�vel estabelecer o n�mero de c�pias inicial e configurar alguns itens de comportamento do
   di�logo de impress�o.
   @pub}
  TRLPrintDialogSetup=class(TComponent)
  private
    { Private declarations }
    function  GetCopies: integer;
    function  GetOptions: TRLPrintDialogOptions;
    function  GetBackgroundMode: boolean;
    procedure SetCopies(const Value: integer);
    procedure SetOptions(const Value: TRLPrintDialogOptions);
    procedure SetBackgroundMode(const Value: boolean);
    function  GetFilter: TRLCustomPrintFilter;
    function  GetPrintToFile: boolean;
    procedure SetFilter(const Value: TRLCustomPrintFilter);
    procedure SetPrintToFile(const Value: boolean);
  public
    { Public declarations }
  published
    {@prop Options - Op��es diversas do di�logo de impress�o. @links TRLPrintDialogOptions. :/}
    property Options:TRLPrintDialogOptions read GetOptions write SetOptions default DefaultPrintOptions;
    {@prop Copies - Indica o valor inicial para o n�mero de c�pias. :/}
    property Copies:integer read GetCopies write SetCopies default 1;
    {@prop BackgroundMode - Indica o valor inicial para a op��o de imprimir em segundo plano. :/}
    property BackgroundMode:boolean read GetBackgroundMode write SetBackgroundMode default False;
    {@prop PrintToFile - Indica o estado inicial para a op��o de imprimir em arquivo. :/}
    property PrintToFile:boolean read GetPrintToFile write SetPrintToFile default False;
    {@prop Filter - Indica o filtro a oferecer no di�logo. @links TRLCustomPrintFilter:/}
    property Filter:TRLCustomPrintFilter read GetFilter write SetFilter;
  end;
  {/@class}

  {@class TRLPrintParams - Par�metros de impress�o.}
  TRLPrintParams=class(TComponent)
  private
    fMaxPage       :integer;
    fToPage        :integer;
    fMinPage       :integer;
    fFromPage      :integer;
    fOptions       :TRLPrintDialogOptions;
    fPrintRange    :TRLPrintRange;
    fPrintToFile   :boolean;
    fBackgroundMode:boolean;
    fFileName      :string;
    fOrientation   :TRLPageOrientation;
    fFilter        :TRLCustomPrintFilter;
    fHelpContext   :integer;
    fCopies        :integer;
    //
    procedure SetMaxPage(const Value: integer);
    procedure SetFilter(const Value: TRLCustomPrintFilter);
    procedure SetCopies(const Value: integer);
  protected
    { Protected declarations }
    procedure Notification(Component:TComponent; Operation:TOperation); override;
  public
    { Public declarations }
    constructor Create(aOwner:TComponent); override;
    {@method Clear - Preenche todas as props com valores default.:/}
    procedure Clear;
    {@method Apply - Aplica as configura��es ao relat�rio em andamento.:/}
    procedure Apply;
  published
    {@prop Options - Op��es diversas do di�logo de impress�o. @links TRLPrintDialogOptions. :/}
    property  Options          :TRLPrintDialogOptions read fOptions           write fOptions default DefaultPrintOptions;
    {@prop MaxPage - M�ximo para n�mero de p�gina final. :/}
    property  MaxPage          :integer               read fMaxPage           write SetMaxPage;
    {@prop MinPage - M�nimo para n�mero de p�gina inicial. :/}
    property  MinPage          :integer               read fMinPage           write fMinPage;
    {@prop FromPage - N�mero selecionado pelo usu�rio como p�gina inicial. :/}
    property  FromPage         :integer               read fFromPage          write fFromPage;
    {@prop ToPage - N�mero selecionado pelo usu�rio como p�gina final. :/}
    property  ToPage           :integer               read fToPage            write fToPage;
    {@prop PrintRange - Modo de sele��o de p�ginas. @links TRLPrintRange:/}
    property  PrintRange       :TRLPrintRange         read fPrintRange        write fPrintRange;
    {@prop Copies - Indica o valor inicial para o n�mero de c�pias. :/}
    property  Copies           :integer               read fCopies            write SetCopies default 1;
    {@prop PrintToFile - Imprimir para arquivo.:/}
    property  PrintToFile      :boolean               read fPrintToFile       write fPrintToFile;
    {@prop BackgroundMode - Imprimir em segundo plano.:/}
    property  BackgroundMode   :boolean               read fBackgroundMode    write fBackgroundMode;
    {@prop FileName - Nome do arquivo a gerar.:/}
    property  FileName         :string                read fFileName          write fFileName;
    {@prop Orientation - Orienta��o do papel. @links TRLPageOrientation:/}
    property  Orientation      :TRLPageOrientation    read fOrientation       write fOrientation;
    {@prop Filter - Filtro atualmente selecionado. @links TRLCustomPrintFilter:/}
    property  Filter           :TRLCustomPrintFilter  read fFilter            write SetFilter;
    {@prop HelpContext - Contexto de ajuda.:/}
    property  HelpContext      :integer               read fHelpContext       write fHelpContext;
  end;
  {/@class}

{@var PrintParams - Par�metros de impress�o atuais. @links TRLPrintParams:/}
var PrintParams:TRLPrintParams=nil;

{/@unit}

implementation

// UTILS

function IntToEmptyStr(aInt:integer):string;
begin
  if aInt=0 then
    Result:=emptystr
  else
    Result:=IntToStr(aInt);
end;

function EmptyStrToInt(const aStr:string):integer;
begin
  Result:=StrToIntDef(aStr,0);
end;

{ TRLPrintDialog }

// PUBLIC

function TRLPrintDialog.Execute:boolean;
begin
  LoadPrinterList;
  LoadFilterList;
  LoadEditors;
  ActiveControl:=ComboBoxPrinterNames;
  Result:=(ShowModal=mrOk);
  if Result then
    SaveEditors;
end;

// PRIVATE

procedure TRLPrintDialog.LoadPrinterList;
var
  i,j:integer;
  n,p:string;
begin
  ComboBoxPrinterNames.Items.Clear;
  j:=0;
  RLPrinter.Refresh;
  for i:=0 to RLPrinter.Printers.Count-1 do
  begin
    n:=RLPrinter.PrinterNames[i];
    if n=RLPrinter.PrinterName then
      j:=i;
    p:=RLPrinter.PrinterPorts[i];
    if (p<>emptystr) and (p<>n) then
      n:=n+' '+LS_AtStr+' '+p;
    ComboBoxPrinterNames.Items.Add(n);
  end;
  ComboBoxPrinterNames.ItemIndex:=j
end;

procedure TRLPrintDialog.LoadFilterList;
var
  i,j,p:integer;
  n:string;
  f:TRLCustomPrintFilter;
begin
  ComboBoxFilters.Items.Clear;
  ComboBoxFilters.Items.AddObject(LS_DefaultStr,nil);
  //
  j:=0;
  for i:=0 to ActiveFilters.Count-1 do
    if TObject(ActiveFilters[i]) is TRLCustomPrintFilter then
    begin
      f:=TRLCustomPrintFilter(ActiveFilters[i]);
      n:=f.GetDisplayLabel;
      if n<>emptystr then
      begin
        p:=ComboBoxFilters.Items.AddObject(n,f);
        if Assigned(PrintParams.Filter) and (PrintParams.Filter=f) then
          j:=p;
      end;
    end;
  //
  ComboBoxFilters.ItemIndex:=j;
  if ComboBoxFilters.Items.Count<=1 then
  begin
    ComboBoxFilters.Enabled:=False;
    ComboBoxFilters.Color  :=Self.Color;
  end;
  ComboBoxFiltersChange(ComboBoxFilters);
end;

procedure TRLPrintDialog.LoadEditors;
const
  StateColors:array[boolean] of TColor=(clBtnFace,clWindow);
begin
  case PrintParams.PrintRange of
    rprAllPages : RadioButtonPagesAll.Checked     :=True;
    rprSelection: RadioButtonPagesSelect.Checked  :=True;
    rprPageNums : RadioButtonPagesInterval.Checked:=True;
  end;
  EditFromPage.Text               :=IntToEmptyStr(PrintParams.FromPage);
  EditToPage.Text                 :=IntToEmptyStr(PrintParams.MaxPage);
  EditCopies.Text                 :=IntToEmptyStr(PrintParams.Copies);
  EditCopies.Enabled              :=not (rpoDisableCopies in PrintParams.Options);
  EditCopies.Color                :=StateColors[EditCopies.Enabled];
  CheckBoxPrintToFile.Visible     :=(rpoPrintToFile in PrintParams.Options);
  CheckBoxPrintToFile.Enabled     :=not (rpoDisablePrintToFile in PrintParams.Options);
  CheckBoxPrintToFile.Checked     :=PrintParams.PrintToFile;
  CheckBoxBackgroundMode.Enabled  :=not (rpoDisableBackgroundMode in PrintParams.Options);
  CheckBoxBackgroundMode.Checked  :=PrintParams.BackgroundMode;
  RadioButtonPagesInterval.Enabled:=(rpoPageNums in PrintParams.Options);
  EditFromPage.Enabled            :=(rpoPageNums in PrintParams.Options);
  EditFromPage.Color              :=StateColors[EditFromPage.Enabled];
  EditToPage.Enabled              :=(rpoPageNums in PrintParams.Options);
  EditToPage.Color                :=StateColors[EditToPage.Enabled];
  RadioButtonPagesSelect.Enabled  :=(rpoSelection in PrintParams.Options);
  if rpoHelp in PrintParams.Options then
    BorderIcons:=BorderIcons+[biHelp]
  else
    BorderIcons:=BorderIcons-[biHelp];
end;

procedure TRLPrintDialog.SaveEditors;
begin
  if RadioButtonPagesAll.Checked then
    PrintParams.PrintRange:=rprAllPages
  else if RadioButtonPagesSelect.Checked then
    PrintParams.PrintRange:=rprSelection
  else if RadioButtonPagesInterval.Checked then
    PrintParams.PrintRange:=rprPageNums;
  case PrintParams.PrintRange of
    rprAllPages : begin
                    PrintParams.FromPage:=PrintParams.MinPage;
                    PrintParams.ToPage  :=PrintParams.MaxPage;
                  end;
    rprSelection: begin
                    PrintParams.FromPage:=EmptyStrToInt(EditFromPage.Text);
                    PrintParams.ToPage  :=PrintParams.FromPage;
                  end;
    rprPageNums : begin
                    PrintParams.FromPage:=EmptyStrToInt(EditFromPage.Text);
                    PrintParams.ToPage  :=EmptyStrToInt(EditToPage.Text);
                  end;
  end;
  PrintParams.Copies        :=EmptyStrToInt(EditCopies.Text);
  PrintParams.PrintToFile   :=CheckBoxPrintToFile.Checked;
  PrintParams.BackgroundMode:=CheckBoxBackgroundMode.Checked;
  //
  if (ComboBoxPrinterNames.ItemIndex<>-1) and (RLPrinter.PrinterIndex<>ComboBoxPrinterNames.ItemIndex) then begin
      RLPrinter.PrinterIndex:=ComboBoxPrinterNames.ItemIndex;
      {$IFDEF MSWINDOWS}
      {$IFNDEF FPC}
      RLPrinter.CreateDevM;
      {$ENDIF}
      {$ENDIF}
      end;
  if ComboBoxFilters.ItemIndex<>0 then
    PrintParams.Filter:=TRLCustomPrintFilter(ComboBoxFilters.Items.Objects[ComboBoxFilters.ItemIndex])
  else
    PrintParams.Filter:=nil;
  //
  RLPrinter.Copies:=PrintParams.Copies;
end;

// EVENTS

procedure TRLPrintDialog.EditFromToPageChange(Sender: TObject);
begin
  if not RadioButtonPagesInterval.Checked then
    RadioButtonPagesInterval.Checked:=True;
end;

procedure TRLPrintDialog.ComboBoxFiltersChange(Sender: TObject);
var
  p:TRLCustomPrintFilter;
begin
  if ComboBoxFilters.ItemIndex=-1 then
    p:=nil
  else
    p:=TRLCustomPrintFilter(ComboBoxFilters.Items.Objects[ComboBoxFilters.ItemIndex]);
  ButtonOptions.Enabled:=(p<>nil) and (fsSetupDialog in p.FilterStyle);
end;

procedure TRLPrintDialog.ButtonOptionsClick(Sender: TObject);
var
  p:TRLCustomPrintFilter;
begin
  if ComboBoxFilters.ItemIndex=-1 then
    p:=nil
  else
    p:=TRLCustomPrintFilter(ComboBoxFilters.Items.Objects[ComboBoxFilters.ItemIndex]);
  if (p<>nil) and (fsSetupDialog in p.FilterStyle) then
    p.ExecuteDialog;
end;

procedure TRLPrintDialog.FormCreate(Sender: TObject);
begin
  Caption := LS_PrintStr;
  GroupBoxPrinter.Caption := ' '+LS_PrinterStr+' ';
  LabelPrinterName.Caption := '&'+LS_NameStr+':';
  LabelFilterName.Caption := LS_UseFilterStr;
  ButtonOptions.Caption := LS_OptionsStr;
  CheckBoxPrintToFile.Caption := LS_PrintToFileStr;
  CheckBoxBackgroundMode.Caption := LS_PrintInBackgroundStr;
  GroupBoxPages.Caption := ' ' + LS_PageRangeStr + ' ';
  LabelFromPage.Caption := LS_OfStr;
  LabelToPage.Caption := '&'+LS_RangeToStr;
  RadioButtonPagesAll.Caption := LS_AllStr;
  RadioButtonPagesInterval.Caption := LS_PagesStr;
  RadioButtonPagesSelect.Caption := LS_SelectionStr;
  GroupBoxCopies.Caption := ' ' + LS_CopiesStr + ' ';
  LabelCopies.Caption := LS_NumberOfCopiesStr+':';
  ButtonAplicar.Caption := Ls_Aplicar;
  ButtonCancel.Caption := LS_CancelStr;
  SpeedButtonSetup.Caption := Ls_Propriedades;
  ButtonImprimir.Caption := LS_PrintStr;
end;

{ TRLPrintDialogSetup }

function TRLPrintDialogSetup.GetCopies: integer;
begin
  Result:=PrintParams.Copies;
end;

function TRLPrintDialogSetup.GetFilter: TRLCustomPrintFilter;
begin
  Result:=PrintParams.Filter;
end;

function TRLPrintDialogSetup.GetOptions: TRLPrintDialogOptions;
begin
  Result:=PrintParams.Options;
end;

function TRLPrintDialogSetup.GetBackgroundMode: boolean;
begin
  Result:=PrintParams.BackgroundMode;
end;

function TRLPrintDialogSetup.GetPrintToFile: boolean;
begin
  Result:=PrintParams.PrintToFile;
end;

procedure TRLPrintDialogSetup.SetCopies(const Value: integer);
begin
  PrintParams.Copies:=Value;
end;

procedure TRLPrintDialogSetup.SetFilter(const Value: TRLCustomPrintFilter);
begin
  PrintParams.Filter:=Value;
end;

procedure TRLPrintDialogSetup.SetOptions(const Value: TRLPrintDialogOptions);
begin
  PrintParams.Options:=Value;
end;

procedure TRLPrintDialogSetup.SetBackgroundMode(const Value: boolean);
begin
  PrintParams.BackgroundMode:=Value;
end;

procedure TRLPrintDialogSetup.SetPrintToFile(const Value: boolean);
begin
  PrintParams.PrintToFile:=Value;
end;

{ TRLPrintParams }

constructor TRLPrintParams.Create(aOwner: TComponent);
begin
  fOptions       :=DefaultPrintOptions;
  fPrintToFile   :=False;
  fBackgroundMode:=False;
  fFilter        :=nil;
  //
  inherited;
end;

procedure TRLPrintParams.Notification(Component: TComponent;
  Operation: TOperation);
begin
  inherited;
  //
  if Operation=opRemove then
    if Component=fFilter then
      fFilter:=nil;
end;

procedure TRLPrintParams.Clear;
begin
  fMinPage    :=1;
  fMaxPage    :=fToPage;
  fFromPage   :=fMinPage;
  fToPage     :=fMaxPage;
  fPrintRange :=rprAllPages;
  fFileName   :=emptystr;
  fHelpContext:=0;
  fCopies     :=RLPrinter.Copies;
  fOptions    :=PrintParams.Options - [rpoprintdirect];
end;

procedure TRLPrintParams.SetMaxPage(const Value: integer);
begin
  if fToPage=fMaxPage then
    fToPage:=Value;
  fMaxPage:=Value;
end;

procedure TRLPrintParams.SetFilter(const Value: TRLCustomPrintFilter);
begin
  if Assigned(fFilter) then
    fFilter.RemoveFreeNotification(Self);
  fFilter:=Value;
  if Assigned(fFilter) then
    fFilter.FreeNotification(Self);
end;

procedure TRLPrintParams.SetCopies(const Value: integer);
begin
  fCopies:=Value;
end;

procedure TRLPrintParams.Apply;
begin
  RLPrinter.Copies:=fCopies;
end;

procedure TRLPrintDialog.SpeedButtonSetupClick(Sender: TObject);
begin
SaveEditors;
{$IFDEF FPC}
Showmessage('Item n�o implementado');
{$ENDIF}
{$IFNDEF FPC}
RLPrinter.ExecuteSetup;
{$ENDIF}
end;

procedure TRLPrintDialog.ButtonImprimirClick(Sender: TObject);
begin
//showmessage('button imprimir');
PrintParams.Options:= PrintParams.Options + [rpoprintdirect];
SaveEditors;
end;


initialization
  {$i rlprintdialog.lrs}
  PrintParams:=TRLPrintParams.Create(nil);

finalization
  PrintParams.Free;

end.

