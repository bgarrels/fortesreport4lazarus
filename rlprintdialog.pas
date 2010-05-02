{$I RLReport.inc}

{@unit RLPrintDialog - Implementação do diálogo de impressão e sua classe de setup.
Portado para o Lazarus - Trabalho inicial de Isaac Trindade da Silva contato tioneobrasil@yahoo.com.br dicas4lazarus@yahoo.com.br
Lazarus Ported - initial work by Isaac 07/2009
}
unit RLPrintDialog;
{$MODE DELPHI}{$H+}
interface

uses
  LCLIntf, LCLType, Classes, SysUtils,
  LMessages, Graphics, Controls, Forms, Dialogs, StdCtrls,
  RLFilters, RLConsts, RLPrinters, RLTypes, RLSpoolFilter;

type
  {@type TRLPrintDialogOptions - Opções de configuração do diálogo de impressão.
  Pode ser um conjunto dos seguintes valores:
  rpoPrintToFile - Mostrar a opção de impressão em arquivo;
  rpoPageNums - Mostrar a seleção de páginas por intervalo;
  rpoSelection - Mostrar a seleção de páginas por números específicos;
  rpoWarning - Exibir mensagens de advertência;
  rpoHelp - Utilizar ajuda do sistema;
  rpoDisablePrintToFile - Desabilitar opção de impressão em arquivo;
  rpoDisableCopies - Desabilitar a opção de número de cópias;
  rpoDisableBackgroundMode - Desabilitar a opção de impressão em segundo plano.
  rpoprintdirect - Ao sair habilita imprissão imediata
  :}
  TRLPrintDialogOption =(rpoPrintToFile,rpoPageNums,rpoSelection,rpoWarning,rpoHelp,
    rpoDisablePrintToFile,rpoDisableCopies,rpoDisableBackgroundMode, rpoprintdirect);
  TRLPrintDialogOptions=set of TRLPrintDialogOption;
  {/@type}

  {@type TRLPrintRange - Opções para o modo seleção de páginas.
  Pode ser um dos seguintes valores:
  rprAllPages - Imprimir todas as páginas;
  rprSelection - Imprimir as páginas de números indicados;
  rprPageNums - Imprimir o intervalo de páginas indicado.
  :/}
  TRLPrintRange=(rprAllPages,rprSelection,rprPageNums);

const
  DefaultPrintOptions=[rpoPrintToFile,rpoPageNums,rpoSelection,rpoWarning,rpoDisablePrintToFile];

type
  TRLPrintDialog = class(TForm)
    GroupBoxPrinter: TGroupBox;
    ComboBoxPrinterNames: TComboBox;
    LabelPrinterName: TLabel;
    GroupBoxPages: TGroupBox;
    GroupBoxCopies: TGroupBox;
    ButtonAplicar: TButton;
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
    SpeedButtonSetup: Tbutton;
    ButtonImprimir  : Tbutton;
    procedure EditFromToPageChange(Sender: TObject);
    procedure ComboBoxFiltersChange(Sender: TObject);
    procedure ButtonOptionsClick(Sender: TObject);
    procedure SpeedButtonSetupClick(Sender: TObject);
    procedure ButtonImprimirClick(Sender: TObject);
  private
    { Private declarations }
    procedure LoadEditors;
    procedure SaveEditors;
    procedure LoadPrinterList;
    procedure LoadFilterList;
    procedure Init;
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(aOwner:TComponent); override;
    function Execute:boolean;
  end;

  {@class TRLPrintDialogSetup - Opções do diálogo de impressão.
   O diálogo de impressão obedecerá as configurações da instância deste componente. Com ele, é
   possível estabelecer o número de cópias inicial e configurar alguns itens de comportamento do
   diálogo de impressão.
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
    {@prop Options - Opções diversas do diálogo de impressão. @links TRLPrintDialogOptions. :/}
    property Options:TRLPrintDialogOptions read GetOptions write SetOptions default DefaultPrintOptions;
    {@prop Copies - Indica o valor inicial para o número de cópias. :/}
    property Copies:integer read GetCopies write SetCopies default 1;
    {@prop BackgroundMode - Indica o valor inicial para a opção de imprimir em segundo plano. :/}
    property BackgroundMode:boolean read GetBackgroundMode write SetBackgroundMode default False;
    {@prop PrintToFile - Indica o estado inicial para a opção de imprimir em arquivo. :/}
    property PrintToFile:boolean read GetPrintToFile write SetPrintToFile default False;
    {@prop Filter - Indica o filtro a oferecer no diálogo. @links TRLCustomPrintFilter:/}
    property Filter:TRLCustomPrintFilter read GetFilter write SetFilter;
  end;
  {/@class}

  {@class TRLPrintParams - Parâmetros de impressão.}
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
    destructor  Destroy; override;
    {@method Clear - Preenche todas as props com valores default.:/}
    procedure Clear;
    {@method Apply - Aplica as configurações ao relatório em andamento.:/}
    procedure Apply;
  published
    {@prop Options - Opções diversas do diálogo de impressão. @links TRLPrintDialogOptions. :/}
    property  Options          :TRLPrintDialogOptions read fOptions           write fOptions default DefaultPrintOptions;
    {@prop MaxPage - Máximo para número de página final. :/}
    property  MaxPage          :integer               read fMaxPage           write SetMaxPage;
    {@prop MinPage - Mínimo para número de página inicial. :/}
    property  MinPage          :integer               read fMinPage           write fMinPage;
    {@prop FromPage - Número selecionado pelo usuário como página inicial. :/}
    property  FromPage         :integer               read fFromPage          write fFromPage;
    {@prop ToPage - Número selecionado pelo usuário como página final. :/}
    property  ToPage           :integer               read fToPage            write fToPage;
    {@prop PrintRange - Modo de seleção de páginas. @links TRLPrintRange:/}
    property  PrintRange       :TRLPrintRange         read fPrintRange        write fPrintRange;
    {@prop Copies - Indica o valor inicial para o número de cópias. :/}
    property  Copies           :integer               read fCopies            write SetCopies default 1;
    {@prop PrintToFile - Imprimir para arquivo.:/}
    property  PrintToFile      :boolean               read fPrintToFile       write fPrintToFile;
    {@prop BackgroundMode - Imprimir em segundo plano.:/}
    property  BackgroundMode   :boolean               read fBackgroundMode    write fBackgroundMode;
    {@prop FileName - Nome do arquivo a gerar.:/}
    property  FileName         :string                read fFileName          write fFileName;
    {@prop Orientation - Orientação do papel. @links TRLPageOrientation:/}
    property  Orientation      :TRLPageOrientation    read fOrientation       write fOrientation;
    {@prop Filter - Filtro atualmente selecionado. @links TRLCustomPrintFilter:/}
    property  Filter           :TRLCustomPrintFilter  read fFilter            write SetFilter;
    {@prop HelpContext - Contexto de ajuda.:/}
    property  HelpContext      :integer               read fHelpContext       write fHelpContext;
  end;
  {/@class}

{@var PrintParams - Parâmetros de impressão atuais. @links TRLPrintParams:/}
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

// OVERRIDE

constructor TRLPrintDialog.Create(aOwner:TComponent);
begin
  inherited CreateNew(aOwner,0);
  //
  Init;
end;

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

procedure TRLPrintDialog.Init;
begin
  Left := 324;
  Top := 372;
  BorderStyle := bsDialog;
  Caption := LS_PrintStr;
  ClientHeight := 317;//267;
  ClientWidth := 523;//430;
  Color := clBtnFace;
  Font.Charset := DEFAULT_CHARSET;
  Font.Color := clWindowText;
  Font.Name := 'default';
  Font.Height := 0;
  Font.Style := [];
  Position := poScreenCenter;
  PixelsPerInch := 96;
  GroupBoxPrinter:=TGroupBox.Create(Self);
  with GroupBoxPrinter do
  begin
    Name := 'GroupBoxPrinter';
    Parent := Self;
    Left := 8;
    Top := 4;
    Width := 504;//413;
//  Height := 113;
    Height := 150;
    Caption := ' '+LS_PrinterStr+' ';
    TabOrder := 0;
    LabelPrinterName:=TLabel.Create(Self);
    with LabelPrinterName do
    begin
      Name := 'LabelPrinterName';
      Parent := GroupBoxPrinter;
      Left := 12;
      Top := 24;
      Width := 31;
      Height := 13;
      Caption := '&'+LS_NameStr+':';
      FocusControl := ComboBoxPrinterNames;
    end;
    ComboBoxPrinterNames:=TComboBox.Create(Self);
    with ComboBoxPrinterNames do
    begin
      Name := 'ComboBoxPrinterNames';
      Parent := GroupBoxPrinter;
      Left := 74;//68;
      Top := 20;
      Width := 290;//329;
      Height := 21;
      Style := csDropDownList;
      ItemHeight := 13;
      TabOrder := 0;
    end;
    LabelFilterName:=TLabel.Create(Self);
    with LabelFilterName do
    begin
      Name := 'LabelFilterName';
      Parent := GroupBoxPrinter;
      Left := 12;
      Height := 16;
      Top := 62;
      Width := 76;

{
      Left := 12;
      Top := 48;
      Width := 47;
      Height := 13;
}
      Caption := LS_UseFilterStr;
      FocusControl := ComboBoxPrinterNames;
    end;
    ComboBoxFilters:=TComboBox.Create(Self);
    with ComboBoxFilters do
    begin
      Name := 'ComboBoxFilters';
      Parent := GroupBoxPrinter;
      Left := 74;
      Height := 29;
      Top := 52;
      Width := 290;
      {
      Left := 68;
      Top := 44;
      Width := 177;
      Height := 21;
      }
      Style := csDropDownList;
      TabOrder := 1;
      OnChange := ComboBoxFiltersChange;
    end;
    ButtonOptions:=TButton.Create(Self);
    with ButtonOptions do
    begin
      Name := 'ButtonOptions';
      Parent := GroupBoxPrinter;
      Left := 394;
      Height := 27;
      Top := 52;
      Width := 85;
{
      Left := 248;
      Top := 44;
      Width := 61;
      Height := 21;
}
      Caption := UTF8Encode(LS_OptionsStr);
      TabOrder := 2;
      OnClick := ButtonOptionsClick;
    end;
    CheckBoxPrintToFile:=TCheckBox.Create(Self);
    with CheckBoxPrintToFile do
    begin
      Name := 'CheckBoxPrintToFile';
      Parent := GroupBoxPrinter;
      Left := 12;
      Top := 91;//68;
      Width := 325;
      Height := 17;
      Caption := LS_PrintToFileStr;
      TabOrder := 3;
    end;
    CheckBoxBackgroundMode:=TCheckBox.Create(Self);
    with CheckBoxBackgroundMode do
    begin
      Name := 'CheckBoxBackgroundMode';
      Parent := GroupBoxPrinter;
      Left := 12;
      Top := 111;//88;
      Width := 325;
      Height := 17;
      Caption := LS_PrintInBackgroundStr;
      TabOrder := 4;
    end;
  end;
  GroupBoxPages:=TGroupBox.Create(Self);
  with GroupBoxPages do
  begin
    Name := 'GroupBoxPages';
    Parent := Self;
    Left := 8;
    Top := 160;//120
    Width := 259;//217;
    Height := 103;
    Caption := ' ' + LS_PageRangeStr + ' ';
    TabOrder := 1;
    LabelFromPage:=TLabel.Create(Self);
    with LabelFromPage do
    begin
      Name := 'LabelFromPage';
      Parent := GroupBoxPages;
      Left := 97;//72;
      //Top := 49;
      Top := 31;
      Width := 15;
      Height := 13;
      Caption := LS_OfStr;
      FocusControl := EditFromPage;
    end;
    LabelToPage:=TLabel.Create(Self);
    with LabelToPage do
    begin
      Name := 'LabelToPage';
      Parent := GroupBoxPages;
      Left := 165;//140;
//    Top := 49;
      Top := 31;
      Width := 18;
      Height := 13;
      Caption := '&'+LS_RangeToStr;
      FocusControl := EditToPage;
    end;
    RadioButtonPagesAll:=TRadioButton.Create(Self);
    with RadioButtonPagesAll do
    begin
      Name := 'RadioButtonPagesAll';
      Parent := GroupBoxPages;
      Left := 12;
      Height := 21;
      Top := 7;
      Width := 53;
{
      Left := 12;
      Top := 24;
      Width := 113;
      Height := 17;
      }
      Caption := LS_AllStr;
      Checked := True;
      TabOrder := 0;
      TabStop := True;
    end;
    RadioButtonPagesInterval:=TRadioButton.Create(Self);
    with RadioButtonPagesInterval do
    begin
      Name := 'RadioButtonPagesInterval';
      Parent := GroupBoxPages;
      Left := 12;
      Height := 21;
      Top := 31;
      Width := 73;
{
      Left := 12;
      Top := 48;
      Width := 61;
      Height := 17;
      }
      Caption := LS_PagesStr;
      TabOrder := 1;
    end;
    EditFromPage:=TEdit.Create(Self);
    with EditFromPage do
    begin
      Name := 'EditFromPage';
      Parent := GroupBoxPages;
      Left := 121;
      Height := 21;
      Top := 27;
      Width := 41;
{
      Left := 92;
      Top := 44;
      Width := 41;
      Height := 21;
      TabOrder := 2;
      }
      Text := '1';
      OnChange := EditFromToPageChange;
    end;
    EditToPage:=TEdit.Create(Self);
    with EditToPage do
    begin
      Name := 'EditToPage';
      Parent := GroupBoxPages;
      Left := 193;
      Height := 21;
      Top := 27;
      Width := 41;
{
      Left := 164;
      Top := 44;
      Width := 41;
      Height := 21;
}
      TabOrder := 3;
      OnChange := EditFromToPageChange;
    end;
    RadioButtonPagesSelect:=TRadioButton.Create(Self);
    with RadioButtonPagesSelect do
    begin
      Name := 'RadioButtonPagesSelect';
      Parent := GroupBoxPages;
      Left := 12;
      Height := 21;
      Top := 55;
      Width := 77;
 {
      Left := 12;
      Top := 72;
      Width := 73;
      Height := 17;
      }
      Caption := LS_SelectionStr;
      TabOrder := 4;
    end;
  end;
  GroupBoxCopies:=TGroupBox.Create(Self);
  with GroupBoxCopies do
  begin
    Name := 'GroupBoxCopies';
    Parent := Self;
    Left := 276;
    Height := 103;
    Top := 160;
    Width := 236;

{
    Left := 236;
    Top := 156;//120;
    Width := 185;
    Height := 102;
}
    Caption := ' ' + LS_CopiesStr + ' ';
    TabOrder := 2;
    LabelCopies:=TLabel.Create(Self);
    with LabelCopies do
    begin
      Name := 'LabelCopies';
      Parent := GroupBoxCopies;
      Left := 12;
      Height := 16;
      Top := 24;
      Width := 129;
{
      Left := 12;
      Top := 24;
      Width := 89;
      Height := 13;
 }
      Caption := LS_NumberOfCopiesStr+':';
    end;
    EditCopies:=TEdit.Create(Self);
    with EditCopies do
    begin
      Name := 'EditCopies';
      Parent := GroupBoxCopies;
      Left := 158;
      Height := 21;
      Top := 20;
      Width := 49;
{
      Left := 108;
      Top := 20;
      Width := 49;
      Height := 21;
}
      TabOrder := 0;
      Text := '1';
    end;
  end;
  ButtonAplicar:=TButton.Create(Self);
  with ButtonAplicar do
  begin
    Name := 'ButtonAplicar';
    Parent := Self;
    Left := 160-2;
    Top := 280;
    Width := 115;
    Height := 25;
    Caption := Ls_Aplicar;
    ModalResult := 1;
    TabOrder := 3;
  end;
  ButtonCancel:=TButton.Create(Self);
  with ButtonCancel do
  begin
    Name := 'ButtonCancel';
    Parent := Self;
    Left := 400-2;
    Top := 280;
    Width := 115;
    Height := 25;
    Cancel := True;
    Caption := LS_CancelStr;
    ModalResult := 2;
    TabOrder := 4;
  end;
  SpeedButtonSetup:=TButton.Create(Self);
  with SpeedButtonSetup do
  begin
    Name := 'SpeedButtonSetup';
    Parent := Self;
    Left := 394+10;
    Top := 20+12;
    Width := 85;
    Height := 29;
    Caption := Ls_Propriedades;
    ModalResult := 0;
    TabOrder := 5;
    OnClick := SpeedButtonSetupClick;
  end;
  ButtonImprimir:=TButton.Create(Self);
  with ButtonImprimir do
  begin
    Name := 'ButtonImprimir';
    Parent := Self;
    Left := 280-2;
    Top := 280;
    Width := 115;
    Height := 25;
    Cancel := True;
    Caption := LS_PrintStr;
    ModalResult := 1;
    TabOrder := 6;
    OnClick := ButtonImprimirClick;
  end;
  //
end;

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

destructor TRLPrintParams.Destroy;
begin
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
Showmessage('Item não implementado');
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
  PrintParams:=TRLPrintParams.Create(nil);

finalization
  PrintParams.Free;

end.

