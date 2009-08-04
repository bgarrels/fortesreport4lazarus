{$I RLReport.inc}

{@unit RLPageSetupConfig - Implementação do diálogo configuração da página.
Portado para o Lazarus - Trabalho inicial de Isaac Trindade da Silva contato tioneobrasil@yahoo.com.br dicas4lazarus@yahoo.com.br
Lazarus Ported - initial work by Isaac 07/2009
}
unit RLPageSetupConfig;
{$MODE DELPHI}{$H+}
interface

uses
  Classes, SysUtils,
{$IFDEF FPC}
  LCLIntf,
 {$ifdef MSWINDOWS}
//   Windows,
 {$else}
//   LCLIntf,
 {$endif}
 LCLType,
{$ELSE}
 {$ifdef MSWINDOWS}
   Windows,
 {$endif}
{$ENDIF}

{$ifdef VCL}
  Messages, Graphics, Controls, Forms, Dialogs, StdCtrls,
{$else}
  QGraphics, QControls, QForms, QDialogs, QStdCtrls,
{$endif}
  RLFilters, RLConsts, RLPrinters, RLTypes, RLSpoolFilter;

type

  TRLPageSetupConfig = class(TForm)
    GroupBoxPages: TGroupBox;
    GroupBoxCopies: TGroupBox;
    ButtonAplicar: TButton;
    ButtonCancel: TButton;
    RadioButtonPagesAll: TRadioButton;
    RadioButtonPagesInterval: TRadioButton;
    RadioButtonPagesSelect: TRadioButton;
    EditMarginsTop: TEdit;
    LabelMarginsTop: TLabel;
    LabelMarginsBottom: Tlabel;
    LabelMarginsLeft: Tlabel;
    LabelMarginsRigth: Tlabel;            
    EditMarginsBottom: TEdit;
    EditMarginsLeft: TEdit;
    EditMarginsRigth: TEdit;
    LabelSetPaper: TLabel;
    LabelPaperWidth: TLabel;
    EditPaperWidth: Tedit;
    LabelPaperHeight: Tlabel;
    EditPaperHeight: Tedit;              
    ComboBoxSetPaper: TComboBox;
    ComboBoxPaperOrientation: TComboBox;
    LabelPaperOrientation: Tlabel;    
    ButtonOptions: TButton;
    SpeedButtonok: Tbutton;
    procedure EditFromToPageChange(Sender: TObject);
    procedure ComboBoxSetPaperChange(Sender: TObject);
  private
    { Private declarations }
    procedure LoadEditors;
    procedure SaveEditors;
    procedure Init;
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(aOwner:TComponent); override;
    function Execute:boolean;
  end;

  {@class TRLPageParams - Parâmetros de impressão.}
  TRLPageParams=class(TComponent)
  private
    fPageOrientation   :TRLPageOrientation;
    fPaperSize         :TRLPaperSize;
    fPaperMargins      :TRect;
    fHelpContext       :integer;

    Procedure SetPaperSize(Value: TRLPaperSize);
    Procedure SetPageOrientation(Value: TRLPageOrientation);
    //
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
    {$IFDEF FPC}
    {@prop PaperMargins - Tamanho do Papel.:/}
    property  PaperMargins     :TRect                 read fPaperMargins      write fPaperMargins;
    {$ENDIF}
  published
    {$IFNDEF FPC}
    {@prop PaperMargins - Tamanho do Papel.:/}
    property  PaperMargins     :TRect                 read fPaperMargins      write fPaperMargins;
    {$ENDIF}
    {@prop PaperSize - Tamanho do Papel.:/}
    property  PaperSize        :TRLPaperSize          read fPaperSize         write setPaperSize;
    {@prop PaperMargins - Tamanho do Papel.:/}
    property  PageOrientation  :TRLPageOrientation    read fPageOrientation   write SetPageOrientation;
    {@prop HelpContext - Contexto de ajuda.:/}
    property  HelpContext      :integer               read fHelpContext       write fHelpContext;
  end;
  {/@class}

{@var PageParams - Parâmetros de impressão atuais. @links TRLPageParams:/}
var PageParams:TRLPageParams=nil;

{/@unit}

implementation

///{$R *.dfm}

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

constructor TRLPageSetupConfig.Create(aOwner:TComponent);
begin
  {$IFDEF FPC}
  inherited CreateNew(aOwner,0);
  {$ELSE}
  inherited CreateNew(aOwner);
  {$ENDIF}
  //
  Init;
end;

// PUBLIC

function TRLPageSetupConfig.Execute:boolean;
begin
  LoadEditors;
  ActiveControl:=GroupBoxPages;
  Result:=(ShowModal=mrOk);
  if Result then
    SaveEditors;
end;

// PRIVATE

procedure TRLPageSetupConfig.Init;
begin
  Left := 324;
  Top := 372;
{$ifdef VCL}
  BorderStyle := bsDialog
{$else}
  BorderStyle := fbsDialog;
{$endif};
  Caption := Ls_Page_Setings;
  ClientHeight := 150;
  ClientWidth := 430;
  Color := clBtnFace;
  Font.Charset := DEFAULT_CHARSET;
  Font.Color := clWindowText;
  Font.Height := -11;
  Font.Name := 'MS Sans Serif';
  Font.Style := [];
  Position := poScreenCenter;
  {$IFNDEF FPC}
  Scaled := False;
  {$ENDIF}
  PixelsPerInch := 96;
  GroupBoxPages:=TGroupBox.Create(Self);
  with GroupBoxPages do
  begin
    Name := 'GroupBoxPages';
    Parent := Self;
    Left := 8;
    Top := 10;
    Width := 217;
    Height := 130;
    Caption := ' ' + Ls_Page_margins + ' ';
    TabOrder := 1;
    LabelMarginsTop:=TLabel.Create(Self);
    with LabelMarginsTop do
    begin
      Name := 'LabelMarginsTop';
      Parent := GroupBoxPages;
      Left := 10;
      Top := 49;
      Width := 18;
      Height := 13;
      Caption := '&'+Ls_Page_margins_top;
      FocusControl := EditMarginsBottom;
    end;
    LabelMarginsBottom:=TLabel.Create(Self);
    with LabelMarginsBottom do
    begin
      Name := 'LabelMarginsBottom';
      Parent := GroupBoxPages;
      Left := 10;
      Top := 20;
      Width := 18;
      Height := 13;
      Caption := '&'+Ls_Page_margins_bottom;
      FocusControl := EditMarginsBottom;
    end;
    LabelMarginsLeft:=TLabel.Create(Self);
    with LabelMarginsLeft do
    begin
      Name := 'LabelMarginsLeft';
      Parent := GroupBoxPages;
      Left := 110;
      Top := 49;
      Width := 18;
      Height := 13;
      Caption := '&'+Ls_Page_margins_left;
      FocusControl := EditMarginsBottom;
    end;
    LabelMarginsRigth:=TLabel.Create(Self);
    with LabelMarginsRigth do
    begin
      Name := 'LabelMarginsRigth';
      Parent := GroupBoxPages;
      Left := 110;
      Top := 20;
      Width := 18;
      Height := 13;
      Caption := '&'+Ls_Page_margins_rigth;
      FocusControl := EditMarginsBottom;
    end;
    EditMarginsTop:=TEdit.Create(Self);
    with EditMarginsTop do
    begin
      Name := 'EditMarginsTop';
      Parent := GroupBoxPages;
      Left := 60;
      Top := 15;
      Width := 41;
      Height := 21;
      TabOrder := 2;
      Text := ' ';
      OnChange := EditFromToPageChange;
    end;
    EditMarginsBottom:=TEdit.Create(Self);
    with EditMarginsBottom do
    begin
      Name := 'EditMarginsBottom';
      Parent := GroupBoxPages;
      Left := 60;
      Top := 45;
      Width := 41;
      Height := 21;
      TabOrder := 3;
      OnChange := EditFromToPageChange;
    end;
    EditMarginsLeft:=TEdit.Create(Self);
    with EditMarginsLeft do
    begin
      Name := 'EditMarginsLeft';
      Parent := GroupBoxPages;
      Left := 165;
      Top := 45;
      Width := 41;
      Height := 21;
      TabOrder := 3;
      Text := '';
      OnChange := EditFromToPageChange;
    end;
    EditMarginsRigth:=TEdit.Create(Self);
    with EditMarginsRigth do
    begin
      Name := 'EditMarginsRigth';
      Parent := GroupBoxPages;
      Left := 165;
      Top := 15;
      Width := 41;
      Height := 21;
      TabOrder := 3;
      Text := '';
      OnChange := EditFromToPageChange;
    end;
    ComboBoxPaperOrientation:=TComboBox.Create(Self);
    with ComboBoxPaperOrientation do
    begin
      Name := 'ComboBoxPaperOrientation';
      Parent := GroupBoxPages;
      Left := 10;
      Top := 100;
      Width := 198;
      Height :=  21;
      Style := csDropDownList;
      ItemHeight  := 13;
      TabOrder :=  0;
      items.Add (Ls_Paper_Orientation_Portrait);
      items.Add (Ls_Paper_Orientation_Landscape);
      ItemIndex:=0;
      OnChange := ComboBoxSetPaperChange;
    end;
    LabelPaperOrientation:=TLabel.Create(Self);
    with LabelPaperOrientation do
    begin
      Name := 'LabelPaperOrientation';
      Parent := GroupBoxPages;
      Left := 10;
      Top := 85;
      Width := 18;
      Height := 13;
      Caption := '&'+Ls_Paper_Orientation;
      FocusControl := ComboBoxPaperOrientation;
    end;
  end;
  GroupBoxCopies:=TGroupBox.Create(Self);
  with GroupBoxCopies do
  begin
    Name := 'GroupBoxCopies';
    Parent := Self;
    Left := 236;
    Top := 10;
    Width := 185;
    Height := 101;
    Caption := ' ' + Ls_Page_Paper + ' ';
    TabOrder := 2;
    LabelSetPaper:=TLabel.Create(Self);
    with LabelSetPaper do
    begin
      Name := 'LabelSetPaper';
      Parent := GroupBoxCopies;
      Left := 10;
      Top := 15;
      Width := 15;
      Height := 13;
      Caption := Ls_Paper_Size;
      FocusControl := ComboBoxSetPaper;
    end;
    ComboBoxSetPaper:=TComboBox.Create(Self);
    with ComboBoxSetPaper do
    begin
      Name := 'ComboBoxSetPaper';
      Parent := GroupBoxCopies;
      Left := 10;
      Top := 30;
      Width := 165;
      Height := 21;
      Style := csDropDownList;
      ItemHeight := 13;
      TabOrder := 0;
      OnChange := ComboBoxSetPaperChange;
    end;
    LabelPaperWidth:=TLabel.Create(Self);
    with LabelPaperWidth do
    begin
      Name := 'LabelPaperWidth';
      Parent := GroupBoxCopies;
      Left := 10;
      Top := 55;
      Width := 15;
      Height := 13;
      Caption := '&'+Ls_Paper_Size_Width;
      FocusControl := GroupBoxCopies;
    end;
    EditPaperWidth:=TEdit.Create(Self);
    with EditPaperWidth do
    begin
      Name := 'EditPaperWidth';
      Parent := GroupBoxCopies;
      Left := 10;
      Top := 70;
      Width := 50;
      Height := 21;
      TabOrder := 3;
      Text := '';
      OnChange := EditFromToPageChange;
    end;
    LabelPaperHeight:=TLabel.Create(Self);
    with LabelPaperHeight do
    begin
      Name := 'LabelPaperHeight';
      Parent := GroupBoxCopies;
      Left := 80;
      Top := 55;
      Width := 20;
      Height := 13;
      Caption := '&'+Ls_Paper_Size_Width;
      FocusControl := EditPaperHeight;
    end;
    EditPaperHeight:=TEdit.Create(Self);
    with EditPaperHeight do
    begin
      Name := 'EditPaperHeight';
      Parent := GroupBoxCopies;
      Left := 80;
      Top := 70;
      Width := 50;
      Height := 21;
      TabOrder := 3;
      Text := '';
      OnChange := EditFromToPageChange;
    end;
  end;
  ButtonAplicar:=TButton.Create(Self);
  with ButtonAplicar do
  begin
    Name := 'ButtonAplicar';
    Parent := Self;
    Left := 260;
    Top := 115;
    Width := 75;
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
    Left := 345;
    Top := 115;
    Width := 75;
    Height := 25;
    Cancel := True;
    Caption := LS_CancelStr;
    ModalResult := 2;
    TabOrder := 4;
  end;
  //
end;


procedure TRLPageSetupConfig.LoadEditors;
var
  i         :TRLPaperSize;
  v         :string;
  c         :integer;
begin
  c:=-1;
  for i:=low(TRLPaperSize) to high(TRLPaperSize) do
  begin
    v:=PaperInfo[i].Description;
    if PaperInfo[i].Emulated then
      v:=v+'*';
    ComboBoxSetPaper.Items.Add(v);
    if v=PaperInfo[PageParams.PaperSize].Description then
      c:=ComboBoxSetPaper.Items.count-1;
  end;
  ComboBoxSetPaper.itemindex:= c;
  EditPaperWidth.Text   := FloatToStr(PaperInfo[PageParams.PaperSize].Width);
  EditPaperHeight.Text  := FloatToStr(PaperInfo[PageParams.PaperSize].Height);
  EditMarginsRigth.Text := Inttostr(PageParams.PaperMargins.Right);
  EditMarginsLeft.Text  := Inttostr(PageParams.PaperMargins.Left);
  EditMarginsBottom.Text:= Inttostr(PageParams.PaperMargins.Bottom);
  EditMarginsTop.Text   := Inttostr(PageParams.PaperMargins.Top);
  if PageParams.PageOrientation = poLandscape then
     ComboBoxPaperOrientation.ItemIndex:= 1;
end;

procedure TRLPageSetupConfig.SaveEditors;
begin
PageParams.PaperMargins:= Rect(StrToInt(EditMarginsLeft.Text),  StrToInt(EditMarginsTop.Text), StrToInt(EditMarginsRigth.Text), StrToInt(EditMarginsBottom.Text));
PaperInfo[PageParams.PaperSize].Width := StrToInt(EditPaperWidth.Text);
PaperInfo[PageParams.PaperSize].Height:= StrToInt(EditPaperHeight.Text);

if ComboBoxPaperOrientation.ItemIndex=1 then
   PageParams.PageOrientation := poLandscape;
//---->> ver    ComboBoxZoom.Items.IndexOf(ComboBoxZoom.Text);
  //
end;

// EVENTS

procedure TRLPageSetupConfig.EditFromToPageChange(Sender: TObject);
begin

end;

procedure TRLPageSetupConfig.ComboBoxSetPaperChange(Sender: TObject);
var
  PaperSize:TRLPaperSize;
begin
  SaveEditors;
  PaperInfo[PaperSize].Width:= 0; ///?
end;

{ TRLPageParams }

constructor TRLPageParams.Create(aOwner: TComponent);
begin
  //
  inherited;


end;

destructor TRLPageParams.Destroy;
begin
  inherited;
end;

procedure TRLPageParams.Notification(Component: TComponent;
  Operation: TOperation);
begin
  inherited;
  //
end;

procedure TRLPageParams.Clear;
begin
{  fMinPage    :=1;
  fMaxPage    :=fToPage;
  fFromPage   :=fMinPage;
  fToPage     :=fMaxPage;
  fPrintRange :=rprAllPages;
  fFileName   :=emptystr;
  fHelpContext:=0;
  fCopies     :=RLPrinter.Copies;
  fOptions    :=PrintParams.Options - [rpoprintdirect];}
end;

procedure TRLPageParams.Apply;
begin
//  RLPrinter.Copies:=fCopies;
end;

procedure TRLPageParams.SetPaperSize(Value: TRLPaperSize);
begin
  fPaperSize:=Value;
end;


procedure TRLPageParams.SetPageOrientation(Value: TRLPageOrientation);
begin
fPageOrientation:=Value;
end;

initialization
  PageParams:=TRLPageParams.Create(nil);

finalization
  PageParams.Free;

end.

