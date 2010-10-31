{$I RLReport.inc}

{@unit RLPageSetupConfig - Implementação do diálogo configuração da página.
Portado para o Lazarus - Trabalho inicial de Isaac Trindade da Silva contato tioneobrasil@yahoo.com.br dicas4lazarus@yahoo.com.br
Lazarus Ported - initial work by Isaac 07/2009
}
unit RLPageSetupConfig;
{$MODE DELPHI}{$H+}
interface

uses
  Classes, SysUtils, LCLType, Graphics, Controls, Forms, Dialogs, StdCtrls, LResources,
  RLFilters, RLConsts, RLTypes, RLSpoolFilter;

type

  { TRLPageSetupConfig }

  TRLPageSetupConfig = class(TForm)
    ButtonAplicar: TButton;
    ComboBoxPaperOrientation: TComboBox;
    ComboBoxSetPaper: TComboBox;
    EditPaperHeight: TEdit;
    EditPaperWidth: TEdit;
    EditMarginsLeft: TEdit;
    GroupBoxPaper: TGroupBox;
    GroupBoxPageMargins: TGroupBox;
    ButtonCancel: TButton;
    LabelPaperHeight: TLabel;
    LabelPaperWidth: TLabel;
    LabelSetPaper: TLabel;
    LabelPaperOrientation: TLabel;
    RadioButtonPagesAll: TRadioButton;
    RadioButtonPagesInterval: TRadioButton;
    RadioButtonPagesSelect: TRadioButton;
    EditMarginsTop: TEdit;
    LabelMarginsTop: TLabel;
    LabelMarginsBottom: Tlabel;
    LabelMarginsLeft: Tlabel;
    LabelMarginsRigth: Tlabel;            
    EditMarginsBottom: TEdit;
    EditMarginsRigth: TEdit;
    procedure EditFromToPageChange(Sender: TObject);
    procedure ComboBoxSetPaperChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure LoadEditors;
    procedure SaveEditors;
  protected
    { Protected declarations }
  public
    { Public declarations }
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

function TRLPageSetupConfig.Execute:boolean;
begin
  LoadEditors;
  ActiveControl:=GroupBoxPageMargins;
  Result:=(ShowModal=mrOk);
  if Result then
    SaveEditors;
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
  //todo: implementar
end;

procedure TRLPageSetupConfig.ComboBoxSetPaperChange(Sender: TObject);
var
  PaperSize:TRLPaperSize;
begin
  SaveEditors;
  PaperInfo[PaperSize].Width:= 0; ///?
end;

procedure TRLPageSetupConfig.FormCreate(Sender: TObject);
begin
  Caption := Ls_Page_Settings;
  GroupBoxPageMargins.Caption := Ls_Page_margins;
  LabelMarginsTop.Caption := '&'+Ls_Page_margins_top;
  LabelMarginsBottom.Caption := '&'+Ls_Page_margins_bottom;
  LabelMarginsLeft.Caption := '&'+Ls_Page_margins_left;
  LabelMarginsRigth.Caption := '&'+Ls_Page_margins_rigth;
  LabelPaperOrientation.Caption := '&'+Ls_Paper_Orientation;
  GroupBoxPaper.Caption := Ls_Page_Paper;
  LabelSetPaper.Caption := Ls_Paper_Size;
  LabelPaperWidth.Caption := '&'+Ls_Paper_Size_Width;
  LabelPaperHeight.Caption := '&'+Ls_Paper_Size_Heigth;
  ButtonAplicar.Caption := Ls_Aplicar;
  ButtonCancel.Caption := LS_CancelStr;
  ComboBoxPaperOrientation.Items.Add(Ls_Paper_Orientation_Portrait);
  ComboBoxPaperOrientation.Items.Add(Ls_Paper_Orientation_Landscape);
  ComboBoxPaperOrientation.ItemIndex := 0;
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
  {$i rlpagesetupconfig.lrs}
  PageParams:=TRLPageParams.Create(nil);

finalization
  PageParams.Free;

end.

