{$I RLReport.inc}
{@unit RLFindDialog
Portado para o Lazarus - Trabalho inicial de Isaac Trindade da Silva contato tioneobrasil@yahoo.com.br dicas4lazarus@yahoo.com.br
Lazarus Ported - initial work by Isaac 07/2009
}
unit RLFindDialog;
{$MODE DELPHI}
interface

uses
  SysUtils, Classes, LCLIntf, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, RLConsts;

type
  TRLFindOption =(foWholeWords,foMatchCase,foFindBackward);
  TRLFindOptions=set of TRLFindOption;

  TRLOnFindEvent=procedure(Sender:TObject; const Text:string; Options:TRLFindOptions; var Found:boolean) of object;  

  TfrmRLFindDialog = class(TForm)
    LabelTextToFind: TLabel;
    EditTextToFind: TEdit;
    BitBtnFindNext: TBitBtn;
    BitBtnCancel: TBitBtn;
    CheckBoxWholeWords: TCheckBox;
    CheckBoxMatchCase: TCheckBox;
    RadioGroupDirection: TRadioGroup;
    procedure BitBtnCancelClick(Sender: TObject);
    procedure BitBtnFindNextClick(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
  private
    { Private declarations }
    fOnFind:TRLOnFindEvent;
    //
    function    GetTextValue:string;
    procedure   SetTextValue(const Value:string);
    function    GetOptions:TRLFindOptions;
    procedure   SetOptions(const Value:TRLFindOptions);
    //
    procedure   Init;
  public
    { Public declarations }
    constructor Create(aOwner:TComponent); override;
    //
    property    Text   :string         read GetTextValue write SetTextValue;
    property    Options:TRLFindOptions read GetOptions   write SetOptions;
    //
    property    OnFind:TRLOnFindEvent  read fOnFind      write fOnFind;
  end;

var
  frmRLFindDialog: TfrmRLFindDialog;

implementation

{ TfrmRLFindDialog }

constructor TfrmRLFindDialog.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  Init;
end;

procedure TfrmRLFindDialog.Init;
begin
  BorderStyle := bsDialog;
  Caption := LS_FindCaptionStr;
  ClientHeight := 94;
  ClientWidth := 367;
  OnDeactivate := FormDeactivate;
  LabelTextToFind := TLabel.Create(Self);
  with LabelTextToFind do
  begin
    Name := 'LabelTextToFind';
    Parent := Self;
    Left := 4;
    Top := 8;
    BorderSpacing.Left := 4;
    BorderSpacing.Top := 8;
    FocusControl := EditTextToFind;
    Caption := LS_TextToFindStr+':';
  end;
  EditTextToFind:=TEdit.Create(Self);
  with EditTextToFind do
  begin
    Name := 'EditTextToFind';
    Parent := Self;
    Width := 230;
    Text := '';
    TabOrder := 0;
    AnchorVerticalCenterTo(LabelTextToFind);
    AnchorToNeighbour(akLeft, 4, LabelTextToFind);
  end;
  BitBtnFindNext:=TBitBtn.Create(Self);
  with BitBtnFindNext do
  begin
    Name := 'BitBtnFindNext';
    Parent := Self;
    Width := 75;
    Height := EditTextToFind.Height + 2;
    BorderSpacing.Right := 4;
    Caption := LS_FindNextStr;
    Default := True;
    TabOrder := 1;
    OnClick := BitBtnFindNextClick;
    AnchorVerticalCenterTo(EditTextToFind);
    AnchorToNeighbour(akLeft, 4, EditTextToFind);
  end;
  BitBtnCancel:=TBitBtn.Create(Self);
  with BitBtnCancel do
  begin
    Name := 'BitBtnCancel';
    Parent := Self;
    Width := 75;
    Height := BitBtnFindNext.Height;
    Cancel := True;
    Caption := LS_CancelStr;
    TabOrder := 2;
    OnClick := BitBtnCancelClick;
    AnchorHorizontalCenterTo(BitBtnFindNext);
    AnchorToNeighbour(akTop, 4, BitBtnFindNext);
  end;
  CheckBoxWholeWords:=TCheckBox.Create(Self);
  with CheckBoxWholeWords do
  begin
    Name := 'CheckBoxWholeWords';
    Parent := Self;
    Caption := LS_WholeWordsStr;
    TabOrder := 3;
    AnchorToNeighbour(akTop, 6, EditTextToFind);
    AnchorParallel(akLeft, 0, LabelTextToFind);
  end;
  CheckBoxMatchCase:=TCheckBox.Create(Self);
  with CheckBoxMatchCase do
  begin
    Name := 'CheckBoxMatchCase';
    Parent := Self;
    Caption := LS_MatchCaseStr;
    TabOrder := 4;
    AnchorToNeighbour(akTop, 2, CheckBoxWholeWords);
    AnchorParallel(akLeft, 0, CheckBoxWholeWords);
  end;
  RadioGroupDirection:=TRadioGroup.Create(Self);
  with RadioGroupDirection do
  begin
    Name := 'RadioGroupDirection';
    Parent := Self;
    Constraints.MinWidth := 80;
    Caption := ' '+LS_DirectionCaptionStr+' ';
    Items.Add(LS_DirectionUpStr);
    Items.Add(LS_DirectionDownStr);
    BorderSpacing.Bottom := 4;
    ItemIndex := 1;
    AutoSize := True;
    TabOrder := 5;
    Anchors := [akTop, akRight];
    AnchorParallel(akRight, 0, EditTextToFind);
    AnchorToNeighbour(akTop, 4, EditTextToFind);
  end;
  AutoSize := True;
  Position := poScreenCenter;
end;

function TfrmRLFindDialog.GetTextValue:string;
begin
  result:=EditTextToFind.Text;
end;

procedure TfrmRLFindDialog.SetTextValue(const Value:string);
begin
  EditTextToFind.Text:=Value;
end;

function TfrmRLFindDialog.GetOptions:TRLFindOptions;
begin
  result:=[];
  if CheckBoxWholeWords.Checked then
    Include(result,foWholeWords);
  if CheckBoxMatchCase.Checked then
    Include(result,foMatchCase);
  if RadioGroupDirection.ItemIndex=0 then
    Include(result,foFindBackward);
end;

procedure TfrmRLFindDialog.SetOptions(const Value:TRLFindOptions);
begin
  CheckBoxWholeWords.Checked   :=foWholeWords in Value;
  CheckBoxMatchCase.Checked    :=foMatchCase in Value;
  RadioGroupDirection.ItemIndex:=1-Byte(foFindBackward in Value);
end;

procedure TfrmRLFindDialog.BitBtnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmRLFindDialog.FormDeactivate(Sender: TObject);
begin
  Close;
end;

procedure TfrmRLFindDialog.BitBtnFindNextClick(Sender: TObject);
var
  found:boolean;
begin
  found:=false;
  if Assigned(fOnFind) then
  begin
    Screen.Cursor:=crHourGlass;
    try
      fOnFind(Self,Text,Options,found);
    finally
      Screen.Cursor:=crDefault;
    end;
  end;  
  if not found then
    ShowMessage(LS_NotFoundStr);    
end;

end.

