{$I RLReport.inc}
{@unit RLFindDialog
Portado para o Lazarus - Trabalho inicial de Isaac Trindade da Silva contato tioneobrasil@yahoo.com.br dicas4lazarus@yahoo.com.br
Lazarus Ported - initial work by Isaac 07/2009
}
unit RLFindDialog;
{$MODE DELPHI}{$H+}
interface

uses
  SysUtils, Contnrs, Classes,
{$ifdef VCL}
 {$IFDEF MSWINDOWS}
  Windows,
  {$ELSE}
  LCLIntf,
  {$ENDIF}
  Messages, Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons, ExtCtrls,
{$else}
  Types, QControls, Qt, QButtons, QExtCtrls, QForms, QDialogs, QStdCtrls, QTypes, QGraphics,
{$endif}
  RLConsts;

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
    constructor CreateNew(aOwner:TComponent; Dummy:integer=0); override;
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

constructor TfrmRLFindDialog.CreateNew(aOwner: TComponent; Dummy: integer);
begin
  fOnFind:=nil;
  //
  inherited;
  //
  Init;
end;

procedure TfrmRLFindDialog.Init;
begin
  BorderIcons := [biSystemMenu];
{$ifdef VCL}
  BorderStyle := bsDialog;
{$else}
  BorderStyle := fbsDialog;
{$endif}
  Caption := 'Procurar';
  ClientHeight := 94;
  ClientWidth := 367;
  Color := clBtnFace;
  Position := poScreenCenter;
  OnDeactivate := FormDeactivate;
  LabelTextToFind:=TLabel.Create(Self);
  with LabelTextToFind do
  begin
    Name := 'LabelTextToFind';
    Parent := Self;
    Left := 8;
    Top := 16;
    Width := 30;
    Height := 13;
    Caption := 'Te&xto:';
  end;
  EditTextToFind:=TEdit.Create(Self);
  with EditTextToFind do
  begin
    Name := 'EditTextToFind';
    Parent := Self;
    Left := 48;
    Top := 12;
    Width := 229;
    Height := 21;
    TabOrder := 0;
  end;
  BitBtnFindNext:=TBitBtn.Create(Self);
  with BitBtnFindNext do
  begin
    Name := 'BitBtnFindNext';
    Parent := Self;
    Left := 284;
    Top := 12;
    Width := 75;
    Height := 21;
    Caption := '&Próxima';
    Default := True;
    TabOrder := 1;
    OnClick := BitBtnFindNextClick;
  end;
  BitBtnCancel:=TBitBtn.Create(Self);
  with BitBtnCancel do
  begin
    Name := 'BitBtnCancel';
    Parent := Self;
    Left := 284;
    Top := 36;
    Width := 75;
    Height := 21;
    Cancel := True;
    Caption := '&Cancelar';
    TabOrder := 2;
    OnClick := BitBtnCancelClick;
  end;
  CheckBoxWholeWords:=TCheckBox.Create(Self);
  with CheckBoxWholeWords do
  begin
    Name := 'CheckBoxWholeWords';
    Parent := Self;
    Left := 8;
    Top := 44;
    Width := 133;
    Height := 17;
    Caption := 'Palavras &inteiras';
    TabOrder := 3;
  end;
  CheckBoxMatchCase:=TCheckBox.Create(Self);
  with CheckBoxMatchCase do
  begin
    Name := 'CheckBoxMatchCase';
    Parent := Self;
    Left := 8;
    Top := 64;
    Width := 193;
    Height := 17;
    Caption := 'Diferenciar &maiúsculas e minúsculas';
    TabOrder := 4;
  end;
  RadioGroupDirection:=TRadioGroup.Create(Self);
  with RadioGroupDirection do
  begin
    Name := 'RadioGroupDirection';
    Parent := Self;
    Left := 204;
    Top := 36;
    Width := 73;
    Height := 49;
    Caption := ' Direção ';
    Items.Text := 'A&cima'#13'A&baixo';
    ItemIndex := 1;
    TabOrder := 5;
  end;
  //
  Caption                     :=LS_FindCaptionStr;
  LabelTextToFind.Caption     :=LS_TextToFindStr+':';
  EditTextToFind.Text         :='';
  BitBtnFindNext.Caption      :=LS_FindNextStr;
  BitBtnCancel.Caption        :=LS_CancelStr;
  CheckBoxWholeWords.Caption  :=LS_WholeWordsStr;
  CheckBoxMatchCase.Caption   :=LS_MatchCaseStr;
  RadioGroupDirection.Caption :=' '+LS_DirectionCaptionStr+' ';
  RadioGroupDirection.Items[0]:=LS_DirectionUpStr;
  RadioGroupDirection.Items[1]:=LS_DirectionDownStr;
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

