{$I RLReport.inc}
{@unit RLDraftFilterDialog
Portado para o Lazarus - Trabalho inicial de Isaac Trindade da Silva contato tioneobrasil@yahoo.com.br dicas4lazarus@yahoo.com.br
Lazarus Ported - initial work by Isaac 07/2009
}
unit RLDraftFilterDialog;
{$MODE DELPHI}
interface

uses
  SysUtils, Classes,
{$ifdef VCL}
  {$IFDEF FPC}
  LCLIntf, LCLType,
  {$ELSE}
  Windows,
  {$ENDIF}
  Controls, Buttons, ExtCtrls, Forms, Dialogs, StdCtrls, Graphics,
{$else}
  Types, QControls, Qt, QButtons, QExtCtrls, QForms, QDialogs, QStdCtrls, QTypes, QGraphics,
{$endif}
  RLConsts, RLDraftFilter;

type
  TRLDraftFilterDialogForm = class(TForm)
    ButtonOk: TButton;
    ButtonCancel: TButton;
    RadioGroupFormulary: TRadioGroup;
  private
    { Private declarations }
    Filter:TRLDraftFilter;
    procedure Init;
  public
    { Public declarations }
    class function Execute(Filter:TRLDraftFilter):boolean;
  end;

var
  RLDraftFilterDialogForm: TRLDraftFilterDialogForm;

implementation

///{$R *.dfm}

{ TRLDraftFilterDialogForm }

class function TRLDraftFilterDialogForm.Execute(
  Filter: TRLDraftFilter): boolean;
var
  Form:TRLDraftFilterDialogForm;
begin
  {$IFDEF FPC}
  Form:=TRLDraftFilterDialogForm.CreateNew(nil,0);
  {$ELSE}
  Form:=TRLDraftFilterDialogForm.CreateNew(nil);
  {$ENDIF}
  try
    Form.Init;
    Form.Filter:=Filter;
    case Filter.FormSelection of
      fs80Cols : Form.RadioGroupFormulary.ItemIndex:=0;
      fs132Cols: Form.RadioGroupFormulary.ItemIndex:=1;
    else
      Form.RadioGroupFormulary.ItemIndex:=0;
    end;
    Result:=(Form.ShowModal=mrOk);
    if Result then
      case Form.RadioGroupFormulary.ItemIndex of
        0: Filter.FormSelection:=fs80Cols;
        1: Filter.FormSelection:=fs132Cols;
      end;
  finally
    Form.Free;
  end;
end;

procedure TRLDraftFilterDialogForm.Init;
begin
  Left := 512;
  Top := 475;
{$ifdef VCL}
  BorderStyle := bsDialog
{$else}
  BorderStyle := fbsDialog;
{$endif};
  Caption := LS_OptionsStr;
  ClientHeight := 122;
  ClientWidth := 262;
  Color := clBtnFace;
  Font.Charset := DEFAULT_CHARSET;
  Font.Color := clWindowText;
  Font.Height := -11;
  Font.Name := 'MS Sans Serif';
  Font.Style := [];
  Position := poScreenCenter;
  PixelsPerInch := 96;
  ButtonOk:=TButton.Create(Self);
  with ButtonOk do
  begin
    Parent := Self;
    Name := 'ButtonOk';
    Left := 92;
    Top := 88;
    Width := 75;
    Height := 25;
    Caption := LS_OkStr;
    Default := True;
    ModalResult := 1;
    TabOrder := 0;
  end;
  ButtonCancel:=TButton.Create(Self);
  with ButtonCancel do
  begin
    Parent := Self;
    Name := 'ButtonCancel';
    Left := 176;
    Top := 88;
    Width := 75;
    Height := 25;
    Cancel := True;
    Caption := LS_CancelStr;
    ModalResult := 2;
    TabOrder := 1;
  end;
  RadioGroupFormulary:=TRadioGroup.Create(Self);
  with RadioGroupFormulary do
  begin
    Parent := Self;
    Name := 'RadioGroupFormulary';
    Left := 8;
    Top := 8;
    Width := 245;
    Height := 65;
    Caption := ' '+LS_FormStr+': ';
    TabOrder := 2;
    Items.Clear;
    Items.Add('80 '+LS_ColumnsStr);
    Items.Add('132 '+LS_ColumnsStr);
    ItemIndex:= 0;
  end;
end;

end.
