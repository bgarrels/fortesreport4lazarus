{@unit RLDraftFilterDialog
Portado para o Lazarus - Trabalho inicial de Isaac Trindade da Silva contato tioneobrasil@yahoo.com.br dicas4lazarus@yahoo.com.br
Lazarus Ported - initial work by Isaac 07/2009
}
unit RLDraftFilterDialog;

{$MODE DELPHI}
{$I RLReport.inc}

interface

uses
  SysUtils, Classes,
  LCLIntf, LCLType, LResources,
  Controls, Buttons, ExtCtrls, Forms, Dialogs, StdCtrls, Graphics,
  RLConsts, RLDraftFilter;

type

  { TRLDraftFilterDialogForm }

  TRLDraftFilterDialogForm = class(TForm)
    ButtonOk: TButton;
    ButtonCancel: TButton;
    RadioGroupFormulary: TRadioGroup;
  private
    { Private declarations }
    Filter:TRLDraftFilter;
  public
    { Public declarations }
    class function Execute(Filter:TRLDraftFilter):boolean;
  end;

var
  RLDraftFilterDialogForm: TRLDraftFilterDialogForm;

implementation

{ TRLDraftFilterDialogForm }

class function TRLDraftFilterDialogForm.Execute(
  Filter: TRLDraftFilter): boolean;
var
  Form:TRLDraftFilterDialogForm;
begin
  Form := TRLDraftFilterDialogForm.Create(nil);
  try
    Form.ButtonOk.Caption := LS_OkStr;
    with Form.RadioGroupFormulary do
    begin
      Items.Add('80 '+LS_ColumnsStr);
      Items.Add('132 '+LS_ColumnsStr);
    end;

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
    Destroy;
  end;
end;

initialization
  {$i rldraftfilterdialog.lrs}
end.
