unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls;

type

  { TMainForm }

  TMainForm = class(TForm)
    CancelButton: TButton;
    PreviewButton: TButton;
    procedure CancelButtonClick(Sender: TObject);
    procedure PreviewButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  MainForm: TMainForm;

implementation

uses
  UnitDesign;

{ TMainForm }

procedure TMainForm.PreviewButtonClick(Sender: TObject);
begin
  FormDesign.RLReport1.Preview;
end;

procedure TMainForm.CancelButtonClick(Sender: TObject);
begin
  Close;
end;

initialization
  {$I Main.lrs}

end.

