unit Main;

{$MODE Delphi}

interface

uses
  LCLIntf, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, DB, dbf, Buttons, LResources, RLReport;

type

  { TMainForm }

  TMainForm = class(TForm)
    Dbf1: TDbf;
    ok: TBitBtn;
    DataSource1: TDataSource;
    RLReport1: TRLReport;
    RLGroup1: TRLGroup;
    RLDetailGrid1: TRLDetailGrid;
    RLDBText1: TRLDBText;
    RLDBText2: TRLDBText;
    RLLabel1: TRLLabel;
    RLLabel3: TRLLabel;
    RLDetailGrid2: TRLDetailGrid;
    RLLabel4: TRLLabel;
    RLDBResult2: TRLDBResult;
    procedure FormCreate(Sender: TObject);
    procedure okClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation


procedure TMainForm.okClick(Sender: TObject);
begin
  RLReport1.PreviewModal;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Dbf1.Open;
end;

initialization
  {$i Main.lrs}

end.
