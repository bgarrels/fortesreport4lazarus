unit ITDesign;

{$MODE Delphi}

interface

uses
  LCLIntf, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, LResources, Db, Sqlite3DS, RLReport;

type

  { TITDesignForm }

  TITDesignForm = class(TForm)
    DataSourceITE: TDataSource;
    RLBand5: TRLBand;
    RLReport1: TRLReport;
    RLGroup1: TRLGroup;
    RLBand2: TRLBand;
    RLBand3: TRLBand;
    RLBand4: TRLBand;
    DatasetITE: TSqlite3Dataset;
    TableITEITE_CODIGO: TIntegerField;
    TableITEITE_NOME: TStringField;
    TableITEITE_GRUPO: TStringField;
    RLLabel1: TRLLabel;
    RLLabel2: TRLLabel;
    RLLabel3: TRLLabel;
    RLDBText2: TRLDBText;
    RLDBText3: TRLDBText;
    RLDBText4: TRLDBText;
    RLBand1: TRLBand;
    RLSystemInfo1: TRLSystemInfo;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ITDesignForm: TITDesignForm;

implementation


{ TITDesignForm }

procedure TITDesignForm.FormCreate(Sender: TObject);
begin
  //Is necessary to order by ITE_GRUPO so the grouping can work correct
  DatasetITE.SQL := 'Select * from ITE Order By ITE_GRUPO';
  DatasetITE.Open;
end;

initialization
  {$i ITDesign.lrs}

end.
