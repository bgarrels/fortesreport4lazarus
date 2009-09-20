unit NFDesign;

{$MODE Delphi}

interface

uses
  LCLIntf, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, dbf, ExtCtrls, LResources, RLReport, RLParser;

type

  { TNFDesignForm }

  TNFDesignForm = class(TForm)
    DatasetCLICLI_CODIGO: TLongintField;
    DatasetCLICLI_NOME: TStringField;
    DatasetINFINF_ITE_CODIGO: TLongintField;
    DatasetINFINF_NF_NUMERO: TStringField;
    DatasetINFINF_QUANT: TFloatField;
    DatasetINFINF_SUBTOTAL: TFloatField;
    DatasetINFINF_VALOR: TFloatField;
    DatasetINFINF_ITE_NOME: TStringField;
    DatasetITEITE_CODIGO: TLongintField;
    DatasetITEITE_GRUPO: TStringField;
    DatasetITEITE_NOME: TStringField;
    DatasetNF: TDbf;
    DatasetINF: TDbf;
    DatasetCLI: TDbf;
    DatasetITE: TDbf;
    DatasetNFNF_CLI_NOME: TStringField;
    DatasetNFNF_CLI_CODIGO: TLongintField;
    DatasetNFNF_DATA: TDateField;
    DatasetNFNF_NUMERO: TStringField;
    RLReport1: TRLReport;
    DataSourceNF: TDataSource;
    DataSourceINF: TDataSource;
    RLBand1: TRLBand;
    RLDBText1: TRLDBText;
    RLSubDetail1: TRLSubDetail;
    RLBand2: TRLBand;
    RLBand3: TRLBand;
    RLBand4: TRLBand;
    RLBand5: TRLBand;
    RLLabel1: TRLLabel;
    RLLabel2: TRLLabel;
    RLDBText2: TRLDBText;
    RLLabel3: TRLLabel;
    RLDBResult1: TRLDBResult;
    RLDBText3: TRLDBText;
    RLLabel4: TRLLabel;
    RLLabel5: TRLLabel;
    RLLabel6: TRLLabel;
    RLDBText4: TRLDBText;
    RLDBText5: TRLDBText;
    RLDBText6: TRLDBText;
    DataSourceCLI: TDataSource;
    DataSourceITE: TDataSource;
    RLLabel7: TRLLabel;
    RLDBText7: TRLDBText;
    RLLabel8: TRLLabel;
    RLDBText8: TRLDBText;
    RLBand6: TRLBand;
    RLImage1: TRLImage;
    RLSystemInfo1: TRLSystemInfo;
    RLSystemInfo2: TRLSystemInfo;
    RLExpressionParser1: TRLExpressionParser;
    procedure TableINFCalcFields(DataSet: TDataSet);
    procedure RLReport1BeforePrint(Sender: TObject; var PrintIt: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  NFDesignForm: TNFDesignForm;

implementation

procedure TNFDesignForm.TableINFCalcFields(DataSet: TDataSet);
begin
  DatasetINFINF_SUBTOTAL.Value := DatasetINFINF_VALOR.Value*DatasetINFINF_QUANT.Value;
end;

procedure TNFDesignForm.RLReport1BeforePrint(Sender: TObject;
  var PrintIt: Boolean);
begin
  DatasetCLI.Open;
  DatasetITE.Open;
  DatasetNF.Open;
  DatasetINF.Open;
  RLReport1.Pages.Macros.Values['FAT'] := 'teste';
end;

initialization
  {$i NFDesign.lrs}

end.

