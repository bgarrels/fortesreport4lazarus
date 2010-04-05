unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, RLHTMLFilter, RLRichFilter, RLPDFFilter;

type

  { TMainForm }

  TMainForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    PreviewItensButton: TButton;
    CloseButton: TButton;
    PreviewNFButton: TButton;
    RLHTMLFilter1: TRLHTMLFilter;
    RLPDFFilter1: TRLPDFFilter;
    RLRichFilter1: TRLRichFilter;
    procedure CloseButtonClick(Sender: TObject);
    procedure PreviewItensButtonClick(Sender: TObject);
    procedure PreviewNFButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  MainForm: TMainForm;

implementation

uses
  NFDesign, ITDesign;

{ TMainForm }

procedure TMainForm.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.PreviewItensButtonClick(Sender: TObject);
begin
  with TITDesignForm.Create(nil) do
  try
    RLReport1.Preview;
  finally
    Destroy;
  end;
end;

procedure TMainForm.PreviewNFButtonClick(Sender: TObject);
begin
  with TNFDesignForm.Create(nil) do
  try
    RLReport1.Preview;
  finally
    Destroy;
  end;
end;

initialization
  {$I main.lrs}

end.

