{@unit RLDesing
Portado para o Lazarus - Trabalho inicial de Isaac Trindade da Silva contato tioneobrasil@yahoo.com.br dicas4lazarus@yahoo.com.br
Lazarus Ported - initial work by Isaac 07/2009

}
unit RLDesign;

{$MODE DELPHI}
{$I RLReport.inc}

interface

uses
  Classes, TypInfo, Db, SysUtils,
  PropEdits, ComponentEditors, LCLType,
  Forms,
  RLReport, RLConsts, RLUtils, RLTypes,
  RLAbout;

type

  { TRLReportDesigner }

  TRLReportDesigner=class(TComponentEditor)
  protected
    // variables
    fReport:TRLReport;
    // custom methods
    procedure   ShowAboutBox;
  public
    // constructors & destructors
    constructor Create(aComponent:TComponent; aDesigner: TComponentEditorDesigner); override;
    // override methods
    procedure   Edit; override;
    procedure   ExecuteVerb(Index:Integer); override;
    function    GetVerb(Index:Integer):string; override;
    function    GetVerbCount:integer; override;
  end;

  // property editors

  { TRLListEditor }

  TRLListEditor=class(TStringProperty)
  public
    function  GetAttributes:TPropertyAttributes; override;
    procedure GetValues(Proc:TGetStrProc); override;
    //
    procedure GetValueList(List:TStrings); virtual; abstract;
  end;

  TRLDataEditor=class(TRLListEditor)
  public
    procedure GetValueList(List:TStrings); override;
    //
    function GetDataSource:TDataSource; virtual; abstract;
  end;

  TRLDataFieldEditor=class(TRLDataEditor)
  public
    function GetDataSource:TDataSource; override;
  end;

  TRLDataFieldsEditor=class(TRLDataEditor)
  public
    function GetDataSource:TDataSource; override;
  end;

  TRLPaperSizeEditor=class(TRLListEditor)
  public
    procedure GetValueList(List:TStrings); override;
  end;

implementation

{ TRLReportDesigner }

constructor TRLReportDesigner.Create(aComponent:TComponent; aDesigner: TComponentEditorDesigner);
begin
  inherited;
  fReport:=TRLReport(aComponent);
end;

function TRLReportDesigner.GetVerb(Index:Integer):string;
begin
  case Index of
    0: result:=LS_AboutTheStr+' '+CS_ProductTitleStr+'...';
    1: result:='-';
    2: result:=LS_PreviewStr;
  end;
end;

function TRLReportDesigner.GetVerbCount:integer;
begin
  result:=3;
end;

procedure TRLReportDesigner.ShowAboutBox;
begin
  with TfrmRLAbout.Create(nil) do
  try
    ShowModal;
  finally
    free;
  end;
end;

procedure TRLReportDesigner.Edit;
begin
{$ifdef unix}
  fReport.ShowProgress := False;
{$endif}
  fReport.Preview;
end;

procedure TRLReportDesigner.ExecuteVerb(Index:Integer);
begin
  case Index of
    0: ShowAboutBox;
    1:;
    2: Edit;
  end;
end;

function GetPropertyValue(Instance:TPersistent; const PropName:string):TPersistent;
var
  PropInfo:PPropInfo;
begin
  result  :=nil;
  PropInfo:=TypInfo.GetPropInfo(Instance.ClassInfo,PropName);
  if (PropInfo<>nil) and (PropInfo^.PropType^.Kind=tkClass) then
    result:=TObject(GetOrdProp(Instance,PropInfo)) as TPersistent;
end;

{ TRLListEditor }

function TRLListEditor.GetAttributes:TPropertyAttributes;
begin
  result:=[paValueList,paSortList,paMultiSelect];
end;

procedure TRLListEditor.GetValues(Proc:TGetStrProc);
var
  l:TStringList;
  i:integer;
begin
  l:=TStringList.Create;
  try
    GetValueList(l);
    for i:=0 to l.Count-1 do
      Proc(l[i]);
  finally
    l.free;
  end;
end;

{ TRLDataEditor }

procedure TRLDataEditor.GetValueList(List:TStrings);
var
  ds:TDataSource;
begin
  ds:=GetDataSource;
  if (ds<>nil) and (ds.DataSet<>nil) then
    ds.DataSet.GetFieldNames(List);
end;

{ TRLDataFieldEditor }

function TRLDataFieldEditor.GetDataSource:TDataSource;
begin
  result:=GetPropertyValue(GetComponent(0),'DataSource') as TDataSource;
end;

{ TRLDataFieldsEditor }

function TRLDataFieldsEditor.GetDataSource:TDataSource;
var
  skipper:TRLCustomSkipper;
begin
  skipper:=TRLGroup(GetComponent(0)).FindParentSkipper;
  if skipper<>nil then
    result:=skipper.DataSource
  else
    result:=nil;
end;

{ TRLPaperSizeEditor }

procedure TRLPaperSizeEditor.GetValueList(List:TStrings);
var
  i:TRLPaperSize;
  v:string;
begin
  for i:=low(TRLPaperSize) to high(TRLPaperSize) do
  begin
    v:=PaperInfo[i].Description;
    if PaperInfo[i].Emulated then
      v:=v+'*';
    List.Add(v);
  end;  
end;

end.
