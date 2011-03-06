{$I RLReport.inc}

{@unit RLPrinters - Implementação do wrapper para o objeto Printer.
Portado para o Lazarus - Trabalho inicial de Isaac Trindade da Silva contato tioneobrasil@yahoo.com.br dicas4lazarus@yahoo.com.br
Lazarus Ported - initial work by Isaac 07/2009
}
unit RLPrinters;
{$MODE DELPHI}{$H+}
interface

uses
  {$ifdef Windows}Windows, WinUtilPrn,{$endif}
  Classes, SysUtils, Math, forms,
  Graphics, Printers, RLConsts, RLTypes, RLUtils;

type
  TRLPrinterWrapper=class
  private
    fPrinters    :TStrings;
    fCustomWidth :double;
    fCustomHeight:double;
    //
    function    GetPrinterIndex:integer;
    procedure   SetPrinterIndex(const Value:integer);
    function    GetCopies:integer;
    procedure   SetCopies(const Value:integer);
    function    GetPrinterName:string;
    procedure   SetPrinterName(const aPrinterName:string);
    function    GetPrinterPort:string;
    procedure   LoadPrintersList(aPrinters:TStrings);
    procedure   PrintersNeeded;
    function    AnyPrinter:boolean;
    function    GetPrinterNames(aIndex:integer):string;
    function    GetPrinterPorts(aIndex:integer):string;
    procedure   SelectSystemPaperSize(aPaperSize:TRLSystemPaperType; aPaperWidthMM,aPaperHeightMM:double; aOrientation:TRLSystemOrientation);
    function    GetCanvas:TCanvas;
  public
    destructor  Destroy; override;
    //
    procedure   BeginDoc;
    procedure   EndDoc;
    procedure   AbortDoc;
    //
    procedure   NewPage;
    function    Printers:TStrings;
    procedure   Refresh;
    Procedure   PrintDocName(const Descricao: String);
    //
    procedure   SetPaperSize(aPaperWidthMM,aPaperHeightMM:double; aOrientationLandscape:boolean; aForceEmulation:boolean);
    procedure   LoadMetrics(var aPrinterMetrics:TRLPrinterMetrics);
    //
    property    PrinterIndex:integer     read GetPrinterIndex write SetPrinterIndex;
    property    PrinterName :string      read GetPrinterName  write SetPrinterName;
    property    PrinterPort :string      read GetPrinterPort;
    property    Copies      :integer     read GetCopies       write SetCopies;
    property    Canvas      :TCanvas     read GetCanvas;
    //
    property    PrinterNames[aIndex:integer]:string read GetPrinterNames;
    property    PrinterPorts[aIndex:integer]:string read GetPrinterPorts;
  end;

function RLPrinter:TRLPrinterWrapper;

implementation

type
  TPrinterAccess = class(TPrinter)

  end;

var
  WarningDisplayed :boolean;
  RLPrinterInstance:TRLPrinterWrapper;

  // UTILS

function RLPrinter:TRLPrinterWrapper;
begin
  if not Assigned(RLPrinterInstance) then
    RLPrinterInstance:=TRLPrinterWrapper.Create;
  Result:=RLPrinterInstance;
end;

function TruePrinterName(const aPrinterName:string):string;
var
  i:integer;
begin
  i:=Pos(' on ',aPrinterName);
  if i>0 then
    Result:=Copy(aPrinterName,1,i-1)
  else
    Result:=aPrinterName;
end;

function TruePrinterPort(const aPrinterName:string):string;
var
  i:integer;
begin
  i:=Pos(' on ',aPrinterName);
  if i>0 then
    Result:=Copy(aPrinterName,i+4,length(aPrinterName))
  else
    Result:=aPrinterName;
end;

{$ifdef Windows}
function GetPrinterDevice(Index:integer):string;
const
  PRINTER_ACCESS_USE=$00000008;
  foodefs:TPrinterDefaults=(pDatatype:nil; pDevMode:nil; DesiredAccess:PRINTER_ACCESS_USE);
var
  PrinterDevice: TPrinterDevice;
  ok         :boolean;
  pInfo      :PPrinterInfo2;
  bytesNeeded:DWORD;
  hPrinter   :THandle;
begin
  Result:='';
  if Printer.Printers.Count > 0 then
    PrinterDevice := TPrinterDevice(Printer.Printers.Objects[Printer.PrinterIndex])
  else
    Exit;

  if not OpenPrinter(PChar(PrinterDevice.Name),hPrinter,@foodefs) then
    RaiseLastOSError;
  try
    GetPrinter(hPrinter,2,nil,0,bytesNeeded);
    if bytesNeeded=0 then
      bytesNeeded:=32768;
    pInfo:=AllocMem(bytesNeeded);
    try
      GetPrinter(hPrinter,2,pInfo,bytesNeeded,bytesNeeded);
      if pInfo^.pServerName<>emptystr then
        Result:=pInfo^.pServerName+'\'+pInfo^.pShareName
      else
        Result:=pInfo^.pPortName;
    finally
      FreeMem(pInfo);
    end;
  finally
    ClosePrinter(hPrinter);
  end;
end;
{$else}
function GetPrinterDevice(Index:integer):string;
var
  AProcess: TProcess;
  l:TStringList;
  i,i2,p:integer;
  s:string;
  devicename,printername:string;
begin
  Result:=emptystr;
  l:=TStringList.Create;
  printername:=Printer.Printers[Index];
  try
  try
  AProcess := TProcess.Create(nil);
  AProcess.CommandLine := 'lpstat -v';
  //AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];
  AProcess.Options := [poWaitOnExit, poUsePipes];
  AProcess.Execute;
  l.LoadFromStream(AProcess.Output);
  except
  showmessage('lpstat error');
  end;
  finally
  AProcess.Free;
  end;
  devicename:='';
  for i:=0 to l.Count-1 do
  begin
    s:=l[i];
    if pos(printername+':',s)>0 then
    begin
      p:=pos(':',s)+1;
      while true do
      begin
        p:=p+1;
        if copy(s,p,1)=':' then
        begin
          p:=p+1;
          i2:=1+length(s)-p;
          devicename:=copy(s,p,i2);
          break;
        end;
      end;

    end;
  end;
  Result:=devicename;
end;
{$endif}


{ TRLPrinterWrapper }

destructor TRLPrinterWrapper.Destroy;
begin
  fPrinters.free;
  //
  inherited;
end;

procedure TRLPrinterWrapper.PrintersNeeded;
begin
  if not Assigned(fPrinters) then
  begin
    fPrinters:=TStringList.Create; 
    LoadPrintersList(fPrinters);
  end;
end;

procedure TRLPrinterWrapper.BeginDoc;
begin
  Printer.BeginDoc;
end;

procedure TRLPrinterWrapper.EndDoc;
var
  savedprinterindex:integer;
begin
  // Na CLX o método EndDoc destroi o objeto PrinterAdapter.
  // Com isso se perde a referência para o OutputDevice escolhido. Nele se baseia o nosso PrinterIndex.
  // Temos que salvá-lo aqui e restaurar depois do EndDoc.
  savedprinterindex:=PrinterIndex;
  Printer.EndDoc;
  PrinterIndex:=savedprinterindex;
end;

procedure TRLPrinterWrapper.AbortDoc;
begin
  Printer.Abort;
end;

procedure TRLPrinterWrapper.NewPage;
begin
  Printer.NewPage;
end;

function TRLPrinterWrapper.GetCanvas:TCanvas;
begin
  Result:=Printer.Canvas;
end;

procedure TRLPrinterWrapper.SelectSystemPaperSize(aPaperSize:TRLSystemPaperType; aPaperWidthMM,aPaperHeightMM:double; aOrientation:TRLSystemOrientation);
begin
  //todo
end;

procedure TRLPrinterWrapper.SetPaperSize(aPaperWidthMM,aPaperHeightMM:double;
  aOrientationLandscape:boolean; aForceEmulation:boolean);
var
  ResultPaperSize  :TRLSystemPaperType;
  ResultPaperWidth :double;
  ResultPaperHeight:double;
  ResultOrientation:TRLSystemOrientation;
begin
  DetectPaperSize(aPaperWidthMM,aPaperHeightMM,aOrientationLandscape,aForceEmulation,
    ResultPaperSize,ResultPaperWidth,ResultPaperHeight,ResultOrientation);
  SelectSystemPaperSize(ResultPaperSize,ResultPaperWidth,ResultPaperHeight,ResultOrientation);
  fCustomWidth :=aPaperWidthMM;
  fCustomHeight:=aPaperHeightMM;
end;

procedure TRLPrinterWrapper.LoadMetrics(var aPrinterMetrics:TRLPrinterMetrics);

begin
  try
    if not AnyPrinter then
      raise Exception.Create(LS_PrinterNotFoundStr);
    aPrinterMetrics.PPIX          :=Printer.XDPI;
    aPrinterMetrics.PPIY          :=Printer.YDPI;
    if Printer.PageWidth=0 then
      aPrinterMetrics.PhysicalWidth:=Round(fCustomWidth*aPrinterMetrics.PPIX/InchAsMM)
    else
      aPrinterMetrics.PhysicalWidth:=Printer.PageWidth;
    if Printer.PageHeight=0 then
      aPrinterMetrics.PhysicalHeight:=Round(fCustomHeight*aPrinterMetrics.PPIY/InchAsMM)
    else
      aPrinterMetrics.PhysicalHeight:=Printer.PageHeight;
    aPrinterMetrics.MarginLeft      :=TPrinterCanvas(Printer.Canvas).LeftMargin;
    aPrinterMetrics.MarginTop       :=TPrinterCanvas(Printer.Canvas).TopMargin;
    aPrinterMetrics.MarginRight     :=TPrinterCanvas(Printer.Canvas).RightMargin;
    aPrinterMetrics.MarginBottom    :=TPrinterCanvas(Printer.Canvas).BottomMargin;
    aPrinterMetrics.ClientWidth     :=aPrinterMetrics.PhysicalWidth -(aPrinterMetrics.MarginLeft+aPrinterMetrics.MarginRight);
    aPrinterMetrics.ClientHeight    :=aPrinterMetrics.PhysicalHeight-(aPrinterMetrics.MarginTop+aPrinterMetrics.MarginBottom);
  except
    on e:Exception do
    begin
      // configuração padrão da "HP LaserJet Plus"
      aPrinterMetrics.PPIX          :=300;
      aPrinterMetrics.PPIY          :=300;
      aPrinterMetrics.PhysicalWidth :=2550;
      aPrinterMetrics.PhysicalHeight:=3300;
      aPrinterMetrics.MarginLeft    :=75;
      aPrinterMetrics.MarginTop     :=75;
      aPrinterMetrics.MarginRight   :=aPrinterMetrics.MarginLeft;
      aPrinterMetrics.MarginBottom  :=aPrinterMetrics.MarginTop;
      aPrinterMetrics.ClientWidth   :=aPrinterMetrics.PhysicalWidth -(aPrinterMetrics.MarginLeft+aPrinterMetrics.MarginRight);
      aPrinterMetrics.ClientHeight  :=aPrinterMetrics.PhysicalHeight-(aPrinterMetrics.MarginTop+aPrinterMetrics.MarginBottom);
      //
      if not WarningDisplayed then
         WarningDisplayed:=True;
    end;
  end;
end;

function TRLPrinterWrapper.Printers:TStrings;
begin
  PrintersNeeded;
  Result:=fPrinters;
end;

function TRLPrinterWrapper.GetPrinterNames(aIndex:integer):string;
begin
  Result:=Token(Printers[aIndex],1,'|');
end;

function TRLPrinterWrapper.GetPrinterPorts(aIndex:integer):string;
begin
  Result:=Token(Printers[aIndex],2,'|');
end;

procedure TRLPrinterWrapper.LoadPrintersList(aPrinters:TStrings);
begin
  aPrinters.Assign(Printer.Printers);
end;

function TRLPrinterWrapper.AnyPrinter:boolean;
begin
  TPrinterAccess(Printer).SelectCurrentPrinterOrDefault;
  Result := Printer.PrinterIndex > -1;
end;

function TRLPrinterWrapper.GetPrinterPort:string;
begin
  //todo
  if not AnyPrinter then
    Result:=emptystr
  else if (PrinterIndex<0) or not (PrinterIndex<Printers.Count) then
    Result:=emptystr
  else
    Result:=GetPrinterDevice(PrinterIndex);
end;

function TRLPrinterWrapper.GetPrinterName:string;
begin
  if not AnyPrinter then
    Result:=emptystr
  else if (PrinterIndex<0) or not (PrinterIndex<Printers.Count) then
    Result:=emptystr
  else
    Result:=Token(Printers[PrinterIndex],1,'|');
end;

procedure TRLPrinterWrapper.SetPrinterName(const aPrinterName:string);
var
  n:string;
  i:integer;
begin
  n:=TruePrinterName(Token(aPrinterName,1,'|'));
  for i:=0 to Printers.Count-1 do
    if AnsiSameText(TruePrinterName(Token(Printers[i],1,'|')),n) then
    begin
      SetPrinterIndex(i);
      break;
    end;
end;

function TRLPrinterWrapper.GetPrinterIndex:integer;
begin
  AnyPrinter;
  Result:=Printer.PrinterIndex;
end;

procedure TRLPrinterWrapper.SetPrinterIndex(const Value:integer);
begin
  PrintersNeeded;
  Printer.PrinterIndex := Value;
end;

procedure TRLPrinterWrapper.Refresh;
begin
  //todo map fPrinters to Printer.Printers
  FreeAndNil(fPrinters);
  Printer.Refresh;
end;

function TRLPrinterWrapper.GetCopies:integer;
begin
  Result:=Max(1,Printer.Copies);
end;

procedure TRLPrinterWrapper.SetCopies(const Value:integer);
begin
  Printer.Copies:=Value;
end;

//Manda o nome do documento para o spool do windows
procedure TRLPrinterWrapper.PrintDocName(const Descricao: String);
begin
  Printer.Title := Descricao;
end;

initialization
  WarningDisplayed :=False;
//  RLPrinterInstance:=nil;

finalization
  if Assigned(RLPrinterInstance) then
    RLPrinterInstance.free;

end.

