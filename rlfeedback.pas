{$I RLReport.inc}
{@unit RLFeedBack
Portado para o Lazarus - Trabalho inicial de Isaac Trindade da Silva contato tioneobrasil@yahoo.com.br dicas4lazarus@yahoo.com.br
Lazarus Ported - initial work by Isaac 07/2009

}
unit RLFeedBack;
{$MODE DELPHI}{$H+}
interface

uses
  SysUtils, Classes,
  Graphics, Forms, Dialogs, StdCtrls, ExtCtrls, Buttons, Controls, ComCtrls;

type
  TfrmRLFeedBack = class(TForm)
    BitBtnCancel: TBitBtn;
    ProgressBar: TProgressBar;
    TimerBlink: TTimer;
    LabelPhase: TLabel;
    procedure BitBtnCancelClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TimerBlinkTimer(Sender: TObject);
  private
    { Private declarations }
    BarIndex:integer;
    Bars    :TFpList;
    //
    procedure   Init;
  public
    { Public declarations }
    Canceled:boolean;
    Finished:boolean;
    //
    constructor Create(const aTitle:string; aCount:integer=1); reintroduce;
    //
    procedure   SetMax(n:integer);
    procedure   StepIt;
    procedure   SetPhase(const aPhase:string);
    procedure   NextBar;
    procedure   Finish;
  end;

var
  frmRLFeedBack: TfrmRLFeedBack;

implementation

uses
  RLReport, RLConsts;

procedure TfrmRLFeedBack.Init;
begin
  Width := 380;
  Height := 100;
  HorzScrollBar.Range := 61;
  VertScrollBar.Range := 45;
  ActiveControl := BitBtnCancel;
  AutoScroll := False;
  Caption := Ls_Progresso;
  BorderStyle := bsDialog;
  Position := poScreenCenter;
  OnDestroy := FormDestroy;
  ProgressBar := TProgressBar.Create(Self);
  with ProgressBar do
  begin
    Name := 'ProgressBar';
    Parent := Self;
    Top := 28;
    Width := 350;
    Left := (Self.ClientWidth - Width) div 2;
    Min := 0;
    Max := 100;
    Step := 1;
    Smooth := True;
  end;
  LabelPhase := TLabel.Create(Self);
  with LabelPhase do
  begin
    Name := 'LabelPhase';
    Parent := Self;
    Left := ProgressBar.Left;
    Top := 12;
    Caption := 'LabelPhase';
  end;
  BitBtnCancel := TBitBtn.Create(Self);
  with BitBtnCancel do
  begin
    Name := 'BitBtnCancel';
    Parent := Self;
    Left := (Self.ClientWidth - Width) div 2;
    Top := 60;
    Caption := LS_CancelStr;
    OnClick := BitBtnCancelClick;
    Kind := bkCancel;
  end;
  TimerBlink:=TTimer.Create(Self);
  with TimerBlink do
  begin
    Name := 'TimerBlink';
    Enabled := False;
    Interval := 300;
    OnTimer := TimerBlinkTimer;
    Left := 4;
    Top := 64;
  end;
  //
  BitBtnCancel.Caption:=LS_CancelStr;
  LabelPhase.Caption  :=LS_WaitStr;
end;

constructor TfrmRLFeedBack.Create(const aTitle:string; aCount:integer=1);
var
  i,h,d:integer;
  b,n:TProgressBar;
begin
  inherited CreateNew(nil, 0);
  Init;
  //
  d:=Height-BitBtnCancel.Top;
  Caption:=aTitle;
  Bars:=TFpList.create;
  h:=0;
  b:=ProgressBar;
  Bars.Add(b);
  for i:=2 to aCount do
  begin
    n:=TProgressBar.Create(self);
    n.Name      :='ProgressBar'+IntToStr(i);
    n.Parent    :=b.Parent;
    n.Boundsrect:=b.BoundsRect;
    n.Step      :=b.Step;
    n.Top       :=b.Top+b.Height+2;
    n.Smooth:=True;
    inc(h,b.Height+2);
    b:=n;
    Bars.Add(b);
  end;
  Height  :=Height+h;
  BitBtnCancel.Top:=Height-d;
  //BarIndex:=0;
  //Canceled:=False;
  //Finished:=False;
end;

procedure TfrmRLFeedBack.BitBtnCancelClick(Sender: TObject);
begin
  if BitBtnCancel.Kind=bkOk then
    Finished:=True
  else
    Canceled:=True;
end;

procedure TfrmRLFeedBack.SetMax(n:integer);
begin
  TProgressBar(Bars[BarIndex]).Max     :=n;
  TProgressBar(Bars[BarIndex]).Position:=0;
end;

procedure TfrmRLFeedBack.SetPhase(const aPhase:string);
begin
  LabelPhase.Caption:=aPhase;
  LabelPhase.Update;
end;

procedure TfrmRLFeedBack.StepIt;
begin
  TProgressBar(Bars[BarIndex]).StepIt;
  Application.ProcessMessages;
end;

procedure TfrmRLFeedBack.NextBar;
begin
  Inc(BarIndex);
end;

procedure TfrmRLFeedBack.Finish;
begin
  LabelPhase.Caption  :=LS_FinishedStr;
  BitBtnCancel.Kind   :=bkOk;
  BitBtnCancel.Caption:=LS_CloseStr;
  BitBtnCancel.Default:=True;
  TimerBlink.Enabled  :=True;
  while not Finished do 
    Application.ProcessMessages;
end;

procedure TfrmRLFeedBack.FormDestroy(Sender: TObject);
begin
  Bars.Destroy;
end;

procedure TfrmRLFeedBack.TimerBlinkTimer(Sender: TObject);
begin
  LabelPhase.Visible:=not LabelPhase.Visible;
end;

end.

