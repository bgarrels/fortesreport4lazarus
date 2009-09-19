program Biolife;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Main, UnitDesign, LResources, fortes324forlaz
  { you can add units after this };

{$IFDEF WINDOWS}{$R Biolife.rc}{$ENDIF}

begin
  {$I Biolife.lrs}
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TFormDesign, FormDesign);
  Application.Run;
end.

