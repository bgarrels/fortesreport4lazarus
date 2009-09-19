program Basic;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Main, NFDesign, ITDesign, fortes324forlaz, LResources, DBFLaz
  { you can add units after this };

{$IFDEF WINDOWS}{$R Basic.rc}{$ENDIF}

begin
  {$I Basic.lrs}
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

