{@unit RLAbout
Portado para o Lazarus - Trabalho inicial de Isaac Trindade da Silva contato tioneobrasil@yahoo.com.br dicas4lazarus@yahoo.com.br
Lazarus Ported - initial work by Isaac 07/2009
}
unit RLAbout;

{$MODE DELPHI}
{$I RLReport.inc}

interface

uses
  SysUtils, Classes, LCLIntf, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, Buttons,
  LCLType, RLReport, RLConsts, RLUtils, LResources;

type
  { TfrmRLAbout }

  TfrmRLAbout = class(TForm)
    imgLazarusLogo: TImage;
    imgLogo: TImage;
    lblLazarusVersion: TLabel;
    lblTitle: TLabel;
    lblVersion: TLabel;
    lblHome: TLabel;
    lblCopyright: TLabel;
    bbtOk: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure lblHomeClick(Sender: TObject);
  private
    { Private declarations }
  protected
    { Protected declarations }
    fAuthorKey: String;
    procedure KeyDown(var Key:Word; Shift:TShiftState); override;
  public
    { Public declarations }
  end;

implementation

{ TfrmRLAbout }

procedure TfrmRLAbout.KeyDown(var Key:Word; Shift:TShiftState);
const
  authorkey='TEAM';
begin
  inherited;
  if (ssCtrl in Shift) and (Key>=65) and (Key<=90) then
  begin
    fAuthorKey:=fAuthorKey+Char(Key);
    if Length(fAuthorKey)>Length(authorkey) then
      Delete(fAuthorKey,1,1);
    if fAuthorKey=authorkey then
      Caption:='Autor: '+CS_AuthorNameStr;
  end;

  if Key=VK_ESCAPE then
    Close;
end;

procedure TfrmRLAbout.FormCreate(Sender: TObject);
begin
  Caption := LS_AboutTheStr + ' ' + CS_ProductTitleStr;
  lblVersion.Caption := IntToStr(LazVersion) + '.' + IntToStr(LazReleaseVersion) + LazCommentVersion + ' for Lazarus';
  lblTitle.Caption := CS_ProductTitleStr;
  lblHome.Caption := CS_URLStr;
  lblCopyright.Caption := CS_CopyrightStr;
  lblLazarusVersion.Caption := 'Versão para Lazarus';
end;

procedure TfrmRLAbout.lblHomeClick(Sender: TObject);
begin
  OpenURL('http://www.fortesreport.com.br');
end;

initialization
  {$i rlabout.lrs}

end.


