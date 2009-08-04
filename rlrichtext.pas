{$I RLReport.inc}

{@unit RLRichText - Implementação dos componentes de impressão de texto no formato RichText.
Portado para o Lazarus - Trabalho inicial de Isaac Trindade da Silva contato tioneobrasil@yahoo.com.br dicas4lazarus@yahoo.com.br
Lazarus Ported - initial work by Isaac 07/2009
}
unit RLRichText;
{$MODE DELPHI}{$H+}
interface

uses
  Classes, SysUtils, Contnrs, Math,
{$ifdef MSWINDOWS}
{$IFDEF FPC}
  LCLIntf,
  LCLType,
  Types,
{$ELSE}
  Windows,
{$ENDIF}
{$else}
  Types,
{$endif}
{$ifdef CLX}
  QGraphics, RLMetaCLX,
{$else}
  Graphics, RLMetaVCL,
{$endif}
  RLReport, RLUtils, RLMetaFile, RLRichParsers;

type

  { TRLCustomRichText }

  {@class TRLCustomRichText - Classe base para caixa de texto formato RichText. }
  TRLCustomRichText=class(TRLCustomMemo)
  private
    fParser:TRLRichParser;
    procedure PrepareParser;
    procedure FormatNeeded(Width:integer);
  protected
    // override & reintroduce
    procedure   CalcSize(var aSize:TPoint); override;
    function    InternalMakeCaption: string; override;
    procedure   InternalPrint; override;
    procedure   InternalPaint; override;
  public
    // constructors & destructors
    constructor Create(aOwner:TComponent); override;
    destructor Destroy; override;
  end;
  {/@class}

  { TRLCustomDBRichText }

  {@class TRLCustomDBRichText - Classe base para caixa de texto formato RichText ligado a campo de dataset. }
  TRLCustomDBRichText=class(TRLCustomDBMemo)
  private
    fParser:TRLRichParser;
    procedure PrepareParser;
    procedure FormatNeeded(Width: integer);
  protected
    // override & reintroduce
    procedure   CalcSize(var aSize:TPoint); override;
    function    InternalMakeCaption: string; override;
    procedure   InternalPrint; override;
    procedure   InternalPaint; override;
  public
    // constructors & destructors
    constructor Create(aOwner:TComponent); override;
    destructor Destroy; override;
  end;
  {/@class}
  

  { TRLRichText }

  {@class TRLRichText - Componente para texto multilinhas em formato RichText.
   @pub }
  TRLRichText=class(TRLCustomRichText)
  published

    // properties

    {@prop Align = ancestor /}
    property    Align;
    {@prop Alignment = ancestor /}
    property    Alignment;
    {@prop Anchors = ancestor /}
    property    Anchors;
    {@prop AutoSize = ancestor /}
    property    AutoSize;
    {@prop Behavior = ancestor /}
    property    Behavior;
    {@prop Borders = ancestor /}
    property    Borders;
    {@prop Color = ancestor /}
    property    Color;
    {@prop Font = ancestor /}
    property    Font;
    {@prop FriendlyName = ancestor /}
    property    FriendlyName;
    {@prop Holder = ancestor /}
    property    Holder;
    {@prop HoldStyle = ancestor /}
    property    HoldStyle;
    {@prop IntegralHeight = ancestor /}
    property    IntegralHeight;
    {@prop Layout = ancestor /}
    property    Layout;
    {@prop Lines = ancestor /}
    property    Lines;
    {@prop ParentColor = ancestor /}
    property    ParentColor;
    {@prop ParentFont = ancestor /}
    property    ParentFont;
    {@prop RealBounds = ancestor /}
    property    RealBounds;
    {@prop SecondHolder = ancestor /}
    property    SecondHolder;
    {@prop SecondHoldStyle = ancestor /}
    property    SecondHoldStyle;
    {@prop Transparent = ancestor /}
    property    Transparent;
    {@prop Visible = ancestor /}
    property    Visible;
    {@prop WordWrap = ancestor /}
    property    WordWrap;

    // events

    {@prop AfterPrint = ancestor /}
    property    AfterPrint;
    {@prop BeforePrint = ancestor /}
    property    BeforePrint;
    {@prop OnMeasureHeight = ancestor /}
    property    OnMeasureHeight;
  end;
  {/@class}
  

  { TRLDBRichText }

  {@class TRLDBRichText - Componente para texto multilinhas em formato RichText ligado a campo de dataset.
   @pub }
  TRLDBRichText=class(TRLCustomDBRichText)
  published

    // properties

    {@prop Align = ancestor /}
    property    Align;
    {@prop Alignment = ancestor /}
    property    Alignment;
    {@prop Anchors = ancestor /}
    property    Anchors;
    {@prop AutoSize = ancestor /}
    property    AutoSize;
    {@prop Behavior = ancestor /}
    property    Behavior;
    {@prop Borders = ancestor /}
    property    Borders;
    {@prop Color = ancestor /}
    property    Color;
    {@prop DataField = ancestor /}
    property    DataField;
    {@prop DataFormula = ancestor /}
    property    DataFormula;
    {@prop DataSource = ancestor /}
    property    DataSource;
    {@prop Font = ancestor /}
    property    Font;
    {@prop FriendlyName = ancestor /}
    property    FriendlyName;
    {@prop Holder = ancestor /}
    property    Holder;
    {@prop HoldStyle = ancestor /}
    property    HoldStyle;
    {@prop IntegralHeight = ancestor /}
    property    IntegralHeight;
    {@prop Layout = ancestor /}
    property    Layout;
    {@prop ParentColor = ancestor /}
    property    ParentColor;
    {@prop ParentFont = ancestor /}
    property    ParentFont;
    {@prop RealBounds = ancestor /}
    property    RealBounds;
    {@prop SecondHolder = ancestor /}
    property    SecondHolder;
    {@prop SecondHoldStyle = ancestor /}
    property    SecondHoldStyle;
    {@prop Transparent = ancestor /}
    property    Transparent;
    {@prop Visible = ancestor /}
    property    Visible;
    {@prop WordWrap = ancestor /}
    property    WordWrap;

    // events

    {@prop AfterPrint = ancestor /}
    property    AfterPrint;
    {@prop BeforePrint = ancestor /}
    property    BeforePrint;
    {@prop OnMeasureHeight = ancestor /}
    property    OnMeasureHeight;
  end;
  {/@class}

{/@unit}

implementation

{ TRLCustomRichText }

constructor TRLCustomRichText.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  fParser:=nil;
end;

destructor TRLCustomRichText.Destroy;
begin
  inherited;
  FreeObj(fParser);
end;

type TFriendRLRichParser=class(TRLRichParser);

procedure TRLCustomRichText.FormatNeeded(Width:integer);
begin
  if (Width<>TFriendRLRichParser(fParser).FormattedWidth) or (WordWrap<>TFriendRLRichParser(fParser).FormattedWrap) then
    fParser.Format(Width);
end;

procedure TRLCustomRichText.PrepareParser;
var
  s:string;
begin
  if not Assigned(fParser) then
    fParser:=TRLRichParser.Create(nil);
  // texto a utilizar para o cálculo
  s:=Caption;
  if (s=emptystr) and not IsPreparing then
    s:=Name;
  fParser.SetText(s);
  fParser.WordWrap:=Self.WordWrap;
  FormatNeeded(Self.ClientWidth);
end;

procedure TRLCustomRichText.CalcSize(var aSize:TPoint);
var
  w:integer;
begin
  aSize:=Point(Width,Height);
  if not AutoSize then
    Exit;
  // dimensões do texto
  PrepareParser;
  FormatNeeded(Self.ClientWidth);
  aSize.Y:=fParser.TextSize.Y;
  // adicional das bordas
  w:=Borders.Width;
  if w>0 then
  begin
    Inc(w);
    if Borders.CanDrawTop then
      Inc(aSize.Y,w);
    if Borders.CanDrawBottom then
      Inc(aSize.Y,w);
  end;
end;

function TRLCustomRichText.InternalMakeCaption: string;
begin
  Result:=Lines.Text;
end;

procedure TRLCustomRichText.InternalPrint;
begin
  PrintAsCustomControl;
  PrepareParser;
  fParser.PaintTo(RequestParentSurface,CalcPrintClientRect, Color);
end;

procedure TRLCustomRichText.InternalPaint;
begin
  PaintAsCustomControl;
  PrepareParser;
  fParser.PaintTo(Canvas,GetClientRect, Color);
end;

{ TRLCustomDBRichText }

constructor TRLCustomDBRichText.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  fParser :=nil;
end;

destructor TRLCustomDBRichText.Destroy;
begin
  inherited;
  FreeObj(fParser);
end;

procedure TRLCustomDBRichText.FormatNeeded(Width:integer);
begin
  if (Width<>TFriendRLRichParser(fParser).FormattedWidth) or (WordWrap<>TFriendRLRichParser(fParser).FormattedWrap) then
    fParser.Format(Width);
end;

procedure TRLCustomDBRichText.PrepareParser;
var
  s:string;
begin
  if not Assigned(fParser) then
    fParser:=TRLRichParser.Create(nil);
  // texto a utilizar para o cálculo
  s:=Caption;
  if (s=emptystr) and not IsPreparing then
    if DataField<>emptystr then
      s:=DataField
    else
      s:=Name;
  fParser.SetText(s);
  fParser.WordWrap:=Self.WordWrap;
  FormatNeeded(Self.ClientWidth);
end;

procedure TRLCustomDBRichText.CalcSize(var aSize:TPoint);
var
  w:integer;
begin
  aSize:=Point(Width,Height);
  if not AutoSize then
    Exit;
  // dimensões do texto
  PrepareParser;
  FormatNeeded(Self.ClientWidth);
  aSize.Y:=fParser.TextSize.Y;
  // adicional das bordas
  w:=Borders.Width;
  if w>0 then
  begin
    Inc(w);
    if Borders.CanDrawTop then
      Inc(aSize.Y,w);
    if Borders.CanDrawBottom then
      Inc(aSize.Y,w);
  end;
end;

function TRLCustomDBRichText.InternalMakeCaption: string;
begin
  Result:=GetFieldText;
end;

procedure TRLCustomDBRichText.InternalPrint;
begin
  PrintAsCustomControl;
  PrepareParser;
  fParser.PaintTo(RequestParentSurface,CalcPrintClientRect, Color);
end;

procedure TRLCustomDBRichText.InternalPaint;
begin
  PaintAsCustomControl;
  PrepareParser;
  fParser.PaintTo(Canvas,GetClientRect, Color);
end;

end.

