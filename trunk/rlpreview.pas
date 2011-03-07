{$I RLReport.inc}

{@unit RLPreview - Implementação dos componentes de pré-visualização.
Portado para o Lazarus - Trabalho inicial de Isaac Trindade da Silva contato tioneobrasil@yahoo.com.br dicas4lazarus@yahoo.com.br
Lazarus Ported - initial work by Isaac 07/2009
}
unit RLPreview;
{$MODE DELPHI}{$H+}
interface

uses
  LCLIntf, Classes, SysUtils, Math, Contnrs, Types,
  Graphics, Controls, ExtCtrls, Forms, Menus, Clipbrd, Dialogs,
  RLMetaFile, RLConsts, RLUtils, RLFilters;

type

  TRLPreviewBox=class;

  { TRLPreview }

  TRLPreviewLastFind=record
                       PageIndex  :integer;
                       ObjectIndex:integer;
                       Found      :boolean;
                     end;

  {@class TRLPreview - Componente para pré-visualização das páginas geradas.
   Possui métodos para controlar a navegação.
   @pub }
  TRLPreview=class(TScrollBox)
  private

    // variables

    fBoxes       :TObjectList;
    fVisibleBoxes:integer;
    fOnChangeView:TNotifyEvent;
    fZoomFactor  :double;
    fPages       :TRLGraphicStorage;
    fPageIndex   :integer;
    fLastSize    :TPoint;
    fEditing     :boolean;
    fPopup       :TPopupMenu;
    fLastFind    :TRLPreviewLastFind;
    fIsActive    :boolean;
    fTimer       :TTimer;
    fLastPgCount :integer;
    fLastBusy    :boolean;

    // assign methods

    function    GetPageNumber:integer;
    procedure   SetPageNumber(const aValue:integer);
    procedure   SetPageIndex(const aValue:integer);
    procedure   SetZoomFactor(const aValue:double);
    procedure   SetPages(const aValue:TRLGraphicStorage);
    procedure   SetEditing(const Value:boolean);
    procedure   SetMultipleMode(const aValue:boolean);
    function    GetMultipleMode:boolean;

    // event handlers

    procedure   CopyPage(Sender:TObject);
    procedure   HandleTimer(Sender: TObject);

    // custom methods

    procedure   RealignBoxes;
    procedure   DoChangeView;
    procedure   CancelEdit;
    procedure   InvalidateBoxes;

  protected

    // override

    procedure   Click; override;

  public

    // constructors & destructors

    constructor Create(aOwner:TComponent); override;
    destructor  Destroy; override;

    procedure   SetActive(aState:boolean);

    // override

    procedure   SetBounds(aLeft,aTop,aWidth,aHeight:integer); override;

    // custom methods
    
    {@method FirstPage - Posiciona na primeira página. :/}
    procedure   FirstPage;

    {@method LastPage - Posiciona na última página. :/}
    procedure   LastPage;

    {@method NextPage - Posiciona na próxima página ou grupo de páginas. :/}
    procedure   NextPage;

    {@method PriorPage - Posiciona na página anterior ou grupo de páginas anterior. :/}
    procedure   PriorPage;

    {@method PageTop - Posiciona no topo da página corrente. :/}
    procedure   PageTop;

    {@method PageBottom - Posiciona na parte inferior da página corrente. :/}
    procedure   PageBottom;

    {@method PageLeft - Posiciona na parte mais à esquerda da página corrente. :/}
    procedure   PageLeft;

    {@method PageRight - Posiciona na parte mais à direita da página corrente. :/}
    procedure   PageRight;

    {@method HalfPageUp - Rola a página para cima. :/}
    procedure   HalfPageUp;

    {@method HalfPageDown - Rola a página para baixo. :/}
    procedure   HalfPageDown;

    {@method HalfPageLeft - Rola a página para a esquerda. :/}
    procedure   HalfPageLeft;

    {@method HalfPageRight - Rola a página para a direita. :/}
    procedure   HalfPageRight;

    {@method ScrollLeft - Rola a página um caractere para a esquerda. :/}
    procedure   ScrollLeft;

    {@method ScrollRight - Rola a página um caractere para a direita. :/}
    procedure   ScrollRight;

    {@method ScrollUp - Rola a página uma linha para cima. :/}
    procedure   ScrollUp;

    {@method ScrollDown - Rola a página uma linha para baixo. :/}
    procedure   ScrollDown;

    {@method ZoomPage - Focaliza uma página quando em modo múltiplas páginas. :/}
    procedure   ZoomPage(Index:integer);

    {@method GetZoomFactorFullPage - Retorna o fator de zoom ideal para que a página caiba inteiramente na tela. :/}
    function    GetZoomFactorFullPage:double;

    {@method ZoomFullPage - Aplica zoom na página corrente para que esta caiba inteira na tela. :/}
    procedure   ZoomFullPage;

    {@method GetZoomFactorFullWidth - Retorna o fator de zoom ideal para que a largura da página caiba na tela. :/}
    function    GetZoomFactorFullWidth:double;

    {@method ZoomFullWidth - Aplica zoom na página corrente para que a sua largura caiba inteira na tela. :/}
    procedure   ZoomFullWidth;

    {@method GetZoomFactorMultiplePages - Retorna o fator de zoom ideal para que várias páginas caibam na tela. :/}
    function    GetZoomFactorMultiplePages:double;

    {@method ZoomMultiplePages - Alterna entre os modos página única e múltiplas páginas. :/}
    procedure   ZoomMultiplePages;

    {@method FindText - Posiciona na próxima ocorrência do texto. :/}
    function    FindText(const aText:string; aWholeWords,aMatchCase,aFindBackward:boolean):boolean;

    // custom properties
    {@prop PageNumber - Determina ou indica o número da página atual.
     Nota: O número da página leva em consideração a numeração inicial. :/}
    property    PageNumber:integer           read GetPageNumber write SetPageNumber;

    {@prop PageIndex - Determina ou indica o índice da página atual.
     Este índice vai de 0 até (Pages.Count-1). :/}
    property    PageIndex :integer           read fPageIndex    write SetPageIndex;

    {@prop ZoomFactor - Determina ou indica o fator de zoom atual. :/}
    property    ZoomFactor:double            read fZoomFactor   write SetZoomFactor;

    {@prop Editing - Determina ou indica se o preview está em modo de edição.
     Quando em modo de edição, o objeto preview permite que se altere algumas características do
     relatório como fontes, cores e até mesmo textos. Estas alterações podem ser salvas novamente,
     exportadas ou impressas. :/}
    property    Editing   :boolean           read fEditing      write SetEditing;

    {@prop Pages - Referência para a coleção de páginas.
     Deve apontar para um objeto TRLGraphicStorage, que pode ser obtido através da prop
     Pages do componente TRLReport. Este objeto também pode ser instanciado e carregar
     um relatório a partir de um arquivo em disco ou stream. :/}
    property    Pages     :TRLGraphicStorage read fPages        write SetPages;

    {@prop MultipleMode - Determina ou indica o estado de visualização de múltiplas páginas. :/}
    property    MultipleMode:boolean read GetMultipleMode write SetMultipleMode;

  published

    // custom properties

    {@prop OnChangeView - Ao mudar as características de visualização. :/}
    property    OnChangeView:TNotifyEvent      read fOnChangeView write fOnChangeView;
  end;
  {/@class}
  

  { TRLPreviewBox }

  {@class TRLPreviewBox - Caixa de visualização de página.
   @pub }
  TRLPreviewBox=class(TCustomControl)  
  private

    // variables

    fPreview :TRLPreview;
    fSelected:TRLGraphicObject;

    // assign methods

    function    GetPage:TRLGraphicSurface;

    // custom methods

    function    DoZoom(X:integer):integer;
    function    UndoZoom(X:integer):integer;

  protected

    // override methods

    procedure   Click; override;
    procedure   DblClick; override;
    procedure   MouseDown(Button:TMouseButton; Shift:TShiftState; X,Y:Integer); override;

    {@method ObjectAt - Retorna referência para o objeto que intercepta as coordenadas. :/}
    function    ObjectAt(X,Y:integer):TRLGraphicObject;

  public

    // constructors & destructors

    constructor Create(aOwner:TComponent); override;

    // override methods

    procedure   Paint; override;

    // custom properties

    {@prop Page - Referência para o objeto página. :/}
    property    Page:TRLGraphicSurface read GetPage;
  end;
  {/@class}

{/@unit}

implementation

// UTILS

procedure DrawGrab(aCanvas:TCanvas; X,Y:integer);
const
  GRABRAD=3;
begin
  with aCanvas do
    Rectangle(X-GRABRAD,Y-GRABRAD,X+GRABRAD,Y+GRABRAD);
end;

procedure DrawGrabs(aCanvas:TCanvas; aRect:TRect; aColor:TColor);
begin
  with aCanvas do
  begin
    Pen.Style  :=psSolid;
    Pen.Color  :=clWhite;
    Pen.Width  :=1;
    Brush.Style:=bsSolid;
    Brush.Color:=aColor;
    //
    DrawGrab(aCanvas,aRect.Left,aRect.Top);
    DrawGrab(aCanvas,(aRect.Left+aRect.Right) div 2,aRect.Top);
    DrawGrab(aCanvas,aRect.Right,aRect.Top);
    DrawGrab(aCanvas,aRect.Right,(aRect.Top+aRect.Bottom) div 2);
    DrawGrab(aCanvas,aRect.Right,aRect.Bottom);
    DrawGrab(aCanvas,(aRect.Left+aRect.Right) div 2,aRect.Bottom);
    DrawGrab(aCanvas,aRect.Left,aRect.Bottom);
    DrawGrab(aCanvas,aRect.Left,(aRect.Top+aRect.Bottom) div 2);
  end;
end;

procedure DrawFound(aCanvas:TCanvas; aRect:TRect);
const
  FoundBorderSize=3;
begin
  with aCanvas do
  begin
    Pen.Style  :=psSolid;
    Pen.Color  :=clWhite;
    Pen.Width  :=FoundBorderSize;
    Pen.Mode   :=pmXor;
    Brush.Style:=bsClear;
    Rectangle(aRect.Left-FoundBorderSize,aRect.Top-FoundBorderSize,aRect.Right+FoundBorderSize,aRect.Bottom+FoundBorderSize);
  end;
end;

{ TRLPreview }

constructor TRLPreview.Create(aOwner:TComponent);
var
  b:TRLPreviewBox;
  i:integer;
  m:TMenuItem;
begin
  fOnChangeView:=nil;
  fPages       :=nil;
  fZoomFactor  :=100;
  fVisibleBoxes:=1;
  fPageIndex   :=0;
  fLastSize    :=Point(0,0);
  fEditing     :=False;
  fBoxes       :=nil;
  fPopup       :=nil;
  fIsActive    :=False;
  fLastPgCount :=0;
  fLastBusy    :=False;
  fTimer       :=TTimer.Create(nil);
  fTimer.Interval:=100;
  fTimer.Enabled :=False;
  fTimer.OnTimer :=HandleTimer;
  fLastFind.Found    :=False;
  fLastFind.PageIndex:=-1;
  //
  fBoxes:=TObjectList.Create;
  //
  fPopup:=TPopupMenu.Create(Self);
  m:=TMenuItem.Create(fPopup);
  m.Caption :=LS_CopyAsImageStr;
  m.ShortCut:=ShortCut(Word('C'),[ssCtrl]);
  m.OnClick :=CopyPage;
  fPopup.Items.Add(m);
  //
  inherited Create(aOwner);
  //
  for i:=0 to 2 do
  begin
    b:=TRLPreviewBox.Create(Self);
    b.PopupMenu:=fPopup;
    b.Visible  :=(i=0);
    b.Parent   :=Self;
    fBoxes.Add(b);
  end;
  //
  HorzScrollBar.Tracking:=True;
  VertScrollBar.Tracking:=True;
end;

destructor TRLPreview.Destroy;
begin
  FreeAndNIL(fBoxes);
  FreeAndNIL(fTimer);
  //
  inherited;
end;

procedure TRLPreview.HandleTimer(Sender:TObject);
var
  pc:integer;
  bu:boolean;
begin
  if Assigned(fPages) then
  begin
    pc:=fPages.PageCount;
    bu:=fPages.Busy;
    if not bu then
      fTimer.Enabled:=False;
    if (pc<>fLastPgCount) or (bu<>fLastBusy) then
    begin
      fLastPgCount:=pc;
      fLastBusy   :=bu;
      DoChangeView;
    end;
  end;
  if not Assigned(fPages) or not fPages.Busy then
    fTimer.Enabled:=False;
end;

procedure TRLPreview.CopyPage(Sender:TObject);
var
  b:TBitmap;
  page:TRLGraphicSurface;
begin
  if fPopup.PopupComponent is TRLPreviewBox then
  begin
    page:=TRLPreviewBox(fPopup.PopupComponent).Page;
    if page<>nil then
    begin
      b:=NewBitmap(page.Width,page.Height); ///Correção de erro memoria
      try
        page.PaintTo(b.Canvas,Rect(0,0,b.Width,b.Height));
        ClipBoard.Assign(b);
      finally
        b.Free;
      end;
    end;
  end;
end;

procedure TRLPreview.FirstPage;
begin
  if Assigned(fPages) then
    PageIndex:=0;
end;

procedure TRLPreview.LastPage;
begin
  if Assigned(fPages) then
    PageIndex:=Max(0,fPages.PageCount-fVisibleBoxes);
end;

procedure TRLPreview.NextPage;
begin
  if Assigned(fPages) then
    PageIndex:=Min(fPages.PageCount-1,PageIndex+fVisibleBoxes);
end;

procedure TRLPreview.PriorPage;
begin
  if Assigned(fPages) then
    PageIndex:=Max(0,PageIndex-fVisibleBoxes);
end;

procedure TRLPreview.PageTop;
begin
  VertScrollBar.Position:=0;
end;

procedure TRLPreview.PageBottom;
begin
  VertScrollBar.Position:=VertScrollBar.range;
end;

procedure TRLPreview.PageLeft;
begin
  HorzScrollBar.Position:=0;
end;

procedure TRLPreview.PageRight;
begin
  HorzScrollBar.Position:=HorzScrollBar.range;
end;

procedure TRLPreview.HalfPageUp;
begin
  VertScrollBar.Position:=VertScrollBar.Position-Height;
end;

procedure TRLPreview.HalfPageDown;
begin
  VertScrollBar.Position:=VertScrollBar.Position+Height;
end;

procedure TRLPreview.HalfPageLeft;
begin
  HorzScrollBar.Position:=HorzScrollBar.Position-Width;
end;

procedure TRLPreview.HalfPageRight;
begin
  HorzScrollBar.Position:=HorzScrollBar.Position+Width;
end;

procedure TRLPreview.ScrollLeft;
begin
  HorzScrollBar.Position:=HorzScrollBar.Position-10;
end;

procedure TRLPreview.ScrollRight;
begin
  HorzScrollBar.Position:=HorzScrollBar.Position+10;
end;

procedure TRLPreview.ScrollUp;
begin
  VertScrollBar.Position:=VertScrollBar.Position-10;
end;

procedure TRLPreview.ScrollDown;
begin
  VertScrollBar.Position:=VertScrollBar.Position+10;
end;

function TRLPreview.GetMultipleMode:boolean;
begin
  Result:=(fVisibleBoxes>1);
end;

procedure TRLPreview.SetMultipleMode(const aValue:boolean);
var
  i:integer;
begin
  if aValue then
    fVisibleBoxes:=fBoxes.Count
  else
    fVisibleBoxes:=1;
  for i:=0 to fBoxes.Count-1 do
    TRLPreviewBox(fBoxes.Items[i]).Visible:=(i<fVisibleBoxes);
end;

procedure TRLPreview.ZoomPage(Index:integer);
begin
  if PageIndex<>Index then
    PageIndex:=Index;
  if MultipleMode then
    ZoomFullWidth
  else
    ZoomMultiplePages;
end;

function TRLPreview.GetZoomFactorFullPage: double;
var
  zw,zh:double;
begin
  if Assigned(fPages) then
  begin
    zw:=Round(100*(Self.Width-7)/fPages.OrientedWidth);
    zh:=Round(100*(Self.Height-7)/fPages.OrientedHeight);
    if zw<zh then
      Result:=zw
    else
      Result:=zh;
  end
  else
    Result:=100;
end;

procedure TRLPreview.ZoomFullPage;
begin
  MultipleMode:=False;
  ZoomFactor  :=GetZoomFactorFullPage;
end;

function TRLPreview.GetZoomFactorFullWidth: double;
begin
  if Assigned(fPages) then
    Result:=Round(100*(Self.Width-7)/fPages.OrientedWidth)
  else
    Result:=100;
end;

procedure TRLPreview.ZoomFullWidth;
begin
  MultipleMode:=False;
  ZoomFactor  :=GetZoomFactorFullWidth;
end;

function TRLPreview.GetZoomFactorMultiplePages: double;
var
  zw,zh:double;
begin
  if Assigned(fPages) then
  begin
    zw:=Round(100*((Self.Width-7)/fVisibleBoxes)/fPages.OrientedWidth);
    zh:=Round(100*(Self.Height-7)/fPages.OrientedHeight);
    if zw<zh then
      Result:=zw
    else
      Result:=zh;
  end
  else
    Result:=100;
end;

procedure TRLPreview.ZoomMultiplePages;
begin
  MultipleMode:=True;
  ZoomFactor  :=GetZoomFactorMultiplePages;
end;

function TRLPreview.FindText(const aText:string; aWholeWords,aMatchCase,aFindBackward:boolean):boolean;
var
  p:TRLGraphicSurface;
  o:TRLGraphicObject;
  i,j:integer;
  s,t:string;
begin
  Result:=False;
  //
  if not Assigned(fPages) then
    Exit;
  t:=aText;
  if not aMatchCase then
    t:=AnsiUpperCase(t);
  i:=fPageIndex;
  while (i>=0) and (i<fPages.PageCount) do
  begin
    p:=fPages[i];
    if fLastFind.PageIndex=i then
    begin
      j:=fLastFind.ObjectIndex;
      if aFindBackward then
        Dec(j)
      else
        Inc(j);
    end
    else
    begin
      fLastFind.PageIndex:=-1;
      if aFindBackward then
        j:=p.ObjectCount-1
      else
        j:=0;
    end;
    while (j>=0) and (j<p.ObjectCount) do
    begin
      o:=p.Objects[j];
      if o is TRLTextObject then
      begin
        s:=TRLTextObject(o).Text;
        if not aMatchCase then
          s:=AnsiUpperCase(s);
        if Pos(t,s)>0 then
        begin
          fLastFind.PageIndex  :=i;
          fLastFind.ObjectIndex:=j;
          fLastFind.Found      :=True;
          CancelEdit;
          Self.PageIndex       :=i;
          Result               :=True;
          Exit;
        end;
      end;
      if aFindBackward then
        Dec(j)
      else
        Inc(j);
    end;
    if aFindBackward then
      Dec(i)
    else
      Inc(i);
  end;
end;

function TRLPreview.GetPageNumber:integer;
begin
  if Assigned(fPages) then
    Result:=PageIndex+1
  else
    Result:=0;  
end;

procedure TRLPreview.DoChangeView;
begin
  if Assigned(fOnChangeView) then
    fOnChangeView(Self);
end;

procedure TRLPreview.CancelEdit;
var
  i:integer;
begin
  for i:=0 to fVisibleBoxes-1 do
    with TRLPreviewBox(fBoxes.Items[i]) do
    begin
      fSelected:=nil;
      Invalidate;
    end;
end;

procedure TRLPreview.SetPageIndex(const aValue:integer);
begin
  if Assigned(fPages) then
    if (aValue>=0) and (aValue<fPages.PageCount) and (aValue<>fPageIndex) then
    begin
      fPageIndex            :=aValue;
      VertScrollBar.Position:=0;
      HorzScrollBar.Position:=0;
      RealignBoxes;
    end;
end;

procedure TRLPreview.SetPageNumber(const aValue:integer);
begin
  if Assigned(fPages) then
    PageIndex:=aValue-1;
end;

procedure TRLPreview.SetPages(const aValue:TRLGraphicStorage);
begin
  if Assigned(fPages) then
    fPages.Unlink(Self);
  fPages:=aValue;
  if Assigned(fPages) then
    fPages.Link(Self);
  //
  RealignBoxes;
  FirstPage;
  //
  if Assigned(fPages) then
  begin
    fLastPgCount  :=fPages.PageCount;
    fLastBusy     :=fPages.Busy;
    fTimer.Enabled:=True;
  end
  else
  begin
    fLastPgCount  :=0;
    fLastBusy     :=False;
    fTimer.Enabled:=False;
  end;
end;

procedure TRLPreview.SetZoomFactor(const aValue:double);
begin
  if (aValue>=10) and (aValue<>fZoomFactor) then
  begin
    fZoomFactor:=aValue;
    RealignBoxes;
  end;
end;

procedure TRLPreview.InvalidateBoxes;
var
  i:integer;
begin
  for i:=0 to fVisibleBoxes-1 do
    TRLPreviewBox(fBoxes.Items[i]).Invalidate;
end;

procedure TRLPreview.RealignBoxes;
var
  i,offsetleft,offsettop,docwidth,docheight,pagewidth,pageheight,totalwidth,totalheight:integer;
  box:TRLPreviewBox;
begin
  if Assigned(fPages) then
  begin
    docwidth   :=Round(fPages.OrientedWidth *fZoomFactor/100)+2+5;
    docheight  :=Round(fPages.OrientedHeight*fZoomFactor/100)+2+5;
    totalwidth :=0;
    totalheight:=0;
    for i:=0 to fVisibleBoxes-1 do
    begin
      box:=TRLPreviewBox(fBoxes.Items[i]);
      if box.Page<>nil then
      begin
        pagewidth :=Round(box.Page.Width *fZoomFactor/100)+2+5;
        pageheight:=Round(box.Page.Height*fZoomFactor/100)+2+5;
      end
      else
      begin
        pagewidth :=docwidth;
        pageheight:=docheight;
      end;
      Inc(totalwidth,pagewidth);
      totalheight:=Max(totalheight,pageheight);
    end;
    if totalwidth<Width then
    begin
      HorzScrollBar.Position:=0;
      offsetleft:=(Width-totalwidth) div 2;
    end
    else
      offsetleft:=-HorzScrollBar.Position;
    if totalheight<Height then
    begin
      VertScrollBar.Position:=0;
      offsettop:=(Height-totalheight) div 2;
    end
    else
      offsettop:=-VertScrollBar.Position;
    for i:=0 to fVisibleBoxes-1 do
    begin
      box:=TRLPreviewBox(fBoxes.Items[i]);
      if box.Page<>nil then
      begin
        pagewidth :=Round(box.Page.Width *fZoomFactor/100)+2+5;
        pageheight:=Round(box.Page.Height*fZoomFactor/100)+2+5;
      end
      else
      begin
        pagewidth :=docwidth;
        pageheight:=docheight;
      end;
      box.BoundsRect:=Rect(offsetleft,offsettop+(totalheight-pageheight) div 2,offsetleft+pagewidth,offsettop+(totalheight-pageheight) div 2+pageheight);
      Inc(offsetleft,pagewidth);
      box.Invalidate;
    end;
    DoChangeView;
  end;
end;

procedure TRLPreview.SetBounds(aLeft,aTop,aWidth,aHeight:integer);
begin
  inherited SetBounds(aLeft,aTop,aWidth,aHeight);
  //
  if (Width<>fLastSize.X) or (Height<>fLastSize.Y) then
    RealignBoxes;
  fLastSize:=Point(Width,Height);  
end;

procedure TRLPreview.Click;
begin
  inherited;
  //
  DoEnter;
end;

procedure TRLPreview.SetEditing(const Value: boolean);
begin
  if Value=fEditing then
    exit;
  fEditing:=Value;
  if not fEditing then
    CancelEdit;
end;

procedure TRLPreview.SetActive(aState: boolean);
begin
  fIsActive:=aState;
  InvalidateBoxes;
end;

{ TRLPreviewBox }

constructor TRLPreviewBox.Create(aOwner:TComponent);
begin
  fPreview :=TRLPreview(aOwner);
  fSelected:=nil;
  //
  inherited Create(nil);
  //
  Left          :=0;
  Top           :=0;
  Width         :=1;
  Height        :=1;
  ControlStyle  :=ControlStyle+[csOpaque];
{$ifdef VCL}
  DoubleBuffered:=True;
{$endif}
end;

function TRLPreviewBox.GetPage:TRLGraphicSurface;
var
  i,j:integer;
begin
  if Assigned(fPreview.Pages) then
  begin
    i:=fPreview.fPageIndex;
    j:=fPreview.fBoxes.IndexOf(Self);
    Result:=fPreview.Pages.WaitPage(i+j);
  end
  else
    Result := nil;
end;

function TRLPreviewBox.DoZoom(X:integer):integer;
begin
  Result:=Round(X*(fPreview.ZoomFactor/100));
end;

function TRLPreviewBox.UndoZoom(X:integer):integer;
begin
  Result:=Round(X/(fPreview.ZoomFactor/100));
end;

procedure TRLPreviewBox.Paint;
var
  r,rr,rb:TRect;
  surface:TRLGraphicSurface;
  obj    :TRLGraphicObject;
  l      :TList;
  i      :integer;
  cl     :TColor;
begin
  inherited;
  //
  surface:=Page;
  with Canvas do
  begin
    if Assigned(surface) then
      Brush.Color:=clWhite
    else
      Brush.Color:=clGray;
    Brush.Style:=bsSolid;
    Pen.Width  :=1;
    if fPreview.fIsActive then
      Pen.Color:=clBlack
    else  
      Pen.Color:=clSilver;
    Pen.Mode   :=pmCopy;
    Pen.Style  :=psSolid;
    //
    r:=BoundsRect;
    OffsetRect(r,-r.Left,-r.Top);
    Dec(r.Right ,2+5);
    Dec(r.Bottom,2+5);
    //
    rr:=Rect(r.Right,r.Top+5,r.Right+5,r.Bottom+5);
    rb:=Rect(r.Left+5,r.Bottom,r.Right,r.Bottom+5);
    Rectangle(r.Left,r.Top,r.Right,r.Bottom);
    InflateRect(r,-1,-1);
    if Assigned(surface) then
      surface.PaintTo(Self.Canvas,r)
    else
    begin
      MoveTo(r.Left,r.Top);  LineTo(r.Right,r.Bottom);
      MoveTo(r.Right,r.Top); LineTo(r.Left,r.Bottom);
    end;
    Brush.Color:=clGray;
    FillRect(rr);
    FillRect(rb);
  end;
  //
  if Assigned(surface) and Assigned(fSelected) then
  begin
    // pega todos de mesmo grupo
    l:=TList.Create;
    try
      for i:=0 to surface.ObjectCount-1 do
      begin
        obj:=surface.Objects[i];
        if (obj.GroupId<>0) and (obj.GroupId=fSelected.GroupId) then
          l.Add(obj);
      end;    
      if l.Count>1 then
        cl:=clSilver
      else
        cl:=clBlack;
      for i:=0 to l.Count-1 do
        with TRLGraphicObject(l[i]) do
          DrawGrabs(Self.Canvas,Rect(DoZoom(BoundsRect.Left),
                                     DoZoom(BoundsRect.Top),
                                     DoZoom(BoundsRect.Right),
                                     DoZoom(BoundsRect.Bottom)),
                                     cl);
    finally
      l.Free;
    end;
  end;
  //
  if Assigned(surface) and (surface.PageIndex=fPreview.fLastFind.PageIndex) and fPreview.fLastFind.Found then
  begin
    i:=fPreview.fLastFind.ObjectIndex;
    if i<surface.ObjectCount then
      with surface.Objects[i] do
        DrawFound(Self.Canvas,Rect(DoZoom(BoundsRect.Left),
                                   DoZoom(BoundsRect.Top),
                                   DoZoom(BoundsRect.Right),
                                   DoZoom(BoundsRect.Bottom)));
  end;
end;

procedure TRLPreviewBox.DblClick;
begin
  inherited;
  //
  fPreview.ZoomPage(fPreview.PageIndex+fPreview.fBoxes.IndexOf(Self));
end;

procedure TRLPreviewBox.Click;
begin
  inherited;
  //
  fPreview.DoEnter;
end;

procedure TRLPreviewBox.MouseDown(Button:TMouseButton; Shift:TShiftState; X,Y:Integer);
var
  obj:TRLGraphicObject;
begin
  inherited;
  //
  if fPreview.Editing then
  begin
    obj:=ObjectAt(UndoZoom(X),UndoZoom(Y));
    if obj<>fSelected then
    begin
      fSelected:=obj;
      Invalidate;
    end;
  end;  
end;

function TRLPreviewBox.ObjectAt(X, Y: integer): TRLGraphicObject;
var
  i,leastarea,area:integer;
  surface:TRLGraphicSurface;
  obj:TRLGraphicObject;
begin
  Result:=nil;
  //
  surface  :=Page;
  leastarea:=0;
  if surface<>nil then
    for i:=0 to surface.ObjectCount-1 do
    begin
      obj:=surface.Objects[i];
      if (obj.BoundsRect.Left<=X) and (obj.BoundsRect.Right>=X) and (obj.BoundsRect.Top<=Y) and (obj.BoundsRect.Bottom>=Y) then
      begin
        area:=(obj.BoundsRect.Right-obj.BoundsRect.Left)*(obj.BoundsRect.Bottom-obj.BoundsRect.Top);
        if (Result=nil) or (area<leastarea) then
        begin
          Result   :=obj;
          leastarea:=area;
        end;
      end;
    end;

end;

end.

