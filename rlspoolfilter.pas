{$I RLReport.inc}

{@unit RLSpoolFilter - Implementação do filtro padrão para impressoras.
Portado para o Lazarus - Trabalho inicial de Isaac Trindade da Silva contato tioneobrasil@yahoo.com.br dicas4lazarus@yahoo.com.br
Lazarus Ported - initial work by Isaac 07/2009
}
unit RLSpoolFilter;
{$MODE DELPHI}{$H+}
interface

uses
  LCLIntf, LCLType, Classes, SysUtils, Contnrs, Graphics, RLMetaVCL, Types,
  RLMetaFile, RLFilters, RLTypes, RLPrinters, RLConsts;

type

  { TRLSpoolFilter }

  {@class TRLSpoolFilter - Filtro de impressão padrão.
   É o filtro natural que envia as páginas para o spooler do SO.
   @links TRLDraftFilter.
   @pub }
  TRLSpoolFilter=class(TRLCustomPrintFilter)
  private

    // variables

    fPrinterRect:TRect;
    fDocStarted :boolean;
    fPaperWidth :double;
    fPaperHeight:double;
    fOrientation:TRLMetaOrientation;
    
    procedure   SetDocBounds(aPaperWidth,aPaperHeight:double; aOrientation:TRLMetaOrientation);

  protected

    // override methods

    procedure   InternalBeginDoc; override;
    procedure   InternalEndDoc; override;
    procedure   InternalNewPage; override;
    procedure   InternalDrawPage(aPage:TRLGraphicSurface); override;

  public

    // constructors & destructors
    
    constructor Create(aOwner:TComponent); override;
  end;
  {/@class}

{@func SpoolFilter - Referência para o filtro padrão de impressora.
 @links TRLSpoolFilter. :/}
function SpoolFilter:TRLSpoolFilter;

{/@unit}

implementation

var
  SpoolFilterInstance:TRLSpoolFilter=nil;

function SpoolFilter:TRLSpoolFilter;
begin
  if not Assigned(SpoolFilterInstance) then
    SpoolFilterInstance:=TRLSpoolFilter.Create(nil);
  result:=SpoolFilterInstance;
end;

{ TRLSpoolFilter }

constructor TRLSpoolFilter.Create(aOwner: TComponent);
begin
  inherited;
end;

procedure TRLSpoolFilter.SetDocBounds(aPaperWidth,aPaperHeight:double; aOrientation:TRLMetaOrientation);
var
  changed:boolean;
  metrics:TRLPrinterMetrics;
begin
  changed:=(aPaperWidth<>fPaperWidth) or (aPaperHeight<>fPaperHeight) or (aOrientation<>fOrientation);
  if not fDocStarted or changed then
  begin
    fPaperWidth :=aPaperWidth;
    fPaperHeight:=aPaperHeight;
    fOrientation:=aOrientation;
    if fDocStarted then
      RLPrinter.EndDoc;
    //  
    RLPrinter.SetPaperSize(fPaperWidth,fPaperHeight,fOrientation=MetaOrientationLandscape,false);
    RLPrinter.LoadMetrics(metrics);
    //
    fPrinterRect.Left:=0;
    fPrinterRect.Top :=0;
    if fOrientation=MetaOrientationLandscape then
    begin
      fPrinterRect.Right :=Round(fPaperHeight*metrics.PPIX/InchAsMM);
      fPrinterRect.Bottom:=Round(fPaperWidth *metrics.PPIY/InchAsMM);
      OffsetRect(fPrinterRect,-metrics.MarginBottom,-metrics.MarginLeft);
    end
    else
    begin
      fPrinterRect.Right :=Round(fPaperWidth *metrics.PPIX/InchAsMM);
      fPrinterRect.Bottom:=Round(fPaperHeight*metrics.PPIY/InchAsMM);
      OffsetRect(fPrinterRect,-metrics.MarginLeft,-metrics.MarginTop);
    end;
    //
    RLPrinter.BeginDoc;
    fDocStarted:=true;
  end;
end;

procedure TRLSpoolFilter.InternalBeginDoc;
begin
  fDocStarted:=false;
  SetDocBounds(Pages.PaperWidth,Pages.PaperHeight,Pages.Orientation);
end;

procedure TRLSpoolFilter.InternalEndDoc;
begin
  RLPrinter.EndDoc;
end;

procedure TRLSpoolFilter.InternalNewPage;
begin
  RLPrinter.NewPage;
end;

procedure TRLSpoolFilter.InternalDrawPage(aPage:TRLGraphicSurface);
var
  xfactor  :double;
  yfactor  :double;
  cliprct  :TRect;
  clipstack:TList;
  obj      :TRLGraphicObject;
  i        :integer;
  thecanvas:TCanvas;
  procedure ProjectX(const s:integer; var d:integer);
  begin
    d:=fPrinterRect.Left+Round(s*xfactor);
  end;
  procedure ProjectY(const s:integer; var d:integer);
  begin
    d:=fPrinterRect.Top+Round(s*yfactor);
  end;
  procedure ProjectRect(const s:TRLMetaRect; var d:TRect);
  begin
    ProjectX(s.Left  ,d.Left);
    ProjectY(s.Top   ,d.Top);
    ProjectX(s.Right ,d.Right);
    ProjectY(s.Bottom,d.Bottom);
    if not (d.Right>d.Left) then
      d.Right:=d.Left+1;
    if not (d.Bottom>d.Top) then
      d.Bottom:=d.Top+1;
  end;
  procedure DrawPixel(aObj:TRLPixelObject);
  var
    r:TRect;
  begin
    ProjectRect(aObj.BoundsRect,r);
    //
    thecanvas.Brush.Style:=bsSolid;
    thecanvas.Brush.Color:=FromMetaColor(aObj.Color);
    thecanvas.FillRect(r);
  end;
  procedure ProjectPoint(const s:TRLMetaPoint; var d:TPoint);
  begin
    ProjectX(s.X,d.X);
    ProjectY(s.Y,d.Y);
  end;
  procedure DrawLine(aObj:TRLLineObject);
  var
    p1,p2:TPoint;
  begin
    ProjectPoint(aObj.FromPoint,p1);
    ProjectPoint(aObj.ToPoint,p2);
    //
    FromMetaPen(aObj.Pen,thecanvas.Pen);
    PenInflate(thecanvas.Pen,xfactor);
    FromMetaBrush(aObj.Brush,thecanvas.Brush);
    thecanvas.MoveTo(p1.X,p1.Y);
    CanvasLineToEx(thecanvas,p2.X,p2.Y);
  end;
  procedure DrawRectangle(aObj:TRLRectangleObject);
  var
    r:TRect;
  begin
    ProjectRect(aObj.BoundsRect,r);
    //
    FromMetaPen(aObj.Pen,thecanvas.Pen);
    PenInflate(thecanvas.Pen,xfactor);
    FromMetaBrush(aObj.Brush,thecanvas.Brush);
    thecanvas.Rectangle(r.Left,r.Top,r.Right,r.Bottom);
  end;
  procedure DrawText(aObj:TRLTextObject);
  var
    r:TRect;
    o:TPoint;
    t:string;
  begin
    ProjectRect(aObj.BoundsRect,r);
    ProjectPoint(aObj.Origin,o);
    //
    FromMetaBrush(aObj.Brush,thecanvas.Brush);
    FromMetaFont(aObj.Font,thecanvas.Font,yfactor);
    t:=aObj.DisplayText;
    CanvasTextRectEx(thecanvas,r,o.X,o.Y,t,aObj.Alignment,aObj.Layout,aObj.TextFlags);
  end;
  procedure DrawFillRect(aObj:TRLFillRectObject);
  var
    r:TRect;
  begin
    ProjectRect(aObj.BoundsRect,r);
    //
    FromMetaBrush(aObj.Brush,thecanvas.Brush);
    thecanvas.FillRect(r);
  end;
  procedure DrawEllipse(aObj:TRLEllipseObject);
  var
    r:TRect;
  begin
    ProjectRect(aObj.BoundsRect,r);
    //
    FromMetaPen(aObj.Pen,thecanvas.Pen);
    PenInflate(thecanvas.Pen,xfactor);
    FromMetaBrush(aObj.Brush,thecanvas.Brush);
    thecanvas.Ellipse(r);
  end;
  procedure ProjectPoints(const s:TRLMetaPointArray; var p:TPointArray);
  var
    i:integer;
  begin
    SetLength(p,High(s)+1);
    for i:=0 to High(p) do
      ProjectPoint(s[i],p[i]);
  end;
  procedure DrawPolygon(aObj:TRLPolygonObject);
  var
    p:TPointArray;
  begin
    ProjectPoints(aObj.Points,p);
    //  
    FromMetaPen(aObj.Pen,thecanvas.Pen);
    PenInflate(thecanvas.Pen,xfactor);
    FromMetaBrush(aObj.Brush,thecanvas.Brush);
    thecanvas.Polygon(p);
  end;
  procedure DrawPolyline(aObj:TRLPolylineObject);
  var
    p:TPointArray;
  begin
    ProjectPoints(aObj.Points,p);
    //
    FromMetaPen(aObj.Pen,thecanvas.Pen);
    PenInflate(thecanvas.Pen,xfactor);
    thecanvas.Brush.Style:=bsClear;
    thecanvas.Polyline(p);
  end;
  procedure DrawImage(aObj:TRLImageObject);
  var
    r:TRect;
  begin
    ProjectRect(aObj.BoundsRect,r);
    CanvasStretchDraw(thecanvas,r,aObj.Data,aObj.Parity);
  end;
  procedure PushClipRect(const aRect:TRect);
  var
    p:PRect;
  begin
    New(p);
    p^:=aRect;
    clipstack.Insert(0,p);
  end;
  procedure PopClipRect(var aRect:TRect);
  var
    p:PRect;
  begin
    p:=clipstack[0];
    aRect:=p^;
    Dispose(p);
    clipstack.Delete(0);
  end;
  procedure DrawSetClipRect(aObj:TRLSetClipRectObject);
  begin
    PushClipRect(cliprct);
    ProjectRect(aObj.BoundsRect,cliprct);
    CanvasSetClipRect(thecanvas,cliprct);
  end;
  procedure DrawResetClipRect(aObj:TRLResetClipRectObject);
  begin
    PopClipRect(cliprct);
    CanvasSetClipRect(thecanvas,cliprct);
  end;
begin
  SetDocBounds(aPage.PaperWidth,aPage.PaperHeight,aPage.Orientation);
  //
  if aPage.Width=0 then
    xfactor:=1
  else
    xfactor:=(fPrinterRect.Right-fPrinterRect.Left)/aPage.Width;
  if aPage.Height=0 then
    yfactor:=1
  else
    yfactor:=(fPrinterRect.Bottom-fPrinterRect.Top)/aPage.Height;
  //
  thecanvas:=RLPrinter.Canvas;
  clipstack:=TList.Create;
  try
    CanvasStart(thecanvas);
    try
      cliprct:=fPrinterRect;
      CanvasSetClipRect(thecanvas,cliprct);
      try
        for i:=0 to aPage.ObjectCount-1 do
        begin
          obj:=TRLGraphicObject(aPage.Objects[i]);
          if obj is TRLPixelObject then
            DrawPixel(TRLPixelObject(obj))
          else if obj is TRLLineObject then
            DrawLine(TRLLineObject(obj))
          else if obj is TRLRectangleObject then
            DrawRectangle(TRLRectangleObject(obj))
          else if obj is TRLTextObject then
            DrawText(TRLTextObject(obj))
          else if obj is TRLFillRectObject then
            DrawFillRect(TRLFillRectObject(obj))
          else if obj is TRLEllipseObject then
            DrawEllipse(TRLEllipseObject(obj))
          else if obj is TRLPolygonObject then
            DrawPolygon(TRLPolygonObject(obj))
          else if obj is TRLPolylineObject then
            DrawPolyline(TRLPolylineObject(obj))
          else if obj is TRLImageObject then
            DrawImage(TRLImageObject(obj))
          else if obj is TRLSetClipRectObject then
            DrawSetClipRect(TRLSetClipRectObject(obj))
          else if obj is TRLResetClipRectObject then
            DrawResetClipRect(TRLResetClipRectObject(obj));
        end;
      finally
        CanvasResetClipRect(thecanvas);
      end;
    finally
      CanvasStop(thecanvas);
    end;
  finally
    while clipstack.Count>0 do
      PopClipRect(cliprct);
    clipstack.free;
  end;
end;


initialization

finalization
  SpoolFilterInstance.free;

end.
