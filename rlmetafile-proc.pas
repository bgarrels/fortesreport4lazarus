  TRLGraphicObject=class;
  TRLMetaPen=class
  TRLMetaBrush=class
  TRLMetaFont=class
  TRLGraphicStorage=class;
  TRLGraphicSurface=class;
  TRLGraphicObjectClass=class of TRLGraphicObject;
  TInt64List=class
  TRLGraphicStorage=class(TComponent)
  TRLGraphicSurface=class
  TRLGraphicObject=class
  TRLPixelObject=class(TRLGraphicObject)
  TRLLineObject=class(TRLGraphicObject)
  TRLRectangleObject=class(TRLGraphicObject)
  TRLTextObject=class(TRLGraphicObject)
  TRLFillRectObject=class(TRLGraphicObject)
  TRLEllipseObject=class(TRLGraphicObject)
  TRLPolygonObject=class(TRLGraphicObject)
  TRLPolylineObject=class(TRLGraphicObject)
  TRLImageObject=class(TRLGraphicObject)
  TRLSetClipRectObject=class(TRLGraphicObject)
  TRLResetClipRectObject=class(TRLGraphicObject)

function  GetPointsBounds(const aPoints:TRLMetaPointArray):TRect;
function  FloatToPtStr(f:double):string;
function  PtStrToFloat(const s:string; def:double=0):double;
function  ClipGraphic(aGraphic:TGraphic; var aRect:TRect; const aCenter:boolean):TBitmap;
function  ClipSurface(aSurface:TRLGraphicSurface; var aRect:TRect; const aCenter:boolean):TRLGraphicSurface;

function  MetaPoint(X,Y:integer):TRLMetaPoint;
function  MetaRect(aLeft,aTop,aRight,aBottom:integer):TRLMetaRect;
function  MetaColor(aRed,aGreen,aBlue:byte):TRLMetaColor;

{@function NewGroupId - Cria um identificador para um novo grupo de elementos gráficos.
 @links TRLGraphicObject.GroupId, TRLGraphicSurface.GeneratorId, TRLGraphicObject.GeneratorId. :/}
function NewGroupId:integer;

{/@unit}

implementation
