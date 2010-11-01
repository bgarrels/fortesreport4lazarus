{@unit RLUtils - Rotinas de uso geral.
Portado para o Lazarus - Trabalho inicial de Isaac Trindade da Silva contato tioneobrasil@yahoo.com.br dicas4lazarus@yahoo.com.br
Lazarus Ported - initial work by Isaac 07/2009
}
unit RLUtils;

{$MODE DELPHI}
{$I RLReport.inc}

interface

uses
  LCLIntf, LCLType,
  SysUtils, Classes, Math, DB,
  SyncObjs,
  Types,
  Graphics, Forms;

{@var TempDir - Especifica aonde deverão ser criados os arquivos temporários.
 Na inicialização do sistema é atribuido um valor padrão a esta variável. Este valor pode ser alterado depois.
 No Windows o diretório padrão é "WINDOWS\TEMP", e no Linux é o "/tmp".
 @links GetTempFileName. :/}
var TempDir:string='.';

{@proc FreeObj - Libera objeto se não for nil e em seguida limpa a variável.
 @links FreePtr. :/}
procedure FreeObj(var aObj);

{@proc FreePtr - Libera ponteiro se não for nil e em seguida limpa a variável.
 @links FreeObj. :/}
procedure FreePtr(var aPtr);

{@func ByteToHex - Retorna o byte em notação hexadecimal de dois dígitos.
 @links HexToByte. :/}
function  ByteToHex(const aByte:byte):string;

{@func HexToByte - Retorna o valor hexadecimal como byte.
 @links ByteToHex. :/}
function  HexToByte(const aHex:string):byte;

{@func HexToBitmap - Cria bitmap a partir de uma cadeia hexadecimal.
 @links HexToGraphic, HexToByte. :/}
function  HexToBitmap(const aHex:string):TBitmap;

{@func HexToGraphic - Cria um gráfico qualquer a partir de uma cadeia hexadecimal.
 @links HexToBitmap, HexToByte. :/}
function  HexToGraphic(const aHex:string):TGraphic;

{@func NewComponentName - Cria um nome para um novo componente. :/}
function  NewComponentName(aComponent:TComponent):string;

{@func GetTempFileName - Retorna nome de arquivo temporário.
 @links TempDir. :/}
function  GetTempFileName:string;

{@func Token - Esta função retorna a parte de índice "aIndex" da string "aTokenList",
cujas partes estão separadas pelo caractere "aTokenSeparator". :/}
function Token(const aTokenList:string; aIndex:integer; aTokenSeparator:char='|'):string;

type
  TRLThread=class(TThread)
  private
    MethodRef   :TThreadMethod;
    ProcedureRef:TProcedure;
    KeepLoop    :boolean;
    Synchronized:boolean;
    procedure InternalCall;
  public
    procedure Execute; override;
  end;

{@func ThreadIt - Esta função executa uma procedure ou um método de um objeto em segundo plano.
A thread é criada suspensa, por isso é necessário invocar o método Resume assim que ela estiver pronta para rodar.
A prop FreeOnTerminate da thread é setada para True como padrão, o que significa que não é necessário destruir a
instância.
O parâmetro KeepLoop indica que a thread deverá executar o método continuamente até a sua finalização.
O parâmetro Synchronized indica que a execução do método deverá ser sincronizada com a thread principal para que
não ocorram travamentos por concorrência. É essencial se o método leva a alguma chamada à VCL/CLX. :}
function ThreadIt(MethodRef:TThreadMethod; KeepLoop:boolean=False; Synchronized:boolean=False):TRLThread; overload;
function ThreadIt(ProcedureRef:TProcedure; KeepLoop:boolean=False; Synchronized:boolean=False):TRLThread; overload;
{/@func}

{@proc ThreadSafeCall - Faz uma chamada "aprova de threads" a um método de usuário. Se o processo corrente é o processo principal,
este procedimento é equivalente a uma chamada direta ao método "Method". Se o processo é uma thread, criada com "ThreadIt" por exemplo, o
método "Method" é chamado através do método de sincronização da thread.
@links TRLCustomReport.BackgroundMode, ThreadIt, CurrentThread, IsMainThread. :/}
procedure ThreadSafeCall(Method:TThreadMethod);

{@func IsMainThread - Retorna verdadeiro se a thread que está em execução é a thread principal do sistema.
@links ThreadIt, CurrentThread. :/}
function  IsMainThread:boolean;

{@func CurrentThread - Retorna referência à thread corrente. Retorna referência apenas para threads criadas pelo método
ThreadIt.
Nota: Não há um objeto thread associado à thread principal.
@links ThreadIt, IsMainThread. :/}
function  CurrentThread:TRLThread;

{@func FormatFileExt - Adiciona ponto a uma extensão, se não houver. :/}
function  FormatFileExt(const aExt:string):string;

{@func AddFileFilter - Adiciona filtro de arquivos com nome aFilter, descrição aDescription e extensão padrão aExt. :/}
function  AddFileFilter(const aFilter:string; const aDescription,aExt:string):string;

{@func GetFileFilterExt - Devolve a extensão padrão para arquivos correspondentes ao filtro aFilter. :/}
function  GetFileFilterExt(const aFilter:string; aIndex:integer):string;

{@proc RotatePoints - Rotaciona os pontos aPoints em 2D de acordo com o ângulo aAngle.
 @links RotateBitmap. :/}
procedure RotatePoints(var aPoints:array of TPoint; const aAngle:double);

{@func RotateBitmap - Rotaciona o bitmap TBitmap em 2D de acordo com o ângulo aAngle e devolve em aDest.
 Nota: O bitmap aDest deve ter tamanho suficiente para a imagem rotacionada. Este cálculo pode ser feito
 previamente com a proc RotatePoints.
 @links RotatePoints, RotatedBitmap. :/}
procedure RotateBitmap(aSource,aDest:TBitmap; aAngle:double; aAxis,aOffset:TPoint);

{@func RotatedBitmap - Cria e devolve um bitmap compatível com o bitmap aSource rotacionado em 2D de acordo com o ângulo aAngle com
 tamanho calculado.
 @links RotateBitmap. :/}
function  RotatedBitmap(aSource:TBitmap; aAngle:double):TBitmap;

{@func PointsRect - Retorna um retângulo delimitando a área definida pelos pontos aPoints.
 @links PointsSize. :/}
function  PointsRect(const aPoints:array of TPoint):TRect;

{@func PointsSize - Retorna o tamanho da área definida pelos pontos aPoints.
 @links PointsRect. :/}
function  PointsSize(const aPoints:array of TPoint):TPoint;

{@func ScalePoints - Modifica as dimensões dos pontos aPoints para que caibam no retângulo definido por aRect respeitando a proporção.
 @links PointsRect. :/}
procedure ScalePoints(var aPoints:array of TPoint; const aRect:TRect);

{@func StretchPoints - Amplia ou reduz as dimensões dos pontos aPoints para que caibam no retângulo definido por aRect.
 @links PointsRect. :/}
procedure StretchPoints(var aPoints:array of TPoint; const aRect:TRect);

{@func CenterPoints - Centraliza os pontos aPoints no retâgulo aRect.
 @links PointsRect. :/}
procedure CenterPoints(var aPoints:array of TPoint; const aRect:TRect);

{@func TextBounds - Calcula as dimensões do texto aText de acordo com a fonte aFont e opcionalmente rotacionado em
 2D de acordo com o ângulo aAngle.
 @links PointsRect. :/}
function  TextBounds(const aText:string; aFont:TFont; aAngle:double):TPoint;

{@proc MoveRect - Desloca o retângulo horizontalmente de acordo com aX e verticalmente de acordo com aY.
 Nota: Valores positivos deslocam o retângulo para a direita ou abaixo. :/}
procedure MoveRect(var aRect:TRect; aX,aY:integer);

{@func RectWidth - Retorna a largura do retângulo aRect.
 @links RectHeight. :/}
function  RectWidth(const aRect:TRect):integer;

{@func RectHeight - Retorna a largura do retângulo aRect.
 @links RectWidth. :/}
function  RectHeight(const aRect:TRect):integer;

{@func ReduceRect - Retorna o retângulo aRect reduzido de acordo com os decrementos especificados em aPixels. :/}
function  ReduceRect(const aRect:TRect; aPixels:TRect):TRect;

{@func IncreaseRect - Retorna o retângulo aRect ampliado de acordo com os incrementos especificados em aPixels. :/}
function  IncreaseRect(const aRect:TRect; aPixels:TRect):TRect;

{@func DiffRect - Retorna a diferença entre os retângulos aRectOut e aRectIn, desde que aRectIn esteja dentro
 de aRectOut. :/}
function  DiffRect(const aRectOut,aRectIn:TRect):TRect;

{@func IterateJustification - Faz a justificação do texto distribuindo espaços. A função deve ser executada até
 se obter a largura total do texto. :/}
function  IterateJustification(var aText:string; var aIndex:integer):boolean;

{@func ScaleRect - Calcula a maior amostra do retângulo aSource escalonado de modo a caber em aTarget. :/}
function  ScaleRect(const aSource,aTarget:TRect; aCenter:boolean):TRect;

procedure StreamWrite(aStream:TStream; const aStr:string);
procedure StreamWriteLn(aStream:TStream; const aStr:string='');

{@proc RegisterTempFile - Registra um arquivo temporário para ser excluído na finalização. :/}
procedure RegisterTempFile(const aFileName:string);
{@proc UnregisterTempFile - Retira arquivo temporário da lista de arquivos a excluir na finalização. :/}
procedure UnregisterTempFile(const aFileName:string);
{@proc ClearTempFiles - Destroi arquivos temporários registrados pela proc RegisterTempFile. :/}
procedure ClearTempFiles;

{@proc SmartGetFieldDisplayText - Retorna a verdadeira intenção do texto de exibição do valor do campo. :/}
function SmartGetFieldDisplayText(Field:TField):string;

{$ifdef DELPHI5}
function VarIsNumeric(v:variant):boolean;
{$endif}

function  CRC32(const Data; DataLen:integer):cardinal; overload;
function  CRC32(const Str:string):cardinal; overload;
function  CRC32(Stream:TStream):cardinal; overload;

var
  LogFileName:string='rlib.log';
  LogActive  :boolean=False;

procedure LogClear;
procedure Log(const aMsg:string);

type
//{$ifdef KYLIX}
{$IFDEF LINUX}
  TRGBQuad=packed record
    rgbBlue    :byte;
    rgbGreen   :byte;
    rgbRed     :byte;
    rgbReserved:byte;
  end;
{$endif}
  TRGBArray=array[0..0] of TRGBQuad;
  PRGBArray=^TRGBArray;

procedure ChronoStart;
procedure ChronoMark;
procedure ChronoLap(const Msg:string);
function  ChronoStop:string;

function AuxBitmapNeeded(Width,Height:integer):TBitmap; overload;
function AuxBitmapNeeded:TBitmap; overload;
function NewBitmap(Width,Height:integer):TBitmap; overload;
function NewBitmap:TBitmap; overload;

{/@unit}

implementation

uses
  ExtCtrls, IntfGraphics;

{$ASMMODE INTEL}
function GetProcessorTick:Int64;register;
asm
  DB $0F,$31
end;

var
  ChronoStarted   :Int64=0;
  ChronoMarks     :array[0..1024] of Int64;
  ChronoInsertions:array[0..1024] of integer;
  ChronoMarkCount :integer=0;
  ChronoResults   :string='';

procedure ChronoStart;
begin
  ChronoStarted  :=GetProcessorTick;
  ChronoMarkCount:=0;
end;

procedure ChronoMark;
begin
  ChronoInsertions[ChronoMarkCount]:=length(ChronoResults);
  ChronoMarks[ChronoMarkCount]     :=GetProcessorTick;
  Inc(ChronoMarkCount);
end;

procedure ChronoLap(const Msg:string);
var
  Elapsed:Int64;
begin
  Dec(ChronoMarkCount);
  Elapsed:=GetProcessorTick-ChronoMarks[ChronoMarkCount];
  if Elapsed>0 then
  begin
    if ChronoInsertions[ChronoMarkCount]<Length(ChronoResults) then
      Insert(StringOfChar(#32,ChronoMarkCount*4)+Msg+#13,ChronoResults,ChronoInsertions[ChronoMarkCount]);
    ChronoResults:=ChronoResults+StringOfChar(#32,ChronoMarkCount*4)+Msg+' '+IntToStr(Elapsed)+'mips'#13;
  end;
end;

function ChronoStop:string;
begin
  Result:='Total time '+IntToStr(GetProcessorTick-ChronoStarted)+'mips'#13+ChronoResults;
  ChronoResults:=emptystr;
end;
// CHRONOMETER END

function NewBitmap:TBitmap;
begin
  Result:=NewBitmap(1,1);
end;

function NewBitmap(Width,Height:integer):TBitmap;
begin
  Result:= TBitmap.Create;
  Result.Width      :=Width;
  Result.Height     :=Height;
  Result.PixelFormat:=pf32bit;
end;

var
  __AuxBitmap:TBitmap=nil;
function AuxBitmapNeeded:TBitmap;
begin
  if __AuxBitmap<>nil then
    Result:=__AuxBitmap
  else
    Result:= AuxBitmapNeeded(1,1);
end;

function AuxBitmapNeeded(Width,Height:integer):TBitmap;
begin
  if __AuxBitmap=nil then
    __AuxBitmap:=NewBitmap(Width,Height)
  else
  begin
    if __AuxBitmap.Width<>Width then
      __AuxBitmap.Width:=Width;
    if __AuxBitmap.Height<>Height then
      __AuxBitmap.Height:=Height;
  end;
  Result:=__AuxBitmap;
end;

procedure LogClear;
begin
  if FileExists(LogFileName) then
    SysUtils.DeleteFile(LogFileName);
end;

function Milliseconds:cardinal;
var
  h,n,s,m:word;
begin
  DecodeTime(Now,h,n,s,m);
  Result:=((h*60+n)*60+s)*1000+m;
end;

var
  InProc :integer =0;
  LogLast:cardinal=0;

procedure Log(const aMsg:string);
label
  p1;
var
  LogHandle:TextFile;
  LogWrote :boolean;
  Elapsed  :cardinal;
begin
  if not LogActive then
    Exit;
  LogWrote:=False;
  p1:
  Inc(InProc);
  try
    if InProc=1 then
    begin
      LogWrote:=True;
      AssignFile(LogHandle,LogFileName);
      if FileExists(LogFileName) then
        Append(LogHandle)
      else
        Rewrite(LogHandle);
      try
        if LogLast=0 then
          Elapsed:=0
        else
          Elapsed:=Milliseconds-LogLast;
        WriteLn(LogHandle,TimeToStr(Now)+' '+FormatFloat('000000',Elapsed)+'ms: '+aMsg);
        LogLast:=Milliseconds;
      finally
        CloseFile(LogHandle);
      end;
    end;
  finally
    Dec(InProc);
  end;
  if not LogWrote then
    goto p1;
end;

type
  dw=record
    h,l:word;
  end;

const
  HEXDIGITS:string[16]='0123456789ABCDEF';

function ByteToHex(const aByte:byte):string;
begin
  Result:=HEXDIGITS[(aByte and $f0) shr 4+1]+HEXDIGITS[(aByte and $0f)+1];
end;

function HexToByte(const aHex:string):byte;
begin
  Result:=(Pos(UpCase(aHex[1]),HEXDIGITS)-1)*16+Pos(UpCase(aHex[2]),HEXDIGITS)-1;
end;

procedure FreeObj(var aObj);
begin
  if TObject(aObj)<>nil then
  begin
    TObject(aObj).Free;
    TObject(aObj):=nil;
  end;  
end;

procedure FreePtr(var aPtr);
begin
  if Pointer(aPtr)<>nil then
  begin
    FreeMem(Pointer(aPtr));
    Pointer(aPtr):=nil;
  end;  
end;

type
  TPublicGraphic=class(TGraphic);

function HexToBitmap(const aHex:string):TBitmap;
var
  Stream: TMemoryStream;
  i     :integer;
begin
  Stream:=TMemoryStream.Create;
  try
    // traduz string hex em binária
    i:=Length(aHex) div 2;
    Stream.Size:=i;
    HexToBin(@aHex[1],Stream.Memory,i);
    // procura referência para a classe
    Result:=TBitmap.Create;
    Stream.Position:=0;
    TPublicGraphic(Result).ReadData(Stream);
  finally
    Stream.Free;
  end;
end;

function HexToGraphic(const aHex:string):TGraphic;
var
  graphclassname:string[63];
  graphclass    :TGraphicClass;
  stream        :TStringStream;
  i,l           :integer;
begin
  Result:=nil;
  stream:=TStringStream.Create(emptystr);
  try
    // traduz string hex em binária
    l:=Length(aHex);
    i:=1;
    while i<l do
    begin
      stream.WriteString(char(HexToByte(aHex[i]+aHex[i+1])));
      inc(i,2);
    end;
    // pega o nome da classe
    stream.Seek(0,0);
    stream.Read(graphclassname[0],1);
    stream.Read(graphclassname[1],byte(graphclassname[0]));
    // procura referência para a classe
    graphclassname:=UpperCase(graphclassname);
    if graphclassname='TBITMAP' then
      graphclass:=TBitmap
    else if graphclassname='TICON' then
      graphclass:=TIcon
    else
      graphclass:=nil;
    // instancia e carrega o grafico
    if graphclass<>nil then
    begin
      Result:=graphclass.Create;
      try
        TPublicGraphic(Result).ReadData(stream);
      except
        FreeObj(Result);
        raise;
      end;
    end;
  finally
    FreeObj(stream);
  end;
end;

function NewComponentName(aComponent:TComponent):string;
var
  p,n:string;
  i,m:integer;
begin
  p:=aComponent.ClassName;
  if UpperCase(p[1])='T' then
    delete(p,1,1);
  m:=0;
  for i:=0 to aComponent.Owner.ComponentCount-1 do
  begin
    n:=aComponent.Owner.Components[i].Name;
    if AnsiSameText(Copy(n,1,Length(p)),p) then
      m:=Max(m,StrToIntDef(Copy(n,Length(p)+1,Length(n)),0));
  end;
  Result:=p+IntToStr(m+1);
end;

function GetTempFileName:string;
var
  tmppath:string;
begin
  Randomize;
  tmppath:=TempDir;
  if tmppath<>emptystr then
    tmppath:=IncludeTrailingBackslash(tmppath);
  repeat
    Result:=tmppath+'~fr'+IntToStr(Random(MaxInt))+'.tmp';
  until not FileExists(Result);
end;

function Token(const aTokenList:string; aIndex:integer; aTokenSeparator:char='|'):string;
var
  i,m,count:integer;
begin
  Result:=emptystr;
  count:=0;
  i:=1;
  while i<=Length(aTokenList) do
  begin
    m:=i;
    while (i<=Length(aTokenList)) and (aTokenList[i]<>aTokenSeparator) do
      inc(i);
    inc(count);
    if count=aIndex then
    begin
      Result:=Copy(aTokenList,m,i-m);
      break;
    end;
    inc(i);
  end;
end;

procedure TRLThread.InternalCall;
begin
  if Assigned(@MethodRef) then
    MethodRef;
  if Assigned(@ProcedureRef) then
    ProcedureRef;
end;

threadvar
  CurrentThreadRef:TRLThread;

procedure TRLThread.Execute;
label
  p1;
begin
  CurrentThreadRef:=Self;
  p1:
  if Terminated then
    Exit;
  if Synchronized then
    Synchronize(InternalCall)
  else
    InternalCall;
  if KeepLoop then
    goto p1;
end;

function NewThread(MethodRef:TThreadMethod; ProcedureRef:TProcedure; KeepLoop:boolean; Synchronized:boolean):TRLThread;
begin
  Result:=TRLThread.Create(True);
  Result.MethodRef      :=MethodRef;
  Result.ProcedureRef   :=ProcedureRef;
  Result.KeepLoop       :=KeepLoop;
  Result.Synchronized   :=Synchronized;
  Result.FreeOnTerminate:=True;
end;

function ThreadIt(MethodRef:TThreadMethod; KeepLoop:boolean=False; Synchronized:boolean=False):TRLThread;
begin
  Result:=NewThread(MethodRef,nil,KeepLoop,Synchronized);
end;

function ThreadIt(ProcedureRef:TProcedure; KeepLoop:boolean=False; Synchronized:boolean=False):TRLThread;
begin
  Result:=NewThread(nil,ProcedureRef,KeepLoop,Synchronized);
end;

procedure ThreadSafeCall(Method:TThreadMethod);
begin
  if CurrentThreadRef<>nil then
    CurrentThreadRef.Synchronize(Method)
  else
    Method;
end;

function IsMainThread:boolean;
begin
  Result:=GetCurrentThreadId=MainThreadID;
end;

function CurrentThread:TRLThread;
begin
  Result:=CurrentThreadRef;
end;

function FormatFileExt(const aExt:string):string;
begin
  if (aExt<>emptystr) and (aExt[1]<>'.') then
    Result:='.'+aExt
  else
    Result:=aExt;
end;

function AddFileFilter(const aFilter:string; const aDescription,aExt:string):string;
begin
  Result:=aFilter;
  if Result<>emptystr then
    Result:=Result+'|';
  Result:=Result+aDescription+' (*'+FormatFileExt(aExt)+')';
{$ifdef VCL}
  Result:=Result+'|*'+FormatFileExt(aExt);
{$else} {$ifdef DELPHI7}
  Result:=Result+'|*'+FormatFileExt(aExt);
{$endif}
{$endif}
end;

function GetFileFilterExt(const aFilter:string; aIndex:integer):string;
var
  p,i:integer;
  m:string;
begin
  if aIndex=0 then
    aIndex:=1;
  i:=1;
  while i<=aIndex do
  begin
    m:=Token(aFilter,i,'|');
    p:=Pos('(',m);
    if p>0 then
      delete(m,1,p);
    p:=Pos(')',m);
    if p>0 then
      m:=Copy(m,1,p-1);
    inc(i);
{$ifdef VCL}
    inc(i);
{$else} {$ifdef DELPHI7}
    inc(i);
{$endif}
{$endif}
  end;
  p:=Pos('.',m);
  if p>0 then
    delete(m,1,p);
  Result:=FormatFileExt(m);
end;

procedure RotatePoints(var aPoints:array of TPoint; const aAngle:double);
var
  theta   :double;
  costheta:double;
  sintheta:double;
  center  :TPoint;
  i,q     :integer;
procedure RotatePoint(var aPoint:TPoint);
var
  saved:TPoint;
begin
  saved   :=aPoint;
  aPoint.x:=round(saved.x*costheta-saved.y*sintheta);
  aPoint.y:=round(saved.x*sintheta+saved.y*costheta);
end;
begin
  theta   :=-aAngle*pi/180; // radians
  sintheta:=sin(theta);
  costheta:=cos(theta);
  // calcula centro
  center.x:=0;
  center.y:=0;
  q:=High(aPoints)+1;
  for i:=0 to q-1 do
  begin
    inc(center.x,aPoints[i].x);
    inc(center.y,aPoints[i].y);
  end;
  center.x:=round(center.x/q);
  center.y:=round(center.y/q);
  // roda
  for i:=0 to q-1 do
  begin
    dec(aPoints[i].x,center.x);
    dec(aPoints[i].y,center.y);
    RotatePoint(aPoints[i]);
    inc(aPoints[i].x,center.x);
    inc(aPoints[i].y,center.y);
  end;
end;

procedure RotateBitmap(aSource,aDest:TBitmap; aAngle:double; aAxis,aOffset:TPoint);
var
  x            :integer;
  xDest        :integer;
  xOriginal    :integer;
  xPrime       :integer;
  xPrimeRotated:integer;
  //
  y            :integer;
  yDest        :integer;
  yOriginal    :integer;
  yPrime       :integer;
  yPrimeRotated:integer;
  //
  Radians      :double;
  RadiansCos   :double;
  RadiansSin   :double;
  NormalImg    :TLazIntfImage;
  RotateImg    :TLazIntfImage;
begin
  // create intermediary images
  NormalImg := aSource.CreateIntfImage;
  RotateImg := TLazIntfImage.Create(aDest.Width, aDest.Height);
  RotateImg.DataDescription := NormalImg.DataDescription;
  RotateImg.SetSize(aDest.Width, aDest.Height);

  // Convert degrees to radians. Use minus sign to force clockwise rotation.
  Radians   :=aAngle*PI/180;
  RadiansSin:=sin(Radians);
  RadiansCos:=cos(Radians);
  // Step through each row of rotated image.
  for y:=0 to aDest.Height-1 do
  begin
    yDest  :=y-aOffset.y;
    yPrime :=2*(yDest-aAxis.y)+1; // center y: -1,0,+1
    // Step through each col of rotated image.
    for x:=0 to aDest.Width-1 do
    begin
      xDest :=x-aOffset.x;
      xPrime:=2*(xDest-aAxis.x)+1; // center x: -1,0,+1
      // Rotate (xPrime, yPrime) to location of desired pixel
      // Note:  There is negligible difference between floating point and scaled integer arithmetic here, so keep the math simple (and readable).
      xPrimeRotated:=round(xPrime*RadiansCos-yPrime*RadiansSin);
      yPrimeRotated:=round(xPrime*RadiansSin+yPrime*RadiansCos);
      // Transform back to pixel coordinates of image, including translation
      // of origin from axis of rotation to origin of image.
      xOriginal:=(xPrimeRotated-1) div 2+aAxis.x;
      yOriginal:=(yPrimeRotated-1) div 2+aAxis.y;
      // Make sure (xOriginal, yOriginal) is in aSource.  If not, assign blue color to corner points.
      if (xOriginal>=0) and (xOriginal<=aSource.Width-1) and (yOriginal>=0) and (yOriginal<=aSource.Height-1) then
      begin
        // Assign pixel from rotated space to current pixel in aDest
        RotateImg.Colors[x,y]:=NormalImg.Colors[xOriginal,yOriginal];
      end
      else if aSource.Height>0 then
      begin
        RotateImg.Colors[x,y]:=NormalImg.Colors[0,0];
      end
      else
        RotateImg.Colors[x,y]:=TColorToFPColor(clBlack);
    end;
  end;
  aDest.LoadFromIntfImage(RotateImg);
  RotateImg.Destroy;
  NormalImg.Destroy;
end;

function RotatedBitmap(aSource:TBitmap; aAngle:double):TBitmap;
var
  p:array[0..3] of TPoint;
  r:TRect;
begin
  p[0]:=Point(0,0);
  p[1]:=Point(aSource.Width-1,0);
  p[2]:=Point(aSource.Width-1,aSource.Height-1);
  p[3]:=Point(0,aSource.Height-1);
  RotatePoints(p,aAngle);
  r:=PointsRect(p);
  //
  Result:=NewBitmap(r.Right-r.Left,r.Bottom-r.Top);
  try
    Result.Transparent     :=aSource.Transparent;
    Result.TransparentColor:=aSource.TransparentColor;
    Result.TransparentMode :=aSource.TransparentMode;
    RotateBitmap(aSource,Result,aAngle,Point(aSource.Width div 2,aSource.Height div 2),Point(-r.Left,-r.Top));
  except
    Result.free;
    raise;
  end;
end;

function PointsRect(const aPoints:array of TPoint):TRect;
var
  i:integer;
begin
  for i:=0 to High(aPoints) do
    if i=0 then
    begin
      Result.Left  :=aPoints[i].x;
      Result.Top   :=aPoints[i].y;
      Result.Right :=aPoints[i].x;
      Result.Bottom:=aPoints[i].y;
    end
    else
    begin
      Result.Left  :=Min(Result.Left  ,aPoints[i].x);
      Result.Top   :=Min(Result.Top   ,aPoints[i].y);
      Result.Right :=Max(Result.Right ,aPoints[i].x);
      Result.Bottom:=Max(Result.Bottom,aPoints[i].y);
    end;
end;

function PointsSize(const aPoints:array of TPoint):TPoint;
begin
  with PointsRect(aPoints) do
  begin
    Result.x:=Right-Left;
    Result.y:=Bottom-Top;
  end;
end;

procedure ScalePoints(var aPoints:array of TPoint; const aRect:TRect);
var
  bounds:TRect;
  fx,fy :double;
  i,len :integer;
begin
  bounds:=PointsRect(aPoints);
  if RectWidth(bounds)<>0 then
    fx:=RectWidth(aRect)/RectWidth(bounds)
  else
    fx:=0;
  if RectHeight(bounds)<>0 then
    fy:=RectHeight(aRect)/RectHeight(bounds)
  else
    fy:=0;
  if fx=0 then
    fx:=fy;
  if fy=0 then
    fy:=fx;
  if (fx=0) or (fy=0) then
    Exit;  
  if fx<fy then
    fy:=fx
  else
    fx:=fy;
  len:=High(aPoints)+1;
  for i:=0 to len-1 do
    with aPoints[i] do
    begin
      x:=Round((x-bounds.Left)*fx)+aRect.Left;
      y:=Round((y-bounds.Top)*fy)+aRect.Top;
    end;
end;

procedure StretchPoints(var aPoints:array of TPoint; const aRect:TRect);
var
  bounds:TRect;
  fx,fy :double;
  i,len :integer;
begin
  bounds:=PointsRect(aPoints);
  if RectWidth(bounds)<>0 then
    fx:=RectWidth(aRect)/RectWidth(bounds)
  else
    fx:=0;
  if RectHeight(bounds)<>0 then
    fy:=RectHeight(aRect)/RectHeight(bounds)
  else
    fy:=0;
  if fx=0 then
    fx:=1;
  if fy=0 then
    fy:=1;
  if (fx=0) or (fy=0) then
    Exit;  
  len:=High(aPoints)+1;
  for i:=0 to len-1 do
    with aPoints[i] do
    begin
      x:=Round((x-bounds.Left)*fx)+aRect.Left;
      y:=Round((y-bounds.Top)*fy)+aRect.Top;
    end;
end;

procedure CenterPoints(var aPoints:array of TPoint; const aRect:TRect);
var
  bounds :TRect;
  ofx,ofy:integer;
  i,len  :integer;
begin
  bounds:=PointsRect(aPoints);
  ofx   :=(RectWidth(aRect)-RectWidth(bounds)) div 2;
  ofy   :=(RectHeight(aRect)-RectHeight(bounds)) div 2;
  len   :=High(aPoints)+1;
  for i:=0 to len-1 do
    with aPoints[i] do
    begin
      x:=x-bounds.Left+aRect.Left+ofx;
      y:=y-bounds.Top+aRect.Top+ofy;
    end;
end;

function TextBounds(const aText:string; aFont:TFont; aAngle:double):TPoint;
var
  b:TBitmap;
  p:array[0..3] of TPoint;
begin
  b:=AuxBitmapNeeded;
  b.Canvas.Font.Assign(aFont);
  Result.x:=b.Canvas.TextWidth(aText);
  Result.y:=b.Canvas.TextHeight(aText);
  if aAngle<>0 then
  begin
    p[0]:=Point(0,0);
    p[1]:=Point(Result.x,0);
    p[2]:=Point(Result.x,Result.y);
    p[3]:=Point(0,Result.y);
    RotatePoints(p,aAngle);
    Result:=PointsSize(p);
  end;
end;

procedure MoveRect(var aRect:TRect; aX,aY:integer);
begin
  OffsetRect(aRect,-aRect.Left+aX,-aRect.Top+aY);
end;

function RectWidth(const aRect:TRect):integer;
begin
  Result:=aRect.Right-aRect.Left;
end;

function RectHeight(const aRect:TRect):integer;
begin
  Result:=aRect.Bottom-aRect.Top;
end;

function ReduceRect(const aRect:TRect; aPixels:TRect):TRect;
begin
  Result.Left  :=aRect.Left  +aPixels.Left;
  Result.Top   :=aRect.Top   +aPixels.Top;
  Result.Right :=aRect.Right -aPixels.Right;
  Result.Bottom:=aRect.Bottom-aPixels.Bottom;
end;

function IncreaseRect(const aRect:TRect; aPixels:TRect):TRect;
begin
  Result.Left  :=aRect.Left  -aPixels.Left;
  Result.Top   :=aRect.Top   -aPixels.Top;
  Result.Right :=aRect.Right +aPixels.Right;
  Result.Bottom:=aRect.Bottom+aPixels.Bottom;
end;

function DiffRect(const aRectOut,aRectIn:TRect):TRect;
begin
  Result.Left  :=aRectIn.Left   +aRectOut.Left;
  Result.Top   :=aRectIn.Top    +aRectOut.Top;
  Result.Right :=aRectOut.Right -aRectIn.Right;
  Result.Bottom:=aRectOut.Bottom-aRectIn.Bottom;
end;

function IterateJustification(var aText:string; var aIndex:integer):boolean;
  function FindSpc:boolean;
  const
    SPC=[#32,#9,#13,#10];
  begin
    Result:=false;
    while (aIndex>0) and (aText[aIndex] in SPC) do
      Dec(aIndex);
    while aIndex>0 do
      if aText[aIndex] in SPC then
      begin
        while (aIndex>0) and (aText[aIndex] in SPC) do
          Dec(aIndex);
        if aIndex>0 then
        begin
          Insert(#32,aText,aIndex+1);
          Result:=true;
        end;
        break;
      end
      else
        Dec(aIndex);
  end;
begin
  Result:=FindSpc;
  if not Result then
  begin
    aIndex:=Length(aText);
    Result:=FindSpc;
  end;
end;

function ScaleRect(const aSource,aTarget:TRect; aCenter:boolean):TRect;
var
  sw,sh,tw,th,w,h:integer;
  fw,fh:double;
begin
  sw:=aSource.Right-aSource.Left;
  sh:=aSource.Bottom-aSource.Top;
  tw:=aTarget.Right-aTarget.Left;
  th:=aTarget.Bottom-aTarget.Top;
  // calcula o maior dos fatores de proporção entre largura e altura
  fw:=tw/sw;
  fh:=th/sh;
  if fw>fh then
  begin
    h:=th;
    w:=round(h*sw/sh);
  end
  else
  begin
    w:=tw;
    h:=round(w*sh/sw);
  end;
  Result.Left  :=aTarget.Left;
  Result.Top   :=aTarget.Top;
  Result.Right :=Result.Left+w;
  Result.Bottom:=Result.Top+h;
  if aCenter then
    OffsetRect(Result,(tw-w) div 2,(th-h) div 2);
end;

procedure StreamWrite(aStream:TStream; const aStr:string);
begin
  if aStr<>emptystr then
    aStream.Write(aStr[1],Length(aStr));
end;

procedure StreamWriteLn(aStream:TStream; const aStr:string='');
begin
  StreamWrite(aStream,aStr);
  StreamWrite(aStream,#13#10);
end;

var
  TempFileNames:TStringList=nil;

procedure RegisterTempFile(const aFileName:string);
begin
  if not Assigned(TempFileNames) then
    TempFileNames:=TStringList.Create;
  TempFileNames.Add(aFileName);
end;

procedure UnregisterTempFile(const aFileName:string);
var
  i:integer;
begin
  if Assigned(TempFileNames) then
  begin
    i:=TempFileNames.IndexOf(aFileName);
    if i<>-1 then
      TempFileNames.Delete(i);
  end;
end;

procedure ClearTempFiles;
var
  i:integer;
begin
  if Assigned(TempFileNames) then
  begin
    for i:=0 to TempFileNames.Count-1 do
      SysUtils.DeleteFile(TempFileNames[i]);
    TempFileNames.Free;
    TempFileNames:=nil;
  end;
end;

function SmartGetFieldDisplayText(Field:TField):string;
begin
  if (Field is TBlobField) and not Assigned(Field.OnGetText) then
    Result:=Field.AsString
  else
    Result:=Field.DisplayText;
end;

{$ifdef DELPHI5}
function VarIsNumeric(v:variant):boolean;
begin
  Result:=VarType(v) in [varSmallint,varInteger,varSingle,varDouble,varCurrency];
end;
{$endif}

const
  CRC32Table:array[0..255] of cardinal=(
    $0       ,$77073096,$EE0E612C,$990951BA,$76DC419 ,$706AF48F,$E963A535,$9E6495A3,
    $EDB8832 ,$79DCB8A4,$E0D5E91E,$97D2D988,$9B64C2B ,$7EB17CBD,$E7B82D07,$90BF1D91,
    $1DB71064,$6AB020F2,$F3B97148,$84BE41DE,$1ADAD47D,$6DDDE4EB,$F4D4B551,$83D385C7,
    $136C9856,$646BA8C0,$FD62F97A,$8A65C9EC,$14015C4F,$63066CD9,$FA0F3D63,$8D080DF5,
    $3B6E20C8,$4C69105E,$D56041E4,$A2677172,$3C03E4D1,$4B04D447,$D20D85FD,$A50AB56B,
    $35B5A8FA,$42B2986C,$DBBBC9D6,$ACBCF940,$32D86CE3,$45DF5C75,$DCD60DCF,$ABD13D59,
    $26D930AC,$51DE003A,$C8D75180,$BFD06116,$21B4F4B5,$56B3C423,$CFBA9599,$B8BDA50F,
    $2802B89E,$5F058808,$C60CD9B2,$B10BE924,$2F6F7C87,$58684C11,$C1611DAB,$B6662D3D,
    $76DC4190,$1DB7106 ,$98D220BC,$EFD5102A,$71B18589,$6B6B51F ,$9FBFE4A5,$E8B8D433,
    $7807C9A2,$F00F934 ,$9609A88E,$E10E9818,$7F6A0DBB,$86D3D2D ,$91646C97,$E6635C01,
    $6B6B51F4,$1C6C6162,$856530D8,$F262004E,$6C0695ED,$1B01A57B,$8208F4C1,$F50FC457,
    $65B0D9C6,$12B7E950,$8BBEB8EA,$FCB9887C,$62DD1DDF,$15DA2D49,$8CD37CF3,$FBD44C65,
    $4DB26158,$3AB551CE,$A3BC0074,$D4BB30E2,$4ADFA541,$3DD895D7,$A4D1C46D,$D3D6F4FB,
    $4369E96A,$346ED9FC,$AD678846,$DA60B8D0,$44042D73,$33031DE5,$AA0A4C5F,$DD0D7CC9,
    $5005713C,$270241AA,$BE0B1010,$C90C2086,$5768B525,$206F85B3,$B966D409,$CE61E49F,
    $5EDEF90E,$29D9C998,$B0D09822,$C7D7A8B4,$59B33D17,$2EB40D81,$B7BD5C3B,$C0BA6CAD,
    $EDB88320,$9ABFB3B6,$3B6E20C ,$74B1D29A,$EAD54739,$9DD277AF,$4DB2615 ,$73DC1683,
    $E3630B12,$94643B84,$D6D6A3E ,$7A6A5AA8,$E40ECF0B,$9309FF9D,$A00AE27 ,$7D079EB1,
    $F00F9344,$8708A3D2,$1E01F268,$6906C2FE,$F762575D,$806567CB,$196C3671,$6E6B06E7,
    $FED41B76,$89D32BE0,$10DA7A5A,$67DD4ACC,$F9B9DF6F,$8EBEEFF9,$17B7BE43,$60B08ED5,
    $D6D6A3E8,$A1D1937E,$38D8C2C4,$4FDFF252,$D1BB67F1,$A6BC5767,$3FB506DD,$48B2364B,
    $D80D2BDA,$AF0A1B4C,$36034AF6,$41047A60,$DF60EFC3,$A867DF55,$316E8EEF,$4669BE79,
    $CB61B38C,$BC66831A,$256FD2A0,$5268E236,$CC0C7795,$BB0B4703,$220216B9,$5505262F,
    $C5BA3BBE,$B2BD0B28,$2BB45A92,$5CB36A04,$C2D7FFA7,$B5D0CF31,$2CD99E8B,$5BDEAE1D,
    $9B64C2B0,$EC63F226,$756AA39C,$26D930A ,$9C0906A9,$EB0E363F,$72076785,$5005713 ,
    $95BF4A82,$E2B87A14,$7BB12BAE,$CB61B38 ,$92D28E9B,$E5D5BE0D,$7CDCEFB7,$BDBDF21 ,
    $86D3D2D4,$F1D4E242,$68DDB3F8,$1FDA836E,$81BE16CD,$F6B9265B,$6FB077E1,$18B74777,
    $88085AE6,$FF0F6A70,$66063BCA,$11010B5C,$8F659EFF,$F862AE69,$616BFFD3,$166CCF45,
    $A00AE278,$D70DD2EE,$4E048354,$3903B3C2,$A7672661,$D06016F7,$4969474D,$3E6E77DB,
    $AED16A4A,$D9D65ADC,$40DF0B66,$37D83BF0,$A9BCAE53,$DEBB9EC5,$47B2CF7F,$30B5FFE9,
    $BDBDF21C,$CABAC28A,$53B39330,$24B4A3A6,$BAD03605,$CDD70693,$54DE5729,$23D967BF,
    $B3667A2E,$C4614AB8,$5D681B02,$2A6F2B94,$B40BBE37,$C30C8EA1,$5A05DF1B,$2D02EF8D);

procedure CRC32Add(var Result:cardinal; const Data; DataLen:integer);
var
  i:integer;
  p:PChar;
begin
  p:=@Data;
  for i:=0 to DataLen-1 do
    Result:=((Result shr 8) and $00FFFFFF) xor CRC32Table[(Result xor Byte(p[i])) and $000000FF];
end;

function CRC32(const Data; DataLen:integer):cardinal;
begin
  Result:=$FFFFFFFF;
  CRC32Add(Result,Data,DataLen);
  Result:=not Result;
end;

function CRC32(const Str:string):cardinal;
var
  len:integer;
begin
  len:=Length(Str);
  if len=0 then
    Result:=0
  else
    Result:=CRC32(Str[1],len);
end;

function CRC32(Stream:TStream):cardinal;
const
  MaxBuf=16384;
var
  Buf:packed array[0..MaxBuf-1] of char;
  BufLen:integer;
begin
  Result:=$FFFFFFFF;
  repeat
    BufLen:=Stream.Read(Buf,MaxBuf);
    if BufLen=0 then
      Break;
    CRC32Add(Result,Buf[0],BufLen);
  until False;
  Result:=not Result;
end;

initialization
  CurrentThreadRef:=nil;
  LogFileName:=IncludeTrailingBackslash(ExtractFilePath(ParamStr(0)))+'RLib.log';
  TempDir    :=GetTempDir;

finalization
  ClearTempFiles;
  __AuxBitmap.Free;

end.


