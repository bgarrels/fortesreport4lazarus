{$I RLReport.inc}

{@unit RLRichParsers - Implementação do leitor de texto no formato RichText.
Portado para o Lazarus - Trabalho inicial de Isaac Trindade da Silva contato tioneobrasil@yahoo.com.br dicas4lazarus@yahoo.com.br
Lazarus Ported - initial work by Isaac 07/2009
}
unit RLRichParsers;
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
{$IFDEF FPC}
  LCLIntf,
  LCLType,
  Types,
{$ELSE}
  Types,
{$ENDIF}
{$endif}

{$ifdef CLX}
  QGraphics, RLMetaCLX,
{$else}
  Graphics, RLMetaVCL,
{$endif}
{$IFDEF FPC}
  dynlibs,
  rlshared,
{$ENDIF}
  RLUtils, RLMetaFile;

type
  TRichEditVersion = 1..3; //Verifica a versão do rich para possiveis correções no formato

const
  MaxTabs=255;
  TabSize=48;

type
  TRLRichFont=class
  public
    Num    :integer;
    Family :string;
    Charset:integer;
    Name   :string;
  end;

  TRLRichFontList=class(TObjectList)
  private
    function GetFonts(i: integer): TRLRichFont;
  public
    property Fonts[i:integer]:TRLRichFont read GetFonts; default;
  end;

  TRLRichColor=class
  public
    Red  :byte;
    Green:byte;
    Blue :byte;
  end;

  TRLRichColorList=class(TObjectList)
  private
    function GetColors(i: integer): TRLRichColor;
  public
    property Colors[i:integer]:TRLRichColor read GetColors; default;
  end;

  TRLRichStyle=class
  public
    Bold     :boolean;
    Italic   :boolean;
    Underline:boolean;
    FontSize :integer;
    FontNum  :integer;
    ForeColor:integer;
    BkgdColor:integer;
    //
    procedure Assign(Style:TRLRichStyle);
    procedure Clear;
    function  Same(Style:TRLRichStyle):boolean;
  end;

  TRLRichWord=class;
  TRLRichTab=class;
  TRLRichLine=class;
  TRLRichParser=class;
  TRLRichParagraph=class;
  TRLRichElement=class;

  TRLRichElementList=class(TObjectList)
  private
    function GetElements(i: integer): TRLRichElement;
  public
    property Elements[i:integer]:TRLRichElement read GetElements; default;
  end;

  TRLRichElement=class
  private
    Destroying:boolean;
  public
    Parent  :TRLRichElement;
    Elements:TRLRichElementList;
    //
    constructor Create(aParent:TRLRichElement); virtual;
    destructor  Destroy; override;
    //
    function Paragraph:TRLRichParagraph;
    function Parser:TRLRichParser;
  end;

  TRLRichObject=class(TRLRichElement)
  public
    Position   :TPoint;
    LeftSiblin :TRLRichObject;
    RightSiblin:TRLRichObject;
    Alignment  :TRLMetaTextAlignment;
    constructor Create(aParent:TRLRichElement); override;
    function GetBaseLine:integer; virtual; abstract;
    function GetSize:TPoint; virtual; abstract;
    function GetBounds:TRect;
  end;

  TRLRichParagraph=class(TRLRichElement)
  public
    Alignment:TRLMetaTextAlignment;
    Tabs     :array[1..MaxTabs] of integer;
    //
    constructor Create(aParent:TRLRichElement); override;
    //
    function  TabAfter(Pos:integer):integer;
    function  CurrentWordNeeded:TRLRichWord;
    function  NewWord:TRLRichWord;
    function  NewTab:TRLRichTab;
    function  NewLine:TRLRichLine;
    function  CurrentWord: TRLRichWord;
    function  GetBounds:TRect;
  end;

  TRLRichWordPart=class;

  TRLRichWord=class(TRLRichObject)
  private
    function GetParts(i: integer): TRLRichWordPart;
  public
    function GetBaseLine:integer; override;
    function GetSize:TPoint; override;
    //
    function GetText:string;
    function NewPart(const Text:string):TRLRichWordPart;
    //
    property Parts[i:integer]:TRLRichWordPart read GetParts; default;
  end;

  TRLRichTab=class(TRLRichElement)
  public
  end;

  TRLRichLine=class(TRLRichElement)
  public
  end;

  TRLRichWordPart=class(TRLRichObject)
  public
    Text    :string;
    Style   :TRLRichStyle;
    BaseLine:integer;
    Size    :TPoint;
    //
    constructor Create(aParent:TRLRichElement); override;
    Destructor Destroy;override;
    function GetBaseLine:integer; override;
    function GetSize:TPoint; override;
    function Word:TRLRichWord;
  end;

  TRLRichImg=class(TRLRichObject)
  public
    Data:string;
    Size:TPoint;
    function GetBaseLine:integer; override;
    function GetSize:TPoint; override;
  end;

  TBookmark=integer;

  TRLRichParser=class(TRLRichElement)
  private
    FileVer         :integer;
    FileAnsi        :boolean;
    FileDeff        :integer;
    FileDefLang     :integer;
    FileAnsiCpg     :integer;
    FileViewKind    :integer;
    FontList        :TRLRichFontList;
    ColorList       :TRLRichColorList;
    CurrentStyle    :TRLRichStyle;
    CurrentParagraph:TRLRichParagraph;
    Stream          :TStream;
    function GetFont(Index: integer): TRLRichFont;
    function GetColor(Index: integer): TRLRichColor;
    function CurrentStyleNeeded: TRLRichStyle;
    function CreateParagraph(Reset:boolean):TRLRichParagraph;
    function CurrentParagraphNeeded: TRLRichParagraph;
    function GetParagraphs(i: integer): TRLRichParagraph;
    procedure CreateObjsFromStr(const Str: string);
  protected
    FormattedWidth:integer;
    FormattedWrap :boolean;
    function  GetBookmark:TBookmark;
    procedure GotoBookmark(Bookmark:TBookmark);
    procedure SkipNulls;
    procedure SkipSpaces;
    function  ReadCh(var Ch:char):boolean;
    function  ReadThisCh(Ch: char): boolean;
    function  ReadThisText(const Text: string): boolean;
    function  ReadAnyWord(var Word: string): boolean;
    function  ReadThisWord(const Word: string): boolean;
    function  ReadAnyNum(var Num: integer): boolean;
    function  ReadAnyLiteralCh(var Ch: char): boolean;
    function  ReadAnyTextualCh(var Ch: char): boolean;
    function  ReadAnyCtrlCh(var Ch: char): boolean;
    function  ReadAnyLiteral(var Literal: string): boolean;
    function  ReadAnyTextual(var Textual: string): boolean;
    function  ReadAnyCtrlWord(var Word: string; var Param: integer): boolean;
    function  ReadThisCtrlWord(const Word: string; var Param: integer): boolean; overload;
    function  ReadThisCtrlWord(const Word: string): boolean; overload;
    function  ReadFileHeader: boolean;
    function  ReadFontItem: boolean;
    function  ReadFontTable: boolean;
    function  ReadColorItem: boolean;
    function  ReadColorTable: boolean;
    function  ReadAnyImage: boolean;
    function  ReadAnyGroup: boolean;
    function  ReadDocumentBody: boolean;
    function  ReadDocument:boolean;
    //
    function  NewParagraph(Reset:boolean):TRLRichParagraph;
    procedure CopyStyleToCanvas(Style:TRLRichStyle; Canvas: TObject);
  public
    DefaultFontName  :string;
    DefaultRedValue  :byte;
    DefaultGreenValue:byte;
    DefaultBlueValue :byte;
    WordWrap         :boolean;
    WrapMargin       :integer;
    TextCRC          :cardinal;
    TextLength       :integer;
    TextSize         :TPoint;
    //
    constructor Create(aParent:TRLRichElement); override;
    destructor Destroy; override;
    //
    procedure Clear;
    procedure LoadFromStream(Stream:TStream);
    procedure SetText(const NewText:string);
    procedure PaintTo(Canvas:TObject; const Rect:TRect; Cor: Tcolor);
    procedure Format(RightMargin:integer);
    //
    property Paragraphs[i:integer]:TRLRichParagraph read GetParagraphs; default;
  end;
{/@unit}

var
  RichEditVersion: TRichEditVersion;

implementation

const
  RichEdit10ModuleName = 'RICHED32.DLL';
  RichEdit20ModuleName = 'RICHED20.DLL';
  NullSet   =[#13,#10,#26];
  SpaceSet  =NullSet+[#32];
  CtrlSet   =['{','}','\'];
  DigitSet  =['0'..'9'];
  HexSet    =DigitSet+['a'..'f','A'..'F'];
  AlphaSet  =['a'..'z','A'..'Z','_'];
  AlphaSetEx=AlphaSet+DigitSet;
  LiteralSet=[#9,#32..#126,#128..#255]-CtrlSet-NullSet;
  TextualSet=AlphaSet+DigitSet+[#32];
  NullParam =MaxInt;

procedure DoFillRect(Canvas:TObject; const Rect:TRect);
begin
  if Canvas is TCanvas then
    TCanvas(Canvas).FillRect(Rect)
  else if Canvas is TRLGraphicSurface then
    TRLGraphicSurface(Canvas).FillRect(Rect);
end;

procedure SetBrushStyle(Canvas:TObject; Style:TBrushStyle);
begin
  if Canvas is TCanvas then
    TCanvas(Canvas).Brush.Style:=Style
  else if Canvas is TRLGraphicSurface then
    TRLGraphicSurface(Canvas).Brush.Style:=Style;
end;

procedure SetBrushColor(Canvas:TObject; Color:TColor);
begin
  if Canvas is TCanvas then
    TCanvas(Canvas).Brush.Color:=Color
  else if Canvas is TRLGraphicSurface then
    TRLGraphicSurface(Canvas).Brush.Color:=Color;
end;

procedure SetFontName(Canvas:TObject; const Name:string);
begin
  if Canvas is TCanvas then
    TCanvas(Canvas).Font.Name:=Name
  else if Canvas is TRLGraphicSurface then
    TRLGraphicSurface(Canvas).Font.Name:=Name;
end;

procedure SetFontCharset(Canvas:TObject; Charset:TFontCharset);
begin
  if Canvas is TCanvas then
    TCanvas(Canvas).Font.Charset:=Charset
  else if Canvas is TRLGraphicSurface then
    TRLGraphicSurface(Canvas).Font.Charset:=Charset;
end;

function GetFontName(Canvas:TObject):string;
begin
  if Canvas is TCanvas then
    Result:=TCanvas(Canvas).Font.Name
  else if Canvas is TRLGraphicSurface then
    Result:=TRLGraphicSurface(Canvas).Font.Name;
end;

function GetFontCharset(Canvas:TObject):TFontCharset;
begin
  if Canvas is TCanvas then
    Result:=TCanvas(Canvas).Font.Charset
  else if Canvas is TRLGraphicSurface then
    Result:=TRLGraphicSurface(Canvas).Font.Charset
  else
    FillChar(Result,SizeOf(Result),0);
end;

function GetFontColor(Canvas:TObject):TColor;
begin
  if Canvas is TCanvas then
    Result:=TCanvas(Canvas).Font.Color
  else if Canvas is TRLGraphicSurface then
    Result:=TRLGraphicSurface(Canvas).Font.Color
  else
    Result:=0;
end;

procedure SetFontColor(Canvas:TObject; Color:TColor);
begin
  if Canvas is TCanvas then
    TCanvas(Canvas).Font.Color:=Color
    else if Canvas is TRLGraphicSurface then
    TRLGraphicSurface(Canvas).Font.Color:=Color;
end;

function GetFontStyle(Canvas:TObject):TFontStyles;
begin
  if Canvas is TCanvas then
    Result:=TCanvas(Canvas).Font.Style
  else if Canvas is TRLGraphicSurface then
    Result:=TRLGraphicSurface(Canvas).Font.Style;
end;

procedure SetFontStyle(Canvas:TObject; Style:TFontStyles);
begin
  if Canvas is TCanvas then
    TCanvas(Canvas).Font.Style:=Style
  else if Canvas is TRLGraphicSurface then
    TRLGraphicSurface(Canvas).Font.Style:=Style;
end;

procedure SetFontSize(Canvas:TObject; Size:integer);
begin
  if Canvas is TCanvas then
    TCanvas(Canvas).Font.Size:=Size
  else if Canvas is TRLGraphicSurface then
    TRLGraphicSurface(Canvas).Font.Size:=Size;
end;

function GetTextWidth(Canvas:TObject; const Text:string):integer;
begin
  if Canvas is TCanvas then
    Result:=TCanvas(Canvas).TextWidth(Text)
  else if Canvas is TRLGraphicSurface then
    Result:=TRLGraphicSurface(Canvas).TextWidth(Text)
  else
    Result:=0;
end;

function GetTextHeight(Canvas:TObject; const Text:string):integer;
begin
  if Canvas is TCanvas then
    Result:=TCanvas(Canvas).TextHeight(Text)
  else if Canvas is TRLGraphicSurface then
    Result:=TRLGraphicSurface(Canvas).TextHeight(Text)
  else
    Result:=0;
end;

procedure DoTextOut(Canvas:TObject; X,Y:integer; const Text:string; Continuous:boolean;
  Alignment:TRLMetaTextAlignment);
var
  Flags:TRLMetaTextFlags;
begin
  if Canvas is TCanvas then
    TCanvas(Canvas).TextOut(X,Y,Text)
  else if Canvas is TRLGraphicSurface then
  begin
    Flags:=MetaTextFlagAutoSize;
    if Continuous then
      Flags:=Flags or MetaTextFlagContinuous;
    TRLGraphicSurface(Canvas).TextOutEx(X,Y,Text,Flags);
  end;
end;

procedure DoTextRect(Canvas:TObject; const Rect:TRect; X,Y:integer; const Text:string;
  Continuous:boolean; Alignment:TRLMetaTextAlignment);
var
  Flags:TRLMetaTextFlags;
begin
  if Canvas is TCanvas then
    TCanvas(Canvas).TextRect(Rect,X,Y,Text)
  else if Canvas is TRLGraphicSurface then
  begin
    Flags:=MetaTextFlagAutoSize;
    if Continuous then {erro no alinhamento justificado aqui}
      Flags:=Flags or MetaTextFlagContinuous;
      TRLGraphicSurface(Canvas).TextRectEx(Rect,X,Y,Text,Alignment,0,Flags);
  end;
end;

{ TRLRichFontList }

function TRLRichFontList.GetFonts(i: integer): TRLRichFont;
begin
  Result:=Items[i] as TRLRichFont;
end;

{ TRLRichColorList }

function TRLRichColorList.GetColors(i: integer): TRLRichColor;
begin
  Result:=Items[i] as TRLRichColor;
end;

{ TRLRichParser }

constructor TRLRichParser.Create(aParent:TRLRichElement);
begin
  inherited;
  //
  DefaultFontName  :='Arial';
  DefaultRedValue  :=0;
  DefaultGreenValue:=0;
  DefaultBlueValue :=0;
  //
  FileVer          :=0;
  FileAnsi         :=False;
  FileDeff         :=0;
  FileDefLang      :=0;
  FileAnsiCpg      :=0;
  FileViewKind     :=0;
  FontList.Free;
  FontList         :=nil;
  ColorList        :=nil;
  CurrentStyle     :=nil;
  FormattedWidth   :=0;
  FormattedWrap    :=True;
  CurrentParagraph :=nil;
  //
  FontList :=TRLRichFontList.Create;
  ColorList:=TRLRichColorList.Create;
  //
  Stream    :=nil;
  WordWrap  :=True;
  WrapMargin:=0;
  TextSize  :=Point(0,0);
  TextCRC   :=0;
  TextLength:=0;
end;

destructor TRLRichParser.Destroy;
begin
  inherited;
  FontList.Free;
  ColorList.Free;
  if Assigned(CurrentStyle) then
    CurrentStyle.Free;
end;

procedure TRLRichParser.Clear;
begin
  FileVer       :=0;
  FileAnsi      :=False;
  FileDeff      :=0;
  FileDefLang   :=0;
  FileAnsiCpg   :=0;
  FileViewKind  :=0;
  FormattedWidth:=0;
  FormattedWrap :=True;
  //
  FontList.Clear;
  ColorList.Clear;
  Elements.Clear;
  //
  CurrentStyleNeeded.Clear;
  CurrentParagraph:=nil;
  //
  TextSize  :=Point(0,0);
  TextCRC   :=0;
  TextLength:=0;
end;

function TRLRichParser.CurrentStyleNeeded:TRLRichStyle;
begin
  if not Assigned(CurrentStyle) then
    CurrentStyle:=TRLRichStyle.Create;
  Result:=CurrentStyle;
end;

function TRLRichParser.GetBookmark: TBookmark;
begin
  Result:=Stream.Position;
end;

procedure TRLRichParser.GotoBookmark(Bookmark: TBookmark);
begin
  Stream.Position:=Bookmark;
end;

function TRLRichParser.ReadCh(var Ch: char): boolean;
begin
  Result:=(Stream.Read(Ch,1)=1);
end;

procedure TRLRichParser.SkipNulls;
var
  ch:char;
  m :TBookmark;
begin
  repeat
    m:=GetBookmark;
    if not ReadCh(ch) then
      Break;
    if not (ch in NullSet) then
    begin
      GotoBookmark(m);
      Break;
    end
  until False;
end;

procedure TRLRichParser.SkipSpaces;
var
  ch:char;
  m :TBookmark;
begin
  repeat
    m:=GetBookmark;
    if not ReadCh(ch) then
      Break;
    if not (ch in SpaceSet) then
    begin
      GotoBookmark(m);
      Break;
    end;
  until False;
end;

function TRLRichParser.ReadThisCh(Ch:char):boolean;
var
  Bookmark:TBookmark;
  c       :char;
begin
  Result:=False;
  Bookmark:=GetBookmark;
  try
    Result:=ReadCh(c) and (c=Ch);
  finally
    if not Result then
      GotoBookmark(Bookmark);
  end;
end;

function TRLRichParser.ReadThisText(const Text:string):boolean;
var
  Bookmark:TBookmark;
  ch      :char;
  i       :integer;
begin
  Result:=False;
  Bookmark:=GetBookmark;
  try
    i:=1;
    while (i<=Length(Text)) and ReadCh(ch) and (ch=Text[i]) do
      Inc(i);
    Result:=(i>Length(Text));
  finally
    if not Result then
      GotoBookmark(Bookmark);
  end;
end;

function TRLRichParser.ReadAnyWord(var Word: string): boolean;
var
  Bookmark:TBookmark;
  ch      :char;
begin
  Bookmark:=GetBookmark;
  try
    Word:=emptystr;
    while ReadCh(ch) and (ch in AlphaSet) do
    begin
      Word    :=Word+ch;
      Bookmark:=GetBookmark;
    end;
    Result:=(Word<>emptystr);
  finally
    GotoBookmark(Bookmark);
  end;
end;

function TRLRichParser.ReadThisWord(const Word:string):boolean;
var
  Bookmark:TBookmark;
  w       :string;
begin
  Result:=False;
  Bookmark:=GetBookmark;
  try
    Result:=ReadAnyWord(w) and SameText(w,Word);
  finally
    if not Result then
      GotoBookmark(Bookmark);
  end;
end;

function TRLRichParser.ReadAnyNum(var Num: integer): boolean;
var
  Bookmark:TBookmark;
  s       :string;
  ch      :char;
  i       :integer;
begin
  Result:=False;
  Bookmark:=GetBookmark;
  try
    s:=emptystr;
    i:=0;
    while ReadCh(ch) and ((ch in DigitSet) or ((ch in ['+','-']) and (i=0))) do
    begin
      s:=s+ch;
      Inc(i);
      Bookmark:=GetBookmark;
    end;
    if s<>'' then
    begin
      Num   :=StrToInt(s);
      Result:=True;
    end;
  finally
    GotoBookmark(Bookmark);
  end;
end;

function TRLRichParser.ReadAnyLiteralCh(var Ch: char): boolean;
var
  Bookmark:TBookmark;
begin
  Result:=False;
  Bookmark:=GetBookmark;
  try
    if ReadCh(Ch) and (Ch in LiteralSet) then
      Result:=True;
  finally
    if not Result then
      GotoBookmark(Bookmark);
  end;
end;

function TRLRichParser.ReadAnyTextualCh(var Ch: char): boolean;
var
  Bookmark:TBookmark;
begin
  Result:=False;
  Bookmark:=GetBookmark;
  try
    if ReadCh(Ch) and (Ch in TextualSet) then
      Result:=True;
  finally
    if not Result then
      GotoBookmark(Bookmark);
  end;
end;

function TRLRichParser.ReadAnyCtrlCh(var Ch: char): boolean;
var
  Bookmark:TBookmark;
  c,h1,h2:char;
begin
  Result:=False;
  Bookmark:=GetBookmark;
  try
    if ReadCh(c) then
      if c in [#9,#13] then
      begin
        Ch:=c;
        if c=#13 then
          ReadThisCh(#10);
        Result:=True;
      end
      else if c='\' then
        if ReadThisWord('tab') then
        begin
          Ch    :=#9;
          Result:=True;
        end
        else if ReadThisWord('line') then
        begin
          Ch    :=#13;
          Result:=True;
        end
        else if ReadCh(c) and (c='''') then
          if ReadCh(h1) and (h1 in HexSet) then
            if ReadCh(h2) and (h2 in HexSet) then
            begin
              Ch    :=Char(StrToInt('$'+h1+h2));
              Result:=True;
            end;
  finally
    if not Result then
      GotoBookmark(Bookmark);
  end;
end;

function TRLRichParser.ReadAnyLiteral(var Literal: string): boolean;
var
  Bookmark:TBookmark;
  ch      :char;
begin
  Bookmark:=GetBookmark;
  try
    Literal:='';
    while ReadAnyLiteralCh(ch) or ReadAnyCtrlCh(ch) do
    begin
     if ch=#13 then
        ch:=#32;
      Literal :=Literal+ch;
      Bookmark:=GetBookmark;

    end;
    Result:=(Literal<>'');
  finally
    GotoBookmark(Bookmark);
  end;
end;

function TRLRichParser.ReadAnyTextual(var Textual: string): boolean;
var
  Bookmark:TBookmark;
  ch      :char;
begin
  Bookmark:=GetBookmark;
  try
    Textual:='';
    while ReadAnyTextualCh(ch) or ReadAnyCtrlCh(ch) do
    begin
      Textual :=Textual+ch;
      Bookmark:=GetBookmark;
    end;
    Result:=(Textual<>'');
  finally
    GotoBookmark(Bookmark);
  end;
end;

function TRLRichParser.ReadAnyCtrlWord(var Word: string; var Param: integer): boolean;
var
  Bookmark:TBookmark;
begin
  Result:=False;
  Bookmark:=GetBookmark;
  try
    if ReadThisCh('\') then
    begin
      if ReadThisCh(#13) then
      begin
        Word:='par';
        ReadThisCh(#10);
      end
      else if ReadThisCh(#10) then
        Word:='par'
      else if ReadThisCh('*') then
        Word:='*'
      else if ReadAnyWord(Word) then
        if SameText(Word,'tab') or SameText(Word,'line') then
          Exit
        else
      else
        Exit;
      if not ReadAnyNum(Param) then
        Param:=NullParam;
      ReadThisCh(#32);
      Result:=True;
    end;
  finally
    if not Result then
      GotoBookmark(Bookmark);
  end;
end;

function TRLRichParser.ReadThisCtrlWord(const Word: string; var Param: integer): boolean;
var
  Bookmark:TBookmark;
  w       :string;
begin
  Result:=False;
  Bookmark:=GetBookmark;
  try
    if ReadAnyCtrlWord(w,Param) then
      if SameText(w,Word) then
        Result:=True;
  finally
    if not Result then
      GotoBookmark(Bookmark);
  end;
end;

function TRLRichParser.ReadThisCtrlWord(const Word: string): boolean;
var
  Param: integer;
begin
  Result:=ReadThisCtrlWord(Word,Param);
end;

function TRLRichParser.ReadFileHeader: boolean;
var
  Bookmark:TBookmark;
  Word    :string;
  Param   :integer;
begin
  Result:=False;
  Bookmark:=GetBookmark;
  try
    if ReadThisCtrlWord('rtf',Self.FileVer) then
    begin
      SkipSpaces;
      while ReadAnyCtrlWord(Word,Param) do
      begin
        if SameText(Word,'ansi') then
          Self.FileAnsi:=True
        else if SameText(Word,'ansicpg') then
          Self.FileAnsiCpg:=Param
        else if SameText(Word,'deff') then
          Self.FileDeff:=Param
        else if SameText(Word,'deflang') then
          Self.FileDefLang:=Param
        else
          ;
        SkipSpaces;
      end;
      Result:=True;
    end;
  finally
    if not Result then
      GotoBookmark(Bookmark);
  end;
end;

function TRLRichParser.ReadFontItem: boolean;
var
  Bookmark:TBookmark;
  Word    :string;
  Param   :integer;
  Font    :TRLRichFont;
begin
  Result:=False;
  Font:=nil;
  Bookmark:=GetBookmark;
  try
    if ReadThisCh('{') then
    begin
      SkipSpaces;
      if ReadThisCtrlWord('f',Param) then
      begin
        Font:=TRLRichFont.Create;
        Font.Num:=Param;
        SkipSpaces;
        while ReadAnyCtrlWord(Word,Param) do
        begin
          if SameText(Word,'fnil') or SameText(Word,'froman') or SameText(Word,'fswiss') or
            SameText(Word,'fmodern') or SameText(Word,'fscript') or SameText(Word,'fdecor') or
            SameText(Word,'ftech') or SameText(Word,'fbidi') then
            Font.Family:=Copy(Word,2,MaxInt)
          else if SameText(Word,'fcharset') then
            Font.Charset:=Param;
          SkipSpaces;
        end;
        if ReadAnyTextual(Font.Name) then
          if ReadThisCh(';') then
            if ReadThisCh('}') then
            begin
              Self.FontList.Add(Font);
              Result:=True;
            end;
      end;
    end;
  finally
    if not Result then
    begin
      if Assigned(Font) then
        Font.Free;
      GotoBookmark(Bookmark);
    end;
  end;
end;

function TRLRichParser.ReadFontTable: boolean;
var
  Bookmark:TBookmark;
begin
  Result:=False;
  Bookmark:=GetBookmark;
  try
    if ReadThisCh('{') then
      if ReadThisCtrlWord('fonttbl') then
      begin
        SkipSpaces;
        while ReadFontItem do
          SkipSpaces;
        if ReadThisCh('}') then
          Result:=True;
      end;
  finally
    if not Result then
      GotoBookmark(Bookmark);
  end;
end;

function TRLRichParser.ReadColorItem: boolean;
var
  Bookmark:TBookmark;
  Param   :integer;
  Color   :TRLRichColor;
begin
  Result:=False;
  Color:=nil;
  Bookmark:=GetBookmark;
  try
    if ReadThisCtrlWord('red',Param) then
    begin
      Color:=TRLRichColor.Create;
      Color.Red:=Param;
      SkipSpaces;
      if ReadThisCtrlWord('green',Param) then
      begin
        Color.Green:=Param;
      SkipSpaces;
        if ReadThisCtrlWord('blue',Param) then
        begin
          Color.Blue:=Param;
          SkipSpaces;
          if ReadThisCh(';') then
          begin
            Self.ColorList.Add(Color);
            Result:=True;
          end;
        end;
      end;
    end;
  finally
    if not Result then
    begin
      if Assigned(Color) then
        Color.Free;
      GotoBookmark(Bookmark);
    end;
  end;
end;

function TRLRichParser.ReadColorTable: boolean;
var
  Bookmark:TBookmark;
begin
  Result:=False;
  Bookmark:=GetBookmark;
  try
    if ReadThisCh('{') then
      if ReadThisCtrlWord('colortbl') then
      begin
        SkipSpaces;
        ReadThisCh(';');
        SkipSpaces;
        while ReadColorItem do
          SkipSpaces;
        if ReadThisCh('}') then
          Result:=True;
      end;
  finally
    if not Result then
      GotoBookmark(Bookmark);
  end;
end;

function TRLRichParser.ReadAnyImage: boolean;
var
  Bookmark:TBookmark;
  Word    :string;
  Param   :integer;
begin
  Result:=False;
  Bookmark:=GetBookmark;
  try
    if ReadThisCh('{') then
    begin
      repeat
        SkipSpaces;
        if ReadAnyCtrlWord(Word,Param) then
        else if ReadAnyGroup then
        else
          Break;
      until False;
      if ReadThisCh('}') then
        Result:=True;
    end;
  finally
    if not Result then
      GotoBookmark(Bookmark);
  end;
end;

function TRLRichParser.ReadAnyGroup: boolean;
var
  Bookmark:TBookmark;
  Level   :integer;
  ch      :char;
begin
  Result:=False;
  Bookmark:=GetBookmark;
  try
    Level:=0;
    if ReadThisCh('{') then
    begin
      Inc(Level);
      while (Level>0) and ReadCh(ch) do
        if ch='{' then
          Inc(Level)
        else if ch='}' then
          Dec(Level);
      Result:=True;
    end;
  finally
    if not Result then
      GotoBookmark(Bookmark);
  end;
end;

// cria objetos a partir de uma string
procedure TRLRichParser.CreateObjsFromStr(const Str:string);
var
  i,m:integer;
begin
  i:=1;

  while i<=Length(Str) do
    case Str[i] of
      #9: // adiciona tabulacao
      begin
        CurrentParagraphNeeded.NewTab;
        Inc(i);
      end;
      #13: // adiciona linha
      begin
        CurrentParagraphNeeded.NewLine;
        Inc(i);
      end;
      #32: // adiciona espacos
      begin
        m:=i;
        while (i<=Length(Str)) and (Str[i]=#32) do
          Inc(i);
        CurrentParagraphNeeded.NewWord.NewPart(Copy(Str,m,i-m));
      end;
    else
      // adiciona pedaco de palavra a palavra corrente
      m:=i;
      while (i<=Length(Str)) and not (Str[i] in [#32,#9,#13]) do
        Inc(i);
      CurrentParagraphNeeded.CurrentWordNeeded.NewPart(Copy(Str,m,i-m));
    end;
 end;

function TRLRichParser.ReadDocumentBody: boolean;
var
  Word   :string;
  Param  :integer;
  Literal:string;
  LastAlg: Byte; ///?
begin
  repeat
    SkipNulls;

    if ReadAnyCtrlWord(Word,Param) then
      if SameText(Word,'viewkind') then
        FileViewKind:=Param
      else if SameText(Word,'par') then
      begin
        NewParagraph(False);
        CurrentParagraphNeeded.Alignment:= LastAlg;
      end
      else if SameText(Word,'pard') then
        NewParagraph(True)
      else if SameText(Word,'plain') then
        CurrentStyleNeeded.Clear
      else if SameText(Word,'f') then // font num
        CurrentStyleNeeded.FontNum:=Param
      else if SameText(Word,'fs') then // font size
        CurrentStyleNeeded.FontSize:=Param
      else if SameText(Word,'b') then // bold
        CurrentStyleNeeded.Bold:=(Param<>0)
      else if SameText(Word,'i') then // italic
        CurrentStyleNeeded.Italic:=(Param<>0)
      else if SameText(Word,'ul') then // continuous underline
        CurrentStyleNeeded.Underline:=(Param<>0)
      else if SameText(Word,'ulnone') then // underline off
        CurrentStyleNeeded.Underline:=False
      else if SameText(Word,'cf') then // foreground color
        CurrentStyleNeeded.ForeColor:=Param
      else if SameText(Word,'cb') then // background color
        CurrentStyleNeeded.BkgdColor:=Param
      else if SameText(Word,'ql') then // align left
        CurrentParagraphNeeded.Alignment:=MetaTextAlignmentLeft
      else if SameText(Word,'qr') then // align right
        CurrentParagraphNeeded.Alignment:=MetaTextAlignmentRight
      else if SameText(Word,'qc') then // align center
        CurrentParagraphNeeded.Alignment:=MetaTextAlignmentCenter
      else if SameText(Word,'qj') then // align justify
        CurrentParagraphNeeded.Alignment:=MetaTextAlignmentJustify
      else if SameText(Word,'debug') then // debug
        Word:='' else
    else
     if ReadAnyLiteral(Literal) then
      CreateObjsFromStr(Literal)
    else if ReadAnyImage then
    else if ReadAnyGroup then
       else
       Break;

   LastAlg:= CurrentParagraphNeeded.Alignment;
   until False;
  Result:=True;
end;

function TRLRichParser.ReadDocument: boolean;
var
  Bookmark:TBookmark;
begin
  Result:=False;
  Bookmark:=GetBookmark;
  try
    // tenta ler um documento completo no format rtf (header+body+footer+...)
    if ReadThisCh('{') then
      if ReadFileHeader then
      begin
        SkipSpaces;
        // lê a tabela de fontes
        ReadFontTable;
        SkipSpaces;
        // lê a tabela de cores
        ReadColorTable;
        SkipSpaces;
        // lê o conteúdo do documento
        ReadDocumentBody;
        SkipSpaces;
        if ReadThisCh('}') then
          Result:=True;
      end
      else
    // tenta ler apenas o conteúdo
    else if ReadDocumentBody then
      Result:=True;
  finally
    if not Result then
      GotoBookmark(Bookmark);
  end;
end;

function TRLRichParser.GetFont(Index: integer): TRLRichFont;
var
  Font:TRLRichFont;
begin
  while FontList.Count<Index+1 do
  begin
    Font:=TRLRichFont.Create;
    Font.Name   :=DefaultFontName;
    Font.Family :='nil';
    Font.Charset:=0;
    Font.Num    :=FontList.Count;
    FontList.Add(Font);
  end;
  Result:=FontList[Index];
end;

function TRLRichParser.GetColor(Index:integer):TRLRichColor;
var
  Color:TRLRichColor;
begin
  while ColorList.Count-1 < Index do
  begin
    Color:=TRLRichColor.Create;
    Color.Red   :=DefaultRedValue;
    Color.Green :=DefaultGreenValue;
    Color.Blue  :=DefaultBlueValue;
    ColorList.Add(Color);
  end;
  if RichEditVersion <> 1 then
    Result:=ColorList[index] else // index mais um no contador correção do erro da cor de font para o wp 1.0
    Result:=ColorList[index+1];
end;

procedure TRLRichParser.CopyStyleToCanvas(Style:TRLRichStyle; Canvas:TObject);
var
  NewStyle:TFontStyles;
begin
  SetFontName(Canvas,GetFont(Style.FontNum).Name);
  SetFontSize(Canvas,Style.FontSize div 2);
  NewStyle:=GetFontStyle(Canvas);
  if Style.Bold then
    Include(NewStyle,fsBold)
  else
    Exclude(NewStyle,fsBold);
  if Style.Italic then
    Include(NewStyle,fsItalic)
  else
    Exclude(NewStyle,fsItalic);
  if Style.Underline then
    Include(NewStyle,fsUnderline)
  else
    Exclude(NewStyle,fsUnderline);
  SetFontStyle(Canvas,NewStyle);

{  if Style.BkgdColor=0 then
    SetBrushColor(Canvas,Clwhite)
  else
    with GetColor(Style.BkgdColor-1) do
      SetBrushColor(Canvas,RGB(Red,Green,Blue));}

  if Style.ForeColor=0 then
    SetFontColor(Canvas,clWindowText)
  else
    with GetColor(Style.ForeColor-1) do
         SetFontColor(Canvas, Rgb(Red, Green, Blue));
end;

function TRLRichParser.CreateParagraph(Reset:boolean):TRLRichParagraph;
begin
  Result:=TRLRichParagraph.Create(Self);
  if Reset then
     CurrentStyleNeeded.Clear;
end;

function TRLRichParser.CurrentParagraphNeeded:TRLRichParagraph;
begin
  if not Assigned(CurrentParagraph) then
    CurrentParagraph:=CreateParagraph(False);
  Result:=CurrentParagraph;
end;

function TRLRichParser.NewParagraph(Reset:boolean):TRLRichParagraph;
begin
  Result:=CreateParagraph(Reset);
  CurrentParagraph:=Result;
end;

procedure TRLRichParser.LoadFromStream(Stream:TStream);
var
  StreamText:string;
begin
  SetLength(StreamText,Stream.Size);
  if Stream.Size>0 then
    Stream.Read(StreamText[1],Stream.Size);
  SetText(StreamText);
end;

procedure TRLRichParser.SetText(const NewText: string);
var
  OldCRC,NewCRC:cardinal;
  OldLen,NewLen:integer;
begin
  NewLen:=Length(NewText);
  NewCRC:=CRC32(NewText);
  OldLen:=TextLength;
  OldCRC:=TextCRC;
  if (NewLen=OldLen) and (NewCRC=OldCRC) then
    Exit;
  //
  Clear;
  FormattedWidth:=0;
  Self.Stream:=TStringStream.Create(NewText);
  try
    Self.Stream.Position:=0;
    ReadDocument;
  finally
    Self.Stream.Free;
  end;
  TextLength:=NewLen;
  TextCRC   :=NewCRC;
end;

procedure TRLRichParser.Format(RightMargin:integer);
var
  LineHeight    :integer;
  LastHeight    :integer;
  LineWidth     :integer;
  LineObjCount  :integer;
  LineObjWidth  :integer;
  ObjsDrawn     :integer;
  LineFirstElem :integer;
  LineLastElem  :integer;
  LineFirstObj  :integer;
  LineLastObj   :integer;
  MaxBaseLin    :integer;
  WrapLimit     :integer;
  ParagraphIndex:integer;
  ElemIndex     :integer;
  PartIndex     :integer;
  Elem          :TRLRichElement;
  Paragraph     :TRLRichParagraph;
  ParFirstObj   :integer;
  ParLastObj    :integer;
  Part          :TRLRichWordPart;
  Part0         :TRLRichObject;
  AWord         :TRLRichWord;
  bd            :TRect;
  Cur           :TPoint;
  Obj           :TRLRichObject;
  ObjSize       :TPoint;
  ObjBase       :integer;
  TabPos        :integer;
  EoL,EoP       :boolean;
  aux           :integer;
begin
  TextSize.X:=0;
  TextSize.Y:=0;
  Cur.X     :=0;
  Cur.Y     :=0;
  LastHeight:=10; // magic

  // largura máxima para a quebra de linha
  if WordWrap then
    WrapLimit:=RightMargin
  else
    WrapLimit:=MaxInt;
  // percorre parágrafos
  for ParagraphIndex:=0 to Self.Elements.Count-1 do
  begin
    Paragraph:=Paragraphs[ParagraphIndex];
    // se o parágrafo tiver algum elemento
    if Paragraph.Elements.Count>0 then
    begin
      // verifica quem é o último obj do parágrafo
      ParFirstObj:=-1;
      ParLastObj :=-1;
      for ElemIndex:=0 to Paragraph.Elements.Count-1 do
        if Paragraph.Elements[ElemIndex] is TRLRichObject then
        begin
          if ParFirstObj=-1 then
            ParFirstObj:=ElemIndex;
          ParLastObj:=ElemIndex;
        end;
      // processa todos os elementos do paragrafo
      ElemIndex:=0;
      while ElemIndex<Paragraph.Elements.Count do
      begin
        // calc quantos objetos cabem e quais as dimensoes da linha
        LineWidth   :=0;
        LineHeight  :=0;
        MaxBaseLin  :=0;
        LineObjCount:=0;
        LineObjWidth:=0;
        LineFirstObj:=-1;
        LineLastObj :=-1;
        // marca o primeiro Elem da linha
        LineFirstElem:=ElemIndex;
        LineLastElem :=ElemIndex;
        while ElemIndex<Paragraph.Elements.Count do
        begin
          Elem:=Paragraph.Elements[ElemIndex];
          // pega a largura dependendo do tipo de objeto
          if Elem is TRLRichObject then
          begin
            Obj    :=Elem as TRLRichObject;
            ObjSize:=Obj.GetSize;
            ObjBase:=Obj.GetBaseLine;
          end
          else if Elem is TRLRichTab then
          begin
            TabPos:=Paragraph.TabAfter(LineWidth);
            if TabPos>0 then
              ObjSize.X:=TabPos-LineWidth
            else
              ObjSize.X:=TabSize;
            ObjSize.Y:=0;
            ObjBase  :=0;
          end
          else
          begin
            ObjSize.X:=0;
            ObjSize.Y:=0;
            ObjBase  :=0;
          end;
          // quebra linha somente apos o primeiro obj e se ainda houver outros objs (só considera objs)
          if (Elem is TRLRichObject) and (LineFirstObj<>LineLastObj) and (LineLastObj<ParLastObj) and
            (LineWidth+ObjSize.X>WrapLimit) then
            Break;
          // contabiliza somente objs
          if Elem is TRLRichObject then
          begin
            Inc(LineObjCount);
            Inc(LineObjWidth,ObjSize.X);
            if LineFirstObj=-1 then
              LineFirstObj:=ElemIndex;
            LineLastObj:=ElemIndex;
          end;
          // o último elem colocado na linha
          LineLastElem:=ElemIndex;
          Inc(LineWidth,ObjSize.X);
          LineHeight:=Max(LineHeight,ObjSize.Y);
          MaxBaseLin:=Max(MaxBaseLin,ObjBase);
          Inc(ElemIndex);
          // quebra de linha forçada
          if Elem is TRLRichLine then
             Break;
        end;
        // fim de parágrafo
        EoP:=(LineLastElem>=Paragraph.Elements.Count-1);
        // quebra o vínculo entre palavras de linhas diferentes
        Part0:=nil;
        // faz a distribuição dos objetos de acordo com o alinhamento
        // ElemIndex aponta para o 1o Elem da próx linha, e LineFirstElem para o 1o desta linha
        ElemIndex:=LineFirstElem;
        ObjsDrawn:=0;
        while ElemIndex<=LineLastElem do
        begin
          Elem:=Paragraph.Elements[ElemIndex];
          if Elem is TRLRichObject then
          begin
            Obj:=Elem as TRLRichObject;
            Obj.Position.Y:=Cur.Y+LineHeight-Obj.GetSize.Y-(MaxBaseLin-Obj.GetBaseLine);
            ObjSize:=Obj.GetSize;
            Dec(LineObjWidth,ObjSize.X);
            case Paragraph.Alignment of
{Esquerda}    MetaTextAlignmentLeft:
              begin
                Obj.Position.X:=Cur.X;
                Obj.Alignment :=MetaTextAlignmentLeft;
                Inc(Cur.X,ObjSize.X);
              end;
{Direita}     MetaTextAlignmentRight:
              begin
                Obj.Position.X:=RightMargin-LineWidth+Cur.X;
                Obj.Alignment :=MetaTextAlignmentRight;
                Inc(Cur.X,ObjSize.X);
              end;
{Centro}      MetaTextAlignmentCenter:
              begin
                Obj.Position.X:=(RightMargin-LineWidth) div 2+Cur.X;
                Obj.Alignment :=MetaTextAlignmentCenter;
                Inc(Cur.X,ObjSize.X);
              end;
{Justificar}  MetaTextAlignmentJustify:
              begin
                 EoL:=(ElemIndex>=LineLastObj);
                // o ultimo Elem da linha que nao eh o ultimo do paragrafo eh colado aa direita
                if EoL and not EoP then
                  Cur.X:=RightMargin-ObjSize.X;
                Obj.Position.X:=Cur.X;
                if EoP then
                  Obj.Alignment:=MetaTextAlignmentLeft
                else
                  Obj.Alignment:=MetaTextAlignmentJustify;
                // os espaços são distribuidos, exceto na última linha
                Inc(Cur.X,ObjSize.X);
                if not EoP then
                begin
                  // o espaço q sobrou após o desenho de obj, menos a larg dos objs q falta desenhar, dividido pela quant de obj q falta
                  aux:= (RightMargin-Cur.X-LineObjWidth) div (LineObjCount-ObjsDrawn);
                  if aux>0 then
                    Inc(Cur.X,aux);
                end;
              end;
            end;

            // ajusta a posicao das partes se for palavra
            if Obj is TRLRichWord then
            begin
              AWord:=(Obj as TRLRichWord);
              aux:=AWord.Position.X;
              for PartIndex:=0 to AWord.Elements.Count-1 do
              begin
                Part:=AWord[PartIndex];
                Part.Position.X:=aux;
                Part.Position.Y:=AWord.GetBounds.Bottom-Part.Size.Y-(MaxBaseLin-Part.BaseLine);
               if Assigned(Part0) then
                begin
                  Part0.RightSiblin:=Part;
                  Part.LeftSiblin  :=Part0;
                end;
                Part0:=Part;
                Inc(aux,Part.Size.X);
              end;
            end
            else
              Part0:=Obj;
            // recalcula dimensao do richtext
            bd        :=Obj.GetBounds;
            TextSize.X:=Max(TextSize.X,bd.Right);
            TextSize.Y:=Max(TextSize.Y,bd.Bottom);
            Inc(ObjsDrawn);
          end
          else if Elem is TRLRichTab then
          begin
            TabPos:=Paragraph.TabAfter(Cur.X);
            if TabPos=0 then
              Inc(Cur.X,TabSize)
            else
              Inc(Cur.X,TabPos-Cur.X);
          end
          else if Elem is TRLRichLine then
              ;
        Inc(ElemIndex);
          //
        end;
        //
        LastHeight:=LineHeight;
        Inc(Cur.Y, LineHeight);
        Cur.X:=0;
      end;
    end else Inc(Cur.Y,LastHeight);
  end;
  FormattedWidth:=RightMargin;
  FormattedWrap :=WordWrap;
end;

procedure TRLRichParser.PaintTo(Canvas:TObject; const Rect:TRect; Cor: Tcolor);
var
  ParagraphIndex:integer;
  ElemIndex     :integer;
  PartIndex     :integer;
  AWord         :TRLRichWord;
  Paragraph     :TRLRichParagraph;
  Elem          :TRLRichElement;
  NewWidth      :integer;
  aux           :TRect;
  r0            :integer;
  p             :TRLRichWordPart;
//  I: integer;
//  T: String;
begin
  SetBrushColor(Canvas,Cor);
  NewWidth:=Rect.Right-Rect.Left;
  if (NewWidth<>FormattedWidth) or (WordWrap<>FormattedWrap) then
    Format(NewWidth);
  // todos os parágrafos
  for ParagraphIndex:=0 to Elements.Count-1 do
  begin
    Paragraph:=Paragraphs[ParagraphIndex];
    // todos os objetos do parágrafo
    for ElemIndex:=0 to Paragraph.Elements.Count-1 do
    begin
      Elem:=Paragraph.Elements[ElemIndex];
      if Elem is TRLRichWord then
      begin
        AWord:=Elem as TRLRichWord;
        // todas as partes da palavra
        r0:=0;
        for PartIndex:=0 to AWord.Elements.Count-1 do
        begin
          p:=AWord.Parts[PartIndex];
          CopyStyleToCanvas(p.Style,Canvas);
          aux:=p.GetBounds;
          OffsetRect(aux,Rect.Left,Rect.Top);
          if Assigned(p.LeftSiblin) and (aux.Left<r0) then
            OffsetRect(aux,r0-aux.Left,0);
                                                        
          DoTextRect(Canvas,aux,aux.Left,aux.Top,p.Text,false{Assigned(p.RightSiblin)},p.Alignment);
          r0:=aux.Right;
        end;
      end
      else if Elem is TRLRichImg then
      begin
      end;
    end;
  end;
end;

function TRLRichParser.GetParagraphs(i: integer): TRLRichParagraph;
begin
  Result:=Elements[i] as TRLRichParagraph;
end;

{ TRLRichParagraph }

constructor TRLRichParagraph.Create(aParent:TRLRichElement);
var
  i:integer;
begin
  inherited;
  Alignment:= MetaTextAlignmentLeft;
  for i:=1 to MaxTabs do
    Tabs[i]:=i*TabSize;
end;

function TRLRichParagraph.CurrentWord: TRLRichWord;
begin
  if (Elements.Count>0) and (Elements[Elements.Count-1] is TRLRichWord) then
    Result:=Elements[Elements.Count-1] as TRLRichWord
  else
    Result:=nil;
end;

function TRLRichParagraph.GetBounds: TRect;
var
  i,p:integer;
  b:TRect;
begin
  Result:=Rect(0,0,0,0);
  p:=0;
  for i:=0 to Elements.Count-1 do
    if Elements[i] is TRLRichObject then
    begin
      Inc(p);
      b:=TRLRichObject(Elements[i]).GetBounds;
      if p=1 then
        Result:=b
      else
      begin
        Result.Left  :=Min(Result.Left,b.Left);
        Result.Top   :=Min(Result.Top,b.Top);
        Result.Right :=Max(Result.Right,b.Right);
        Result.Bottom:=Max(Result.Bottom,b.Bottom);
      end;
    end;
end;

function TRLRichParagraph.CurrentWordNeeded: TRLRichWord;
begin
  if (CurrentWord=nil) or (Copy(CurrentWord.GetText,1,1)=#32) then
    NewWord;
  Result:=CurrentWord;
end;

function TRLRichParagraph.NewTab: TRLRichTab;
begin
  Result:=TRLRichTab.Create(Self);
end;

function TRLRichParagraph.NewLine: TRLRichLine;
begin
  Result:=TRLRichLine.Create(Self);
end;

function TRLRichParagraph.NewWord: TRLRichWord;
begin
  Result:=TRLRichWord.Create(Self);
end;

function TRLRichParagraph.TabAfter(Pos: integer): integer;
var
  i:integer;
begin
  i:=1;
  while (i<=MaxTabs) and (Pos>=Tabs[i]) do
    Inc(i);
  if i<=MaxTabs then
    Result:=Tabs[i]
  else
    Result:=0;
end;

{ TRLRichElement }

constructor TRLRichElement.Create(aParent: TRLRichElement);
begin
  Destroying:=False;
  inherited Create;
  Parent:=aParent;
  Elements :=TRLRichElementList.Create;
  if Assigned(Parent) then
    Parent.Elements.Add(Self);
end;

destructor TRLRichElement.Destroy;
begin
  Destroying:=True;
  if Assigned(Parent) and not Parent.Destroying then
    Parent.Elements.Extract(Self);
  Elements.Free;
  inherited;
end;

function TRLRichElement.Paragraph: TRLRichParagraph;
begin
  Result:=Parent as TRLRichParagraph;
end;

function TRLRichElement.Parser: TRLRichParser;
begin
  if Assigned(Parent) then
    if Parent is TRLRichParser then
      Result:=Parent as TRLRichParser
    else
      Result:=Parent.Parser
  else
    Result:=nil;
end;

{ TRLRichWord }

function TRLRichWord.GetBaseLine: integer;
var
  i:integer;
begin
  Result:=0;
  for i:=0 to Elements.Count-1 do
    Result:=Max(Result,Parts[i].BaseLine);
end;

function TRLRichWord.GetParts(i: integer): TRLRichWordPart;
begin
  Result:=Elements[i] as TRLRichWordPart;
end;

function TRLRichWord.GetSize: TPoint;
var
  i:integer;
begin
  Result:=Point(0,0);
  for i:=0 to Elements.Count-1 do
  begin
    Inc(Result.X,Parts[i].Size.X);
    Result.Y:=Max(Result.Y,Parts[i].Size.Y);
  end;
end;

function TRLRichWord.GetText: string;
var
  i:integer;
begin
  Result:=emptystr;
  for i:=0 to Elements.Count-1 do
    Result:=Result+Parts[i].Text;
end;

function TRLRichWord.NewPart(const Text:string):TRLRichWordPart;
var
  bmp:TBitmap;
begin
  Result     :=TRLRichWordPart.Create(Self);
  Result.Text:=Text;
  Result.Style.Assign(Parser.CurrentStyleNeeded);
  // calcula size
  bmp:=AuxBitmapNeeded;
  Parser.CopyStyleToCanvas(Result.Style,bmp.Canvas);
  Result.Size.X  :=bmp.Canvas.TextWidth(Result.Text);
  Result.Size.Y  :=bmp.Canvas.TextHeight(Result.Text);
  Result.BaseLine:=CanvasGetDescent(bmp.Canvas);
  Bmp.free;
end;

{ TRLRichWordPart }

constructor TRLRichWordPart.Create(aParent:TRLRichElement);
begin
  inherited;
  Text    :=emptystr;
  Style   :=TRLRichStyle.Create;
  BaseLine:=0;
  Size    :=Point(0,0);
end;

destructor TRLRichWordPart.Destroy;
begin
inherited;
Style.Free;
end;

function TRLRichWordPart.GetBaseLine: integer;
begin
  Result:=BaseLine;
end;

function TRLRichWordPart.GetSize: TPoint;
begin
  Result:=Size;
end;

function TRLRichWordPart.Word: TRLRichWord;
begin
  Result:=Parent as TRLRichWord;
end;

{ TRLRichStyle }

procedure TRLRichStyle.Assign(Style: TRLRichStyle);
begin
  Self.Bold     :=Style.Bold;
  Self.Italic   :=Style.Italic;
  Self.Underline:=Style.Underline;
  Self.FontSize :=Style.FontSize;
  Self.FontNum  :=Style.FontNum;
  Self.ForeColor:=Style.ForeColor;
  Self.BkgdColor:=Style.BkgdColor;
end;

procedure TRLRichStyle.Clear;
begin
  Bold     :=False;
  Italic   :=False;
  Underline:=False;
  FontSize :=0;
  FontNum  :=0;
  ForeColor:=0;
  BkgdColor:=0;
end;

function TRLRichStyle.Same(Style: TRLRichStyle): boolean;
begin
  Result:=(Self.Bold=Style.Bold) and
    (Self.Italic=Style.Italic) and
    (Self.Underline=Style.Underline) and
    (Self.FontSize=Style.FontSize) and
    (Self.FontNum=Style.FontNum) and
    (Self.ForeColor=Style.ForeColor) and
    (Self.BkgdColor=Style.BkgdColor);
end;

{ TRLRichObject }

constructor TRLRichObject.Create(aParent:TRLRichElement);
begin
  inherited;
  Position   :=Point(0,0);
  LeftSiblin :=nil;
  RightSiblin:=nil;
  Alignment  :=MetaTextAlignmentLeft;
end;

function TRLRichObject.GetBounds: TRect;
var
  sz:TPoint;
begin
  Result.TopLeft:=Position;
  sz            :=GetSize;
  Result.Right  :=Result.Left+sz.X;
  Result.Bottom :=Result.Top +sz.Y;
end;

{ TRLRichImg }

function TRLRichImg.GetBaseLine: integer;
begin
  Result:=0;
end;

function TRLRichImg.GetSize: TPoint;
begin
  Result:=Size;
end;

{ TRLRichElementList }

function TRLRichElementList.GetElements(i: integer): TRLRichElement;
begin
  Result:=Items[i] as TRLRichElement;
end;

{ Initialization part }

var
  OldError: Longint;
  FLibHandle: THandle;
  Ver: TOsVersionInfo;

initialization
  RichEditVersion := 1;
  {$IFDEF FPC}
  OldError := 0;
  {$ELSE}
  OldError := SetErrorMode(SEM_NOOPENFILEERRORBOX);
  {$ENDIF}
  try
    FLibHandle := LoadLibrary(RichEdit20ModuleName);
    if (FLibHandle > 0) and (FLibHandle < HINSTANCE_ERROR) then FLibHandle := 0;
    if FLibHandle = 0 then begin
      FLibHandle := LoadLibrary(RichEdit10ModuleName);
      if (FLibHandle > 0) and (FLibHandle < HINSTANCE_ERROR) then FLibHandle := 0;
    end
    else begin
      RichEditVersion := 2;
      {$IFNDEF FPC}
      Ver.dwOSVersionInfoSize := SizeOf(Ver);
      GetVersionEx(Ver);
      with Ver do begin
        if (dwPlatformId = VER_PLATFORM_WIN32_NT) and
          (dwMajorVersion >= 5) then
          RichEditVersion := 3;
      end;
      {$ENDIF}
    end;
  finally
  {$IFNDEF FPC}
    SetErrorMode(OldError);
  {$ENDIF}
  end;
finalization
  if FLibHandle <> 0 then FreeLibrary(FLibHandle);

end.

