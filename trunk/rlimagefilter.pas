unit RLImagefilter;
{$MODE DELPHI}
interface

//{$I frx.inc}

uses
{$ifdef VCL}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ELSE}
  LCLIntf,
  LCLType,
  {$ENDIF}
  Types,
{$endif}
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Jpeg
{$IFDEF Delphi6}
, Variants
{$ENDIF};

type
  TfrxCustomImageExport = class(TfrxCustomExportFilter)
  private
    FBitmap: TBitmap;
    FCrop: Boolean;
    FCurrentPage: Integer;
    FJPEGQuality: Integer;
    FMaxX: Integer;
    FMaxY: Integer;
    FMinX: Integer;
    FMinY: Integer;
    FMonochrome: Boolean;
  protected
    procedure Save; virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    function ShowModal: TModalResult; override;
    function Start: Boolean; override;
    procedure FinishPage(Page: TfrxReportPage; Index: Integer); override;
    procedure StartPage(Page: TfrxReportPage; Index: Integer); override;
    procedure ExportObject(Obj: TfrxComponent); override;

    property JPEGQuality: Integer read FJPEGQuality write FJPEGQuality default 90;
    property CropImages: Boolean read FCrop write FCrop default True;
    property Monochrome: Boolean read FMonochrome write FMonochrome default False;
  end;

  TfrxBMPExport = class(TfrxCustomImageExport)
  protected
    procedure Save; override;
  public
    class function GetDescription: String; override;
  published
    property CropImages;
    property Monochrome;
  end;

  TfrxTIFFExport = class(TfrxCustomImageExport)
  private
    procedure SaveTiffToStream(Stream: TStream; Bitmap: TBitmap);
  protected
    procedure Save; override;
  public
    class function GetDescription: String; override;
  published
    property CropImages;
    property Monochrome;
  end;

  TfrxJPEGExport = class(TfrxCustomImageExport)
  protected
    procedure Save; override;
  public
    class function GetDescription: String; override;
  published
    property JPEGQuality;
    property CropImages;
    property Monochrome;
  end;

  TfrxIMGExportDialog = class(TForm)
    OK: TButton;
    Cancel: TButton;
    GroupPageRange: TGroupBox;
    GroupBox1: TGroupBox;
    CropPage: TCheckBox;
    Label2: TLabel;
    Quality: TEdit;
    Mono: TCheckBox;
    SaveDialog1: TSaveDialog;
    DescrL: TLabel;
    AllRB: TRadioButton;
    CurPageRB: TRadioButton;
    PageNumbersRB: TRadioButton;
    PageNumbersE: TEdit;
    procedure FormCreate(Sender: TObject);
  private
    FFilter: TfrxCustomImageExport;
    procedure SetFilter(const Value: TfrxCustomImageExport);
  public
    property Filter: TfrxCustomImageExport read FFilter write SetFilter;
  end;


implementation

uses frxUtils, frxRes, frxrcExports;

{$R *.dfm}

const
  TifHeader: array[0..7] of Byte = (
    $49, $49, $2A, $00, $08, $00, $00, $00);

  NoOfDirs: array[0..1] of Byte = ($0F, $00);

type
  PDirEntry = ^TDirEntry;
  TDirEntry = record
    _Tag: Word;
    _Type: Word;
    _Count: LongInt;
    _Value: LongInt;
  end;

var
  D_BW: array[0..13] of TDirEntry = (
    (_Tag: $00FE; _Type: $0004; _Count: $00000001; _Value: $00000000),
    (_Tag: $0100; _Type: $0003; _Count: $00000001; _Value: $00000000),
    (_Tag: $0101; _Type: $0003; _Count: $00000001; _Value: $00000000),
    (_Tag: $0102; _Type: $0003; _Count: $00000001; _Value: $00000001),
    (_Tag: $0103; _Type: $0003; _Count: $00000001; _Value: $00000001),
    (_Tag: $0106; _Type: $0003; _Count: $00000001; _Value: $00000001),
    (_Tag: $0111; _Type: $0004; _Count: $00000001; _Value: $00000000),
    (_Tag: $0115; _Type: $0003; _Count: $00000001; _Value: $00000001),
    (_Tag: $0116; _Type: $0004; _Count: $00000001; _Value: $00000000),
    (_Tag: $0117; _Type: $0004; _Count: $00000001; _Value: $00000000),
    (_Tag: $011A; _Type: $0005; _Count: $00000001; _Value: $00000000),
    (_Tag: $011B; _Type: $0005; _Count: $00000001; _Value: $00000000),
    (_Tag: $0128; _Type: $0003; _Count: $00000001; _Value: $00000002),
    (_Tag: $0131; _Type: $0002; _Count: $0000000A; _Value: $00000000));

  D_COL: array[0..14] of TDirEntry = (
    (_Tag: $00FE; _Type: $0004; _Count: $00000001; _Value: $00000000),
    (_Tag: $0100; _Type: $0003; _Count: $00000001; _Value: $00000000),
    (_Tag: $0101; _Type: $0003; _Count: $00000001; _Value: $00000000),
    (_Tag: $0102; _Type: $0003; _Count: $00000001; _Value: $00000008),
    (_Tag: $0103; _Type: $0003; _Count: $00000001; _Value: $00000001),
    (_Tag: $0106; _Type: $0003; _Count: $00000001; _Value: $00000003),
    (_Tag: $0111; _Type: $0004; _Count: $00000001; _Value: $00000000),
    (_Tag: $0115; _Type: $0003; _Count: $00000001; _Value: $00000001),
    (_Tag: $0116; _Type: $0004; _Count: $00000001; _Value: $00000000),
    (_Tag: $0117; _Type: $0004; _Count: $00000001; _Value: $00000000),
    (_Tag: $011A; _Type: $0005; _Count: $00000001; _Value: $00000000),
    (_Tag: $011B; _Type: $0005; _Count: $00000001; _Value: $00000000),
    (_Tag: $0128; _Type: $0003; _Count: $00000001; _Value: $00000002),
    (_Tag: $0131; _Type: $0002; _Count: $0000000A; _Value: $00000000),
    (_Tag: $0140; _Type: $0003; _Count: $00000300; _Value: $00000008));

  D_RGB: array[0..14] of TDirEntry = (
    (_Tag: $00FE; _Type: $0004; _Count: $00000001; _Value: $00000000),
    (_Tag: $0100; _Type: $0003; _Count: $00000001; _Value: $00000000),
    (_Tag: $0101; _Type: $0003; _Count: $00000001; _Value: $00000000),
    (_Tag: $0102; _Type: $0003; _Count: $00000003; _Value: $00000008),
    (_Tag: $0103; _Type: $0003; _Count: $00000001; _Value: $00000001),
    (_Tag: $0106; _Type: $0003; _Count: $00000001; _Value: $00000002),
    (_Tag: $0111; _Type: $0004; _Count: $00000001; _Value: $00000000),
    (_Tag: $0115; _Type: $0003; _Count: $00000001; _Value: $00000003),
    (_Tag: $0116; _Type: $0004; _Count: $00000001; _Value: $00000000),
    (_Tag: $0117; _Type: $0004; _Count: $00000001; _Value: $00000000),
    (_Tag: $011A; _Type: $0005; _Count: $00000001; _Value: $00000000),
    (_Tag: $011B; _Type: $0005; _Count: $00000001; _Value: $00000000),
    (_Tag: $011C; _Type: $0003; _Count: $00000001; _Value: $00000001),
    (_Tag: $0128; _Type: $0003; _Count: $00000001; _Value: $00000002),
    (_Tag: $0131; _Type: $0002; _Count: $0000000A; _Value: $00000000));

  NullString: array[0..3] of Byte = ($00, $00, $00, $00);
  X_Res_Value: array[0..7] of Byte = ($6D, $03, $00, $00, $0A, $00, $00, $00);
  Y_Res_Value: array[0..7] of Byte = ($6D, $03, $00, $00, $0A, $00, $00, $00);
  Software: array[0..9] of Char = ('F', 'a', 's', 't', 'R', 'e', 'p', 'o', 'r', 't');
  BitsPerSample: array[0..2] of Word = ($0008, $0008, $0008);


{ TfrxCustomImageExport }

constructor TfrxCustomImageExport.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCrop := True;
  FJPEGQuality := 90;
end;

function TfrxCustomImageExport.ShowModal: TModalResult;
begin
  with TfrxIMGExportDialog.Create(nil) do
  begin
    Filter := Self;
    Quality.Text := IntToStr(FJPEGQuality);
    CropPage.Checked := FCrop;
    Mono.Checked := FMonochrome;
    Quality.Enabled := Self is TfrxJPEGExport;
    Result := ShowModal;
    if Result = mrOk then
    begin
      FJPEGQuality := StrToInt(Quality.Text);
      FCrop := CropPage.Checked;
      FMonochrome := Mono.Checked;
      PageNumbers := '';
      CurPage := False;
      if CurPageRB.Checked then
        CurPage := True
      else if PageNumbersRB.Checked then
        PageNumbers := PageNumbersE.Text;

      if SaveDialog1.Execute then
        FileName := SaveDialog1.FileName else
        Result := mrCancel;
    end;
    Free;
  end;
end;

function TfrxCustomImageExport.Start: Boolean;
begin
  CurPage := False;
  FCurrentPage := 0;
  Result := FileName <> '';
end;

procedure TfrxCustomImageExport.StartPage(Page: TfrxReportPage; Index: Integer);
begin
  Inc(FCurrentPage);
  FBitmap := TBitmap.Create;
  FBitmap.Canvas.Brush.Color := clWhite;
  FBitmap.Monochrome := Monochrome;
  FBitmap.Width := Round(Page.Width);
  FBitmap.Height := Round(Page.Height);
  FMaxX := 0;
  FMaxY := 0;
  FMinX := Round(Page.Width);
  FMinY := Round(Page.Height);
end;

procedure TfrxCustomImageExport.ExportObject(Obj: TfrxComponent);
begin
  if Obj is TfrxView then
  begin
    if Obj.AbsLeft < FMinX then
      FMinX := Round(Obj.AbsLeft);
    if Obj.AbsTop < FMinY then
      FMinY := Round(Obj.AbsTop);
    if (Obj.AbsLeft + Obj.Width) > FMaxX then
      FMaxX := Round(Obj.AbsLeft + Obj.Width) + 1;
    if (Obj.AbsTop + Obj.Height) > FMaxY then
      FMaxY := Round(Obj.AbsTop + Obj.Height) + 1;
    TfrxView(Obj).Draw(FBitmap.Canvas, 1, 1, 0, 0);
  end;
end;

procedure TfrxCustomImageExport.FinishPage(Page: TfrxReportPage; Index: Integer);
var
  RFrom, RTo: TRect;
begin
  if FCrop then
  begin
    RFrom := Rect(FMinX, FMinY, FMaxX, FMaxY);
    RTo := Rect(0, 0, FMaxX - FMinX, FMaxY - FMinY);
    FBitmap.Canvas.CopyRect(RTo, FBitmap.Canvas, RFrom);
    FBitmap.Width := FMaxX - FMinX;
    FBitmap.Height := FMaxY - FMinY;
  end;
  Save;
  FBitmap.Free;
end;


{ TfrxIMGExportDialog }

procedure TfrxIMGExportDialog.FormCreate(Sender: TObject);
begin
  frxResources.LocalizeForm(Self);
end;

procedure TfrxIMGExportDialog.SetFilter(const Value: TfrxCustomImageExport);
begin
  FFilter := Value;
  if FFilter is TfrxTIFFExport then
  begin
    SaveDialog1.Filter := frxResources.Get('TIFFexportFilter');
    SaveDialog1.DefaultExt := '.tif';
  end
  else if FFilter is TfrxBMPExport then
  begin
    SaveDialog1.Filter := frxResources.Get('BMPexportFilter');
    SaveDialog1.DefaultExt := '.bmp';
  end
  else if FFilter is TfrxJPEGExport then
  begin
    SaveDialog1.Filter := frxResources.Get('JPEGexportFilter');
    SaveDialog1.DefaultExt := '.jpg';
  end;
end;


{ TfrxBMPExport }

class function TfrxBMPExport.GetDescription: String;
begin
  Result := frxResources.Get('BMPexport');
end;

procedure TfrxBMPExport.Save;
begin
  FBitmap.SaveToFile(ChangeFileExt(FileName, '.' + IntToStr(FCurrentPage) + '.bmp'));
end;


{ TfrxTIFFExport }

class function TfrxTIFFExport.GetDescription: String;
begin
  Result := frxResources.Get('TIFFexport');
end;

procedure TfrxTIFFExport.Save;
var
  Stream: TFileStream;
begin
  try
    Stream := TFileStream.Create(ChangeFileExt(FileName, '.' +
      IntToStr(FCurrentPage) + '.tif'), fmCreate);
    try
      SaveTiffToStream(Stream, FBitmap);
    finally
      Stream.Free;
    end;
  except
    on e: Exception do
      if Report.EngineOptions.SilentMode then
        Report.Errors.Add(e.Message)
      else frxErrorMsg(e.Message);
  end;
end;

procedure TfrxTIFFExport.SaveTIFFToStream(Stream: TStream; Bitmap: TBitmap);
var
  BM: HBitmap;
  Header, Bits, BitsPtr, TmpBitsPtr, NewBits: PChar;
  HeaderSize, BitsSize: DWORD;
  Width, Height, DataWidth, BitCount: Integer;
  MapRed, MapGreen, MapBlue: array[0..255, 0..1] of Byte;
  ColTabSize, i, k, BmpWidth: Integer;
  Red, Blue, Green: Char;
  O_XRes, O_YRes, O_Soft, O_Strip, O_Dir, O_BPS: LongInt;
  RGB: Word;
begin
  BM := Bitmap.Handle;
  if BM = 0 then Exit;

  GetDIBSizes(BM, HeaderSize, BitsSize);
  GetMem(Header, HeaderSize + BitsSize);
  try
    Bits := Header + HeaderSize;
    if GetDIB(BM, Bitmap.Palette, Header^, Bits^) then
    begin
      Width := PBITMAPINFO(Header)^.bmiHeader.biWidth;
      Height := PBITMAPINFO(Header)^.bmiHeader.biHeight;
      BitCount := PBITMAPINFO(Header)^.bmiHeader.biBitCount;
      ColTabSize := (1 shl BitCount);
      BmpWidth := Trunc(BitsSize / Height);
      if BitCount = 1 then
      begin
        DataWidth := ((Width + 7) div 8);
        D_BW[1]._Value := LongInt(Width);
        D_BW[2]._Value := LongInt(abs(Height));
        D_BW[8]._Value := LongInt(abs(Height));
        D_BW[9]._Value := LongInt(DataWidth * abs(Height));
        Stream.Write(TifHeader, sizeof(TifHeader));
        O_XRes := Stream.Position;
        Stream.Write(X_Res_Value, sizeof(X_Res_Value));
        O_YRes := Stream.Position;
        Stream.Write(Y_Res_Value, sizeof(Y_Res_Value));
        O_Soft := Stream.Position;
        Stream.Write(Software, sizeof(Software));
        D_BW[6]._Value := 0;
        D_BW[10]._Value := O_XRes;
        D_BW[11]._Value := O_YRes;
        D_BW[13]._Value := O_Soft;
        O_Dir := Stream.Position;
        Stream.Write(NoOfDirs, sizeof(NoOfDirs));
        Stream.Write(D_BW, sizeof(D_BW));
        O_Strip := Stream.Position;
        if Height < 0 then
          for i := 0 to Height - 1 do
          begin
            BitsPtr := Bits + i * BmpWidth;
            Stream.Write(BitsPtr^, DataWidth);
          end
        else
          for i := 1 to Height do
          begin
            BitsPtr := Bits + (Height - i) * BmpWidth;
            Stream.Write(BitsPtr^, DataWidth);
          end;
        Stream.Write(NullString, sizeof(NullString));
        D_BW[6]._Value := O_Strip;
        Stream.Seek(O_Dir, soFromBeginning);
        Stream.Write(NoOfDirs, sizeof(NoOfDirs));
        Stream.Write(D_BW, sizeof(D_BW));
        Stream.Seek(4, soFromBeginning);
        Stream.Write(O_Dir, sizeof(O_Dir));
      end;
      if BitCount in [4, 8] then
      begin
        DataWidth := Width;
        if BitCount = 4 then
        begin
          Width := (Width div BitCount) * BitCount;
          if BitCount = 4 then
            DataWidth := Width div 2;
        end;
        D_COL[1]._Value := LongInt(Width);
        D_COL[2]._Value := LongInt(abs(Height));
        D_COL[3]._Value := LongInt(BitCount);
        D_COL[8]._Value := LongInt(Height);
        D_COL[9]._Value := LongInt(DataWidth * abs(Height));
        for i := 0 to ColTabSize - 1 do
        begin
          MapRed[i][1] := PBITMAPINFO(Header)^.bmiColors[i].rgbRed;
          MapRed[i][0] := 0;
          MapGreen[i][1] := PBITMAPINFO(Header)^.bmiColors[i].rgbGreen;
          MapGreen[i][0] := 0;
          MapBlue[i][1] := PBITMAPINFO(Header)^.bmiColors[i].rgbBlue;
          MapBlue[i][0] := 0;
        end;
        D_COL[14]._Count := LongInt(ColTabSize * 3);
        Stream.Write(TifHeader, sizeof(TifHeader));
        Stream.Write(MapRed, ColTabSize * 2);
        Stream.Write(MapGreen, ColTabSize * 2);
        Stream.Write(MapBlue, ColTabSize * 2);
        O_XRes := Stream.Position;
        Stream.Write(X_Res_Value, sizeof(X_Res_Value));
        O_YRes := Stream.Position;
        Stream.Write(Y_Res_Value, sizeof(Y_Res_Value));
        O_Soft := Stream.Position;
        Stream.Write(Software, sizeof(Software));
        O_Strip := Stream.Position;
        if Height < 0 then
          for i := 0 to Height - 1 do
          begin
            BitsPtr := Bits + i * BmpWidth;
            Stream.Write(BitsPtr^, DataWidth);
          end
        else
          for i := 1 to Height do
          begin
            BitsPtr := Bits + (Height - i) * BmpWidth;
            Stream.Write(BitsPtr^, DataWidth);
          end;
        D_COL[6]._Value := O_Strip;
        D_COL[10]._Value := O_XRes;
        D_COL[11]._Value := O_YRes;
        D_COL[13]._Value := O_Soft;
        O_Dir := Stream.Position;
        Stream.Write(NoOfDirs, sizeof(NoOfDirs));
        Stream.Write(D_COL, sizeof(D_COL));
        Stream.Write(NullString, sizeof(NullString));
        Stream.Seek(4, soFromBeginning);
        Stream.Write(O_Dir, sizeof(O_Dir));
      end;
      if BitCount = 16 then
      begin
        D_RGB[1]._Value := LongInt(Width);
        D_RGB[2]._Value := LongInt(Height);
        D_RGB[8]._Value := LongInt(Height);
        D_RGB[9]._Value := LongInt(3 * Width * Height);
        Stream.Write(TifHeader, sizeof(TifHeader));
        O_XRes := Stream.Position;
        Stream.Write(X_Res_Value, sizeof(X_Res_Value));
        O_YRes := Stream.Position;
        Stream.Write(Y_Res_Value, sizeof(Y_Res_Value));
        O_BPS := Stream.Position;
        Stream.Write(BitsPerSample, sizeof(BitsPerSample));
        O_Soft := Stream.Position;
        Stream.Write(Software, sizeof(Software));
        O_Strip := Stream.Position;
        GetMem(NewBits, Width * Height * 3);
        for i := 0 to Height - 1 do
        begin
          BitsPtr := Bits + i * BmpWidth;
          TmpBitsPtr := NewBits + i * Width * 3;
          for k := 0 to Width - 1 do
          begin
            RGB := PWord(BitsPtr)^;
            Blue := Char((RGB and $1F) shl 3 or $7);
            Green := Char((RGB shr 5 and $1F) shl 3 or $7);
            Red := Char((RGB shr 10 and $1F) shl 3 or $7);
            PByte(TmpBitsPtr)^ := Byte(Red);
            PByte(TmpBitsPtr + 1)^ := Byte(Green);
            PByte(TmpBitsPtr + 2)^ := Byte(Blue);
            BitsPtr := BitsPtr + 2;
            TmpBitsPtr := TmpBitsPtr + 3;
          end;
        end;
        for i := 1 to Height do
        begin
          TmpBitsPtr := NewBits + (Height - i) * Width * 3;
          Stream.Write(TmpBitsPtr^, Width * 3);
        end;
        FreeMem(NewBits);
        D_RGB[3]._Value := O_BPS;
        D_RGB[6]._Value := O_Strip;
        D_RGB[10]._Value := O_XRes;
        D_RGB[11]._Value := O_YRes;
        D_RGB[14]._Value := O_Soft;
        O_Dir := Stream.Position;
        Stream.Write(NoOfDirs, sizeof(NoOfDirs));
        Stream.Write(D_RGB, sizeof(D_RGB));
        Stream.Write(NullString, sizeof(NullString));
        Stream.Seek(4, soFromBeginning);
        Stream.Write(O_Dir, sizeof(O_Dir));
      end;
      if BitCount in [24, 32] then
      begin
        D_RGB[1]._Value := LongInt(Width);
        D_RGB[2]._Value := LongInt(Height);
        D_RGB[8]._Value := LongInt(Height);
        D_RGB[9]._Value := LongInt(3 * Width * Height);
        Stream.Write(TifHeader, sizeof(TifHeader));
        O_XRes := Stream.Position;
        Stream.Write(X_Res_Value, sizeof(X_Res_Value));
        O_YRes := Stream.Position;
        Stream.Write(Y_Res_Value, sizeof(Y_Res_Value));
        O_BPS := Stream.Position;
        Stream.Write(BitsPerSample, sizeof(BitsPerSample));
        O_Soft := Stream.Position;
        Stream.Write(Software, sizeof(Software));
        O_Strip := Stream.Position;
        for i := 0 to Height - 1 do
        begin
          BitsPtr := Bits + i * BmpWidth;
          for k := 0 to Width - 1 do
          begin
            Blue := (BitsPtr)^;
            Red := (BitsPtr + 2)^;
            (BitsPtr)^ := Red;
            (BitsPtr + 2)^ := Blue;
            BitsPtr := BitsPtr + BitCount div 8;
          end;
        end;
        if BitCount = 32 then
          for i := 0 to Height - 1 do
          begin
            BitsPtr := Bits + i * BmpWidth;
            TmpBitsPtr := BitsPtr;
            for k := 0 to Width - 1 do
            begin
              (TmpBitsPtr)^ := (BitsPtr)^;
              (TmpBitsPtr + 1)^ := (BitsPtr + 1)^;
              (TmpBitsPtr + 2)^ := (BitsPtr + 2)^;
              TmpBitsPtr := TmpBitsPtr + 3;
              BitsPtr := BitsPtr + 4;
            end;
          end;
        BmpWidth := Trunc(BitsSize / Height);
        if Height < 0 then
          for i := 0 to Height - 1 do
          begin
            BitsPtr := Bits + i * BmpWidth;
            Stream.Write(BitsPtr^, Width * 3);
          end
        else
          for i := 1 to Height do
          begin
            BitsPtr := Bits + (Height - i) * BmpWidth;
            Stream.Write(BitsPtr^, Width * 3);
          end;
        D_RGB[3]._Value := O_BPS;
        D_RGB[6]._Value := O_Strip;
        D_RGB[10]._Value := O_XRes;
        D_RGB[11]._Value := O_YRes;
        D_RGB[14]._Value := O_Soft;
        O_Dir := Stream.Position;
        Stream.Write(NoOfDirs, sizeof(NoOfDirs));
        Stream.Write(D_RGB, sizeof(D_RGB));
        Stream.Write(NullString, sizeof(NullString));
        Stream.Seek(4, soFromBeginning);
        Stream.Write(O_Dir, sizeof(O_Dir));
      end;
    end;
  finally
    FreeMem(Header);
  end;
end;


{ TfrxJPEGExport }

class function TfrxJPEGExport.GetDescription: String;
begin
  Result := frxResources.Get('JPEGexport');
end;

procedure TfrxJPEGExport.Save;
var
  Image: TJPEGImage;
  Stream: TFileStream;
begin
  try
    Stream := TFileStream.Create(ChangeFileExt(FileName, '.' +
      IntToStr(FCurrentPage) + '.jpg'), fmCreate);
    try
      Image := TJPEGImage.Create;
      try
        Image.CompressionQuality := FJPEGQuality;
        Image.Assign(FBitmap);
        Image.SaveToStream(Stream);
      finally
        Image.Free;
      end;
    finally
      Stream.Free;
    end;
  except
    on e: Exception do
      if Report.EngineOptions.SilentMode then
        Report.Errors.Add(e.Message)
      else frxErrorMsg(e.Message);
  end;
end;

end.

