{ ****************************************************************************** }
{ * memory Rasterization JPEG support                                          * }
{ * by QQ 600585@qq.com                                                        * }
{ ****************************************************************************** }
{ * https://zpascal.net                                                        * }
{ * https://github.com/PassByYou888/zAI                                        * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/PascalString                               * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zChinese                                   * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/FFMPEG-Header                              * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/InfiniteIoT                                * }
{ * https://github.com/PassByYou888/FastMD5                                    * }
{ ****************************************************************************** }
unit Raster_JPEG_Image;

{$INCLUDE zDefine.inc}

interface

uses Classes, SysUtils,
  CoreClasses, MemoryStream64, PascalStrings,
  Raster_JPEG_type,
  Raster_JPEG_Marker,
  Raster_JPEG_MapIterator,
  Raster_JPEG_DICT,
  Raster_JPEG_BitStream,
  Raster_JPEG_Huffman,
  Raster_JPEG_Lossless,
  Raster_JPEG_ColorTransforms,
  Raster_JPEG_Coder;

const
  // Version number changes with updates. See "versions.txt" for a list of
  // updated features.
  cNativeJpgVersion = '1.4, update byQQ600585';

type
  TJpegSaveOptions = class;

  TJpegLoadOption = (
    loOnlyMetadata, // If set, only meta-data is read (exits when SOS is encountered)
    loTileMode      // If set, the loadfromstream only finds the start of each MCU tile
    );
  TJpegLoadOptions = set of TJpegLoadOption;

  // Ask the application to create a map (usually a TBitmap in Win) based on Iterator_:
  // width, height and cellstride (bytecount per pixel). Iterator_ must also be
  // updated by the application: Iterator_.Map and Iterator_.ScanStride (bytecount per scanline).
  TCreateMapEvent = function(var Iterator_: TMapIterator): TObject of object;

  // provide the jpeg strip by strip
  TJpegProvideStripEvent = procedure(Sender: TObject; Left_, Top_: integer; ABitmapIter: TMapIterator) of object;

  // TJpegImage is a non-VCL component that can be used to load and save
  // Jpeg files. It is best to create just one instance of TJpegImage, and
  // use it over and over again to load Jpeg files, because this way memory
  // for the coefficients and bitmaps will be reused, instead of repeatedly
  // allocated/deallocated (which would happen if you each time create a new
  // TJpegImage instance).
  // Use the LoadFromFile or LoadFromStream method to load a Jpeg image, and use
  // the SaveToFile and SaveToStream method to save a Jpeg image. Use the Bitmap
  // property to assign the bitmap to another bitmap, or to work with the actual
  // image. The StoredColors and BitmapColors properties provide information and
  // control over which colour spaces and conversions are used. The Lossless property
  // gives access to a TLosslessOperation class with which you can perform
  // lossless operations on the Jpeg. The SaveOptions property gives access to
  // options used when saving the Jpeg.
  TJpegImage = class(TJPEG_Base_Object)
  private
    // FOnDebugOut: TDebugEvent; this is already defined in TJPEG_Base_Object
    FDataSize: int64;
    FOnUpdate: TUpdateEvent;
    FCoder: TJpegCoder;
    FMarkers: TJpegMarkerList;

    // Jpeg coding info
    FJpegInfo: TJpegInfo;

    // coder stream
    FCoderStream: TMemoryStream64;
    FLoadOptions: TJpegLoadOptions;
    FLoadScale: TJpegScale;
    FMapIterator: TMapIterator;
    FMapWidth: integer;
    FMapHeight: integer;
    FPixelFormat: TPixelFormat;
    FICCProfile: TJpegICCProfile;
    FStoredCS: TJpegColorSpace;
    FBitmapCS: TJpegColorSpace;
    FDCTCodingMethod: TJpegDCTCodingMethod;
    FLossless: TLosslessOperation;
    FSaveOptions: TJpegSaveOptions;
    FOnCreateMap: TCreateMapEvent;
    FOnProvideStrip: TJpegProvideStripEvent;
    FOnExternalCMS: TJpegExternalCMSEvent;
    function GetExifInfo: TEXIFMarker;
    function GetIptcInfo: TIPTCMarker;
    function GetJfifInfo: TJFIFMarker;
    function GetAdobeAPP14Info: TAdobeApp14Marker;
    function GetICCProfile: TJpegICCProfile;
    procedure SetICCProfile(const Value: TJpegICCProfile);
    function GetLossless: TLosslessOperation;
    function GetComment: TPascalString;
    procedure SetComment(const Value: TPascalString);
    function GetHeight: integer;
    function GetImageHeight: integer;
    function GetImageWidth: integer;
    function GetWidth: integer;
    procedure GetBitmapTileSize(Scale_: TJpegScale; var Width_, Height_: integer);
    procedure BeforeLosslessUpdate(Sender: TObject);
    procedure AfterLosslessUpdate(Sender: TObject);
    procedure AVI1MarkerCheck;
  protected
    procedure EntropyDecodeSkip(S: TMemoryStream64);
    procedure InitializeDecode;
    // Load the next marker; returns the markertag read (or mkNone if not found)
    function LoadMarker(S: TMemoryStream64): byte;
    // Get the required color transform from bitmap to samples, based on detected
    // color space in file and bitmap
    procedure GetColorTransformFromBitmap(var Class_: TColorTransformClass);
    // Get the required color transform from samples to bitmap, based on detected
    // color space in file and bitmap
    procedure GetColorTransformToBitmap(var Class_: TColorTransformClass; var Format_: TPixelFormat);
    function HasSamples: boolean;
    function HasCoefficients: boolean;
    function VerifyBitmapColorSpaceForSave: TJpegColorSpace;
    procedure AddMinimalMarkersForColorSpaceDetection(Colors_: TJpegColorSpace);
  public
    constructor Create(Owner_: TJPEG_Base_Object); override;
    destructor Destroy; override;
    // Clear the jpeg PFormat: all data (coder and markers) and the bitmap
    procedure Clear;
    // Save info in the bitmap iterator to the Jpeg stream, aka compress the image.
    procedure Compress(Iterator_: TMapIterator);
    // Get the size of the bitmap that must be created to hold the decoded
    // information
    procedure GetBitmapSize(Scale_: TJpegScale; var Width_, Height_: integer);

    // After the image is loaded from stream, LoadJpeg actually decodes the
    // image. If DoCreateBitmap is true, it creates and renders the bitmap, thru
    // OnCreateMap
    procedure LoadJpeg(Scale_: TJpegScale; DoCreateBitmap: boolean);

    // Load a Jpeg image from the file FileName_.
    procedure LoadFromFile(const FileName_: string);

    // Load a Jpeg image from the stream. It is best to use a TMemoryStream64
    // because the bitreader (which causes most reads to the stream) has
    // a specially optimized version to read from TMemoryStream64 streams. The
    // stream will be read from from Steam.Position, and if everything goes well,
    // the stream will be positioned directly after the last (EOI) marker. An
    // exception will be raised if the stream is corrupt or truncated.
    procedure LoadFromStream(Stream: TMemoryStream64);

    // In case of LoadOption [loTileMode] is included, after the LoadFromStream,
    // individual tile blocks can be loaded which will be put in the resulting
    // bitmap. The tile loaded will contain all the MCU blocks that fall within
    // the specified bounds Left_/Top_/Right_/Bottom_. Note that these are var
    // parameters, after calling this procedure they will be updated to the MCU
    // block borders. Left_/Top_ can subsequently be used to draw the resulting
    // TJpegFormat.Bitmap to a canvas.
    procedure LoadTileBlock(var Left_, Top_, Right_, Bottom_: integer);
    // Reload
    procedure Reload;
    // After SaveJpeg, the original CoderStream is replaced by the new encoded stream
    // by the coder.
    procedure SaveJpeg;
    // Save the Jpeg image to file FileName_
    procedure SaveToFile(const FileName_: string);
    // Save the Jpeg image to stream Set SaveOptions before saving. Use
    // SaveBitmap first in order to encode the new bitmap to be saved.
    procedure SaveToStream(Stream: TMemoryStream64);
    // Encode a Jpeg image strip by strip. OnCreateMap with the partial
    // bitmap needs to be provided to fill one strip of the
    // jpeg file at a time (a strip consists of a row of MCU blocks, usually
    // 8 or 16 pixels high).
    // Only one pass over the data is required, but the resulting jpeg will
    // be saved with standard Huffman tables (as provided in the Jpeg specification)
    procedure SaveBitmapStripByStrip(Width_, Height_: integer);
    // call UpdateBitmap after calling LoadJpeg(Scale, False)
    function UpdateBitmap: TObject;
    // All metadata markers will be extracted from the file, and put in List_.
    // List_ must be initialized (List_ := TJpegMarkerList.Create)
    procedure ExtractMetadata(List_: TJpegMarkerList);
    // Inject the metadata in List_ into the marker list of the file. All existing
    // metadata will be removed first; then the markers in List_ will be added
    // below the SOI marker.
    procedure InjectMetadata(List_: TJpegMarkerList);
    // does the jpeg PFormat have a bitmap already?
    function HasBitmap: boolean;
    // Returns true if the Jpeg has a valid ICC profile. Use property ICCProfile
    // to actually get it.
    function HasICCProfile: boolean;
    // Call this function to detect what colorspace is used in the file. This
    // can only be detected *after* the file is loaded. Set LoadOptions to
    // [loOnlyMetadata] to quickly do a test load to use this function.
    function DetectInternalColorSpace: TJpegColorSpace;
    // Reference to low-level information of file. JpegInfo.Width/Height provides
    // the size of the coded image.
    property JpegInfo: TJpegInfo read FJpegInfo;
    // Reference to the Jpeg coder. TJpegCoder is a generic coder, and specific
    // implementations are present for baseline DCT and progressive DCT.
    property Coder: TJpegCoder read FCoder;
    // size in bytes of raw data
    property DataSize: int64 read FDataSize;
    // Reference to the list of low-level markers present in the file (valid after
    // loading).
    property Markers: TJpegMarkerList read FMarkers;
    // Perform lossless operations on the jpeg coefficients
    property Lossless: TLosslessOperation read GetLossless;

    // Pointer to JFIF info marker (if any)
    property JfifInfo: TJFIFMarker read GetJfifInfo;
    // Pointer to EXIF info marker (if any)
    property ExifInfo: TEXIFMarker read GetExifInfo;
    // Pointer to IPTC info marker (if any)
    property IptcInfo: TIPTCMarker read GetIptcInfo;
    // Pointer to Adobe APP14 info marker (if any)
    property AdobeAPP14Info: TAdobeApp14Marker read GetAdobeAPP14Info;
    // Read ICCProfile to get a TJpegICCProfile object back, in case ICC profile
    // data is available in the markers. The object has a Data pointer and a
    // DataLength property, and it can be used with e.g. LittleCMS. The profile
    // is valid until you load a new file or free the jpeg component.
    property ICCProfile: TJpegICCProfile read GetICCProfile write SetICCProfile;
    // Read and write a Jpeg comment (implemented through the COM marker).
    property Comment: TPascalString read GetComment write SetComment;
    // Read Bitmap to get a pointer to a TBitmap object back, that has the currently
    // loaded image. The TBitmap object is no longer valid when the jpeg PFormat class is freed.
    // Assign to Bitmap in order to save the bitmap to a jpeg file. Note: when
    // assigning a new bitmap, the metadata is destroyed. In order to preserve
    // metadata, use a construct like this:
    // <code>
    // List := TJpegMarkerList.Create;
    // Jpg.ExtractMetadata(List);
    // Jpg.Bitmap := MyBitmap;
    // Jpg.InjectMetadata(List);
    // Jpg.SaveToFile('test.jpg');
    // List.Free;
    // </code>
    property ImageWidth: integer read GetImageWidth;
    property ImageHeight: integer read GetImageHeight;
    property Width: integer read GetWidth;
    property Height: integer read GetHeight;
    // Include loOnlyMetadata if you want to only load metadata and not decode
    // the image. Include loTileMode to load the image first, without decoding
    // it directly, so that LoadTileBlock can be used to load tiles of the image.
    property LoadOptions: TJpegLoadOptions read FLoadOptions write FLoadOptions;
    // Set LoadScale to anything other than jsFull to load a downscaled image, which
    // will be faster than the full image. jsDiv2 will download an image that is
    // half the size in X and Y, jsDiv4 will be quarter size, jsDiv8 will be 1/8
    // size.
    property LoadScale: TJpegScale read FLoadScale write FLoadScale;
    // The colorspace present in the file. If jcAutoDetect (default), the software will
    // try to detect which colorspace the JPEG file contains. This info is present
    // in some markers, or in the frame definition. If set to another value,
    // the software will use this as given, and assume that the data in the file
    // is using the given colorspace.
    property StoredCS: TJpegColorSpace read FStoredCS write FStoredCS;
    // The colorspace that will be generated when outputting to a bitmap. If set to
    // jcAutoDetect, the stored color space present in the file will be used to
    // directly output the data without color conversion. Default value for
    // BitmapCS is jcRGB.
    property BitmapCS: TJpegColorSpace read FBitmapCS write FBitmapCS;
    // Method used for forward and inverse DCT transform. dmAccurate will use an
    // accurate integer method (slow), dmFast will use fast but less accurate
    // integer method. When reading a Jpeg, this only impacts the visual quality.
    // When writing Jpeg, the resulting file quality will be impacted by the
    // method chosen.
    property DCTCodingMethod: TJpegDCTCodingMethod read FDCTCodingMethod write FDCTCodingMethod;
    // Options that influence how Jpeg images are compressed and saved.
    property SaveOptions: TJpegSaveOptions read FSaveOptions;
{$IFDEF JPEG_Debug}
    // Connect to this event to get low-level textual debug information
    property OnDebugOut: TDebugEvent read FOnDebugOut write FOnDebugOut;
{$ENDIF JPEG_Debug}
    // Connect to this event before calling SaveToStreamStripByStrip. The implementation
    // of this event should fill the passed ABitmap parameter with the part of the
    // image at Left_/Top_ position.
    property OnProvideStrip: TJpegProvideStripEvent read FOnProvideStrip write FOnProvideStrip;
    // Connect to this event to use an external color management system (e.g. apply an
    // ICC profile to an image, with an external library, like LittleCMS). If this event is
    // implemented, no internal color transforms are applied.
    property OnExternalCMS: TJpegExternalCMSEvent read FOnExternalCMS write FOnExternalCMS;
    // connect to this event to get updates from the jpeg PFormat
    property OnUpdate: TUpdateEvent read FOnUpdate write FOnUpdate;
    // event that creates the bitmap for the jpeg image
    property OnCreateMap: TCreateMapEvent read FOnCreateMap write FOnCreateMap;
  end;

  TJpegSaveOptions = class(TCoreClassObject)
  private
    FOwner: TJpegImage;
    FQuality: TJpegQuality;
    FOptimizeHuffmanTables: boolean;
    FCodingMethod: TJpegEncodingMethod;
    FUseSubSampling: boolean;
  protected
    procedure AddMarkers(Stored_: TJpegColorSpace; Width_, Height_: integer);
    procedure SetupDefaultHuffmanTables; virtual;
    procedure SetupQuantTables; virtual;
    procedure SetTableMultiplication(Table_: TQuantizationTable; MultiplyPercent: integer; const DefaultTable_: TIntArray64);
  public
    constructor Create(Owner_: TJpegImage);
    property Quality: TJpegQuality read FQuality write FQuality;
    property OptimizeHuffmanTables: boolean read FOptimizeHuffmanTables write FOptimizeHuffmanTables;
    property CodingMethod: TJpegEncodingMethod read FCodingMethod write FCodingMethod;
    property UseSubSampling: boolean read FUseSubSampling write FUseSubSampling;
  end;

implementation

uses Math;

function TJpegImage.GetExifInfo: TEXIFMarker;
begin
  Result := TEXIFMarker(FMarkers.ByTag(mkApp1));
end;

function TJpegImage.GetIptcInfo: TIPTCMarker;
begin
  Result := TIPTCMarker(FMarkers.ByTag(mkApp13));
end;

function TJpegImage.GetJfifInfo: TJFIFMarker;
begin
  Result := TJFIFMarker(FMarkers.ByTag(mkAPP0));
end;

function TJpegImage.GetAdobeAPP14Info: TAdobeApp14Marker;
begin
  Result := TAdobeApp14Marker(FMarkers.ByTag(mkAPP14));
end;

function TJpegImage.GetICCProfile: TJpegICCProfile;
// return the ICC profile from the ICCProfile markers
var
  M: TICCProfileMarker;
begin
  if Assigned(FICCProfile) then
    begin
      Result := FICCProfile;
      exit;
    end;

  // Do we have an ICC profile?
  Result := nil;
  M := TICCProfileMarker(FMarkers.ByClass(TICCProfileMarker));
  if not Assigned(M) or not M.IsValid then
      exit;

  FICCProfile := TJpegICCProfile.Create;
  FICCProfile.ReadFromMarkerList(FMarkers);
  Result := FICCProfile;
end;

procedure TJpegImage.SetICCProfile(const Value: TJpegICCProfile);
begin
  DisposeObjectAndNil(FICCProfile);
  FMarkers.RemoveMarkers([mkApp2]);
  if Assigned(Value) then
      Value.WriteToMarkerList(FMarkers);
end;

function TJpegImage.GetLossless: TLosslessOperation;
begin
  if not Assigned(FLossless) then
    begin
      FLossless := TLosslessOperation.Create(Self);
      FLossless.OnBeforeLossless := {$IFDEF FPC}@{$ENDIF FPC}BeforeLosslessUpdate;
      FLossless.OnAfterLossless := {$IFDEF FPC}@{$ENDIF FPC}AfterLosslessUpdate;
    end;
  Result := FLossless;
end;

function TJpegImage.GetComment: TPascalString;
var
  M: TCOMMarker;
begin
  M := TCOMMarker(FMarkers.ByTag(mkCOM));
  if not Assigned(M) then
      Result := ''
  else
      Result := M.Comment;
end;

procedure TJpegImage.SetComment(const Value: TPascalString);
var
  M: TCOMMarker;
begin
  M := TCOMMarker(FMarkers.ByTag(mkCOM));
  if not Assigned(M) then
    begin
      // We do not yet have a marker
      if not FMarkers.HasMarker([mkSOI]) then
          raise EInvalidImage.Create(sCommentCannotBeSet);
      // Create the marker and insert after SOI or JFIF marker (whichever comes last)
      M := TCOMMarker.Create(FJpegInfo, mkCOM);
      FMarkers.InsertAfter([mkSOI, mkAPP0], M);
    end;
  M.Comment := Value;
end;

function TJpegImage.GetHeight: integer;
var
  D: integer;
begin
  D := sdGetDivisor(FLoadScale);
  Result := (GetImageHeight + D - 1) div D;
end;

function TJpegImage.GetImageHeight: integer;
begin
  if Assigned(FJpegInfo) then
      Result := FJpegInfo.FHeight
  else
      Result := 0;
end;

function TJpegImage.GetImageWidth: integer;
begin
  if Assigned(FJpegInfo) then
      Result := FJpegInfo.FWidth
  else
      Result := 0;
end;

function TJpegImage.GetWidth: integer;
var
  D: integer;
begin
  D := sdGetDivisor(FLoadScale);
  Result := (GetImageWidth + D - 1) div D;
end;

procedure TJpegImage.GetBitmapTileSize(Scale_: TJpegScale; var Width_, Height_: integer);
var
  W, H, Divisor: integer;
begin
  W := FJpegInfo.FTileWidth;
  H := FJpegInfo.FTileHeight;
  Divisor := sdGetDivisor(Scale_);
  Width_ := (W + Divisor - 1) div Divisor;
  Height_ := (H + Divisor - 1) div Divisor;
end;

procedure TJpegImage.BeforeLosslessUpdate(Sender: TObject);
begin
  // the loadfromfile does not decode the jpeg, so use LoadJpeg
  // with option DoCreateMap = False, if not yet done
  if not HasCoefficients then
      LoadJpeg(jsFull, False);
end;

procedure TJpegImage.AfterLosslessUpdate(Sender: TObject);
begin
  // after a lossless operation, SaveJpeg must be called
  // so that the huffman tables are recreated.
  SaveJpeg;
  Reload;
  LoadJpeg(jsFull, False);

  // call OnUpdate for the application
  if Assigned(FOnUpdate) then
      FOnUpdate(Sender);
end;

procedure TJpegImage.AVI1MarkerCheck;
// check if this jpeg is part of ab AVI file
var
  i: integer;
  DHTExists: boolean;
  AVI1Exists: boolean;
  DHTMarker: TDHTMarker;
  MS: TMemoryStream64;
begin
  // Added by Dec start
  DHTExists := False;
  AVI1Exists := False;
  for i := 0 to FMarkers.Count - 1 do
    begin
      DHTExists := DHTExists or (FMarkers[i] is TDHTMarker);
      AVI1Exists := AVI1Exists or (FMarkers[i] is TAVI1Marker);
    end;
  if (not DHTExists) and AVI1Exists then
    begin
      DHTMarker := TDHTMarker.Create(FJpegInfo, mkDHT);
      FMarkers.Insert(FMarkers.Count - 1, DHTMarker);
      MS := TMemoryStream64.Create;
      try
        MS.SetPointerWithProtectedMode(@cMjpgDHTSeg, SizeOf(cMjpgDHTSeg));
        DHTMarker.LoadFromStream(MS, MS.Size);
      finally
          MS.Free;
      end;
    end;
  // Added by Dec end
end;

procedure TJpegImage.EntropyDecodeSkip(S: TMemoryStream64);
// In case we want to skip the entropy-encoded stream, but inspect markers
var
  B, ReadBytes: byte;
  First, Last, P: PByte;
begin
  // Fast skip based on memorystream
  First := S.Memory;
  Last := First;
  inc(Last, S.Size);
  P := First;
  inc(P, S.Position);
  while cardinal(P) < cardinal(Last) do
    begin
      // Scan stream for $FF + <marker>
      if P^ = $FF then
        begin
          inc(P);
          if P^ <> 0 then
            begin
              dec(P, 1);
              S.Position := cardinal(P) - cardinal(First);
              exit;
            end;
        end;
      inc(P);
    end;
end;

procedure TJpegImage.InitializeDecode;
begin
  // Create correct codec
  case FJpegInfo.FEncodingMethod of
    emBaselineDCT:
      begin
        if not Assigned(FCoder) or (FCoder.ClassType <> TJpegBaselineCoder) then
          begin
            DisposeObject(FCoder);
            FCoder := TJpegBaselineCoder.Create(Self, FJpegInfo);
          end;
      end;
    emExtendedDCT:
      begin
        if not Assigned(FCoder) or (FCoder.ClassType <> TJpegExtendedCoder) then
          begin
            DisposeObject(FCoder);
            FCoder := TJpegExtendedCoder.Create(Self, FJpegInfo);
          end;
      end;
    emProgressiveDCT:
      begin
        if not Assigned(FCoder) or (FCoder.ClassType <> TJpegProgressiveCoder) then
          begin
            DisposeObject(FCoder);
            FCoder := TJpegProgressiveCoder.Create(Self, FJpegInfo);
          end;
      end;
    else
      begin
        DisposeObject(FCoder);
        FCoder := nil;
        exit;
      end;
  end;
  FCoder.Clear;
  FCoder.Method := FDCTCodingMethod;
  FCoder.TileMode := loTileMode in FLoadOptions;
  FCoder.Initialize(FLoadScale);
end;

function TJpegImage.LoadMarker(S: TMemoryStream64): byte;
var
  B, MarkerTag, BytesRead: byte;
  Marker: TJpegMarker;
  JpegMarkerClass: TJpegMarkerClass;
  Size: word;
  StreamPos: integer;
begin
  // default is no marker
  Result := mkNone;

  // Read markers from the stream, until a non $FF is encountered
{$IFDEF JPEG_Debug}
  DoDebugOut(Self, wsInfo, PFormat('Current Pos: %.6d', [S.Position]));
{$ENDIF JPEG_Debug}
  BytesRead := S.Read(B, 1);
  if BytesRead = 0 then
    begin
{$IFDEF JPEG_Debug}
      DoDebugOut(Self, wsWarn, sMarkerExpected);
{$ENDIF JPEG_Debug}
      exit;
    end;

  // Do we have a marker?
  if B = $FF then
    begin
      // Which marker?
      S.Read(MarkerTag, 1);
      while MarkerTag = $FF do
        begin
          MarkerTag := mkNone;
{$IFDEF JPEG_Debug}
          DoDebugOut(Self, wsWarn, PFormat('Error: duplicate $FF encountered at %.6d', [S.Position - 1]));
{$ENDIF JPEG_Debug}
          S.Read(MarkerTag, 1);
        end;

      case MarkerTag of
        mkAPP0 .. mkAPP15:
          begin
            JpegMarkerClass := FindJpegMarkerClassList(MarkerTag, S);
            if JpegMarkerClass = nil then
                JpegMarkerClass := TAPPnMarker;
            Marker := JpegMarkerClass.Create(FJpegInfo, MarkerTag);
          end;
        mkDHT: Marker := TDHTMarker.Create(FJpegInfo, MarkerTag);
        mkDQT: Marker := TDQTMarker.Create(FJpegInfo, MarkerTag);
        mkDRI: Marker := TDRIMarker.Create(FJpegInfo, MarkerTag);
        mkSOF0 .. mkSOF3, mkSOF5 .. mkSOF7, mkSOF9 .. mkSOF11, mkSOF13 .. mkSOF15: Marker := TSOFnMarker.Create(FJpegInfo, MarkerTag);
        mkSOS: Marker := TSOSMarker.Create(FJpegInfo, MarkerTag);
        mkSOI: Marker := TSOIMarker.Create(FJpegInfo, MarkerTag);
        mkEOI: Marker := TEOIMarker.Create(FJpegInfo, MarkerTag);
        mkRST0 .. mkRST7: Marker := TRSTMarker.Create(FJpegInfo, MarkerTag);
        mkCOM: Marker := TCOMMarker.Create(FJpegInfo, MarkerTag);
        mkDNL: Marker := TDNLMarker.Create(FJpegInfo, MarkerTag);
        else
          // General marker
          Marker := TJpegMarker.Create(FJpegInfo, MarkerTag);
      end;

      // Add marker to our list
      FMarkers.Add(Marker);
      Marker.Owner := Self;

      if MarkerTag in [mkAPP0 .. mkAPP15, mkDHT, mkDQT, mkDRI,
        mkSOF0, mkSOF1, mkSOF2, mkSOF3, mkSOF5, mkSOF6, mkSOF7, mkSOF9, mkSOF10, mkSOF11, mkSOF13, mkSOF14, mkSOF15,
        mkSOS, mkCOM, mkDNL] then
        begin
          // Read length of marker
          Size := TJpegMarker.GetWord(S) - 2;
        end
      else
          Size := 0;

      StreamPos := S.Position;

      // Load the marker payload
      Marker.LoadFromStream(S, Size);

      // The SOS marker indicates start of entropy coding (start of scan),
      // EOI indicates end of image. SOF0 and SOF2 indicate
      // baseline and progressive starts, we do not use Marker.LoadFromStream for these.
      if not(MarkerTag in [mkSOF0 .. mkSOF2, mkSOS, mkRST0 .. mkRST7, mkEOI]) then
        begin

          // Find correct stream position
          S.Position := StreamPos + Size;
        end;

    end
  else
    begin

      // B <> $FF is an error, we try to be flexible
{$IFDEF JPEG_Debug}
      DoDebugOut(Self, wsWarn, PFormat('Error: marker expected at %.6d', [S.Position - 1]));
{$ENDIF JPEG_Debug}
      repeat
          BytesRead := S.Read(B, 1);
      until (BytesRead = 0) or (B = $FF);
      if BytesRead = 0 then
          raise EInvalidImage.Create(sMarkerExpected);
      S.Seek(-1, TSeekOrigin.soCurrent);
{$IFDEF JPEG_Debug}
      DoDebugOut(Self, wsHint, PFormat('Resuming at %.6d', [S.Position]));
{$ENDIF JPEG_Debug}
    end;
  Result := MarkerTag;
end;

procedure TJpegImage.GetColorTransformFromBitmap(var Class_: TColorTransformClass);
var
  InternalCS, Input: TJpegColorSpace;
begin
  // At this point we can use DetectInternalColorSpace to find the color space of
  // the file, since all parameters and markes have been set. We can also trust
  // the FBitmapColorspace to match with the bitmap.

{$IFDEF JPEG_Debug}
  DoDebugOut(Self, wsInfo, 'Color conversion bitmap->samples:');
{$ENDIF JPEG_Debug}
  if FStoredCS = jcAutoDetect then
    begin
      InternalCS := DetectInternalColorSpace;
{$IFDEF JPEG_Debug}
      DoDebugOut(Self, wsInfo, PFormat(' Internal colorsp: %s (detected)', [cColorSpaceNames[InternalCS]]));
{$ENDIF JPEG_Debug}
    end
  else
    begin
      InternalCS := FStoredCS;
{$IFDEF JPEG_Debug}
      DoDebugOut(Self, wsInfo, PFormat(' Internal colorsp: %s (selected)', [cColorSpaceNames[InternalCS]]));
{$ENDIF JPEG_Debug}
    end;

  Input := FBitmapCS;
  if Input = jcAutoDetect then
    begin
      case FPixelFormat of
        spf8bit: Input := jcGray;
        spf24bit: Input := jcRGB;
        spf32bit: Input := jcRGBA;
      end;
    end;

  // Defaults
  Class_ := nil;
  case FJpegInfo.FFrameCount of
    1: if FPixelFormat = spf8bit then
          Class_ := TNullTransform8bit;
    2: if FPixelFormat = spf16bit then
          Class_ := TNullTransform16bit;
    3: if FPixelFormat = spf24bit then
          Class_ := TNullTransform24bit;
    4: if FPixelFormat = spf32bit then
          Class_ := TNullTransform32bit;
    else
      begin
{$IFDEF JPEG_Debug}
        DoDebugOut(Self, wsWarn, 'FCodingInfo.FrameCount = 0');
{$ENDIF JPEG_Debug}
      end;
  end;

  // Specific transforms
  case InternalCS of
    jcGray:
      case Input of
        jcRGB: Class_ := TTransformBGRToGray;
        jcRGBA: Class_ := TTransformBGRAToGray;
      end;
    jcGrayA:
      case Input of
        jcRGB: Class_ := TTransformBGRToGrayA;
        jcRGBA: Class_ := TTransformBGRAToGrayA;
      end;
    jcRGB:
      case Input of
        jcRGB: Class_ := TTransformInvertTriplet24bit;
        jcRGBA: Class_ := TTransformRGBAToBGR;
      end;
    jcYCbCr, jcPhotoYCC:
      case Input of
        jcRGB: Class_ := TTransformBGRToYCbCr;
        jcRGBA: Class_ := TTransformBGRAToYCbCr;
      end;
    jcYCbCrA:
      case Input of
        jcRGBA: Class_ := TTransformBGRAToYCbCrA;
      end;
  end;
end;

procedure TJpegImage.GetColorTransformToBitmap(var Class_: TColorTransformClass; var Format_: TPixelFormat);
var
  Warning: boolean;
  InternalCS, OutputCS: TJpegColorSpace;
  // helper
  procedure SetClassAndFormat(C: TColorTransformClass; F: TPixelFormat);
  begin
    Class_ := C;
    Format_ := F;
  end;

begin
  // default class and pixelformat
  case FJpegInfo.FFrameCount of
    1: SetClassAndFormat(TNullTransform8bit, spf8bit);
    2: SetClassAndFormat(TNullTransform16bit, spf16bit);
    3: SetClassAndFormat(TNullTransform24bit, spf24bit);
    4: SetClassAndFormat(TNullTransform32bit, spf32bit);
    else
      begin
{$IFDEF JPEG_Debug}
        DoDebugOut(Self, wsWarn, 'FCodingInfo.FrameCount = 0');
{$ENDIF JPEG_Debug}
        SetClassAndFormat(nil, spf24bit);
      end;
  end;

  // Determine stored colorspace
{$IFDEF JPEG_Debug}
  DoDebugOut(Self, wsInfo, 'Color conversion samples->bitmap:');
{$ENDIF JPEG_Debug}
  if FStoredCS = jcAutoDetect then
    begin
      InternalCS := DetectInternalColorSpace;
{$IFDEF JPEG_Debug}
      DoDebugOut(Self, wsInfo, PFormat(' Internal colorsp: %s (detected)', [cColorSpaceNames[InternalCS]]));
{$ENDIF JPEG_Debug}
    end
  else
    begin
      InternalCS := FStoredCS;
{$IFDEF JPEG_Debug}
      DoDebugOut(Self, wsInfo, PFormat(' Internal colorsp: %s (selected)', [cColorSpaceNames[InternalCS]]));
{$ENDIF JPEG_Debug}
    end;

  // Determine bitmap colorspace
  if FBitmapCS = jcAutoDetect then
    begin
      OutputCS := InternalCS;
{$IFDEF JPEG_Debug}
      DoDebugOut(Self, wsInfo, PFormat(' Bitmap colorsp: %s (no change)', [cColorSpaceNames[OutputCS]]));
{$ENDIF JPEG_Debug}
    end
  else
    begin
      OutputCS := FBitmapCS;
{$IFDEF JPEG_Debug}
      DoDebugOut(Self, wsInfo, PFormat(' Bitmap colorsp: %s (selected)', [cColorSpaceNames[OutputCS]]));
{$ENDIF JPEG_Debug}
    end;

  // External color management
  if Assigned(FOnExternalCMS) then
    begin
      // We leave the handling of ICC transform to the external application
{$IFDEF JPEG_Debug}
      DoDebugOut(Self, wsInfo, ' Color management handled by external application');
{$ENDIF JPEG_Debug}
      exit;
    end;

  // Determine what conversion and pixelformat to use
  Warning := False;
  case InternalCS of
    jcGray:
      case OutputCS of
        jcRGB: SetClassAndFormat(TTransformGrayToBGR, spf24bit);
        jcRGBA: SetClassAndFormat(TTransformGrayToBGRA, spf32bit);
        jcGray:;
        else
          Warning := True;
      end;
    jcGrayA:
      case OutputCS of
        jcRGB: SetClassAndFormat(TTransformGrayAToBGR, spf24bit);
        jcRGBA: SetClassAndFormat(TTransformGrayAToBGRA, spf32bit);
        jcGrayA:;
        else
          Warning := True;
      end;
    jcRGB:
      case OutputCS of
        jcRGBA: SetClassAndFormat(TTransformRGBToBGRA, spf32bit);
        jcRGB: SetClassAndFormat(TTransformInvertTriplet24bit, spf24bit);
        else
          Warning := True;
      end;
    jcRGBA:
      case OutputCS of
        jcRGB: SetClassAndFormat(TTransformRGBAToBGR, spf24bit);
        jcRGBA:;
        else
          Warning := True;
      end;
    jcYCbCr, jcPhotoYCC:
      case OutputCS of
        jcGray: SetClassAndFormat(TTransformYCbCrToGray, spf8bit);
        jcRGB: SetClassAndFormat(TTransformYCbCrToBGR, spf24bit);
        jcRGBA: SetClassAndFormat(TTransformYCbCrToBGRA, spf32bit);
        else
          Warning := True;
      end;
    jcYCbCrA, jcPhotoYCCA:
      case OutputCS of
        jcRGB: SetClassAndFormat(TTransformYCbCrAToBGR, spf24bit);
        jcRGBA: SetClassAndFormat(TTransformYCbCrAToBGRA, spf32bit);
        else
          Warning := True;
      end;
    jcYCbCrK:
      case OutputCS of
        jcRGB: SetClassAndFormat(TTransformYCbCrKToBGR, spf24bit);
        jcRGBA: SetClassAndFormat(TTransformYCbCrKToBGRA, spf32bit);
        else
          Warning := True;
      end;
    jcCMYK:
      case OutputCS of
        jcRGB: SetClassAndFormat(TTransformCMYKToBGR_Adobe, spf24bit);
        jcRGBA: SetClassAndFormat(TTransformCMYKToBGRA_Adobe, spf32bit);
        else
          Warning := True;
      end;
    jcYCCK:
      case OutputCS of
        jcRGB: SetClassAndFormat(TTransformYCCKToBGR, spf24bit);
        jcRGBA: SetClassAndFormat(TTransformYCCKToBGRA, spf32bit);
        else
          Warning := True;
      end;
    jcITUCieLAB:
      case OutputCS of
        jcRGB: SetClassAndFormat(TTransformITUCIELabToBGR, spf24bit);
        jcRGBA: SetClassAndFormat(TTransformITUCIELabToBGRA, spf32bit);
        else
          Warning := True;
      end;
    else
      Warning := True;
  end;

{$IFDEF JPEG_Debug}
  if (OutputCS <> InternalCS) and Warning then
      DoDebugOut(Self, wsWarn, ' Warning: no color transform could be found (stored colors are output)');
{$ENDIF JPEG_Debug}
end;

function TJpegImage.HasSamples: boolean;
begin
  Result := False;
  if Assigned(FCoder) then
      Result := FCoder.HasSamples;
end;

function TJpegImage.HasCoefficients: boolean;
begin
  Result := False;
  if Assigned(FCoder) then
      Result := FCoder.HasCoefficients;
end;

function TJpegImage.VerifyBitmapColorSpaceForSave: TJpegColorSpace;
var
  Error: boolean;
begin
  Error := False;
  Result := FStoredCS;

  // Verify bitmap colorspace, raise error if pixelformat differs
  case FBitmapCS of
    jcAutoDetect:
      begin
        // Ensure we have some valid pixelformat
        if not(FPixelFormat in [spf8bit, spf24bit, spf32bit]) then
            FPixelFormat := spf24bit;
        case FPixelFormat of
          spf8bit: Result := jcGray;
          spf24bit: Result := jcYCbCr;
          spf32bit: Result := jcYCCK;
        end;
      end;
    jcGray: Error := FPixelFormat <> spf8bit;
    jcGrayA: Error := FPixelFormat <> spf16bit;
    jcRGB, jcYCbCr, jcPhotoYCC: Error := FPixelFormat <> spf24bit;
    jcRGBA, jcYCbCrA, jcCMYK, jcYCCK, jcPhotoYCCA: Error := FPixelFormat <> spf32bit;
  end;
  if Error then
      raise EInvalidImage.Create(sInvalidFormatForSelectedCS);

  // Select correct colorspace to store
  if Result = jcAutoDetect then
    begin
      case FBitmapCS of
        jcGray: Result := jcGray;
        jcGrayA: Result := jcGrayA;
        jcRGB, jcYCbCr: Result := jcYCbCr;
        jcRGBA, jcYCbCrA: Result := jcYCbCrA;
        else Result := FBitmapCS;
      end;
    end;
end;

{ TJpegImage }

procedure TJpegImage.AddMinimalMarkersForColorSpaceDetection(Colors_: TJpegColorSpace);
var
  M: TJpegMarker;
begin
  Markers.Insert(0, TSOIMarker.Create(FJpegInfo, mkSOI));
  // JFIF marker if these color spaces
  if Colors_ in [jcGray, jcYCbCr] then
      Markers.Insert(1, TJFIFMarker.Create(FJpegInfo, mkAPP0))
  else
    // Adobe APP14 marker if these color spaces
    if Colors_ in [jcRGB, jcCMYK, jcYCCK] then
      begin
        M := TAdobeApp14Marker.Create(FJpegInfo, mkAPP14);
        case Colors_ of
          jcRGB, jcCMYK: TAdobeApp14Marker(M).Transform := 0;
          jcYCCK: TAdobeApp14Marker(M).Transform := 2;
        end;
        Markers.Insert(1, M);
      end;
end;

constructor TJpegImage.Create(Owner_: TJPEG_Base_Object);
begin
  inherited Create(Owner_);
  // Owned objects
  FMarkers := TJpegMarkerList.Create(Self);
  FJpegInfo := TJpegInfo.Create;
  FCoderStream := TMemoryStream64.CustomCreate(512 * 1024);
  FSaveOptions := TJpegSaveOptions.Create(Self);
  FMapIterator := TMapIterator.Create;

  // Defaults
  FStoredCS := jcRGB;
  FBitmapCS := jcRGB;
  FDCTCodingMethod := dmAccurate;
end;

destructor TJpegImage.Destroy;
begin
  DisposeObject(FSaveOptions);
  FSaveOptions := nil;

  DisposeObject(FCoder);
  FCoder := nil;

  DisposeObject(FMapIterator);
  FMapIterator := nil;

  DisposeObject(FICCProfile);
  FICCProfile := nil;

  DisposeObject(FJpegInfo);
  FJpegInfo := nil;

  DisposeObject(FCoderStream);
  FCoderStream := nil;

  DisposeObject(FMarkers);
  FMarkers := nil;

  DisposeObject(FLossless);
  FLossless := nil;

  inherited;
end;

procedure TJpegImage.Clear;
begin
  // Clear any lists/objects we have
  FMarkers.Clear;
  FJpegInfo.Clear;
  FCoderStream.Clear;

  // Free any coder we used
  if Assigned(FCoder) then
    begin
      DisposeObject(FCoder);
      FCoder := nil;
    end;

  if Assigned(FLossless) then
      FLossless.Clear;

  // We free the color profile
  DisposeObject(FICCProfile);
  FICCProfile := nil;
end;

procedure TJpegImage.Compress(Iterator_: TMapIterator);
// compress the Jpeg image, using the bitmap in Iterator_
var
  TransformClass: TColorTransformClass;
  Transform: TColorTransform;
  StoredCS_: TJpegColorSpace;
begin
  // check iterator
  if not Assigned(Iterator_) or (Iterator_.Width * Iterator_.Height = 0) then
    begin
{$IFDEF JPEG_Debug}
      DoDebugOut(Self, wsFail, sBitmapIsEmptyCannotSave);
{$ENDIF JPEG_Debug}
      exit;
    end;

  FPixelFormat := sdBitCountToPixelFormat(Iterator_.BitCount);

  // coding info and important private data width/height
  FJpegInfo.FWidth := Iterator_.Width;
  FJpegInfo.FHeight := Iterator_.Height;

  FLoadScale := jsFull;
  FMapWidth := FJpegInfo.FWidth;
  FMapHeight := FJpegInfo.FHeight;

  // If no coder yet, we create the baseline coder
  if not Assigned(FCoder) then
      FCoder := TJpegBaselineCoder.Create(Self, FJpegInfo);

  // compressing the bitmap can only be done in full scale (8x8)
  if FCoder.Scale <> jsFull then
    begin
{$IFDEF JPEG_Debug}
      DoDebugOut(Self, wsFail, sOperationOnlyFor8x8);
{$ENDIF JPEG_Debug}
      exit;
    end;

  // Verify incoming bitmap PFormat versus bitmap colorspace
  StoredCS_ := VerifyBitmapColorSpaceForSave;

  // We create minimal default markers to warrant color space detection
  // later on
  AddMinimalMarkersForColorSpaceDetection(StoredCS_);

  // Ask save options to add DQT, SOFn and SOS marker
  FSaveOptions.AddMarkers(StoredCS_, FMapWidth, FMapHeight);

  // Color transform
  GetColorTransformFromBitmap(TransformClass);
  if not Assigned(TransformClass) then
    begin
{$IFDEF JPEG_Debug}
      DoDebugOut(Self, wsWarn, sInvalidFormatForSelectedCS);
{$ENDIF JPEG_Debug}
      exit;
    end;

  Transform := TransformClass.Create;
  if Assigned(Iterator_) then
    begin
      try
        // Initialize coder (this sets map sizes etc)
        FCoder.Initialize(jsFull);
        // Get samples from bitmap data
        // AV here in laz
        FCoder.SamplesFromImage(Iterator_, Transform);
        // Now convert samples to coefficients. This also does the quantization
        FCoder.ForwardDCT;
      finally
          Transform.Free;
      end;
    end;

  // We also must add an EOI marker
  FMarkers.Add(TEOIMarker.Create(FJpegInfo, mkEOI));

  // coder now has coefficients
  FCoder.HasCoefficients := True;
end;

procedure TJpegImage.GetBitmapSize(Scale_: TJpegScale; var Width_, Height_: integer);
var
  W, H, Divisor: integer;
begin
  W := FJpegInfo.FWidth;
  H := FJpegInfo.FHeight;
  Divisor := sdGetDivisor(Scale_);
  Width_ := (W + Divisor - 1) div Divisor;
  Height_ := (H + Divisor - 1) div Divisor;
end;

procedure TJpegImage.LoadJpeg(Scale_: TJpegScale; DoCreateBitmap: boolean);
var
  i: integer;
  Iteration: cardinal;
  MarkerTag, ExtraTag: byte;
begin
  FLoadScale := Scale_;

{$IFDEF JPEG_Debug}
  DoDebugOut(Self, wsInfo, PFormat('LoadJpeg with LoadScale=%d', [integer(FLoadScale)]));
{$ENDIF JPEG_Debug}
  // iterate thru the markers to initialize, decode and finalize the coder
  for i := 0 to FMarkers.Count - 1 do
    begin
      MarkerTag := FMarkers[i].MarkerTag;
      case MarkerTag of
        mkSOF0 .. mkSOF2:
          begin
            // Method is defined.. we can initialise the coder
            InitializeDecode;
          end;
        mkSOS:
          if Assigned(FCoder) then
            begin
{$IFDEF JPEG_Debug}
              DoDebugOut(Self, wsInfo, PFormat('decode with FCoderStream size=%d', [FCoderStream.Size]));
{$ENDIF JPEG_Debug}
              Iteration := 0;
              FCoder.Decode(FCoderStream, Iteration);

              // in progressive jpegs there can be additional SOS markers
              while FCoderStream.Position < FCoderStream.Size do
                begin
                  // optional additional SOS tag?
                  ExtraTag := LoadMarker(FCoderStream);
                  case ExtraTag of
                    mkSOS:
                      begin
                        inc(Iteration);
                        FCoder.Decode(FCoderStream, Iteration);
                      end;
                    mkDHT:
                      begin
                        // not the right place but we will be lenient
{$IFDEF JPEG_Debug}
                        DoDebugOut(Self, wsWarn, 'incorrect place for DHT marker (must be *before* first SOS)');
{$ENDIF JPEG_Debug}
                      end;
                    else
{$IFDEF JPEG_Debug}
                      DoDebugOut(Self, wsInfo, PFormat('FCoderStream pos=%d size=%d', [FCoderStream.Position, FCoderStream.Size]));
{$ENDIF JPEG_Debug}
                      // signal that we are done
                      FCoderStream.Position := FCoderStream.Size;
                  end;
                end;

            end;
        mkEOI:
          begin
            FCoder.Finalize;
          end;
      end;
    end;

  // if DoCreateBitmap, we update the bitmap
  if DoCreateBitmap then
    { Res := } UpdateBitmap;
end;

procedure TJpegImage.LoadFromFile(const FileName_: string);
var
  M: TMemoryStream64;
begin
  M := TMemoryStream64.CustomCreate(512 * 1024);
  try
    M.LoadFromFile(FileName_);
    M.Position := 0;
    LoadFromStream(M);
  finally
      M.Free;
  end;
end;

procedure TJpegImage.LoadFromStream(Stream: TMemoryStream64);
// LoadFromStream does not yet create or load a bitmap, it only
// loads all the properties of the markers and coder so
// that LoadJpeg() can load the bitmap
var
  MarkerTag: byte;
  EOI1, EOI2: PByte;
  EOIMarker: TEOIMarker;
begin
  // first clear our data
  Clear;

  // Update dependent data via OnUpdate
  if Assigned(FOnUpdate) then
      FOnUpdate(Self);

  // size in bytes of the data stream Stream
  FDataSize := Stream.Size;

  try
    // load another marker of the markers in the jpeg
    repeat
        MarkerTag := LoadMarker(Stream);
    until (MarkerTag = mkSOS) or (MarkerTag = mkNone);

    if MarkerTag = mkSOS then
      begin
        // before we start with the scan (SOS), we must check if the
        // AVI1 tag exists, while there is no DHT. If no DHT,
        // it will be created and added just before the SOS.
        AVI1MarkerCheck;

        // the coder stream starts right after the mkSOS marker data
        // and ends till the end of the file
        FCoderStream.Clear;
        FCoderStream.CopyFrom(Stream, Stream.Size - Stream.Position);
        FCoderStream.Position := 0;

        if FCoderStream.Size >= 2 then
          begin
            // detect EOI
            EOI1 := FCoderStream.Memory;
            inc(EOI1, FCoderStream.Size - 2);
            EOI2 := EOI1;
            inc(EOI2);

            if (EOI1^ = $FF) and (EOI2^ = mkEOI) then
              begin
                // the EOI marker is found, we add it to the marker list so
                // we also write the EOI when saving
                // we must remove the two bytes of the FCoderStream, since the
                // EOI is not part of the coder stream
                FCoderStream.Size := FCoderStream.Size - 2;
{$IFDEF JPEG_Debug}
                DoDebugOut(Self, wsInfo, '<EOI marker>');
{$ENDIF JPEG_Debug}
                EOIMarker := TEOIMarker.Create(FJpegInfo, mkEOI);
                FMarkers.Add(EOIMarker);
              end;
          end;
      end;
  except
    on E: Exception do
      begin
{$IFDEF JPEG_Debug}
        DoDebugOut(Self, wsFail, PFormat('Exception during decode: %S', [E.Message]));
{$ENDIF JPEG_Debug}
        // Added by Dec
        if E.Message <> sInputStreamChopped then
            raise;
      end;
  end;
end;

procedure TJpegImage.LoadTileBlock(var Left_, Top_, Right_, Bottom_: integer);
// LoadTileBlock certainly may have bugs! But it seems that tiled loading works
// for the baseline coder.
var
  Divisor, McuW, McuH: integer;
  XStart, YStart, XCount, YCount: integer;
  McuLeft, McuTop, McuRight, McuBottom: integer;
  i: integer;
  MarkerTag: byte;
  Transform: TColorTransform;
  TransformClass: TColorTransformClass;
begin
{$IFDEF JPEG_Debug}
  DoDebugOut(Self, wsInfo, PFormat('Load tile block [%d %d %d %d]', [Left_, Top_, Right_, Bottom_]));
{$ENDIF JPEG_Debug}
  if not Assigned(FCoder) then
    begin
      // iterate thru the markers to initialize, decode and finalize the coder
      for i := 0 to FMarkers.Count - 1 do
        begin
          MarkerTag := FMarkers[i].MarkerTag;
          case MarkerTag of
            mkSOF0 .. mkSOF2:
              begin
                // Method is defined.. we can initialise the coder
                InitializeDecode;
              end;
            mkSOS:
              if FCoder is TJpegBaselineCoder then
                  TJpegBaselineCoder(FCoder).Decode(FCoderStream, 0);
          end;
        end;
    end;

  // baseline check
  if FCoder.ClassType <> TJpegBaselineCoder then
    begin
{$IFDEF JPEG_Debug}
      DoDebugOut(Self, wsWarn, 'tiled loading only possible with baseline jpeg');
{$ENDIF JPEG_Debug}
      exit;
    end;

  // Determine MCU block area
  Divisor := sdGetDivisor(FLoadScale);
  McuW := FJpegInfo.FMCUWidth div Divisor;
  McuH := FJpegInfo.FMCUHeight div Divisor;

  McuLeft := Max(0, Left_ div McuW);
  McuRight := Min(FJpegInfo.FHorzMCUCount, (Right_ + McuW - 1) div McuW);
  McuTop := Max(0, Top_ div McuH);
  McuBottom := Min(FJpegInfo.FVertMcuCount, (Bottom_ + McuH - 1) div McuH);

  XCount := McuRight - McuLeft;
  YCount := McuBottom - McuTop;
  XStart := McuLeft;
  YStart := McuTop;
  Left_ := McuLeft * McuW;
  Top_ := McuTop * McuH;
  Right_ := McuRight * McuW;
  Bottom_ := McuBottom * McuH;

  // Anything to load?
  if (XCount <= 0) or (YCount <= 0) then
      exit;

  FJpegInfo.FTileWidth := McuW * XCount * Divisor;
  FJpegInfo.FTileHeight := McuH * YCount * Divisor;

  // decode the block
  FCoder.DecodeBlock(FCoderStream, XStart, YStart, XCount, YCount);

  // put it in the map
  GetColorTransformToBitmap(TransformClass, FPixelFormat);
  if TransformClass = nil then
    begin
{$IFDEF JPEG_Debug}
      DoDebugOut(Self, wsWarn, sNoColorTransformation);
{$ENDIF JPEG_Debug}
      exit;
    end;

  Transform := TransformClass.Create;
  try
    // Inverse-DCT the coefficients, this also does the unquantization
    FCoder.InverseDCT;

    // Find the bitmap tile size based on decoded info and required scale
    GetBitmapTileSize(FLoadScale, FJpegInfo.FTileWidth, FJpegInfo.FTileHeight);

    // Set tile bitmap size and pixelformat, and get the iterator to pass as argument
    FMapIterator.Width := FJpegInfo.FTileWidth;
    FMapIterator.Height := FJpegInfo.FTileHeight;
    FMapIterator.CellStride := sdPixelFormatToByteCount(FPixelFormat);

    // this should update the FMapIterator from the application
    if Assigned(FOnCreateMap) then
        FOnCreateMap(FMapIterator);

    // Ask the coder to put the samples in the bitmap
    FCoder.SamplesToImage(FMapIterator, Transform);

  finally
      Transform.Free;
  end;
  FCoder.HasSamples := True;

end;

procedure TJpegImage.Reload;
// reload is helpful when savetostream and directly loadfromstream is used
var
  MS: TMemoryStream64;
begin
  MS := TMemoryStream64.CustomCreate(512 * 1024);
  try
    SaveToStream(MS);
    MS.Position := 0;
    LoadFromStream(MS);
  finally
      MS.Free;
  end;
end;

procedure TJpegImage.SaveJpeg;
// write the markers and save the coder stream
const
  cFF: byte = $FF;
var
  i: integer;
  M: TJpegMarker;
  SeenDHT: boolean;
  DHT: TDHTMarker;
begin
  // no coefficients? then CompressJpeg should have been called
  if not HasCoefficients then
    begin
{$IFDEF JPEG_Debug}
      DoDebugOut(Self, wsFail, sNoDCTCoefficentsAvailable);
{$ENDIF JPEG_Debug}
      exit;
    end;

  // We can now repeatedly save up to the last SOS, then ask the codec to encode
  // the scan
  SeenDHT := False;
  i := 0;

  while i < FMarkers.Count do
    begin
      M := Markers[i];

      if M is TDHTMarker then
          SeenDHT := True;

      if (M is TSOSMarker) and not SeenDHT then
        begin
          // We are at Start of Scan but have not saved a Huffman table, so we must
          // create one and do that now. First we apply scan data by writing the SOS marker.
          M.WriteMarker;

          // Now create the optimized huffman tables for this scan, by doing a dry
          // run, indicated by nil
{$IFDEF JPEG_Debug}
          DoDebugOut(Self, wsInfo, 'doing dry-run encoding for Huffman table');
{$ENDIF JPEG_Debug}
          FCoder.Encode(nil, 0);

          // Ask the coder to create the DHT marker for us, as a
          // result of the dry-run information
          DHT := FCoder.CreateDHTMarker;
{$IFDEF JPEG_Debug}
          DoDebugOut(Self, wsInfo, 'writing DHT marker');
{$ENDIF JPEG_Debug}
          if Assigned(DHT) then
            begin
              DHT.WriteMarker;
              // If a marker was created, then insert it and continue, so it will be saved
{$IFDEF JPEG_Debug}
              DoDebugOut(Self, wsInfo, 'inserting new DHT marker');
{$ENDIF JPEG_Debug}
              FMarkers.Insert(i - 1, DHT);
              SeenDHT := True;
              Continue;
            end;
        end;

      if not(M.MarkerTag in [mkSOI, mkEOI, mkRST0 .. mkRST7]) then
        begin
{$IFDEF JPEG_Debug}
          DoDebugOut(Self, wsInfo, PFormat('Writing marker %s', [M.MarkerName.Text]));
{$ENDIF JPEG_Debug}
          // Writing a marker will also make the marker update itself in the CodingInfo
          // object, so when calling FCoder.Encode later, it will have the current data
          M.WriteMarker;
        end;

      // Encode and save data
      if M is TSOSMarker then
        begin
          FCoderStream.Size := 0;
          FCoder.Encode(FCoderStream, 0);
          // bring position back to 0 for future load/saves
          FCoderStream.Position := 0;
        end;

      // Next marker
      inc(i);
    end;

  // the client can now use SaveToStream to save this new jpeg to a stream
end;

procedure TJpegImage.SaveToFile(const FileName_: string);
var
  FS: TMemoryStream64;
begin
  FS := TMemoryStream64.CustomCreate(512 * 1024);
  try
    SaveToStream(FS);
    FS.SaveToFile(FileName_);
  finally
      FS.Free;
  end;
end;

procedure TJpegImage.SaveToStream(Stream: TMemoryStream64);
const
  cFF: byte = $FF;
var
  i: integer;
  Marker: TJpegMarker;
  MS: TMemoryStream64;
  MarkerSize, SwappedSize: word;
begin
  // loop thru the markers
  for i := 0 to FMarkers.Count - 1 do
    begin
      Marker := Markers[i];

      // write the marker tag
      Stream.Write(cFF, 1);
      Stream.Write(Marker.MarkerTag, 1);
{$IFDEF JPEG_Debug}
      DoDebugOut(Self, wsInfo, PFormat('Saving marker %s', [Marker.MarkerName.Text]));
{$ENDIF JPEG_Debug}
      if not(Marker.MarkerTag in [mkSOI, mkEOI, mkRST0 .. mkRST7]) then
        begin
          MS := TMemoryStream64.CustomCreate(8 * 1024);
          try
            // save the marker to a memory stream
            Marker.SaveToStream(MS);
            MarkerSize := MS.Size + 2;
            SwappedSize := Swap(MarkerSize);
            MS.Position := 0;
            // write the marker size
            Stream.Write(SwappedSize, 2);
            // write the marker
            Stream.CopyFrom(MS, MS.Size);
          finally
              MS.Free;
          end;
        end;

      // after the SOS save coding stream
      if Marker is TSOSMarker then
        begin
{$IFDEF JPEG_Debug}
          DoDebugOut(Self, wsInfo, PFormat('Saving coder stream (%d bytes)', [FCoderStream.Size]));
{$ENDIF JPEG_Debug}
          FCoderStream.Position := 0;
          Stream.CopyFrom(FCoderStream, FCoderStream.Size);
          // bring position back to 0 for future load/saves
          FCoderStream.Position := 0;
        end;
    end;
end;

procedure TJpegImage.SaveBitmapStripByStrip(Width_, Height_: integer);
const
  cFF: byte = $FF;
var
  BitmapIter: TMapIterator;
  TransformClass: TColorTransformClass;
  Transform: TColorTransform;
  Stored: TJpegColorSpace;
  i, y: integer;
  M: TJpegMarker;
  BaselineCoder: TJpegBaselineCoder;
begin
  if not Assigned(FOnProvideStrip) then
    begin
{$IFDEF JPEG_Debug}
      DoDebugOut(Self, wsFail, sOnProvideStripMustBeAssigned);
{$ENDIF JPEG_Debug}
      exit;
    end;

  if not Assigned(FOnCreateMap) then
    begin
{$IFDEF JPEG_Debug}
      DoDebugOut(Self, wsFail, sOnCreateMapMustBeAssigned);
{$ENDIF JPEG_Debug}
      exit;
    end;

  Transform := nil;
  BitmapIter := TMapIterator.CreateDebug(Self);
  try
    // Check if bitmap is created with correct pixelformat
    case FBitmapCS of
      jcGray: BitmapIter.BitCount := 8;
      jcGrayA: BitmapIter.BitCount := 16;
      jcRGB, jcYCbCr, jcPhotoYCC: BitmapIter.BitCount := 24;
      jcRGBA, jcYCbCrA, jcCMYK, jcYCCK, jcPhotoYCCA: BitmapIter.BitCount := 32;
      else BitmapIter.BitCount := 24;
    end;
    FPixelFormat := sdBitCountToPixelFormat(BitmapIter.BitCount);
    BitmapIter.CellStride := BitmapIter.BitCount div 8;

    // We create the baseline coder
    DisposeObjectAndNil(FCoder);
    FCoder := TJpegBaselineCoder.Create(Self, FJpegInfo);
    BaselineCoder := TJpegBaselineCoder(FCoder);

    // Verify incoming bitmap PFormat versus bitmap colorspace
    Stored := VerifyBitmapColorSpaceForSave;

    // We create minimal default markers to warrant color space detection
    // later on
    AddMinimalMarkersForColorSpaceDetection(Stored);

    // Ask save options to add DQT, SOFn and SOS marker. We use pre-defined
    // Huffman tables because we only do one pass over the image
    FSaveOptions.OptimizeHuffmanTables := False;
    FSaveOptions.AddMarkers(Stored, Width_, Height_);

    // We also must add an EOI marker
    FMarkers.Add(TEOIMarker.Create(FJpegInfo, mkEOI));

    // Color transform
    GetColorTransformFromBitmap(TransformClass);
    if not Assigned(TransformClass) then
      begin
{$IFDEF JPEG_Debug}
        DoDebugOut(Self, wsFail, sInvalidFormatForSelectedCS);
{$ENDIF JPEG_Debug}
        exit;
      end;
    Transform := TransformClass.Create;

    // Initialize coder (this sets map sizes etc)
    FCoder.TileMode := True;   // avoid allocating full buffer
    FCoder.Initialize(jsFull); // will calculate MCU height

    // bitmap strip size
    BitmapIter.Width := Width_;
    BitmapIter.Height := FJpegInfo.FMCUHeight;

    // Result is usually a TBitmap (Win32/Linux, etc), but it is up to the application
    FOnCreateMap(BitmapIter);

    // Get iterator
    // GetBitmapIterator(FBitmap, BmpIter);

    // Now we can save the image, and interactively ask application for strips
    // We can now repeatedly save up to the last SOS, then ask the codec to encode
    // the scan
    i := 0;
    while i < FMarkers.Count do
      begin

        M := Markers[i];
{$IFDEF JPEG_Debug}
        DoDebugOut(Self, wsInfo, PFormat('Writing marker %s', [IntToHex(M.MarkerTag, 2)]));
{$ENDIF JPEG_Debug}
        if not(M.MarkerTag in [mkSOI, mkEOI, mkRST0 .. mkRST7]) then
          begin
            // Writing a marker will also make the marker update itself in the Info
            // object, so when calling FCoder.Encode later, it will have the current data
            M.WriteMarker;
          end;

        if M is TSOSMarker then
          begin

            // Start encoder in strip-by-strip mode. This will calculate MCU height
            BaselineCoder.EncodeStripStart(FCoderStream);

            // Encode strips one by one
            for y := 0 to FJpegInfo.FVertMcuCount - 1 do
              begin
                // Call the OnProvideStrip event
                // first, reset the bitnapiter method (since it gets changed by FCoder.SamplesFromImage later)
                BitmapIter.Method := imReaderX;
                FOnProvideStrip(Self, 0, y * FJpegInfo.FMCUHeight, BitmapIter);
                // Get samples from bitmap data
                FCoder.SamplesFromImage(BitmapIter, Transform);
                // Now convert samples to coefficients. This also does the quantization
                FCoder.ForwardDCT;
                // And encode them to the stream
                BaselineCoder.EncodeStrip(FCoderStream);
              end;

            // finalise encoder
            BaselineCoder.EncodeStripClose;

          end;

        // Next marker
        inc(i);
      end;

  finally
    BitmapIter.Free;
    Transform.Free;
  end;
end;

function TJpegImage.UpdateBitmap: TObject;
var
  Transform: TColorTransform;
  TransformClass: TColorTransformClass;
begin
  // If we do not have coefficients we have not loaded anything yet, so exit
  if not HasCoefficients then
      exit;

  // Do we need to update the bitmap?
  if not HasSamples then
    begin
      GetColorTransformToBitmap(TransformClass, FPixelFormat);
      if TransformClass = nil then
        begin
{$IFDEF JPEG_Debug}
          DoDebugOut(Self, wsWarn, sNoColorTransformation);
{$ENDIF JPEG_Debug}
          exit;
        end;
      Transform := TransformClass.Create;
      try
        // Inverse-DCT the coefficients, this also does the unquantization
        FCoder.InverseDCT;

        // Find the bitmap size based on decoded info and required scale
        GetBitmapSize(FLoadScale, FMapWidth, FMapHeight);

        // Set bitmap size and pixelformat, and get the iterator to pass as argument
        FMapIterator.Width := FMapWidth;
        FMapIterator.Height := FMapHeight;
        FMapIterator.CellStride := sdPixelFormatToByteCount(FPixelFormat);

        // this should also update the FMapIterator from the application
        if Assigned(FOnCreateMap) then
          begin
            // Res is usually a TBitmap  or TBitmap32 (Windows/Linux, etc), but it is
            // up to the application
            Result := FOnCreateMap(FMapIterator);
          end;

        // Ask the coder to put the samples in the bitmap
        FCoder.SamplesToImage(FMapIterator, Transform);

      finally
          Transform.Free;
      end;
      FCoder.HasSamples := True;

      // Defer color management to the application, if OnExternalCMS is implemented
      if Assigned(FOnExternalCMS) then
        begin
          FOnExternalCMS(Self, Result);
        end;

    end;
end;

procedure TJpegImage.ExtractMetadata(List_: TJpegMarkerList);
var
  Idx: integer;
begin
  Idx := 0;
  while Idx < FMarkers.Count do
    begin
      if FMarkers[Idx].MarkerTag in [mkAPP0 .. mkAPP15, mkCOM] then
          List_.Add(FMarkers.Extract(FMarkers[Idx]))
      else
          inc(Idx);
    end;
end;

procedure TJpegImage.InjectMetadata(List_: TJpegMarkerList);
begin
  FMarkers.RemoveMarkers([mkAPP0 .. mkAPP15, mkCOM]);
  while List_.Count > 0 do
      FMarkers.Insert(1, List_.Extract(List_[List_.Count - 1]));
end;

function TJpegImage.HasBitmap: boolean;
begin
  Result := Assigned(FMapIterator.Map);
end;

function TJpegImage.HasICCProfile: boolean;
// Determine if we have a valid ICC profile
var
  M: TICCProfileMarker;
begin
  // ICC profile already read?
  if Assigned(FICCProfile) then
    begin
      Result := True;
      exit;
    end;
  // Do we have an ICC profile?
  M := TICCProfileMarker(FMarkers.ByClass(TICCProfileMarker));
  Result := Assigned(M) and M.IsValid;
end;

function TJpegImage.DetectInternalColorSpace: TJpegColorSpace;
var
  JFIF: TJFIFMarker;
  Adobe: TAdobeApp14Marker;
  IDStr: TPascalString;
  // local
  function GetComponentIDString: TPascalString;
  var
    i: integer;
  begin
    Result.L := FJpegInfo.FFrameCount;
    for i := 0 to FJpegInfo.FFrameCount - 1 do
        Result[i + 1] := SystemChar(FJpegInfo.FFrames[i].FComponentID);
  end;

begin
  // Defaults: Based on component count
  Result := jcAutoDetect;
  case FJpegInfo.FFrameCount of
    1: Result := jcGray;
    2: Result := jcGrayA;
    3: Result := jcYCbCr;
    4: Result := jcYCCK;
  end;

  // Check JFIF marker
  JFIF := GetJfifInfo;
  if Assigned(JFIF) and JFIF.IsValid then
    // We have a JFIF marker: if component count is 1 or 3, above assumptions are correct
    if FJpegInfo.FFrameCount in [1, 3] then
        exit;

  // Check Adobe APP14 marker
  Adobe := GetAdobeAPP14Info;
  if Assigned(Adobe) and Adobe.IsValid then
    begin
      // We have an Adobe APP14 marker
      case Adobe.Transform of
        0:
          begin
            case FJpegInfo.FFrameCount of
              3: Result := jcRGB;
              4: Result := jcCMYK;
            end;
          end;
        1: Result := jcYCbCr;
        2: Result := jcYCCK;
      end;
      exit;
    end;

  // Check for ITU G3FAX PFormat
  if FMarkers.ByClass(TG3FAXMarker) <> nil then
    begin
      Result := jcITUCieLAB;
      exit;
    end;

  // No subsampling used?
  if (FJpegInfo.FHorzSamplingMax = 1) and (FJpegInfo.FVertSamplingMax = 1) then
    begin
      // No subsampling used -> Change YC method to RGB or CMYK
      case FJpegInfo.FFrameCount of
        3: Result := jcRGB;
        4: Result := jcCMYK;
      end;
    end;

  // Use component ID's
  IDStr := GetComponentIDString;
  case FJpegInfo.FFrameCount of
    3:
      begin
        // Possible ID strings
        if IDStr = #1#2#3 then
            Result := jcYCbCr;
        if IDStr = 'RGB' then
            Result := jcRGB;
        if IDStr = 'YCc' then
            Result := jcPhotoYCC;
      end;
    4:
      begin
        // Possible ID strings
        if IDStr = #1#2#3#4 then
          begin
            if HasICCProfile then
              // Note: in fact, in cases seen, this represents CMYK instead of RGBA,
              // so to decode: decode to RGBA as usual, then pretend these channels
              // are CMYK, and convert to final colour space.
              // Seen in: scanners (always with ICC profile present - which has CMYK profile)
                Result := jcYCbCrK
            else
                Result := jcYCbCrA;
          end;
        if IDStr = 'RGBA' then
            Result := jcRGBA;
        if IDStr = 'YCcA' then
            Result := jcPhotoYCCA;
      end;
  end;
end;

{ TJpegSaveOptions }

procedure TJpegSaveOptions.AddMarkers(Stored_: TJpegColorSpace; Width_, Height_: integer);
var
  i: integer;
  Info: TJpegInfo;
  Frame: TFrameComponent;
  SOF: TSOFnMarker;
  SOS: TSOSMarker;
begin
  // Set the correct FInfo fields
  Info := FOwner.FJpegInfo;
  Info.FWidth := Width_;
  Info.FHeight := Height_;
  Info.FSamplePrecision := 8;
  case Stored_ of
    jcGray:
      Info.FFrameCount := 1;
    jcGrayA:
      Info.FFrameCount := 2;
    jcRGB, jcYCbCr, jcPhotoYCC:
      Info.FFrameCount := 3;
    jcRGBA, jcYCbCrA, jcCMYK, jcYCCK, jcPhotoYCCA:
      Info.FFrameCount := 4;
    else
      raise EInvalidImage.Create(sUnsupportedColorSpace);
  end;

  // Subsampling used?
  case Stored_ of
    jcYCbCr, jcYCbCrA, jcYCCK, jcPhotoYCC, jcPhotoYCCA:
      FUseSubSampling := True
    else
      FUseSubSampling := False;
  end;

  // Set up frame sampling
  for i := 0 to Info.FFrameCount - 1 do
    begin
      Frame := Info.FFrames[i];
      if (i in [1, 2]) then
        begin
          // Subsampled frame
          Frame.FHorzSampling := 1;
          Frame.FVertSampling := 1;
          if FUseSubSampling then
              Frame.FQTable := 1
          else
              Frame.FQTable := 0;
        end
      else
        begin
          // Full frame
          if FUseSubSampling then
            begin
              Frame.FHorzSampling := 2;
              Frame.FVertSampling := 2;
            end
          else
            begin
              Frame.FHorzSampling := 1;
              Frame.FVertSampling := 1;
            end;
          Frame.FQTable := 0;
        end;
      Frame.FComponentID := i + 1;
    end;
  if FUseSubSampling then
    begin
      Info.FHorzSamplingMax := 2;
      Info.FVertSamplingMax := 2;
    end
  else
    begin
      Info.FHorzSamplingMax := 1;
      Info.FVertSamplingMax := 1;
    end;

  // Setup and add quant tables
  SetupQuantTables;

  // Create and add SOFn marker
  SOF := TSOFnMarker.Create(Info, mkSOF0);
  FOwner.Markers.Add(SOF);

  // Create and add default Huffman tables if required
  if not OptimizeHuffmanTables then
      SetupDefaultHuffmanTables;

  // Create and add SOS marker
  SOS := TSOSMarker.Create(Info, mkSOS);
  FOwner.Markers.Add(SOS);

  SetLength(SOS.FMarkerInfo, Info.FFrameCount);
  SOS.FScanCount := Info.FFrameCount;
  for i := 0 to Info.FFrameCount - 1 do
    begin
      SOS.FMarkerInfo[i].ComponentID := i + 1;
      SOS.FMarkerInfo[i].DCTable := Info.FFrames[i].FQTable;
      SOS.FMarkerInfo[i].ACTable := Info.FFrames[i].FQTable;
    end;

end;

procedure TJpegSaveOptions.SetupDefaultHuffmanTables;
var
  M: TDHTMarker;
  procedure FillTable(var Info: TDHTMarkerInfo; Bits, Values: PByte; Tc, Th: byte);
  var
    i, Count: integer;
  begin
    Count := 0;
    Info.Tc := Tc;
    Info.Th := Th;
    for i := 0 to 15 do
      begin
        Info.BitLengths[i] := Bits^;
        inc(Count, Bits^);
        inc(Bits);
      end;
    SetLength(Info.BitValues, Count);
    for i := 0 to Count - 1 do
      begin
        Info.BitValues[i] := Values^;
        inc(Values);
      end;
  end;

begin
  M := TDHTMarker.Create(FOwner.FJpegInfo, mkDHT);
  if FUseSubSampling then
      SetLength(M.FMarkerInfo, 4)
  else
      SetLength(M.FMarkerInfo, 2);

  // Luminance tables (always used)
  FillTable(M.FMarkerInfo[0], @cHuffmanBitsDcLum[0], @cHuffmanValDcLum[0], 0, 0);
  FillTable(M.FMarkerInfo[1], @cHuffmanBitsAcLum[0], @cHuffmanValAcLum[0], 1, 0);

  if FUseSubSampling then
    begin
      // Chrominance tables (only when subsampling is used)
      FillTable(M.FMarkerInfo[2], @cHuffmanBitsDcChrom[0], @cHuffmanValDcChrom[0], 0, 1);
      FillTable(M.FMarkerInfo[3], @cHuffmanBitsAcChrom[0], @cHuffmanValAcChrom[0], 1, 1);
    end;
  FOwner.Markers.Add(M);
end;

procedure TJpegSaveOptions.SetupQuantTables;
var
  QMul: integer;
  T: TQuantizationTable;
  M: TDQTMarker;
begin
  if FQuality < 1 then
      FQuality := 1
  else
    if FQuality > 100 then
      FQuality := 100;

  // Calculation of quant multiplication factor
  if FQuality < 50 then
      QMul := 5000 div FQuality
  else
      QMul := 200 - FQuality * 2;

  // Create DQT marker
  M := TDQTMarker.Create(FOwner.FJpegInfo, mkDQT);
  if FUseSubSampling then
      SetLength(M.FTableIndices, 2)
  else
      SetLength(M.FTableIndices, 1);

  // Quant table 0
  T := FOwner.FJpegInfo.FQuantizationTables[0];
  SetTableMultiplication(T, QMul, cStdLuminanceQuantTbl);
  M.FTableIndices[0] := 0;

  // Quant table 1
  if FUseSubSampling then
    begin
      T := FOwner.FJpegInfo.FQuantizationTables[1];
      SetTableMultiplication(T, QMul, cStdChrominanceQuantTbl);
      M.FTableIndices[1] := 1;
    end;

  // Add DQT marker
  FOwner.Markers.Add(M);
end;

procedure TJpegSaveOptions.SetTableMultiplication(Table_: TQuantizationTable; MultiplyPercent: integer; const DefaultTable_: TIntArray64);
var
  i, Q: integer;
begin
  for i := 0 to 63 do
    begin
      Q := (DefaultTable_[cJpegInverseZigZag8x8[i]] * MultiplyPercent + 50) div 100;
      // ensure that quant factor is in valid range
      if Q <= 0 then
          Q := 1
      else
        if Q > 255 then
          Q := 255;
      // set table quant factor i
      Table_.FQuant[i] := Q;
    end;
end;

constructor TJpegSaveOptions.Create(Owner_: TJpegImage);
begin
  inherited Create;
  FOwner := Owner_;
  FQuality := cDefaultJpgCompressionQuality;
  FOptimizeHuffmanTables := True;
  FCodingMethod := emBaselineDCT;
  FUseSubSampling := True;
end;

end.
