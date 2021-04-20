{ ****************************************************************************** }
{ * Color Transforms for JPEG support                                          * }
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
unit Raster_JPEG_ColorTransforms;

interface

uses CoreClasses, Raster_JPEG_type;

type
  // Abstract color transform
  TColorTransform = class(TJPEG_Persistent)
  public
    constructor Create; virtual;
    // Dest cellstride (#bytes per cell)
    function DstCellStride: Integer; virtual; abstract;
    // Source cellstride (#bytes per cell)
    function SrcCellStride: Integer; virtual; abstract;
    // Transform Count colors from Source to Dest
    procedure Transform(Source, Dest: Pointer; Count: Integer); virtual; abstract;
  end;

  // Transform class
  TColorTransformClass = class of TColorTransform;

  // Null transform: 8bit/pixel direct copy.
  TNullTransform8bit = class(TColorTransform)
  public
    function DstCellStride: Integer; override;
    function SrcCellStride: Integer; override;
    procedure Transform(Source, Dest: Pointer; Count: Integer); override;
  end;

  // Null transform: 16bit/pixel direct copy.
  TNullTransform16bit = class(TColorTransform)
  public
    function DstCellStride: Integer; override;
    function SrcCellStride: Integer; override;
    procedure Transform(Source, Dest: Pointer; Count: Integer); override;
  end;

  // Null transform: 24bit/pixel direct copy.
  TNullTransform24bit = class(TColorTransform)
  public
    function DstCellStride: Integer; override;
    function SrcCellStride: Integer; override;
    procedure Transform(Source, Dest: Pointer; Count: Integer); override;
  end;

  // Null transform: 32bit/pixel direct copy.
  TNullTransform32bit = class(TColorTransform)
  public
    function DstCellStride: Integer; override;
    function SrcCellStride: Integer; override;
    procedure Transform(Source, Dest: Pointer; Count: Integer); override;
  end;

  // Invert gray values from 0->255 and 255->0
  TTransformInverseGray8bit = class(TColorTransform)
  public
    function DstCellStride: Integer; override;
    function SrcCellStride: Integer; override;
    procedure Transform(Source, Dest: Pointer; Count: Integer); override;
  end;

  // Inversion transform: invert colour triplets RGB->BGR, can be used for any
  // 24bit triplet of colours that needs to be inverted in order.
  TTransformInvertTriplet24bit = class(TColorTransform)
  public
    function DstCellStride: Integer; override;
    function SrcCellStride: Integer; override;
    procedure Transform(Source, Dest: Pointer; Count: Integer); override;
  end;

  // Abstract JFIF transform (transforms used by JPEG's JFIF spec)
  TJfifTransform = class(TColorTransform)
  private
    FColorConvScale: Integer;
    function RangeLimitDescale(A: Integer): Integer;
  public
    constructor Create; override;
    function DstCellStride: Integer; override;
    function SrcCellStride: Integer; override;
  end;

  // Abstract JFIF forward transform (YCbCr to RGB space)
  TJfifFwdTransform = class(TJfifTransform)
  private
    FCbtoBT: array [0 .. 255] of Integer;
    FCbtoGT: array [0 .. 255] of Integer;
    FCrtoGT: array [0 .. 255] of Integer;
    FCrtoRT: array [0 .. 255] of Integer;
    FY_toRT: array [0 .. 255] of Integer;
    F__toB: Integer;
    F__toG: Integer;
    F__toR: Integer;
    procedure InitYCbCrTables;
  public
    constructor Create; override;
  end;

  // Abstract JFIF inverse transform (RGB to YCbCr space)
  TJfifInvTransform = class(TJfifTransform)
  private
    FBtoCb: array [0 .. 255] of Integer;
    FBtoCr: array [0 .. 255] of Integer;
    FBtoY_: array [0 .. 255] of Integer;
    FGtoCb: array [0 .. 255] of Integer;
    FGtoCr: array [0 .. 255] of Integer;
    FGtoY_: array [0 .. 255] of Integer;
    FRtoCb: array [0 .. 255] of Integer;
    FRtoCr: array [0 .. 255] of Integer;
    FRtoY_: array [0 .. 255] of Integer;
    F__toCb: Integer;
    F__toCr: Integer;
    procedure InitRGBToYCbCrTables;
  public
    constructor Create; override;
  end;

  // Y-Cb-Cr (24bit) to BGR (24bit) colour transform. It assumes that
  // RGB is layed out in memory as BGR (as windows TBitmap does).
  TTransformYCbCrToBGR = class(TJfifFwdTransform)
  public
    procedure Transform(Source, Dest: Pointer; Count: Integer); override;
  end;

  // Y-Cb-Cr (24bit) to BGRA (32bit) colour transform. It assumes that
  // RGB is layed out in memory as BGR (as windows TBitmap does). The alpha
  // channel A is set to $FF.
  TTransformYCbCrToBGRA = class(TJfifFwdTransform)
  public
    function DstCellStride: Integer; override;
    procedure Transform(Source, Dest: Pointer; Count: Integer); override;
  end;

  // Y-Cb-Cr (24bit) to gray (8it) colour transform. The Y channel is used
  // as grayscale directly, Cb and Cr channels are not used.
  TTransformYCbCrToGray = class(TJfifFwdTransform)
  public
    function DstCellStride: Integer; override;
    procedure Transform(Source, Dest: Pointer; Count: Integer); override;
  end;

  // Y-Cb-Cr-A (32bit) to BGR (24bit) colour transform. It assumes that
  // RGB is layed out in memory as BGR (as windows TBitmap does). The input
  // alpha (A) channel is ignored.
  TTransformYCbCrAToBGR = class(TJfifFwdTransform)
  public
    function SrcCellStride: Integer; override;
    procedure Transform(Source, Dest: Pointer; Count: Integer); override;
  end;

  // YCbCrK to BGR. YCbCr is first converted to CMY, then CMYK is converted to RGB
  TTransformYCbCrKToBGR = class(TJfifFwdTransform)
  public
    function SrcCellStride: Integer; override;
    procedure Transform(Source, Dest: Pointer; Count: Integer); override;
  end;

  // YCbCrK to BGRA. YCbCr is first converted to CMY, then CMYK is converted to RGBA
  TTransformYCbCrKToBGRA = class(TJfifFwdTransform)
  public
    function DstCellStride: Integer; override;
    function SrcCellStride: Integer; override;
    procedure Transform(Source, Dest: Pointer; Count: Integer); override;
  end;

  // Y-Cb-Cr-A (32bit) to BGRA (32bit) colour transform. It assumes that
  // RGB is layed out in memory as BGR (as windows TBitmap does). The alpha
  // channels are copied.
  TTransformYCbCrAToBGRA = class(TJfifFwdTransform)
  public
    function DstCellStride: Integer; override;
    function SrcCellStride: Integer; override;
    procedure Transform(Source, Dest: Pointer; Count: Integer); override;
  end;

  // CMYK (32bit) to BGR (24bit) colour transform.
  TTransformCMYKToBGR = class(TColorTransform)
  public
    function DstCellStride: Integer; override;
    function SrcCellStride: Integer; override;
    procedure Transform(Source, Dest: Pointer; Count: Integer); override;
  end;

  // CMYK (32bit) to BGR (24bit) colour transform, Adobe specific.
  TTransformCMYKToBGR_Adobe = class(TColorTransform)
  public
    function DstCellStride: Integer; override;
    function SrcCellStride: Integer; override;
    procedure Transform(Source, Dest: Pointer; Count: Integer); override;
  end;

  // CMYK (32bit) to BGRA (32bit) colour transform, Adobe specific.
  TTransformCMYKToBGRA_Adobe = class(TColorTransform)
  public
    function DstCellStride: Integer; override;
    function SrcCellStride: Integer; override;
    procedure Transform(Source, Dest: Pointer; Count: Integer); override;
  end;

  // YCCK (32bit) to BGR (24bit) colour transform. The CMY channels are coded as
  // Y-Cb-Cr, thus first unencoded to CMY, then combined with K to do CMYK to RGB.
  TTransformYCCKToBGR = class(TJfifFwdTransform)
  public
    function SrcCellStride: Integer; override;
    procedure Transform(Source, Dest: Pointer; Count: Integer); override;
  end;

  // YCCK (32bit) to BGRA (32bit) colour transform. The CMY channels are coded as
  // Y-Cb-Cr, thus first unencoded to CMY, then combined with K to do CMYK to RGBA.
  TTransformYCCKToBGRA = class(TJfifFwdTransform)
  public
    function DstCellStride: Integer; override;
    function SrcCellStride: Integer; override;
    procedure Transform(Source, Dest: Pointer; Count: Integer); override;
  end;

  // YCCK to BGR color transform, as Adobe does it. Experimental status!
  TTransformYCCKToBGR_Adobe = class(TJfifFwdTransform)
  private
    F0_65: Integer;
    F44_8: Integer;
    procedure InitConst;
  public
    constructor Create; override;
    function SrcCellStride: Integer; override;
    procedure Transform(Source, Dest: Pointer; Count: Integer); override;
  end;

  // Gray (8bit) to BGR (24bit) transform. The R, G and B channels are all set
  // to the gray value.
  TTransformGrayToBGR = class(TColorTransform)
  public
    function DstCellStride: Integer; override;
    function SrcCellStride: Integer; override;
    procedure Transform(Source, Dest: Pointer; Count: Integer); override;
  end;

  // Gray (8bit) to BGRA (32bit) transform. The R, G and B channels are all set
  // to the gray value.
  TTransformGrayToBGRA = class(TColorTransform)
  public
    function DstCellStride: Integer; override;
    function SrcCellStride: Integer; override;
    procedure Transform(Source, Dest: Pointer; Count: Integer); override;
  end;

  // Gray + Alpha (16bit) to BGR (24bit) transform. The R, G and B channels are all set
  // to the gray value. The Alpha channel is ignored.
  TTransformGrayAToBGR = class(TColorTransform)
  public
    function DstCellStride: Integer; override;
    function SrcCellStride: Integer; override;
    procedure Transform(Source, Dest: Pointer; Count: Integer); override;
  end;

  // Gray + Alpha (16bit) to BGRA (32bit) transform. The R, G and B channels are all set
  // to the gray value. The Alpha channels are copied.
  TTransformGrayAToBGRA = class(TColorTransform)
  public
    function DstCellStride: Integer; override;
    function SrcCellStride: Integer; override;
    procedure Transform(Source, Dest: Pointer; Count: Integer); override;
  end;

  // RGB (24bit) to BGRA (32bit) transform. The output alpha (A) channel is
  // set to $FF.
  TTransformRGBToBGRA = class(TColorTransform)
  public
    function DstCellStride: Integer; override;
    function SrcCellStride: Integer; override;
    procedure Transform(Source, Dest: Pointer; Count: Integer); override;
  end;

  // RGBA (32bit) to BGR (24bit) transform. The input alpha (A) channel is ignored.
  TTransformRGBAToBGR = class(TColorTransform)
  public
    function DstCellStride: Integer; override;
    function SrcCellStride: Integer; override;
    procedure Transform(Source, Dest: Pointer; Count: Integer); override;
  end;

  // BGRA (32bit) to BGR (24bit) transform. This transform uses a parameter,
  // BkColor (TColor) that is used to fill the background, while the colors are
  // blended using the "over" operator. The background color by default is clWhite.
  // This routine assumes alpha pre-multiplied colors.
  TTransformBGRAToBGR = class(TColorTransform)
  private
    FBkColor: Cardinal;
    function GetBkColor: Cardinal;
    procedure SetBkColor(const Value: Cardinal);
  public
    constructor Create; override;
    function DstCellStride: Integer; override;
    function SrcCellStride: Integer; override;
    procedure Transform(Source, Dest: Pointer; Count: Integer); override;
    property BkColor: Cardinal read GetBkColor write SetBkColor;
  end;

  // RGB (24bit) to Y-Cb-Cr (24bit) colour transform. It assumes that
  // RGB is layed out in memory as BGR (as windows TBitmap does).
  TTransformBGRToYCbCr = class(TJfifInvTransform)
  public
    procedure Transform(Source, Dest: Pointer; Count: Integer); override;
  end;

  // RGB (24bit) to Gray (8bit) colour transform. It assumes that
  // RGB is layed out in memory as BGR (as windows TBitmap does). It uses
  // the same formula to find Gray as in RGB->YCbCr
  TTransformBGRToGray = class(TJfifInvTransform)
  public
    function DstCellStride: Integer; override;
    procedure Transform(Source, Dest: Pointer; Count: Integer); override;
  end;

  // RGB (24bit) to GrayA (8bit+16bit) colour transform. It assumes that
  // RGB is layed out in memory as BGR (as windows TBitmap does). It uses
  // the same formula to find Gray as in RGB->YCbCr
  TTransformBGRToGrayA = class(TJfifInvTransform)
  public
    function DstCellStride: Integer; override;
    function SrcCellStride: Integer; override;
    procedure Transform(Source, Dest: Pointer; Count: Integer); override;
  end;

  // RGBA (32bit) to Gray (8bit) colour transform. It assumes that
  // RGBA is layed out in memory as BGRA (as memory raster engine does). It uses
  // the same formula to find Gray as in RGBA->YCbCr
  TTransformBGRAToGray = class(TJfifInvTransform)
  public
    function DstCellStride: Integer; override;
    function SrcCellStride: Integer; override;
    procedure Transform(Source, Dest: Pointer; Count: Integer); override;
  end;

  // RGBA (32bit) to Gray (8bit+16bit) colour transform. It assumes that
  // RGBA is layed out in memory as BGRA (as memory raster engine does). It uses
  // the same formula to find Gray as in RGBA->YCbCr
  TTransformBGRAToGrayA = class(TJfifInvTransform)
  public
    function DstCellStride: Integer; override;
    function SrcCellStride: Integer; override;
    procedure Transform(Source, Dest: Pointer; Count: Integer); override;
  end;

  // RGBA (32bit) to Y-Cb-Cr-A (32bit) colour transform. It assumes that
  // RGB is layed out in memory as BGR (as windows TBitmap does). The alpha
  // channels are copied.
  TTransformBGRAToYCbCrA = class(TJfifInvTransform)
  public
    function DstCellStride: Integer; override;
    function SrcCellStride: Integer; override;
    procedure Transform(Source, Dest: Pointer; Count: Integer); override;
  end;

  // RGBA (32bit) to Y-Cb-Cr (24bit) colour transform. It assumes that
  // RGB is layed out in memory as BGR (as windows TBitmap does). The alpha
  // channel is ignored.
  TTransformBGRAToYCbCr = class(TJfifInvTransform)
  public
    function SrcCellStride: Integer; override;
    procedure Transform(Source, Dest: Pointer; Count: Integer); override;
  end;

  // CIE L*a*b* (24bit) to BGR (24bit), using parameters
  // - based on 24bit (3*8 bit) nulltransform
  TTransformCIELabToBGR = class(TNullTransform24bit)
  private
    FAmax: Double;
    FAmin: Double;
    FAofs: Integer;
    FBmax: Double;
    FBmin: Double;
    FBofs: Integer;
    FXw: Double;
    FYw: Double;
    FZw: Double;
  public
    constructor Create; override;
    procedure Transform(Source, Dest: Pointer; Count: Integer); override;
    property Amax: Double read FAmax write FAmax;
    // Range
    property Amin: Double read FAmin write FAmin;
    // Offset
    property Aofs: Integer read FAofs write FAofs;
    property Bmax: Double read FBmax write FBmax;
    property Bmin: Double read FBmin write FBmin;
    property Bofs: Integer read FBofs write FBofs;
    // White point
    property Xw: Double read FXw write FXw;
    property Yw: Double read FYw write FYw;
    property Zw: Double read FZw write FZw;
  end;

  // ITU CIE L*a*b* (24bit) to BGR (24bit), with canned parameters, which are
  // set in the constructor
  TTransformITUCIELabToBGR = class(TTransformCIELabToBGR)
  public
    constructor Create; override;
  end;

  // CIE L*a*b* (24bit) to BGR (24bit), using parameters
  // - based on 24bit (3*8 bit) nulltransform
  TTransformCIELabToBGRA = class(TNullTransform32bit)
  private
    FAmax: Double;
    FAmin: Double;
    FAofs: Integer;
    FBmax: Double;
    FBmin: Double;
    FBofs: Integer;
    FXw: Double;
    FYw: Double;
    FZw: Double;
  public
    constructor Create; override;
    procedure Transform(Source, Dest: Pointer; Count: Integer); override;
    property Amax: Double read FAmax write FAmax;
    // Range
    property Amin: Double read FAmin write FAmin;
    // Offset
    property Aofs: Integer read FAofs write FAofs;
    property Bmax: Double read FBmax write FBmax;
    property Bmin: Double read FBmin write FBmin;
    property Bofs: Integer read FBofs write FBofs;
    // White point
    property Xw: Double read FXw write FXw;
    property Yw: Double read FYw write FYw;
    property Zw: Double read FZw write FZw;
  end;

  // ITU CIE L*a*b* (24bit) to BGR (24bit), with canned parameters, which are
  // set in the constructor
  TTransformITUCIELabToBGRA = class(TTransformCIELabToBGRA)
  public
    constructor Create; override;
  end;

  // Abstract YUV transform
  TYuvTransform = class(TColorTransform)
  private
    FBias: Integer;
    FMaxValue: Integer;
    FPrecision: Integer;
    FScaleFact: Integer;
    function Clip(Value_: Integer): Integer;
    procedure InitScaleFact;
  public
    constructor Create; override;
    function DstCellStride: Integer; override;
    function SrcCellStride: Integer; override;
  end;

  // Abstract YUV forward transform
  TYuvFwdTransform = class(TYuvTransform)
  private
    FUtoB: array [0 .. 255] of Integer;
    FUtoG: array [0 .. 255] of Integer;
    FVtoG: array [0 .. 255] of Integer;
    FVtoR: array [0 .. 255] of Integer;
    FYtoB: array [0 .. 255] of Integer;
    FYtoG: array [0 .. 255] of Integer;
    FYtoR: array [0 .. 255] of Integer;
    procedure InitTables;
  public
    constructor Create; override;
  end;

  TTransformYUVToRGB = class(TYuvFwdTransform)
  public
    procedure Transform(Source, Dest: Pointer; Count: Integer); override;
  end;

  TTransformYUVToCMYK = class(TYuvFwdTransform)
  public
    function DstCellStride: Integer; override;
    procedure Transform(Source, Dest: Pointer; Count: Integer); override;
  end;

  // Abstract YUV inverse transform
  TYuvInvTransform = class(TYuvTransform)
  private
    FUfromB: array [0 .. 255] of Integer;
    FUfromG: array [0 .. 255] of Integer;
    FUfromR: array [0 .. 255] of Integer;
    FVfromB: array [0 .. 255] of Integer;
    FVfromG: array [0 .. 255] of Integer;
    FVfromR: array [0 .. 255] of Integer;
    FYfromB: array [0 .. 255] of Integer;
    FYfromG: array [0 .. 255] of Integer;
    FYfromR: array [0 .. 255] of Integer;
    procedure InitTables;
  public
    constructor Create; override;
  end;

  TTransformRGBToYUV = class(TYuvInvTransform)
  public
    procedure Transform(Source, Dest: Pointer; Count: Integer); override;
  end;

implementation

uses
  Math;

{ TColorTransform }

constructor TColorTransform.Create;
// We need a virtual constructor so it can be overridden
begin
  inherited Create;
end;

{ TNullTransform8bit }

function TNullTransform8bit.DstCellStride: Integer;
begin
  Result := 1;
end;

function TNullTransform8bit.SrcCellStride: Integer;
begin
  Result := 1;
end;

procedure TNullTransform8bit.Transform(Source, Dest: Pointer; Count: Integer);
begin
  CopyPtr(Source, Dest, Count);
end;

{ TNullTransform16bit }

function TNullTransform16bit.DstCellStride: Integer;
begin
  Result := 2;
end;

function TNullTransform16bit.SrcCellStride: Integer;
begin
  Result := 2;
end;

procedure TNullTransform16bit.Transform(Source, Dest: Pointer; Count: Integer);
begin
  CopyPtr(Source, Dest, Count * 2);
end;

{ TNullTransform24bit }

function TNullTransform24bit.DstCellStride: Integer;
begin
  Result := 3;
end;

function TNullTransform24bit.SrcCellStride: Integer;
begin
  Result := 3;
end;

procedure TNullTransform24bit.Transform(Source, Dest: Pointer; Count: Integer);
begin
  CopyPtr(Source, Dest, Count * 3);
end;

{ TNullTransform32bit }

function TNullTransform32bit.DstCellStride: Integer;
begin
  Result := 4;
end;

function TNullTransform32bit.SrcCellStride: Integer;
begin
  Result := 4;
end;

procedure TNullTransform32bit.Transform(Source, Dest: Pointer; Count: Integer);
begin
  CopyPtr(Source, Dest, Count * 4);
end;

{ TTransformInverseGray8bit }

function TTransformInverseGray8bit.DstCellStride: Integer;
begin
  Result := 1;
end;

function TTransformInverseGray8bit.SrcCellStride: Integer;
begin
  Result := 1;
end;

procedure TTransformInverseGray8bit.Transform(Source, Dest: Pointer; Count: Integer);
var
  G, IG: PByte;
begin
  G := Source;
  IG := Dest;
  while Count > 0 do
    begin
      IG^ := G^ xor $FF;
      inc(G);
      inc(IG);
      dec(Count);
    end;
end;

{ TTransformInvertTriplet24bit }

function TTransformInvertTriplet24bit.DstCellStride: Integer;
begin
  Result := 3;
end;

function TTransformInvertTriplet24bit.SrcCellStride: Integer;
begin
  Result := 3;
end;

procedure TTransformInvertTriplet24bit.Transform(Source, Dest: Pointer; Count: Integer);
var
  T: byte;
  X1S, X2S, X3S: PByte;
  X1D, X2D, X3D: PByte;
begin
  // Source pointers straightforward
  X1S := Source;
  X2S := Source;
  inc(X2S);
  X3S := Source;
  inc(X3S, 2);
  // Dest pointers layed out inverted
  X3D := Dest;
  X2D := Dest;
  inc(X2D);
  X1D := Dest;
  inc(X1D, 2);

  // Check if Src = Dst
  if Source = Dest then
    begin

      // Repeat Count times
      while Count > 0 do
        begin
          T := X1S^;
          X1S^ := X3S^;
          X3S^ := T;

          inc(X1S, 3);
          inc(X3S, 3);
          dec(Count);
        end;

    end
  else
    begin

      // Repeat Count times
      while Count > 0 do
        begin
          X1D^ := X1S^;
          X2D^ := X2S^;
          X3D^ := X3S^;
          inc(X1S, 3);
          inc(X2S, 3);
          inc(X3S, 3);
          inc(X1D, 3);
          inc(X2D, 3);
          inc(X3D, 3);
          dec(Count);
        end;

    end;
end;

constructor TJfifTransform.Create;
begin
  inherited Create;
  FColorConvScale := 1 shl 10;
end;

function TJfifTransform.DstCellStride: Integer;
begin
  // these are defaults, can be overridden
  Result := 3;
end;

{ TJfifTransform }

function TJfifTransform.RangeLimitDescale(A: Integer): Integer;
begin
  Result := A div FColorConvScale;
  if Result < 0 then
      Result := 0
  else
    if Result > 255 then
      Result := 255;
end;

function TJfifTransform.SrcCellStride: Integer;
begin
  // these are defaults, can be overridden
  Result := 3;
end;

constructor TJfifFwdTransform.Create;
begin
  inherited Create;
  InitYCbCrTables;
end;

{ TJfifFwdTransform }

procedure TJfifFwdTransform.InitYCbCrTables;
{ YCbCr to RGB conversion: These constants come from JFIF spec

  R = Y                      + 1.402 (Cr-128)
  G = Y - 0.34414 (Cb-128) - 0.71414 (Cr-128)
  B = Y + 1.772 (Cb-128)

  or

  R = Y                + 1.402 Cr - 179.456
  G = Y - 0.34414 Cb - 0.71414 Cr + 135.53664
  B = Y +   1.772 Cb              - 226.816
}
var
  i: Integer;
begin
  F__toR := Round(-179.456 * FColorConvScale);
  F__toG := Round(135.53664 * FColorConvScale);
  F__toB := Round(-226.816 * FColorConvScale);
  for i := 0 to 255 do
    begin
      FY_toRT[i] := Round(1 * FColorConvScale * i);
      FCrtoRT[i] := Round(1.402 * FColorConvScale * i);
      FCbtoGT[i] := Round(-0.34414 * FColorConvScale * i);
      FCrtoGT[i] := Round(-0.71414 * FColorConvScale * i);
      FCbtoBT[i] := Round(1.772 * FColorConvScale * i);
    end;
end;

{ TTransformYCbCrToRGB }

procedure TTransformYCbCrToBGR.Transform(Source, Dest: Pointer; Count: Integer);
var
  R, G, B, Y, Cb, Cr: PByte;
  Yi, Ri, Gi, Bi: Integer;
begin
  Y := Source;
  Cb := Source;
  inc(Cb);
  Cr := Source;
  inc(Cr, 2);

  // RGB is layed out in memory as BGR
  B := Dest;
  G := Dest;
  inc(G);
  R := Dest;
  inc(R, 2);

  // Repeat Count times..
  while Count > 0 do
    begin
      // Do the conversion in int
      Yi := FY_toRT[Y^];
      Ri := Yi + FCrtoRT[Cr^] + F__toR;
      Gi := Yi + FCbtoGT[Cb^] + FCrtoGT[Cr^] + F__toG;
      Bi := Yi + FCbtoBT[Cb^] + F__toB;
      R^ := RangeLimitDescale(Ri);
      G^ := RangeLimitDescale(Gi);
      B^ := RangeLimitDescale(Bi);
      // Advance pointers
      inc(Y, 3);
      inc(Cb, 3);
      inc(Cr, 3);
      inc(R, 3);
      inc(G, 3);
      inc(B, 3);
      dec(Count);
    end;
end;

{ TTransformYCbCrToRGBA }

function TTransformYCbCrToBGRA.DstCellStride: Integer;
begin
  Result := 4;
end;

procedure TTransformYCbCrToBGRA.Transform(Source, Dest: Pointer; Count: Integer);
var
  R, G, B, A, Y, Cb, Cr: PByte;
  Yi, Ri, Gi, Bi: Integer;
begin
  Y := Source;
  Cb := Source;
  inc(Cb);
  Cr := Source;
  inc(Cr, 2);

  // RGB is layed out in memory as BGR
  B := Dest;
  G := Dest;
  inc(G);
  R := Dest;
  inc(R, 2);
  A := Dest;
  inc(A, 3);

  // Repeat Count times..
  while Count > 0 do
    begin
      // Do the conversion in int
      Yi := FY_toRT[Y^];
      Ri := Yi + FCrtoRT[Cr^] + F__toR;
      Gi := Yi + FCbtoGT[Cb^] + FCrtoGT[Cr^] + F__toG;
      Bi := Yi + FCbtoBT[Cb^] + F__toB;
      R^ := RangeLimitDescale(Ri);
      G^ := RangeLimitDescale(Gi);
      B^ := RangeLimitDescale(Bi);
      A^ := $FF;
      // Advance pointers
      inc(Y, 3);
      inc(Cb, 3);
      inc(Cr, 3);
      inc(R, 4);
      inc(G, 4);
      inc(B, 4);
      inc(A, 4);
      dec(Count);
    end;
end;

{ TTransformYCbCrToGray }

function TTransformYCbCrToGray.DstCellStride: Integer;
begin
  Result := 1;
end;

procedure TTransformYCbCrToGray.Transform(Source, Dest: Pointer; Count: Integer);
var
  G, Y: PByte;
begin
  Y := Source;
  G := Dest;
  // Repeat Count times..
  while Count > 0 do
    begin
      // Do the conversion in int
      G^ := Y^;
      // Advance pointers
      inc(Y, 3);
      inc(G);
      dec(Count);
    end;
end;

{ TTransformYCbCrAToRGB }

function TTransformYCbCrAToBGR.SrcCellStride: Integer;
begin
  Result := 4;
end;

procedure TTransformYCbCrAToBGR.Transform(Source, Dest: Pointer; Count: Integer);
var
  R, G, B, Y, Cb, Cr: PByte;
  Yi, Ri, Gi, Bi: Integer;
begin
  Y := Source;
  Cb := Source;
  inc(Cb);
  Cr := Source;
  inc(Cr, 2);

  // RGB is layed out in memory as BGR
  B := Dest;
  G := Dest;
  inc(G);
  R := Dest;
  inc(R, 2);

  // Repeat Count times..
  while Count > 0 do
    begin
      // Do the conversion in int
      Yi := FY_toRT[Y^];
      Ri := Yi + FCrtoRT[Cr^] + F__toR;
      Gi := Yi + FCbtoGT[Cb^] + FCrtoGT[Cr^] + F__toG;
      Bi := Yi + FCbtoBT[Cb^] + F__toB;
      R^ := RangeLimitDescale(Ri);
      G^ := RangeLimitDescale(Gi);
      B^ := RangeLimitDescale(Bi);
      // Advance pointers
      inc(Y, 4);
      inc(Cb, 4);
      inc(Cr, 4);
      inc(R, 3);
      inc(G, 3);
      inc(B, 3);
      dec(Count);
    end;
end;

{ TTransformYCbCrAToRGBA }

function TTransformYCbCrAToBGRA.DstCellStride: Integer;
begin
  Result := 4;
end;

function TTransformYCbCrAToBGRA.SrcCellStride: Integer;
begin
  Result := 4;
end;

procedure TTransformYCbCrAToBGRA.Transform(Source, Dest: Pointer; Count: Integer);
var
  R, G, B, A, Y, Cb, Cr, YA: PByte;
  Yi, Ri, Gi, Bi: Integer;
begin
  Y := Source;
  Cb := Source;
  inc(Cb);
  Cr := Source;
  inc(Cr, 2);
  YA := Source;
  inc(YA, 3);

  // RGB is layed out in memory as BGR
  B := Dest;
  G := Dest;
  inc(G);
  R := Dest;
  inc(R, 2);
  A := Dest;
  inc(A, 3);

  // Repeat Count times..
  while Count > 0 do
    begin
      // Do the conversion in int
      Yi := FY_toRT[Y^];
      Ri := Yi + FCrtoRT[Cr^] + F__toR;
      Gi := Yi + FCbtoGT[Cb^] + FCrtoGT[Cr^] + F__toG;
      Bi := Yi + FCbtoBT[Cb^] + F__toB;
      R^ := RangeLimitDescale(Ri);
      G^ := RangeLimitDescale(Gi);
      B^ := RangeLimitDescale(Bi);
      A^ := YA^;
      // Advance pointers
      inc(Y, 4);
      inc(Cb, 4);
      inc(Cr, 4);
      inc(YA, 4);
      inc(R, 4);
      inc(G, 4);
      inc(B, 4);
      inc(A, 4);
      dec(Count);
    end;
end;

{ TTransformYCbCrKToRGB }

function TTransformYCbCrKToBGR.SrcCellStride: Integer;
begin
  Result := 4;
end;

procedure TTransformYCbCrKToBGR.Transform(Source, Dest: Pointer; Count: Integer);
var
  R, G, B, Y, Cb, Cr, K: PByte;
  Ci, Mi, Yi, Ki, Ii, Ri, Gi, Bi: Integer;
begin
  Y := Source;
  Cb := Source;
  inc(Cb);
  Cr := Source;
  inc(Cr, 2);
  K := Source;
  inc(K, 3);

  // RGB is layed out in memory as BGR
  B := Dest;
  G := Dest;
  inc(G);
  R := Dest;
  inc(R, 2);

  // Repeat Count times..
  while Count > 0 do
    begin
      // Do the conversion in int
      Ii := FY_toRT[Y^];

      Ci := Ii + FCrtoRT[Cr^] + F__toR;                // cyan
      Mi := Ii + FCbtoGT[Cb^] + FCrtoGT[Cr^] + F__toG; // magenta
      Yi := Ii + FCbtoBT[Cb^] + F__toB;                // yellow
      Ki := 255 * FColorConvScale - FY_toRT[K^];       // black

      // In YCbCrK, the CMYK values must be converted to produce RGB
      // Do the conversion in int
      Ri := 255 * FColorConvScale - Ci - Ki;
      Gi := 255 * FColorConvScale - Mi - Ki;
      Bi := 255 * FColorConvScale - Yi - Ki;

      R^ := RangeLimitDescale(Ri);
      G^ := RangeLimitDescale(Gi);
      B^ := RangeLimitDescale(Bi);

      // Advance pointers
      inc(Y, 4);
      inc(Cb, 4);
      inc(Cr, 4);
      inc(K, 4);
      inc(R, 3);
      inc(G, 3);
      inc(B, 3);
      dec(Count);
    end;
end;

{ TTransformYCbCrKToRGBA }

function TTransformYCbCrKToBGRA.DstCellStride: Integer;
begin
  Result := 4;
end;

function TTransformYCbCrKToBGRA.SrcCellStride: Integer;
begin
  Result := 4;
end;

procedure TTransformYCbCrKToBGRA.Transform(Source, Dest: Pointer; Count: Integer);
var
  R, G, B, A, Y, Cb, Cr, K: PByte;
  Ci, Mi, Yi, Ki, Ii, Ri, Gi, Bi: Integer;
begin
  Y := Source;
  Cb := Source;
  inc(Cb);
  Cr := Source;
  inc(Cr, 2);
  K := Source;
  inc(K, 3);

  // RGB is layed out in memory as BGR
  B := Dest;
  G := Dest;
  inc(G);
  R := Dest;
  inc(R, 2);
  A := Dest;
  inc(A, 3);

  // Repeat Count times..
  while Count > 0 do
    begin
      // Do the conversion in int
      Ii := FY_toRT[Y^];

      Ci := Ii + FCrtoRT[Cr^] + F__toR;                // cyan
      Mi := Ii + FCbtoGT[Cb^] + FCrtoGT[Cr^] + F__toG; // magenta
      Yi := Ii + FCbtoBT[Cb^] + F__toB;                // yellow
      Ki := 255 * FColorConvScale - FY_toRT[K^];       // black

      // In YCbCrK, the CMYK values must be converted to produce RGB
      // Do the conversion in int
      Ri := 255 * FColorConvScale - Ci - Ki;
      Gi := 255 * FColorConvScale - Mi - Ki;
      Bi := 255 * FColorConvScale - Yi - Ki;

      R^ := RangeLimitDescale(Ri);
      G^ := RangeLimitDescale(Gi);
      B^ := RangeLimitDescale(Bi);
      A^ := $FF;

      // Advance pointers
      inc(Y, 4);
      inc(Cb, 4);
      inc(Cr, 4);
      inc(K, 4);
      inc(R, 3);
      inc(G, 4);
      inc(B, 4);
      inc(A, 4);
      dec(Count);
    end;
end;

{ TTransformCMYKToRGB }

function TTransformCMYKToBGR.DstCellStride: Integer;
begin
  Result := 3;
end;

function TTransformCMYKToBGR.SrcCellStride: Integer;
begin
  Result := 4;
end;

procedure TTransformCMYKToBGR.Transform(Source, Dest: Pointer; Count: Integer);
var
  R, G, B, C, M, Y, K: PByte;
  Ri, Gi, Bi: Integer;
  function RangeLimit(A: Integer): Integer;
  begin
    Result := A;
    if Result < 0 then
        Result := 0
    else
      if Result > 255 then
        Result := 255;
  end;

begin
  C := Source;
  M := Source;
  inc(M);
  Y := Source;
  inc(Y, 2);
  K := Source;
  inc(K, 3);

  // RGB is layed out in memory as BGR
  B := Dest;
  G := Dest;
  inc(G);
  R := Dest;
  inc(R, 2);

  // Repeat Count times..
  while Count > 0 do
    begin
      // Do the conversion in int
      Ri := 255 - C^ - K^;
      Gi := 255 - M^ - K^;
      Bi := 255 - Y^ - K^;
      R^ := RangeLimit(Ri);
      G^ := RangeLimit(Gi);
      B^ := RangeLimit(Bi);
      // Advance pointers
      inc(C, 4);
      inc(M, 4);
      inc(Y, 4);
      inc(K, 4);
      inc(R, 3);
      inc(G, 3);
      inc(B, 3);
      dec(Count);
    end;
end;

{ TTransformCMYKToBGR_Adobe }

function TTransformCMYKToBGR_Adobe.DstCellStride: Integer;
begin
  Result := 3;
end;

function TTransformCMYKToBGR_Adobe.SrcCellStride: Integer;
begin
  Result := 4;
end;

procedure TTransformCMYKToBGR_Adobe.Transform(Source, Dest: Pointer; Count: Integer);
// When all in range [0..1]
// CMY -> CMYK                         | CMYK -> CMY
// Black=minimum(Cyan,Magenta,Yellow)  | Cyan=minimum(1,Cyan*(1-Black)+Black)
// Cyan=(Cyan-Black)/(1-Black)         | Magenta=minimum(1,Magenta*(1-Black)+Black)
// Magenta=(Magenta-Black)/(1-Black)   | Yellow=minimum(1,Yellow*(1-Black)+Black)
// Yellow=(Yellow-Black)/(1-Black)     |
// RGB -> CMYK                         | CMYK -> RGB
// Black=minimum(1-Red,1-Green,1-Blue) | Red=1-minimum(1,Cyan*(1-Black)+Black)
// Cyan=(1-Red-Black)/(1-Black)        | Green=1-minimum(1,Magenta*(1-Black)+Black)
// Magenta=(1-Green-Black)/(1-Black)   | Blue=1-minimum(1,Yellow*(1-Black)+Black)
// Yellow=(1-Blue-Black)/(1-Black)     |
var
  R, G, B, C, M, Y, K: PByte;
  Ck, Mk, Yk, Cu, Mu, Yu, Ku: Integer;
  Ri, Gi, Bi: Integer;
  function RangeLimit(A: Integer): Integer;
  begin
    Result := A;
    if Result < 0 then
        Result := 0
    else
      if Result > 255 then
        Result := 255;
  end;

begin
  // CMYK layout
  C := Source;
  M := Source;
  inc(M);
  Y := Source;
  inc(Y, 2);
  K := Source;
  inc(K, 3);

  // RGB is layed out in memory as BGR
  B := Dest;
  G := Dest;
  inc(G);
  R := Dest;
  inc(R, 2);

  // Repeat Count times..
  while Count > 0 do
    begin
      // Original colour channels are inverted: uninvert them here
      Ku := 255 - K^;
      Cu := 255 - C^;
      Mu := 255 - M^;
      Yu := 255 - Y^;

      // CMYK -> CMY
      Ck := (Cu * K^) div 255;
      Mk := (Mu * K^) div 255;
      Yk := (Yu * K^) div 255;

      // CMY -> RGB
      Ri := 255 - (Ck + Ku);
      Gi := 255 - (Mk + Ku);
      Bi := 255 - (Yk + Ku);

      // Range limit
      R^ := RangeLimit(Ri);
      G^ := RangeLimit(Gi);
      B^ := RangeLimit(Bi);

      // Advance pointers
      inc(C, 4);
      inc(M, 4);
      inc(Y, 4);
      inc(K, 4);
      inc(R, 3);
      inc(G, 3);
      inc(B, 3);
      dec(Count);
    end;
end;

{ TTransformCMYKToBGRA_Adobe }

function TTransformCMYKToBGRA_Adobe.DstCellStride: Integer;
begin
  Result := 4;
end;

function TTransformCMYKToBGRA_Adobe.SrcCellStride: Integer;
begin
  Result := 4;
end;

procedure TTransformCMYKToBGRA_Adobe.Transform(Source, Dest: Pointer; Count: Integer);
// When all in range [0..1]
// CMY -> CMYK                         | CMYK -> CMY
// Black=minimum(Cyan,Magenta,Yellow)  | Cyan=minimum(1,Cyan*(1-Black)+Black)
// Cyan=(Cyan-Black)/(1-Black)         | Magenta=minimum(1,Magenta*(1-Black)+Black)
// Magenta=(Magenta-Black)/(1-Black)   | Yellow=minimum(1,Yellow*(1-Black)+Black)
// Yellow=(Yellow-Black)/(1-Black)     |
// RGB -> CMYK                         | CMYK -> RGB
// Black=minimum(1-Red,1-Green,1-Blue) | Red=1-minimum(1,Cyan*(1-Black)+Black)
// Cyan=(1-Red-Black)/(1-Black)        | Green=1-minimum(1,Magenta*(1-Black)+Black)
// Magenta=(1-Green-Black)/(1-Black)   | Blue=1-minimum(1,Yellow*(1-Black)+Black)
// Yellow=(1-Blue-Black)/(1-Black)     |
var
  R, G, B, A, C, M, Y, K: PByte;
  Ck, Mk, Yk, Cu, Mu, Yu, Ku: Integer;
  Ri, Gi, Bi: Integer;
  function RangeLimit(A: Integer): Integer;
  begin
    Result := A;
    if Result < 0 then
        Result := 0
    else
      if Result > 255 then
        Result := 255;
  end;

begin
  // CMYK layout
  C := Source;
  M := Source;
  inc(M);
  Y := Source;
  inc(Y, 2);
  K := Source;
  inc(K, 3);

  // RGB is layed out in memory as BGR
  B := Dest;
  G := Dest;
  inc(G);
  R := Dest;
  inc(R, 2);
  A := Dest;
  inc(A, 3);

  // Repeat Count times..
  while Count > 0 do
    begin
      // Original colour channels are inverted: uninvert them here
      Ku := 255 - K^;
      Cu := 255 - C^;
      Mu := 255 - M^;
      Yu := 255 - Y^;

      // CMYK -> CMY
      Ck := (Cu * K^) div 255;
      Mk := (Mu * K^) div 255;
      Yk := (Yu * K^) div 255;

      // CMY -> RGB
      Ri := 255 - (Ck + Ku);
      Gi := 255 - (Mk + Ku);
      Bi := 255 - (Yk + Ku);

      // Range limit
      R^ := RangeLimit(Bi);
      G^ := RangeLimit(Gi);
      B^ := RangeLimit(Ri);
      A^ := $FF;

      // Advance pointers
      inc(C, 4);
      inc(M, 4);
      inc(Y, 4);
      inc(K, 4);
      inc(R, 4);
      inc(G, 4);
      inc(B, 4);
      inc(A, 4);
      dec(Count);
    end;
end;

{ TTransformYCCKToBGR }

function TTransformYCCKToBGR.SrcCellStride: Integer;
begin
  Result := 4;
end;

procedure TTransformYCCKToBGR.Transform(Source, Dest: Pointer; Count: Integer);
// YCCK is a colorspace where the CMY part of CMYK is first converted to RGB, then
// transformed to YCbCr as usual. The K part is appended without any changes.
// To transform back, we do the YCbCr -> RGB transform, then add K
var
  R, G, B, Y, Cb, Cr, K: PByte;
  Yi, Cu, Mu, Yu, Ko, Kk: Integer;
  function RangeLimit(V: Integer): byte;
  begin
    if V < 0 then
        Result := 0
    else
      if V > 255 then
        Result := 255
    else
        Result := V;
  end;

begin
  Y := Source;
  Cb := Source;
  inc(Cb);
  Cr := Source;
  inc(Cr, 2);
  K := Source;
  inc(K, 3);

  // RGB is layed out in memory as BGR
  B := Dest;
  G := Dest;
  inc(G);
  R := Dest;
  inc(R, 2);

  // Repeat Count times..
  while Count > 0 do
    begin
      // Do the conversion in int
      Yi := FY_toRT[Y^];
      Ko := K^;                           // Inverse of K (K seems to be inverted in the file)
      Kk := (255 - Ko) * FColorConvScale; // Real K, with fixed precision

      // YCbCr converted back to CMY part of CMYK
      Cu := (Yi + FCrtoRT[Cr^] + F__toR);                // =original C of CMYK
      Mu := (Yi + FCbtoGT[Cb^] + FCrtoGT[Cr^] + F__toG); // =original M of CMYK
      Yu := (Yi + FCbtoBT[Cb^] + F__toB);                // =original Y of CMYK

      // CMYK->RGB
      R^ := RangeLimitDescale(255 * FColorConvScale - (Cu * Ko) div 255 - Kk);
      G^ := RangeLimitDescale(255 * FColorConvScale - (Mu * Ko) div 255 - Kk);
      B^ := RangeLimitDescale(255 * FColorConvScale - (Yu * Ko) div 255 - Kk);

      // Advance pointers
      inc(Y, 4);
      inc(Cb, 4);
      inc(Cr, 4);
      inc(K, 4);
      inc(R, 3);
      inc(G, 3);
      inc(B, 3);
      dec(Count);
    end;
end;

{ TTransformYCCKToBGRA }

function TTransformYCCKToBGRA.DstCellStride: Integer;
begin
  Result := 4;
end;

function TTransformYCCKToBGRA.SrcCellStride: Integer;
begin
  Result := 4;
end;

procedure TTransformYCCKToBGRA.Transform(Source, Dest: Pointer; Count: Integer);
// YCCK is a colorspace where the CMY part of CMYK is first converted to RGB, then
// transformed to YCbCr as usual. The K part is appended without any changes.
// To transform back, we do the YCbCr -> RGB transform, then add K
var
  R, G, B, A, Y, Cb, Cr, K: PByte;
  Yi, Cu, Mu, Yu, Ko, Kk: Integer;
  function RangeLimit(V: Integer): byte;
  begin
    if V < 0 then
        Result := 0
    else
      if V > 255 then
        Result := 255
    else
        Result := V;
  end;

begin
  Y := Source;
  Cb := Source;
  inc(Cb);
  Cr := Source;
  inc(Cr, 2);
  K := Source;
  inc(K, 3);

  // RGB is layed out in memory as BGR
  B := Dest;
  G := Dest;
  inc(G);
  R := Dest;
  inc(R, 2);
  A := Dest;
  inc(A, 3);

  // Repeat Count times..
  while Count > 0 do
    begin
      // Do the conversion in int
      Yi := FY_toRT[Y^];
      Ko := K^;                           // Inverse of K (K seems to be inverted in the file)
      Kk := (255 - Ko) * FColorConvScale; // Real K, with fixed precision

      // YCbCr converted back to CMY part of CMYK
      Cu := (Yi + FCrtoRT[Cr^] + F__toR);                // =original C of CMYK
      Mu := (Yi + FCbtoGT[Cb^] + FCrtoGT[Cr^] + F__toG); // =original M of CMYK
      Yu := (Yi + FCbtoBT[Cb^] + F__toB);                // =original Y of CMYK

      // CMYK->RGB
      R^ := RangeLimitDescale(255 * FColorConvScale - (Cu * Ko) div 255 - Kk);
      G^ := RangeLimitDescale(255 * FColorConvScale - (Mu * Ko) div 255 - Kk);
      B^ := RangeLimitDescale(255 * FColorConvScale - (Yu * Ko) div 255 - Kk);
      A^ := $FF;

      // Advance pointers
      inc(Y, 4);
      inc(Cb, 4);
      inc(Cr, 4);
      inc(K, 4);
      inc(R, 4);
      inc(G, 4);
      inc(B, 4);
      inc(A, 4);
      dec(Count);
    end;
end;

constructor TTransformYCCKToBGR_Adobe.Create;
begin
  inherited Create;
  InitConst;
end;

{ TTransformYCCKToBGR_Adobe }

procedure TTransformYCCKToBGR_Adobe.InitConst;
begin
  // YCCK to RGB for Adobe images is different. First, the Y, Cr and Cb are inverted,
  // and k* = 220 - K. The normal YCbCr to RGB is then applied. As a last step,
  // the values are scaled by 0.65 around 128
  { float k = 220 - K[i], y = 255 - Y[i], cb = 255 - Cb[i], cr = 255 - Cr[i];

    double val = y + 1.402 * (cr - 128) - k;
    val = (val - 128) * .65f + 128;
    R = val < 0.0 ? (byte) 0 : val > 255.0 ? (byte) 0xff : (byte) (val + 0.5);

    val = y - 0.34414 * (cb - 128) - 0.71414 * (cr - 128) - k;
    val = (val - 128) * .65f + 128;
    G = val < 0.0 ? (byte) 0 : val > 255.0 ? (byte) 0xff : (byte) (val + 0.5);

    val = y + 1.772 * (cb - 128) - k;
    val = (val - 128) * .65f + 128;
    B = val < 0.0 ? (byte) 0 : val > 255.0 ? (byte) 0xff : (byte) (val + 0.5);

    X* = (X - 128) * 0.65 + 128 <=>
    X* = X * 0.65 + 128 - 128 * 0.65 <=>
    X* = X * 0.65 + 44.8
  }
  F0_65 := Round(0.65 * FColorConvScale);
  F44_8 := Round(44.8 * FColorConvScale); // 128 - 0.65 * 128
end;

function TTransformYCCKToBGR_Adobe.SrcCellStride: Integer;
begin
  Result := 4;
end;

procedure TTransformYCCKToBGR_Adobe.Transform(Source, Dest: Pointer;
  Count: Integer);
var
  R, G, B, Y, Cb, Cr, K: PByte;
  Yi, Ki, Ri, Gi, Bi, Cbi, Cri: Integer;
  function ScaleAndRangeLimit(A: Integer): Integer;
  begin
    // First the scaling
    A := (A * F0_65) div FColorConvScale + F44_8;
    // Undo fixed precision and range limit
    Result := A div FColorConvScale;
    if Result < 0 then
        Result := 0
    else
      if Result > 255 then
        Result := 255;
  end;

begin
  Y := Source;
  Cb := Source;
  inc(Cb);
  Cr := Source;
  inc(Cr, 2);
  K := Source;
  inc(K, 3);
  // RGB is layed out in memory as BGR
  B := Dest;
  G := Dest;
  inc(G);
  R := Dest;
  inc(R, 2);
  // Repeat Count times..
  while Count > 0 do
    begin
      // Do the conversion in int
      Yi := FY_toRT[255 - Y^];
      Cbi := 255 - Cb^;
      Cri := 255 - Cr^;
      Ki := (220 - K^) * FColorConvScale;
      Ri := Yi + FCrtoRT[Cri] + F__toR - Ki;
      Gi := Yi + FCbtoGT[Cbi] + FCrtoGT[Cri] + F__toG - Ki;
      Bi := Yi + FCbtoBT[Cbi] + F__toB - Ki;
      R^ := ScaleAndRangeLimit(Ri);
      G^ := ScaleAndRangeLimit(Gi);
      B^ := ScaleAndRangeLimit(Bi);
      // Advance pointers
      inc(Y, 4);
      inc(Cb, 4);
      inc(Cr, 4);
      inc(K, 4);
      inc(R, 3);
      inc(G, 3);
      inc(B, 3);
      dec(Count);
    end;
end;

{ TTransformGrayToBGR }

function TTransformGrayToBGR.DstCellStride: Integer;
begin
  Result := 3;
end;

function TTransformGrayToBGR.SrcCellStride: Integer;
begin
  Result := 1;
end;

procedure TTransformGrayToBGR.Transform(Source, Dest: Pointer; Count: Integer);
var
  R, G, B, Y: PByte;
begin
  Y := Source;

  // RGB is layed out in memory as BGR
  B := Dest;
  G := Dest;
  inc(G);
  R := Dest;
  inc(R, 2);

  // Repeat Count times..
  while Count > 0 do
    begin
      R^ := Y^;
      G^ := Y^;
      B^ := Y^;
      // Advance pointers
      inc(Y, 1);
      inc(R, 3);
      inc(G, 3);
      inc(B, 3);
      dec(Count);
    end;
end;

{ TTransformGrayToBGRA }

function TTransformGrayToBGRA.DstCellStride: Integer;
begin
  Result := 4;
end;

function TTransformGrayToBGRA.SrcCellStride: Integer;
begin
  Result := 1;
end;

procedure TTransformGrayToBGRA.Transform(Source, Dest: Pointer; Count: Integer);
var
  R, G, B, A, Y: PByte;
begin
  Y := Source;

  // RGB is layed out in memory as BGR
  B := Dest;
  G := Dest;
  inc(G);
  R := Dest;
  inc(R, 2);
  A := Dest;
  inc(A, 3);

  // Repeat Count times..
  while Count > 0 do
    begin
      R^ := Y^;
      G^ := Y^;
      B^ := Y^;
      A^ := $FF;
      // Advance pointers
      inc(Y, 1);
      inc(R, 4);
      inc(G, 4);
      inc(B, 4);
      inc(A, 4);
      dec(Count);
    end;
end;

{ TTransformGrayAToBGR }

function TTransformGrayAToBGR.DstCellStride: Integer;
begin
  Result := 3;
end;

function TTransformGrayAToBGR.SrcCellStride: Integer;
begin
  Result := 2;
end;

procedure TTransformGrayAToBGR.Transform(Source, Dest: Pointer; Count: Integer);
var
  R, G, B, Y: PByte;
begin
  Y := Source;

  // RGB is layed out in memory as BGR
  B := Dest;
  G := Dest;
  inc(G);
  R := Dest;
  inc(R, 2);

  // Repeat Count times..
  while Count > 0 do
    begin
      R^ := Y^;
      G^ := Y^;
      B^ := Y^;
      // Advance pointers
      inc(Y, 2);
      inc(R, 3);
      inc(G, 3);
      inc(B, 3);
      dec(Count);
    end;
end;

{ TTransformGrayAToBGRA }

function TTransformGrayAToBGRA.DstCellStride: Integer;
begin
  Result := 4;
end;

function TTransformGrayAToBGRA.SrcCellStride: Integer;
begin
  Result := 2;
end;

procedure TTransformGrayAToBGRA.Transform(Source, Dest: Pointer; Count: Integer);
var
  R, G, B, A, Y, YA: PByte;
begin
  Y := Source;
  YA := Source;
  inc(YA);

  // RGB is layed out in memory as BGR
  B := Dest;
  G := Dest;
  inc(G);
  R := Dest;
  inc(R, 2);
  A := Dest;
  inc(A, 3);

  // Repeat Count times..
  while Count > 0 do
    begin
      R^ := Y^;
      G^ := Y^;
      B^ := Y^;
      A^ := YA^;
      // Advance pointers
      inc(Y, 2);
      inc(YA, 2);
      inc(R, 4);
      inc(G, 4);
      inc(B, 4);
      inc(A, 4);
      dec(Count);
    end;
end;

{ TTransformRGBToBGRA }

function TTransformRGBToBGRA.DstCellStride: Integer;
begin
  Result := 4;
end;

function TTransformRGBToBGRA.SrcCellStride: Integer;
begin
  Result := 3;
end;

procedure TTransformRGBToBGRA.Transform(Source, Dest: Pointer; Count: Integer);
var
  R, G, B, A, Rs, Gs, Bs: PByte;
begin
  Rs := Source;
  Gs := Source;
  inc(Gs);
  Bs := Source;
  inc(Bs, 2);

  // RGB is layed out in memory as BGR
  B := Dest;
  G := Dest;
  inc(G);
  R := Dest;
  inc(R, 2);
  A := Dest;
  inc(A, 3);

  // Repeat Count times..
  while Count > 0 do
    begin
      R^ := Rs^;
      G^ := Gs^;
      B^ := Bs^;
      A^ := $FF;
      // Advance pointers
      inc(Rs, 3);
      inc(Gs, 3);
      inc(Bs, 3);
      inc(R, 4);
      inc(G, 4);
      inc(B, 4);
      inc(A, 4);
      dec(Count);
    end;
end;

{ TTransformRGBAToBGR }

function TTransformRGBAToBGR.DstCellStride: Integer;
begin
  Result := 3;
end;

function TTransformRGBAToBGR.SrcCellStride: Integer;
begin
  Result := 4;
end;

procedure TTransformRGBAToBGR.Transform(Source, Dest: Pointer; Count: Integer);
var
  R, G, B, Rs, Gs, Bs: PByte;
begin
  Rs := Source;
  Gs := Source;
  inc(Gs);
  Bs := Source;
  inc(Bs, 2);

  // RGB is layed out in memory as BGR
  B := Dest;
  G := Dest;
  inc(G);
  R := Dest;
  inc(R, 2);

  // Repeat Count times..
  while Count > 0 do
    begin
      R^ := Rs^;
      G^ := Gs^;
      B^ := Bs^;
      // Advance pointers
      inc(Rs, 4);
      inc(Gs, 4);
      inc(Bs, 4);
      inc(R, 3);
      inc(G, 3);
      inc(B, 3);
      dec(Count);
    end;
end;

constructor TTransformBGRAToBGR.Create;
begin
  inherited;
  FBkColor := $FFFFFFFF;
end;

function TTransformBGRAToBGR.DstCellStride: Integer;
begin
  Result := 3;
end;

function TTransformBGRAToBGR.GetBkColor: Cardinal;
begin
  Result := FBkColor and $00FFFFFF;
end;

procedure TTransformBGRAToBGR.SetBkColor(const Value: Cardinal);
begin
  FBkColor := Value or $FF000000;
end;

function TTransformBGRAToBGR.SrcCellStride: Integer;
begin
  Result := 4;
end;

procedure TTransformBGRAToBGR.Transform(Source, Dest: Pointer; Count: Integer);
var
  T: Integer;
  R, G, B, A, Rd, Gd, Bd: PByte;
  Rb, Gb, Bb: byte;
begin
  // ARGB source is layed out in memory as BGRA
  B := Source;
  G := Source;
  inc(G);
  R := Source;
  inc(R, 2);
  A := Source;
  inc(A, 3);

  // RGB dest is layed out in memory as BGR
  Bd := Dest;
  Gd := Dest;
  inc(Gd);
  Rd := Dest;
  inc(Rd, 2);

  // Background colors
  Rb := FBkColor and $000000FF;
  Gb := (FBkColor and $0000FF00) shr 8;
  Bb := (FBkColor and $00FF0000) shr 16;

  // Repeat Count times..
  while Count > 0 do
    begin
      if A^ = 0 then
        begin
          // Fully transparent: background color
          Rd^ := Rb;
          Gd^ := Gb;
          Bd^ := Bb;
        end
      else
        begin
          if A^ = 255 then
            begin
              // Fully opaque: foreground color
              Rd^ := R^;
              Gd^ := G^;
              Bd^ := B^;
            end
          else
            begin
              // Semi-transparent: "Src over Dst" operator (Porter-Duff),
              // for pre-multiplied colors, unrolled for speed
              T := A^ * Rb + $80;
              Rd^ := R^ + Rb - (T shr 8 + T) shr 8;
              T := A^ * Gb + $80;
              Gd^ := G^ + Gb - (T shr 8 + T) shr 8;
              T := A^ * Bb + $80;
              Bd^ := B^ + Bb - (T shr 8 + T) shr 8;
            end;
        end;

      // Advance pointers
      inc(R, 4);
      inc(G, 4);
      inc(B, 4);
      inc(A, 4);
      inc(Rd, 3);
      inc(Gd, 3);
      inc(Bd, 3);
      dec(Count);
    end;
end;

constructor TJfifInvTransform.Create;
begin
  inherited Create;
  InitRGBToYCbCrTables;
end;

{ TJfifInvTransform }

procedure TJfifInvTransform.InitRGBToYCbCrTables;
{ RGB to YCbCr conversion: These constants come from JFIF spec

  Y =    0.299  R + 0.587  G + 0.114  B
  Cb = - 0.1687 R - 0.3313 G + 0.5    B + 128
  Cr =   0.5    R - 0.4187 G - 0.0813 B + 128
}
var
  i: Integer;
begin
  F__toCb := Round(128 * FColorConvScale);
  F__toCr := Round(128 * FColorConvScale);
  for i := 0 to 255 do
    begin
      FRtoY_[i] := Round(0.299 * FColorConvScale * i);
      FGtoY_[i] := Round(0.587 * FColorConvScale * i);
      FBtoY_[i] := Round(0.114 * FColorConvScale * i);
      FRtoCb[i] := Round(-0.1687 * FColorConvScale * i);
      FGtoCb[i] := Round(-0.3313 * FColorConvScale * i);
      FBtoCb[i] := Round(0.5 * FColorConvScale * i);
      FRtoCr[i] := Round(0.5 * FColorConvScale * i);
      FGtoCr[i] := Round(-0.4187 * FColorConvScale * i);
      FBtoCr[i] := Round(-0.0813 * FColorConvScale * i);
    end;
end;

{ TTransformBGRToYCbCr }

procedure TTransformBGRToYCbCr.Transform(Source, Dest: Pointer; Count: Integer);
var
  R, G, B, Y, Cb, Cr: PByte;
  Ri, Gi, Bi: Integer;
begin
  // DoDebugOut(Self, wsInfo, PFormat('source=%d, count=%d, nulling...(test)', [integer(Source), Count]));
  // RGB is layed out in memory as BGR
  B := Source;
  G := Source;
  inc(G);
  R := Source;
  inc(R, 2);

  Y := Dest;
  Cb := Dest;
  inc(Cb);
  Cr := Dest;
  inc(Cr, 2);

  // Repeat Count times..
  while Count > 0 do
    begin
      // Do the conversion in int
      Ri := R^;
      Gi := G^;
      Bi := B^;
      Y^ := RangeLimitDescale(FRtoY_[Ri] + FGtoY_[Gi] + FBtoY_[Bi]);
      Cb^ := RangeLimitDescale(FRtoCb[Ri] + FGtoCb[Gi] + FBtoCb[Bi] + F__toCb);
      Cr^ := RangeLimitDescale(FRtoCr[Ri] + FGtoCr[Gi] + FBtoCr[Bi] + F__toCr);
      // Advance pointers
      inc(R, 3);
      inc(G, 3);
      inc(B, 3);
      inc(Y, 3);
      inc(Cb, 3);
      inc(Cr, 3);
      dec(Count);
    end;
end;

{ TTransformBGRToGray }

function TTransformBGRToGray.DstCellStride: Integer;
begin
  Result := 1;
end;

procedure TTransformBGRToGray.Transform(Source, Dest: Pointer; Count: Integer);
var
  R, G, B, Y: PByte;
begin
  // RGB is layed out in memory as BGR
  B := Source;
  G := Source;
  inc(G);
  R := Source;
  inc(R, 2);

  Y := Dest;
  // Repeat Count times..
  while Count > 0 do
    begin
      // Do the conversion in int
      Y^ := RangeLimitDescale(FRtoY_[R^] + FGtoY_[G^] + FBtoY_[B^]);
      // Advance pointers
      inc(R, 3);
      inc(G, 3);
      inc(B, 3);
      inc(Y, 1);
      dec(Count);
    end;
end;

{ TTransformBGRToGray }

function TTransformBGRToGrayA.DstCellStride: Integer;
begin
  Result := 2;
end;

function TTransformBGRToGrayA.SrcCellStride: Integer;
begin
  Result := 3;
end;

procedure TTransformBGRToGrayA.Transform(Source, Dest: Pointer; Count: Integer);
var
  R, G, B, Y, YA: PByte;
begin
  // RGB is layed out in memory as BGR
  B := Source;
  G := Source;
  inc(G);
  R := Source;
  inc(R, 2);

  Y := Dest;
  YA := Dest;
  inc(YA);
  // Repeat Count times..
  while Count > 0 do
    begin
      // Do the conversion in int
      Y^ := RangeLimitDescale(FRtoY_[R^] + FGtoY_[G^] + FBtoY_[B^]);
      YA^ := $FF;
      // Advance pointers
      inc(R, 3);
      inc(G, 3);
      inc(B, 3);
      inc(Y, 2);
      inc(YA, 2);
      dec(Count);
    end;
end;

{ TTransformBGRAToGray }

function TTransformBGRAToGray.DstCellStride: Integer;
begin
  Result := 1;
end;

function TTransformBGRAToGray.SrcCellStride: Integer;
begin
  Result := 4;
end;

procedure TTransformBGRAToGray.Transform(Source, Dest: Pointer; Count: Integer);
var
  R, G, B, Y: PByte;
begin
  // RGB is layed out in memory as BGR
  B := Source;
  G := Source;
  inc(G);
  R := Source;
  inc(R, 2);

  Y := Dest;
  // Repeat Count times..
  while Count > 0 do
    begin
      // Do the conversion in int
      Y^ := RangeLimitDescale(FRtoY_[R^] + FGtoY_[G^] + FBtoY_[B^]);
      // Advance pointers
      inc(R, 4);
      inc(G, 4);
      inc(B, 4);
      inc(Y, 1);
      dec(Count);
    end;
end;

{ TTransformBGRAToGray }

function TTransformBGRAToGrayA.DstCellStride: Integer;
begin
  Result := 2;
end;

function TTransformBGRAToGrayA.SrcCellStride: Integer;
begin
  Result := 4;
end;

procedure TTransformBGRAToGrayA.Transform(Source, Dest: Pointer; Count: Integer);
var
  R, G, B, A, Y, YA: PByte;
begin
  // RGB is layed out in memory as BGR
  B := Source;
  G := Source;
  inc(G);
  R := Source;
  inc(R, 2);
  A := Source;
  inc(A, 3);

  Y := Dest;
  YA := Dest;
  inc(YA);
  // Repeat Count times..
  while Count > 0 do
    begin
      // Do the conversion in int
      Y^ := RangeLimitDescale(FRtoY_[R^] + FGtoY_[G^] + FBtoY_[B^]);
      YA^ := A^;
      // Advance pointers
      inc(R, 4);
      inc(G, 4);
      inc(B, 4);
      inc(A, 4);
      inc(Y, 2);
      inc(YA, 2);
      dec(Count);
    end;
end;

{ TTransformBGRAToYCbCrA }

function TTransformBGRAToYCbCrA.DstCellStride: Integer;
begin
  Result := 4;
end;

function TTransformBGRAToYCbCrA.SrcCellStride: Integer;
begin
  Result := 4;
end;

procedure TTransformBGRAToYCbCrA.Transform(Source, Dest: Pointer; Count: Integer);
var
  R, G, B, A, Y, Cb, Cr, Ay: PByte;
  Ri, Gi, Bi: Integer;
begin
  // RGB is layed out in memory as BGR
  B := Source;
  G := Source;
  inc(G);
  R := Source;
  inc(R, 2);
  A := Source;
  inc(A, 3);

  Y := Dest;
  Cb := Dest;
  inc(Cb);
  Cr := Dest;
  inc(Cr, 2);
  Ay := Dest;
  inc(Ay, 3);

  // Repeat Count times..
  while Count > 0 do
    begin
      // Do the conversion in int
      Ri := R^;
      Gi := G^;
      Bi := B^;
      Y^ := RangeLimitDescale(FRtoY_[Ri] + FGtoY_[Gi] + FBtoY_[Bi]);
      Cb^ := RangeLimitDescale(FRtoCb[Ri] + FGtoCb[Gi] + FBtoCb[Bi] + F__toCb);
      Cr^ := RangeLimitDescale(FRtoCr[Ri] + FGtoCr[Gi] + FBtoCr[Bi] + F__toCr);
      Ay^ := A^;
      // Advance pointers
      inc(R, 4);
      inc(G, 4);
      inc(B, 4);
      inc(A, 4);
      inc(Y, 4);
      inc(Cb, 4);
      inc(Cr, 4);
      inc(Ay, 4);
      dec(Count);
    end;
end;

{ TTransformBGRAToYCbCr }

function TTransformBGRAToYCbCr.SrcCellStride: Integer;
begin
  Result := 4;
end;

procedure TTransformBGRAToYCbCr.Transform(Source, Dest: Pointer; Count: Integer);
var
  R, G, B, Y, Cb, Cr: PByte;
  Ri, Gi, Bi: Integer;
begin
  // RGB is layed out in memory as BGR
  B := Source;
  G := Source;
  inc(G);
  R := Source;
  inc(R, 2);

  Y := Dest;
  Cb := Dest;
  inc(Cb);
  Cr := Dest;
  inc(Cr, 2);

  // Repeat Count times..
  while Count > 0 do
    begin
      // Do the conversion in int
      Ri := R^;
      Gi := G^;
      Bi := B^;
      Y^ := RangeLimitDescale(FRtoY_[Ri] + FGtoY_[Gi] + FBtoY_[Bi]);
      Cb^ := RangeLimitDescale(FRtoCb[Ri] + FGtoCb[Gi] + FBtoCb[Bi] + F__toCb);
      Cr^ := RangeLimitDescale(FRtoCr[Ri] + FGtoCr[Gi] + FBtoCr[Bi] + F__toCr);
      // Advance pointers
      inc(R, 4);
      inc(G, 4);
      inc(B, 4);
      inc(Y, 3);
      inc(Cb, 3);
      inc(Cr, 3);
      dec(Count);
    end;
end;

{ TTransformCIELabToBGR }

constructor TTransformCIELabToBGR.Create;
begin
  inherited;
  // White point: Defaults to D65 (as recommended by CCIR XA/11)
  FXw := 0.9505;
  FYw := 1.0;
  FZw := 1.0890;
  // Range
  FAmin := -100;
  FAmax := 100;
  FBmin := -100;
  FBmax := 100;
  // Offset
  FAofs := 128;
  FBofs := 128;
end;

procedure TTransformCIELabToBGR.Transform(Source, Dest: Pointer; Count: Integer);
// Limit to interval [0..1]
  function Limit(X: Double): Double; inline;
  begin
    Result := X;
    if Result < 0 then
        Result := 0
    else if Result > 1 then
        Result := 1;
  end;
  function RangeLimitDescale(X: Double): Integer; inline;
  begin
    Result := Round(X * 255);
    if Result < 0 then
        Result := 0
    else if Result > 255 then
        Result := 255;
  end;
  function GFunc(X: Double): Double; inline;
  // See PDF spec, section 4.5
  begin
    if X >= 6 / 29 then
        Result := X * X * X
    else
        Result := (108 / 841) * (X - (4 / 29));
  end;
// sRGB gamma function
  function Gamma(X: Double): Double; inline;
  begin
    if X < 0.0031308 then
        Result := 12.92 * X
    else
        Result := 1.055 * Power(X, 1.0 / 2.4) - 0.055;
  end;

var
  Lb, Ab, Bb: PByte;
  Ld, Ad, Bd, L, M, N, X, Y, Z, R, G, B: Double;
  Rf, Gf, Bf: PByte;

begin
  // CIE Lab
  Lb := Source;
  Ab := Source;
  inc(Ab);
  Bb := Source;
  inc(Bb, 2);

  // RGB is layed out in memory as BGR
  Bf := Dest;
  Gf := Dest;
  inc(Gf);
  Rf := Dest;
  inc(Rf, 2);

  // Repeat Count times..
  while Count > 0 do
    begin
      // First stage: adjust range
      Ld := Lb^ * (100 / 255);

      Ad := Ab^ - FAofs;
      { if Ad < FAmin then
        Ad := FAmin
        else
        if Ad > FAmax then
        Ad := FAmax; }

      Bd := Bb^ - FBofs;
      { if Bd < FBmin then
        Bd := FBmin
        else
        if Bd > FBmax then
        Bd := FBmax; }

      // Second stage: calculate LMN
      L := (Ld + 16) / 116 + Ad / 500;
      M := (Ld + 16) / 116;
      N := (Ld + 16) / 116 - Bd / 200;

      // Third stage: calculate XYZ
      X := FXw * GFunc(L);
      Y := FYw * GFunc(M);
      Z := FZw * GFunc(N);
      { X := Limit(X);
        Y := Limit(Y);
        Z := Limit(Z); }

      // Fourth stage: calculate sRGB:
      // XYZ to RGB matrix for sRGB, D65.. see
      // http://www.brucelindbloom.com/index.html?ColorCalculator.html

      R := 3.24071 * X + -1.53726 * Y + -0.498571 * Z;
      G := -0.969258 * X + 1.87599 * Y + 0.0415557 * Z;
      B := 0.0556352 * X + -0.203996 * Y + 1.05707 * Z;

      // Correct to sRGB
      R := Gamma(R);
      G := Gamma(G);
      B := Gamma(B);

      // Final stage: convert to RGB and limit
      Rf^ := RangeLimitDescale(R);
      Gf^ := RangeLimitDescale(G);
      Bf^ := RangeLimitDescale(B);

      // Advance pointers
      inc(Lb, 3);
      inc(Ab, 3);
      inc(Bb, 3);
      inc(Rf, 3);
      inc(Gf, 3);
      inc(Bf, 3);
      dec(Count);
    end;
end;

{ TTransformITUCIELabToBGR }

constructor TTransformITUCIELabToBGR.Create;
begin
  inherited;
  // Range
  { FAmin := -21760/255;
    FAmax :=  21590/255;
    FBmin := -19200/255;
    FBmax :=  31800/255; }
  // Offset
  FAofs := 128;
  FBofs := 96;
end;

{ TTransformCIELabToBGR }

constructor TTransformCIELabToBGRA.Create;
begin
  inherited;
  // White point: Defaults to D65 (as recommended by CCIR XA/11)
  FXw := 0.9505;
  FYw := 1.0;
  FZw := 1.0890;
  // Range
  FAmin := -100;
  FAmax := 100;
  FBmin := -100;
  FBmax := 100;
  // Offset
  FAofs := 128;
  FBofs := 128;
end;

procedure TTransformCIELabToBGRA.Transform(Source, Dest: Pointer; Count: Integer);
// Limit to interval [0..1]
  function Limit(X: Double): Double; inline;
  begin
    Result := X;
    if Result < 0 then
        Result := 0
    else if Result > 1 then
        Result := 1;
  end;
  function RangeLimitDescale(X: Double): Integer; inline;
  begin
    Result := Round(X * 255);
    if Result < 0 then
        Result := 0
    else if Result > 255 then
        Result := 255;
  end;
  function GFunc(X: Double): Double; inline;
  // See PDF spec, section 4.5
  begin
    if X >= 6 / 29 then
        Result := X * X * X
    else
        Result := (108 / 841) * (X - (4 / 29));
  end;
// sRGB gamma function
  function Gamma(X: Double): Double; inline;
  begin
    if X < 0.0031308 then
        Result := 12.92 * X
    else
        Result := 1.055 * Power(X, 1.0 / 2.4) - 0.055;
  end;

var
  Lb, Ab, Bb: PByte;
  Ld, Ad, Bd, L, M, N, X, Y, Z, R, G, B: Double;
  Rf, Gf, Bf, Af: PByte;
begin
  // CIE Lab
  Lb := Source;
  Ab := Source;
  inc(Ab);
  Bb := Source;
  inc(Bb, 2);

  // RGB is layed out in memory as BGR
  Bf := Dest;
  Gf := Dest;
  inc(Gf);
  Rf := Dest;
  inc(Rf, 2);
  Af := Dest;
  inc(Af, 3);

  // Repeat Count times..
  while Count > 0 do
    begin
      // First stage: adjust range
      Ld := Lb^ * (100 / 255);

      Ad := Ab^ - FAofs;
      { if Ad < FAmin then
        Ad := FAmin
        else
        if Ad > FAmax then
        Ad := FAmax; }

      Bd := Bb^ - FBofs;
      { if Bd < FBmin then
        Bd := FBmin
        else
        if Bd > FBmax then
        Bd := FBmax; }

      // Second stage: calculate LMN
      L := (Ld + 16) / 116 + Ad / 500;
      M := (Ld + 16) / 116;
      N := (Ld + 16) / 116 - Bd / 200;

      // Third stage: calculate XYZ
      X := FXw * GFunc(L);
      Y := FYw * GFunc(M);
      Z := FZw * GFunc(N);
      { X := Limit(X);
        Y := Limit(Y);
        Z := Limit(Z); }

      // Fourth stage: calculate sRGB:
      // XYZ to RGB matrix for sRGB, D65.. see
      // http://www.brucelindbloom.com/index.html?ColorCalculator.html

      R := 3.24071 * X + -1.53726 * Y + -0.498571 * Z;
      G := -0.969258 * X + 1.87599 * Y + 0.0415557 * Z;
      B := 0.0556352 * X + -0.203996 * Y + 1.05707 * Z;

      // Correct to sRGB
      R := Gamma(R);
      G := Gamma(G);
      B := Gamma(B);

      // Final stage: convert to RGB and limit
      Rf^ := RangeLimitDescale(R);
      Gf^ := RangeLimitDescale(G);
      Bf^ := RangeLimitDescale(B);
      Af^ := $FF;

      // Advance pointers
      inc(Lb, 3);
      inc(Ab, 3);
      inc(Bb, 3);
      inc(Rf, 4);
      inc(Gf, 4);
      inc(Bf, 4);
      inc(Af, 4);
      dec(Count);
    end;
end;

{ TTransformITUCIELabToBGR }

constructor TTransformITUCIELabToBGRA.Create;
begin
  inherited;
  // Range
  { FAmin := -21760/255;
    FAmax :=  21590/255;
    FBmin := -19200/255;
    FBmax :=  31800/255; }
  // Offset
  FAofs := 128;
  FBofs := 96;
end;

constructor TYuvTransform.Create;
begin
  inherited;
  InitScaleFact;
end;

{ TYuvTransform }

function TYuvTransform.Clip(Value_: Integer): Integer;
// Clip the value to the allowed range
begin
  if Value_ < 0 then
      Result := 0
  else
    if Value_ >= FMaxValue then
      Result := FMaxValue
  else
      Result := Value_;
end;

function TYuvTransform.DstCellStride: Integer;
begin
  Result := 3;
end;

procedure TYuvTransform.InitScaleFact;
begin
  FPrecision := 10;
  FScaleFact := 1 shl FPrecision;
  FBias := (FPrecision - 1);
  FMaxValue := $FF * FScaleFact;
end;

function TYuvTransform.SrcCellStride: Integer;
begin
  Result := 3;
end;

{ TYuvFwdTransform }

constructor TYuvFwdTransform.Create;
begin
  inherited;
  InitTables;
end;

procedure TYuvFwdTransform.InitTables;
{ Formulae:

  Y =  0.257R + 0.504G + 0.098B + 16
  U = -0.148R - 0.291G + 0.439B + 128
  V =  0.439R - 0.368G - 0.071B + 128

  G = 1.164(Y-16) - 0.391(U-128) -0.813(V-128)
  R = 1.164(Y-16) +               1.596(V-128)
  B = 1.164(Y-16) + 2.018(U-128)

  or

  R = 1.164Y           + 1.596Cr - 222.912
  G = 1.164Y - 0.391Cb - 0.813Cr + 135.488
  B = 1.164Y + 2.018Cb           - 276.928

  R, G and B range from 0 to 255.
  Y ranges from 16 to 235.
  Cb and Cr range from 16 to 240.

  These values are NOT the ones of the JFIF spec
}
var
  i: Integer;
begin
  // Conversion tables
  for i := 0 to 255 do
    begin
      // YUV to RGB
      FYtoR[i] := Round((1.164 * i - 222.912) * FScaleFact);
      FYtoG[i] := Round((1.164 * i + 135.488) * FScaleFact);
      FYtoB[i] := Round((1.164 * i - 276.928) * FScaleFact);
      FUtoG[i] := Round(-0.391 * i * FScaleFact);
      FUtoB[i] := Round(2.018 * i * FScaleFact);
      FVtoR[i] := Round(1.596 * i * FScaleFact);
      FVtoG[i] := Round(-0.813 * i * FScaleFact);
    end;
end;

{ TYuvInvTransform }

constructor TYuvInvTransform.Create;
begin
  inherited;
  InitTables;
end;

procedure TYuvInvTransform.InitTables;
var
  i: Integer;
begin
  for i := 0 to 255 do
    begin
      // RGB to YUV
      FYfromR[i] := Round((0.257 * i + 16) * FScaleFact);
      FUfromR[i] := Round((-0.148 * i + 128) * FScaleFact);
      FVfromR[i] := Round((0.439 * i + 128) * FScaleFact);
      FYfromG[i] := Round(0.504 * i * FScaleFact);
      FUfromG[i] := Round(-0.291 * i * FScaleFact);
      FVfromG[i] := Round(-0.368 * i * FScaleFact);
      FYfromB[i] := Round(0.098 * i * FScaleFact);
      FUfromB[i] := Round(0.439 * i * FScaleFact);
      FVfromB[i] := Round(-0.071 * i * FScaleFact);
    end;
end;

{ TTransformYUVToRGB }

procedure TTransformYUVToRGB.Transform(Source, Dest: Pointer;
  Count: Integer);
// Convert a list of count colors in 8bpc from YUV to RGB
var
  i: Integer;
  Ri, Gi, Bi: Integer;
  R, G, B, Y, U, V: PByte;
begin
  // Setup pointers
  Y := Source;
  U := Y;
  inc(U);
  V := U;
  inc(V);
  R := Dest;
  G := R;
  inc(G);
  B := G;
  inc(B);
  // Loop through list
  for i := 0 to Count - 1 do
    begin
      Ri := Clip(FYtoR[Y^] + FVtoR[V^] + FBias);
      Gi := Clip(FYtoG[Y^] + FUtoG[U^] + FVtoG[V^] + FBias);
      Bi := Clip(FYtoB[Y^] + FUtoB[U^] + FBias);
      R^ := Ri shr FPrecision;
      G^ := Gi shr FPrecision;
      B^ := Bi shr FPrecision;
      inc(Y, 3);
      inc(U, 3);
      inc(V, 3);
      inc(R, 3);
      inc(G, 3);
      inc(B, 3);
    end;
end;

{ TTransformYUVToCMYK }

function TTransformYUVToCMYK.DstCellStride: Integer;
begin
  Result := 4;
end;

procedure TTransformYUVToCMYK.Transform(Source, Dest: Pointer;
  Count: Integer);
var
  i: Integer;
  Ri, Gi, Bi: Integer;
  C, M, L, Y, U, V: PByte;
begin
  // Setup pointers
  Y := Source;
  U := Y;
  inc(U);
  V := U;
  inc(V);
  C := Dest;
  M := C;
  inc(M);
  L := M;
  inc(L);
  // Loop through list
  for i := 0 to Count - 1 do
    begin
      Ri := Clip(FYtoR[Y^] + FVtoR[V^] + FBias);
      Gi := Clip(FYtoG[Y^] + FUtoG[U^] + FVtoG[V^] + FBias);
      Bi := Clip(FYtoB[Y^] + FUtoB[U^] + FBias);
      C^ := Ri shr FPrecision xor $FF; // Cyan    = 255 - Red
      M^ := Gi shr FPrecision xor $FF; // Magenta = 255 - Green
      L^ := Bi shr FPrecision xor $FF; // Yellow  = 255 - Blue
      // We leave the K channel untouched
      inc(Y, 4);
      inc(U, 4);
      inc(V, 4);
      inc(C, 4);
      inc(M, 4);
      inc(L, 4);
    end;
end;

{ TTransformRGBToYUV }

procedure TTransformRGBToYUV.Transform(Source, Dest: Pointer;
  Count: Integer);
// Convert a list of count colors in 8bpc from RGB to YUV
var
  i: Integer;
  Yi, Ui, Vi: Integer;
  R, G, B, Y, U, V: PByte;
begin
  // Setup pointers
  R := Source;
  G := R;
  inc(G);
  B := G;
  inc(B);
  Y := Dest;
  U := Y;
  inc(U);
  V := U;
  inc(V);
  // loop through list
  for i := 0 to Count - 1 do
    begin
      Yi := Clip(FYfromR[R^] + FYfromG[G^] + FYfromB[B^] + FBias);
      Ui := Clip(FUfromR[R^] + FUfromG[G^] + FUfromB[B^] + FBias);
      Vi := Clip(FVfromR[R^] + FVfromG[G^] + FVfromB[B^] + FBias);
      Y^ := Yi shr FPrecision;
      U^ := Ui shr FPrecision;
      V^ := Vi shr FPrecision;
      inc(R, 3);
      inc(G, 3);
      inc(B, 3);
      inc(Y, 3);
      inc(U, 3);
      inc(V, 3);
    end;
end;

end.
