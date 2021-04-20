{ ****************************************************************************** }
{ * h264Parameters.pas        by qq600585                                      * }
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

unit h264Parameters;

{$INCLUDE zDefine.inc}

interface

uses
  Classes, SysUtils, h264Types, h264Util, PascalStrings;

const
  MIN_QP               = 0;
  MAX_QP               = 51;
  MAX_CHROMA_QP_OFFSET = 12;
  MAX_REFERENCE_FRAMES = 16;

type
  // encoder configuration parameters
  TEncodingParameters = class
  private
    width, height: uint16_t;  // input dimensions
    frames: uint32_t;         // frame count
    fps: Single;              // fps
    qp: uint8_t;              // quantization parameter
    chroma_qp_offset: int8_t; // chroma qp offset

    subme: uint8_t;           // subpixel ME refinement
    { 0 - none (fpel only)
      1 - hpel
      2 - qpel
      3 - qpel SATD
    }

    analyse: uint8_t; // mb type decision quality
    { 0 - none
      1 - heuristics - SAD
      2 - heuristics - SATD
      3 - bitcost
    }

    ref: uint8_t;           // reference frame count
    key_interval: uint16_t; // maximum keyframe interval
    loopfilter: Boolean;    // deblocking
    filter_thread: Boolean; // deblocking in separate thread
    aq: Boolean;            // mb-level adaptive quantization
    luma_only: Boolean;     // ignore chroma

    RC: record
      Enabled: Boolean;  // enable avg. bitrate ratecontrol
      Bitrate: uint32_t; // desired bitrate in kbps
    end;

    procedure SetAnalysisLevel(const AValue: uint8_t);
    procedure SetChromaQParamOffset(const AValue: int8_t);
    procedure SetFilterThreadEnabled(AValue: Boolean);
    procedure SetKeyFrameInterval(const AValue: uint16_t);
    procedure SetNumReferenceFrames(const AValue: uint8_t);
    procedure SetQParam(const AValue: uint8_t);
    procedure SetSubpixelMELevel(const AValue: uint8_t);
    procedure ValidateQParams;
    procedure ValidateSubME;
  public
    property FrameWidth: uint16_t read width;
    property FrameHeight: uint16_t read height;
    property FrameRate: Single read fps;

    property ABRRateControlEnabled: Boolean read RC.Enabled;
    property FrameCount: uint32_t read frames write frames;
    property Bitrate: uint32_t read RC.Bitrate;

    property QParam: uint8_t read qp write SetQParam;
    property ChromaQParamOffset: int8_t read chroma_qp_offset write SetChromaQParamOffset;
    property KeyFrameInterval: uint16_t read key_interval write SetKeyFrameInterval;

    property LoopFilterEnabled: Boolean read loopfilter write loopfilter;
    property FilterThreadEnabled: Boolean read filter_thread write SetFilterThreadEnabled;

    property AnalysisLevel: uint8_t read analyse write SetAnalysisLevel; // mb type decision quality
    { 0 - none
      1 - heuristics - SAD
      2 - heuristics - SATD
      3 - bitcost
    }

    property SubpixelMELevel: uint8_t read subme write SetSubpixelMELevel; // subpixel ME refinement
    { 0 - none (fpel only)
      1 - hpel
      2 - qpel
      3 - qpel SATD
    }

    property NumReferenceFrames: uint8_t read ref write SetNumReferenceFrames;

    property AdaptiveQuant: Boolean read aq write aq;

    property IgnoreChroma: Boolean read luma_only write luma_only;

    constructor Create; overload;
    constructor Create(const width_, height_: uint16_t; const fps_: Double); overload;
    procedure SetABRRateControl(const bitrate_: uint32_t);
    procedure SetStreamParams(const width_, height_, frame_count: int32_t; const fps_: Single);
    function ToPascalString: TPascalString; overload;
  end;

implementation

procedure TEncodingParameters.SetAnalysisLevel(const AValue: uint8_t);
begin
  analyse := clip3(0, AValue, 3);
end;

procedure TEncodingParameters.SetChromaQParamOffset(const AValue: int8_t);
begin
  chroma_qp_offset := clip3(-MAX_CHROMA_QP_OFFSET, AValue, MAX_CHROMA_QP_OFFSET);
  ValidateQParams;
end;

procedure TEncodingParameters.SetFilterThreadEnabled(AValue: Boolean);
begin
  if filter_thread = AValue then
      Exit;
  filter_thread := AValue;
  if AValue and not loopfilter then
      LoopFilterEnabled := True;
end;

procedure TEncodingParameters.SetKeyFrameInterval(const AValue: uint16_t);
begin
  key_interval := AValue;
  if key_interval = 0 then
      key_interval := 1;
end;

procedure TEncodingParameters.SetNumReferenceFrames(const AValue: uint8_t);
begin
  ref := clip3(1, AValue, MAX_REFERENCE_FRAMES);
  ValidateSubME;
end;

procedure TEncodingParameters.SetQParam(const AValue: uint8_t);
begin
  qp := clip3(MIN_QP, AValue, MAX_QP);
  ValidateQParams;
end;

procedure TEncodingParameters.SetSubpixelMELevel(const AValue: uint8_t);
begin
  subme := clip3(0, AValue, 4);
  ValidateSubME;
end;

procedure TEncodingParameters.ValidateQParams;
begin
  if qp + chroma_qp_offset > MAX_QP then
      chroma_qp_offset := MAX_QP - qp;
  if qp + chroma_qp_offset < MIN_QP then
      chroma_qp_offset := MIN_QP - qp;
end;

procedure TEncodingParameters.ValidateSubME;
begin
  if (ref > 1) and (subme < 2) then
      subme := 2;
end;

constructor TEncodingParameters.Create;
begin
  Create(320, 240, 25.0);
end;

constructor TEncodingParameters.Create(const width_, height_: uint16_t; const fps_: Double);
begin
  inherited Create;
  width := width_;
  height := height_;
  fps := fps_;

  qp := 21;
  chroma_qp_offset := 0;
  key_interval := 100;
  subme := 3;
  analyse := 1;
  ref := 1;
  RC.Enabled := False;
  aq := True;
  loopfilter := False;
  filter_thread := False;
  luma_only := False;
  frames := 0;
end;

procedure TEncodingParameters.SetABRRateControl(const bitrate_: uint32_t);
begin
  RC.Enabled := True;
  RC.Bitrate := bitrate_;
end;

procedure TEncodingParameters.SetStreamParams(const width_, height_, frame_count: int32_t; const fps_: Single);
begin
  width := width_;
  height := height_;
  frames := frame_count;
  fps := fps_;
end;

function TEncodingParameters.ToPascalString: TPascalString;
  function b2s(const b: Boolean): SystemString;
  begin
    if b then
        Result := '1'
    else
        Result := '0'
  end;

begin
  Result.Text := PFormat('%dx%d keyint:%d qp:%d subme:%d analyse:%d ref:%d aq:%s '
    + 'chroma_qp_offset:%d loopfilter:%s (threaded:%s)',
    [width, height, key_interval, qp, subme, analyse, ref, b2s(aq),
    chroma_qp_offset, b2s(loopfilter), b2s(filter_thread)]);
end;

end.

