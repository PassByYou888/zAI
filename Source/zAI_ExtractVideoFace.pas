{ ****************************************************************************** }
{ * AI extract face in Video                                                   * }
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
unit zAI_ExtractVideoFace;

{$INCLUDE zDefine.inc}

interface

uses CoreClasses, PascalStrings, UnicodeMixedLib, DoStatusIO, MemoryStream64, ListEngine,
  MemoryRaster, Geometry2DUnit,
  FFMPEG, FFMPEG_Reader,
  zAI, zAI_Common, zAI_FFMPEG;

type
  PDNN_Face_Input_ = ^TDNN_Face_Input_;

  TDNN_Face_Input_ = record
    FaceToken: U_String;
    OutImgL: TAI_ImageList;
  end;

  TAI_ExtractFaceMode = (efmCentre, efmAny);

  TAI_ExtractFaceInVideo = class
  private
    FDNN_Pool: TAI_DNN_ThreadPool;
    FParallel: TAI_Parallel;
    FExtractFaceSize: Integer;
    FExtractPictureWidth, FExtractPictureHeight: Integer;
    FExtractFaceMode: TAI_ExtractFaceMode;
    procedure FaceDetector_OnResult(ThSender: TAI_DNN_Thread_MMOD6L; UserData: Pointer; Input: TMemoryRaster; output: TMMOD_Desc);
  public
    constructor CreateCustom(perDeviceThNum: Integer);
    constructor Create;
    destructor Destroy; override;

    procedure ExtractFaceFromFile(inputFile, FaceToken: U_String; MaxFrame: Integer; OutImgL: TAI_ImageList);
    procedure ExtractFaceFromReaderStream(reader: TFFMPEG_VideoStreamReader; InputStream: TCoreClassStream; FaceToken: U_String; MaxFrame: Integer; OutImgL: TAI_ImageList);
    procedure ExtractFaceFromH264Stream(InputStream: TCoreClassStream; FaceToken: U_String; MaxFrame: Integer; OutImgL: TAI_ImageList);
    procedure ExtractFaceFromMJPEGStream(InputStream: TCoreClassStream; FaceToken: U_String; MaxFrame: Integer; OutImgL: TAI_ImageList);

    property ExtractFaceSize: Integer read FExtractFaceSize write FExtractFaceSize;
    property ExtractPictureWidth: Integer read FExtractPictureWidth write FExtractPictureWidth;
    property ExtractPictureHeight: Integer read FExtractPictureHeight write FExtractPictureHeight;
    property ExtractFaceMode: TAI_ExtractFaceMode read FExtractFaceMode write FExtractFaceMode;
  end;

implementation

procedure TAI_ExtractFaceInVideo.FaceDetector_OnResult(ThSender: TAI_DNN_Thread_MMOD6L; UserData: Pointer; Input: TMemoryRaster; output: TMMOD_Desc);
var
  p: PDNN_Face_Input_;
  AI: TAI;
  fHnd: TFace_Handle;
  i, j: Integer;
  cent: Integer;
  img: TAI_Image;
  det: TAI_DetectorDefine;
  sour_r: TRectV2;
  sour_sp: TArrayVec2;
begin
  p := UserData;
  AI := FParallel.GetAndLockAI;
  fHnd := AI.Face_Detector(Input, output, FExtractFaceSize);
  FParallel.UnLockAI(AI);

  case FExtractFaceMode of
    efmCentre:
      begin
        cent := AI.Face_GetCentreRectIndex(Input, fHnd);
        if (cent >= 0) and RectInRect(output[cent].R, Input.BoundsRectV2) then
          begin
            Input.LocalParallel := False;
            img := TAI_Image.Create(p^.OutImgL);
            img.ResetRaster(Input.NonlinearFitScaleAsNew(FExtractPictureWidth, FExtractPictureHeight));
            img.FileInfo := p^.FaceToken;
            sour_r := RectProjection(Input.BoundsRectV2, img.Raster.BoundsRectV2, output[cent].R);
            det := img.DetectorDefineList.AddDetector(Rect2Rect(sour_r), p^.FaceToken);
            det.ResetPrepareRaster(AI.Face_chips(fHnd, cent));

            // projection shape vertex
            sour_sp := AI.Face_ShapeV2(fHnd, cent);
            for j := 0 to length(sour_sp) - 1 do
                det.Part.Add(RectProjection(Input.BoundsRectV2, img.Raster.BoundsRectV2, sour_sp[j]));

            // done
            LockObject(p^.OutImgL);
            p^.OutImgL.Add(img);
            UnLockObject(p^.OutImgL);
          end;
      end;
    efmAny:
      begin
        Input.LocalParallel := False;
        img := TAI_Image.Create(p^.OutImgL);
        img.ResetRaster(Input.NonlinearFitScaleAsNew(FExtractPictureWidth, FExtractPictureHeight));
        img.FileInfo := p^.FaceToken;

        for i := 0 to AI.Face_chips_num(fHnd) - 1 do
          if RectInRect(output[i].R, Input.BoundsRectV2) then
            begin
              sour_r := RectProjection(Input.BoundsRectV2, img.Raster.BoundsRectV2, output[i].R);
              det := img.DetectorDefineList.AddDetector(Rect2Rect(sour_r), p^.FaceToken);
              det.ResetPrepareRaster(AI.Face_chips(fHnd, i));

              // projection shape vertex
              sour_sp := AI.Face_ShapeV2(fHnd, i);
              for j := 0 to length(sour_sp) - 1 do
                  det.Part.Add(RectProjection(Input.BoundsRectV2, img.Raster.BoundsRectV2, sour_sp[j]));
            end;

        if img.DetectorDefineList.Count > 0 then
          begin
            // done
            LockObject(p^.OutImgL);
            p^.OutImgL.Add(img);
            UnLockObject(p^.OutImgL);
          end
        else
            DisposeObject(img);
      end;
  end;
  AI.Face_Close(fHnd);
  Dispose(p);
end;

constructor TAI_ExtractFaceInVideo.CreateCustom(perDeviceThNum: Integer);
var
  i: Integer;
begin
  inherited Create;
  CheckAndReadAIConfig();
  zAI.Prepare_AI_Engine();
  FDNN_Pool := TAI_DNN_ThreadPool.Create;
  FDNN_Pool.BuildPerDeviceThread(perDeviceThNum, TAI_DNN_Thread_MMOD6L);
  for i := 0 to FDNN_Pool.Count - 1 do
      TAI_DNN_Thread_MMOD6L(FDNN_Pool[i]).Open_Face;
  FParallel := TAI_Parallel.Create;
  FParallel.Prepare_Parallel(FDNN_Pool.Count);
  FParallel.Prepare_FaceSP;
  FExtractFaceSize := C_Metric_Input_Size;
  FExtractPictureWidth := 300;
  FExtractPictureHeight := 300;
  FExtractFaceMode := efmAny;
  DoStatus();
  FDNN_Pool.Wait;
end;

constructor TAI_ExtractFaceInVideo.Create;
begin
  CreateCustom(2);
end;

destructor TAI_ExtractFaceInVideo.Destroy;
begin
  DisposeObject(FDNN_Pool);
  DisposeObject(FParallel);
  inherited Destroy;
end;

procedure TAI_ExtractFaceInVideo.ExtractFaceFromFile(inputFile, FaceToken: U_String; MaxFrame: Integer; OutImgL: TAI_ImageList);
var
  reader: TFFMPEG_Reader;
  Raster: TRaster;
  th: TAI_DNN_Thread_MMOD6L;
  p: PDNN_Face_Input_;
  oriNum, curNum: Integer;
begin
  try
      reader := TFFMPEG_Reader.Create(inputFile, True);
  except
      exit;
  end;
  Raster := NewRaster();

  oriNum := OutImgL.Count;

  while reader.ReadFrame(Raster, False) do
    begin
      while FDNN_Pool.TaskNum > FDNN_Pool.Count do
          TCompute.Sleep(1);
      th := TAI_DNN_Thread_MMOD6L(FDNN_Pool.MinLoad_DNN_Thread());
      new(p);
      p^.FaceToken := FaceToken;
      p^.OutImgL := OutImgL;
      th.ProcessM(p, Raster.Clone, True, {$IFDEF FPC}@{$ENDIF FPC}FaceDetector_OnResult);

      if MaxFrame > 0 then
        begin
          LockObject(OutImgL);
          curNum := OutImgL.Count;
          UnLockObject(OutImgL);
          if curNum - oriNum > MaxFrame then
              break;
        end;
    end;

  DisposeObject(reader);
  DisposeObject(Raster);
  FDNN_Pool.Wait;
  OutImgL.RemoveOutEdgeDetectorDefine(True, True);
end;

procedure TAI_ExtractFaceInVideo.ExtractFaceFromReaderStream(reader: TFFMPEG_VideoStreamReader; InputStream: TCoreClassStream; FaceToken: U_String; MaxFrame: Integer; OutImgL: TAI_ImageList);
const
  C_Chunk_Buff_Size = 1 * 1024 * 1024;
var
  tempBuff: Pointer;
  chunk: NativeInt;
  L: TMemoryRasterList;
  i: Integer;
  p: PDNN_Face_Input_;
  oriNum, curNum: Integer;
begin
  tempBuff := GetMemory(C_Chunk_Buff_Size);
  InputStream.Position := 0;
  oriNum := OutImgL.Count;
  while (InputStream.Position < InputStream.Size) do
    begin
      chunk := umlMin(InputStream.Size - InputStream.Position, C_Chunk_Buff_Size);
      if chunk <= 0 then
          break;
      InputStream.Read(tempBuff^, chunk);
      reader.WriteBuffer(tempBuff, chunk);

      while FDNN_Pool.TaskNum > FDNN_Pool.Count do
          TCompute.Sleep(1);

      L := reader.LockVideoPool;
      while L.Count > 0 do
        begin
          new(p);
          p^.FaceToken := FaceToken;
          p^.OutImgL := OutImgL;
          with TAI_DNN_Thread_MMOD6L(FDNN_Pool.MinLoad_DNN_Thread()) do
              ProcessM(p, L.First, True, {$IFDEF FPC}@{$ENDIF FPC}FaceDetector_OnResult);
          L.Delete(0);
        end;
      reader.UnLockVideoPool(True);

      if MaxFrame > 0 then
        begin
          LockObject(OutImgL);
          curNum := OutImgL.Count;
          UnLockObject(OutImgL);
          if curNum - oriNum > MaxFrame then
              break;
        end;
    end;
  FreeMemory(tempBuff);
  FDNN_Pool.Wait;
  OutImgL.RemoveOutEdgeDetectorDefine(True, True);
end;

procedure TAI_ExtractFaceInVideo.ExtractFaceFromH264Stream(InputStream: TCoreClassStream; FaceToken: U_String; MaxFrame: Integer; OutImgL: TAI_ImageList);
var
  reader: TFFMPEG_VideoStreamReader;
begin
  reader := TFFMPEG_VideoStreamReader.Create;
  reader.OpenH264Decodec;
  ExtractFaceFromReaderStream(reader, InputStream, FaceToken, MaxFrame, OutImgL);
  DisposeObject(reader);
end;

procedure TAI_ExtractFaceInVideo.ExtractFaceFromMJPEGStream(InputStream: TCoreClassStream; FaceToken: U_String; MaxFrame: Integer; OutImgL: TAI_ImageList);
var
  reader: TFFMPEG_VideoStreamReader;
begin
  reader := TFFMPEG_VideoStreamReader.Create;
  reader.OpenMJPEGDecodec;
  ExtractFaceFromReaderStream(reader, InputStream, FaceToken, MaxFrame,
    OutImgL);
  DisposeObject(reader);
end;

end.
