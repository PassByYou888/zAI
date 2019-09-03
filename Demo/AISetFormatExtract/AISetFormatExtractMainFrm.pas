unit AISetFormatExtractMainFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,

  System.Threading,

  System.IOUtils,

  CoreClasses, ListEngine,
  Learn, LearnTypes,
  zAI_Common, zAI_Editor_Common,
  zDrawEngineInterface_SlowFMX, zDrawEngine, Geometry2DUnit, Geometry3DUnit, MemoryRaster,
  MemoryStream64, PascalStrings, UnicodeMixedLib, DoStatusIO;

type
  TAISetFormatExtractMainForm = class(TForm)
    Memo1: TMemo;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    procedure DoStatusMethod(AText: SystemString; const ID: Integer);
  public
    { Public declarations }
  end;

var
  AISetFormatExtractMainForm: TAISetFormatExtractMainForm;

implementation

{$R *.fmx}


procedure TAISetFormatExtractMainForm.DoStatusMethod(AText: SystemString; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
  Memo1.GoToTextEnd;
end;

procedure TAISetFormatExtractMainForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(Self, DoStatusMethod);
  TComputeThread.RunP(nil, nil, procedure(SenderTh: TComputeThread)
    var
      fn: U_String;
      i, j: Integer;
      editor_dataset: TEditorImageDataList;
      editor_ImgData: TEditorImageData;
      editor_ImgData_raster: TMemoryRaster;
      editor_ImgData_Det: TEditorDetectorDefine;
      editor_ImgData_Geo: TEditorGeometry;
      editor_ImgData_SegMask: TEditorSegmentationMask;
    begin
      // .AI_Set 是编辑器的原始数据
      editor_dataset := TEditorImageDataList.Create(True);
      fn.Text := umlCombineFileName(TPath.GetLibraryPath, 'demoDataset.AI_Set');
      DoStatus('load .AI_Set file: %s', [fn.Text]);
      editor_dataset.LoadFromFile(fn);
      DoStatus('load done.', []);

      for i := 0 to editor_dataset.Count - 1 do
        begin
          // 在编辑器中 editor_ImgData 每张图片的数据
          // 包含:图像光栅，框体链表，包围+塌陷几何链表，图像分割光栅链表
          editor_ImgData := editor_dataset[i];

          DoStatus('');

          DoStatus('正在解析 %s 中的数据', [editor_ImgData.FileName.Text]);

          // 原始光栅数据
          editor_ImgData_raster := editor_ImgData.Raster;
          DoStatus(' %s 原始的光栅尺寸 %d * %d', [editor_ImgData.FileName.Text, editor_ImgData_raster.Width, editor_ImgData_raster.Height]);

          for j := 0 to editor_ImgData.DetectorDefineList.Count - 1 do
            begin
              // editor_ImgData_Det里面是检测器框体定义和ShapePredictor数据
              editor_ImgData_Det := editor_ImgData.DetectorDefineList[j];
              DoStatus(' %s 中的检测器框体%d: %d %d %d %d shapePredictor数据有 %d 个', [editor_ImgData.FileName.Text, j,
                editor_ImgData_Det.R.Left, editor_ImgData_Det.R.Top,
                editor_ImgData_Det.R.Right, editor_ImgData_Det.R.Bottom, editor_ImgData_Det.Part.Count]);
            end;

          for j := 0 to editor_ImgData.GeometryList.Count - 1 do
            begin
              // editor_ImgData_Geo里面是用于几何描述的多边形数据
              // 它的数据原型来自Geometry2DUnit中的T2DPolygonGraph
              // T2DPolygonGraph由一个包围多边形和n个塌陷多边形共同组成
              editor_ImgData_Geo := editor_ImgData.GeometryList[j];
              DoStatus(' %s 中的几何描述体 %s 有 %d 个塌陷 ', [editor_ImgData.FileName.Text, editor_ImgData_Geo.Token.Text, editor_ImgData_Geo.CollapsesCount]);
            end;

          for j := 0 to editor_ImgData.SegmentationMaskList.Count - 1 do
            begin
              // editor_ImgData_SegMask 里面是用于图像语义分割的输入蒙版
              // 这些蒙版可以是多个联合的像素描绘体，也可以是单独像素描绘体，他们的最终作用和几何描述结果一致
              editor_ImgData_SegMask := editor_ImgData.SegmentationMaskList[j];
              DoStatus(' %s 中有一个叫 %s 的分割蒙版描述对象 ', [editor_ImgData.FileName.Text, editor_ImgData_SegMask.Token.Text]);
            end;

          // 现在我们开始重构一次数据结构
          // 我们的目的：删除所有数据，然后用图像语义分割的输入蒙版做框体数据，然后再干掉所有语义分割数据，只保留新的框体

          // 第一步，删除所有数据，只保留 图像语义分割的输入蒙版数据
          // 编辑器的数据结构需要全部手动释放
          for j := 0 to editor_ImgData.DetectorDefineList.Count - 1 do
            begin
              editor_ImgData_Det := editor_ImgData.DetectorDefineList[j];
              DisposeObject(editor_ImgData_Det);
            end;
          editor_ImgData.DetectorDefineList.Clear;
          for j := 0 to editor_ImgData.GeometryList.Count - 1 do
            begin
              editor_ImgData_Geo := editor_ImgData.GeometryList[j];
              DisposeObject(editor_ImgData_Geo);
            end;
          editor_ImgData.GeometryList.Clear;
          // 这一步是移出所有由几何描述的多边形数据产生的语义分割蒙版，具体内容请自行研究源码
          editor_ImgData.SegmentationMaskList.RemoveGeometrySegmentationMask;

          // 第二步，用图像语义分割的输入蒙版做框体数据
          for j := 0 to editor_ImgData.SegmentationMaskList.Count - 1 do
            begin
              editor_ImgData_SegMask := editor_ImgData.SegmentationMaskList[j];
              // 创建新的框体数据结构
              editor_ImgData_Det := TEditorDetectorDefine.Create(editor_ImgData);
              // 重新计算输入蒙版FGColor的像素包围框
              editor_ImgData_Det.R := editor_ImgData_SegMask.Raster.ColorBoundsRect(editor_ImgData_SegMask.FGColor);
              editor_ImgData_Det.Token := editor_ImgData_SegMask.Token;
              editor_ImgData.DetectorDefineList.Add(editor_ImgData_Det);
            end;

          // 把第一步没删除干净的尾巴处理了
          // 这一步完成以后，整个数据只会剩下我们刚才提炼出来的检测器框体
          for j := 0 to editor_ImgData.SegmentationMaskList.Count - 1 do
            begin
              editor_ImgData_SegMask := editor_ImgData.SegmentationMaskList[j];
              DisposeObject(editor_ImgData_SegMask);
            end;
          editor_ImgData.SegmentationMaskList.Clear;
        end;

      DoStatus('all done.');

      // 现在，我们把重建后的.ai_set保存成另一个文件，然后用编辑器打开它吧
      // 除非是别人提炼好的样本，否则数据样本提炼就是做建模的前置工作，任何模型都需要做数据提炼
      // 在做数据提炼的过程中，发生内存泄漏，这些都是无所谓的，只要数据没出错，能保证我们提炼的.AI_Set可以被建模工具打开就可以了
      // 剩下的是跑模型训练，调节参数和硬件性能，请参考相关demo
      fn.Text := umlCombineFileName(TPath.GetLibraryPath, 'demoDataset_rebuild_output.AI_Set');
      DoStatus('rebuild output to: %s', [fn.Text]);
      editor_dataset.SaveToFile(fn);

      DisposeObject(editor_dataset);
    end);
end;

procedure TAISetFormatExtractMainForm.Timer1Timer(Sender: TObject);
begin
  DoStatus;
end;

end.
