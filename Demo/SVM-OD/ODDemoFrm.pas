unit ODDemoFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation,
  FMX.StdCtrls, FMX.Objects, FMX.ScrollBox, FMX.Memo,

  CoreClasses, PascalStrings, DoStatusIO,
  zAI, zAI_Common, zDrawEngineInterface_SlowFMX, zDrawEngine, MemoryRaster;

type
  TODDemoForm = class(TForm)
    Image1: TImage;
    Button1: TButton;
    Image2: TImage;
    Button2: TButton;
    Button3: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure DoStatusMethod(AText: SystemString; const ID: Integer);
  public
    { Public declarations }
    bk1, bk2: TMemoryRaster;
    procedure OD_Marshal(bitmap: TBitmap);
    procedure OD_Bear(bitmap: TBitmap);
    procedure OD_Dog(bitmap: TBitmap);
  end;

var
  ODDemoForm: TODDemoForm;

implementation

{$R *.fmx}


procedure TODDemoForm.Button1Click(Sender: TObject);
begin
  MemoryBitmapToBitmap(bk1, Image1.bitmap);
  MemoryBitmapToBitmap(bk2, Image2.bitmap);

  OD_Marshal(Image1.bitmap);
  OD_Marshal(Image2.bitmap);
end;

procedure TODDemoForm.Button2Click(Sender: TObject);
begin
  MemoryBitmapToBitmap(bk1, Image1.bitmap);
  MemoryBitmapToBitmap(bk2, Image2.bitmap);

  OD_Bear(Image1.bitmap);
  OD_Bear(Image2.bitmap);
end;

procedure TODDemoForm.Button3Click(Sender: TObject);
begin
  MemoryBitmapToBitmap(bk1, Image1.bitmap);
  MemoryBitmapToBitmap(bk2, Image2.bitmap);

  OD_Dog(Image1.bitmap);
  OD_Dog(Image2.bitmap);
end;

procedure TODDemoForm.DoStatusMethod(AText: SystemString; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
  Memo1.GoToTextEnd;
end;

procedure TODDemoForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(Self, DoStatusMethod);

  // 读取zAI的配置
  ReadAIConfig;
  // 这一步会连接Key服务器，验证ZAI的Key
  // 连接服务器验证Key是在启动引擎时一次性的验证，只会当程序启动时才会验证，假如验证不能通过，zAI将会拒绝工作
  // 在程序运行中，反复创建TAI，不会发生远程验证
  // 验证需要一个userKey，通过userkey推算出ZAI在启动时生成的随机Key，userkey可以通过web申请，也可以联系作者发放
  // 验证key都是抗量子级，无法被破解
  zAI.Prepare_AI_Engine();

  bk1 := NewRaster();
  BitmapToMemoryBitmap(Image1.bitmap, bk1);
  bk2 := NewRaster();
  BitmapToMemoryBitmap(Image2.bitmap, bk2);
end;

procedure TODDemoForm.OD_Marshal(bitmap: TBitmap);
var
  ai: TAI;
  odm: TOD_Marshal_Handle;
  raster: TMemoryRaster;
begin
  // TAI.OpenEngine，表示打开来自AI.conf配置文件所指定的AI引擎
  ai := TAI.OpenEngine;

  // .svm_od_marshal是对象检测集的扩展名
  // .svm_od_marshal可以使用PackageTool打开编辑，它里面保存的其实是两个svm_od文件，OD_Marshal会以并行方式检测
  odm := ai.OD_Marshal_Open_Stream('bear.svm_od_marshal');

  // 创建光栅实例
  raster := NewRaster();
  // 将fmx的图片转换成光栅格式
  BitmapToMemoryBitmap(bitmap, raster);

  // bear.svm_od_marshal是针对高清图片训练的，如果尺寸不够，检测识别的几率将会降低
  // 这里我们把尺寸调大
  // raster.Scale(2.0);

  // 将检测结果画在raster中
  ai.DrawODM(odm, raster, DEColor(0, 0, 1, 0.9));

  // 关闭对象检测集的句柄
  ai.OD_Marshal_Close(odm);

  // 将光栅实例转换成fmx格式的图片并且显示
  MemoryBitmapToBitmap(raster, bitmap);

  // 释放ai引擎
  disposeObject(ai);
end;

procedure TODDemoForm.OD_Bear(bitmap: TBitmap);
var
  ai: TAI;
  od: TOD_Handle;
  raster: TMemoryRaster;
begin
  // TAI.OpenEngine，表示打开来自AI.conf配置文件所指定的AI引擎
  ai := TAI.OpenEngine;

  // .svm_od是训练好的对象检测数据集扩展名
  od := ai.OD_Open_Stream('bear.svm_od');

  // 创建光栅实例
  raster := NewRaster();
  // 将fmx的图片转换成光栅格式
  BitmapToMemoryBitmap(bitmap, raster);

  // bear.svm_od是针对高清图片训练的，如果尺寸不够，检测识别的几率将会降低
  // 这里我们把尺寸调大
  // raster.Scale(2.0);

  // 将检测结果画在raster中
  ai.DrawOD(od, raster, DEColor(0, 0, 1, 0.9));

  // 关闭对象检测集的句柄
  ai.OD_Close(od);

  // 将光栅实例转换成fmx格式的图片并且显示
  MemoryBitmapToBitmap(raster, bitmap);

  // 释放ai引擎
  disposeObject(ai);
end;

procedure TODDemoForm.OD_Dog(bitmap: TBitmap);
var
  ai: TAI;
  od: TOD_Handle;
  raster: TMemoryRaster;
begin
  // TAI.OpenEngine，表示打开来自AI.conf配置文件所指定的AI引擎
  ai := TAI.OpenEngine;

  // .svm_od是训练好的对象检测数据集扩展名
  od := ai.OD_Open_Stream('dog.svm_od');

  // 创建光栅实例
  raster := NewRaster();
  // 将fmx的图片转换成光栅格式
  BitmapToMemoryBitmap(bitmap, raster);

  // dog.svm_od是针对高清图片训练的，如果尺寸不够，检测识别的几率将会降低
  // 这里我们把尺寸调大
  // raster.Scale(2.0);

  // 将检测结果画在raster中
  ai.DrawOD(od, raster, DEColor(0, 0, 1, 0.9));

  // 关闭对象检测集的句柄
  ai.OD_Close(od);

  // 将光栅实例转换成fmx格式的图片并且显示
  MemoryBitmapToBitmap(raster, bitmap);

  // 释放ai引擎
  disposeObject(ai);
end;

end.
