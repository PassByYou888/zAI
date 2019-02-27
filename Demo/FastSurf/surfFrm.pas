unit surfFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation,
  FMX.StdCtrls, FMX.Objects, FMX.ScrollBox, FMX.Memo, FMX.Layouts, FMX.ExtCtrls,

  System.IOUtils,

  CoreClasses, DoStatusIO, zAI, zAI_Common, zDrawEngineInterface_SlowFMX, zDrawEngine, MemoryRaster, MemoryStream64,
  PascalStrings, UnicodeMixedLib, Geometry2DUnit, Geometry3DUnit, Cadencer, Y4M, h264Image;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure Timer1Timer(Sender: TObject);
  private
    procedure DoStatus_Hook_(AText: SystemString; const ID: Integer);
  public
    drawIntf: TDrawEngineInterface_FMX;
    surf_out: TMemoryRaster;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}


procedure TForm1.DoStatus_Hook_(AText: SystemString; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
  Memo1.GoToTextEnd;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  ai: TAI;
  r1, r2: TMemoryRaster;
  d1, d2: TSurf_DescBuffer;
  matched: TSurfMatchedBuffer;
  tk: TTimeTick;
begin
  AddDoStatusHookM(Self, DoStatus_Hook_);
  // ��ȡzAI������
  ReadAIConfig;

  // ��һ��������Key����������֤ZAI��Key
  // ���ӷ�������֤Key������������ʱһ���Ե���֤��ֻ�ᵱ��������ʱ�Ż���֤��������֤����ͨ����zAI����ܾ�����
  // �ڳ��������У���������TAI�����ᷢ��Զ����֤
  // ��֤��Ҫһ��userKey��ͨ��userkey�����ZAI������ʱ���ɵ����Key��userkey����ͨ��web���룬Ҳ������ϵ���߷���
  // ��֤key���ǿ����Ӽ����޷����ƽ�
  zAI.Prepare_AI_Engine();

  // ʹ��zDrawEngine���ⲿ��ͼʱ(������Ϸ������paintbox)������Ҫһ����ͼ�ӿ�
  // TDrawEngineInterface_FMX������FMX�Ļ�ͼcore�ӿ�
  // �����ָ����ͼ�ӿڣ�zDrawEngine��Ĭ��ʹ�������դ��ͼ(�Ƚ���)
  drawIntf := TDrawEngineInterface_FMX.Create;

  ai := TAI.OpenEngine();

  // ��ȡͼƬ
  r1 := NewRasterFromFile(umlCombineFileName(AI_Configure_Path, 'surf_1.bmp'));
  r2 := NewRasterFromFile(umlCombineFileName(AI_Configure_Path, 'surf_2.bmp'));

  // ʹ��surf�ȶ�ͼƬ�����������surf_out
  tk := GetTimeTick();
  d1 := ai.fast_surf(r1, 20000, 10.0);
  DoStatus('���� surf_1.bmp ��ʱ:%dms', [GetTimeTick() - tk]);

  tk := GetTimeTick();
  d2 := ai.fast_surf(r2, 20000, 10.0);
  DoStatus('���� surf_1.bmp ��ʱ:%dms', [GetTimeTick() - tk]);

  tk := GetTimeTick();
  ai.BuildFeatureView(r1, d1);
  ai.BuildFeatureView(r2, d2);
  DoStatus('ͼ�ι�����ʱ:%dms', [GetTimeTick() - tk]);

  tk := GetTimeTick();
  matched := ai.Surf_Matched(0.4, r1, r2, d1, d2);
  DoStatus('surf����ƥ���ʱ:%dms', [GetTimeTick() - tk]);
  surf_out := ai.BuildMatchInfoView(matched);

  disposeObject(ai);
  disposeObject([r1, r2]);
end;

procedure TForm1.FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
var
  d: TDrawEngine;
begin
  // ��DrawIntf�Ļ�ͼʵ�������paintbox1
  drawIntf.SetSurface(Canvas, Sender);
  d := DrawPool(Sender, drawIntf);

  // ��ʾ�߿��֡��
  d.ViewOptions := [devpFPS, devpFrameEndge];

  // ���������ɺ�ɫ������Ļ�ͼָ���������ִ�еģ������γ����������д����DrawEngine��һ��������
  d.FillBox(d.ScreenRect, DEColor(0, 0, 0, 1));

  d.FitDrawTexture(surf_out, surf_out.BoundsRectV2, d.ScreenRect, 1.0);
  d.Flush;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  EnginePool.Progress(Interval2Delta(Timer1.Interval));
  Invalidate;
end;

end.
