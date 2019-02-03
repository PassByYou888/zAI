{ ****************************************************************************** }
{ * memory Rasterization with AGG support                                      * }
{ * by QQ 600585@qq.com                                                        * }
{ ****************************************************************************** }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }

(*
  ////////////////////////////////////////////////////////////////////////////////
  //                                                                            //
  //  Anti-Grain Geometry (modernized Pascal fork, aka 'AggPasMod')             //
  //    Maintained by Christian-W. Budde (Christian@pcjv.de)                    //
  //    Copyright (c) 2012-2017                                                 //
  //                                                                            //
  //  Based on:                                                                 //
  //    Pascal port by Milan Marusinec alias Milano (milan@marusinec.sk)        //
  //    Copyright (c) 2005-2006, see http://www.aggpas.org                      //
  //                                                                            //
  //  Original License:                                                         //
  //    Anti-Grain Geometry - Version 2.4 (Public License)                      //
  //    Copyright (C) 2002-2005 Maxim Shemanarev (http://www.antigrain.com)     //
  //    Contact: McSeem@antigrain.com / McSeemAgg@yahoo.com                     //
  //                                                                            //
  //  Permission to copy, use, modify, sell and distribute this software        //
  //  is granted provided this copyright notice appears in all copies.          //
  //  This software is provided "as is" without express or implied              //
  //  warranty, and with no claim as to its suitability for any purpose.        //
  //                                                                            //
  ////////////////////////////////////////////////////////////////////////////////
*)
unit AggGammaLUT;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface
uses
  Math,
  AggBasics,
  AggPixelFormat;

type
  TCustomAggGammaLUT = class(TAggGamma)
  private
    FGamma: Double;
    procedure SetGamma(Value: Double);
  protected
    procedure GammaChanged; virtual; abstract;
  public
    property Gamma: Double read FGamma write SetGamma;
  end;

  TAggGammaLUT = class(TCustomAggGammaLUT)
  private
    FGammaSize, FGammaMask: Cardinal;
    FHiResSize, FHiResMask: Cardinal;

    FDirGamma, FInvGamma: PInt8u;

    FGammaShift, FHiResShift, FHiResT, FLoResT: Byte;
  protected
    procedure GammaChanged; override;
    function GetDir(Value: Cardinal): Cardinal; override;
    function GetInv(Value: Cardinal): Cardinal; override;
  public
    constructor Create(GammaShift: Int8u = 8; HiResShift: Int8u = 8); overload;
    constructor Create(GammaValue: Double; GammaShift: Int8u = 8; HiResShift: Int8u = 8); overload;
    destructor Destroy; override;
  end;

  TAggGammaLUT8 = class(TCustomAggGammaLUT)
  private
    FDirGamma, FInvGamma: array [0 .. $FF] of Int8u;
  protected
    procedure GammaChanged; override;
    function GetDir(Value: Cardinal): Cardinal; override;
    function GetInv(Value: Cardinal): Cardinal; override;
  public
    constructor Create; overload;
    constructor Create(GammaValue: Double); overload;
  end;

implementation


{ TCustomAggGammaLUT }

procedure TCustomAggGammaLUT.SetGamma(Value: Double);
begin
  if FGamma <> Value then
    begin
      FGamma := Value;
      GammaChanged;
    end;
end;

{ TAggGammaLut }

constructor TAggGammaLUT.Create(GammaShift: Int8u = 8; HiResShift: Int8u = 8);
var
  i: Cardinal;
begin
  FGammaShift := GammaShift;
  FGammaSize := 1 shl FGammaShift;
  FGammaMask := FGammaSize - 1;

  FHiResShift := HiResShift;
  FHiResSize := 1 shl FHiResShift;
  FHiResMask := FHiResSize - 1;

  FHiResT := FHiResShift div 8;
  FLoResT := FGammaShift div 8;

  AggGetMem(Pointer(FDirGamma), FGammaSize * FHiResT);
  AggGetMem(Pointer(FInvGamma), FHiResSize * FLoResT);

  // dirGamma
  for i := 0 to FGammaSize - 1 do
    try
      case FHiResT of
        1:
          PInt8u(PtrComp(FDirGamma) + i * FHiResT)^ :=
            Int8u(i shl (FHiResShift - FGammaShift));
        2:
          PInt16u(PtrComp(FDirGamma) + i * FHiResT)^ :=
            Int16u(i shl (FHiResShift - FGammaShift));
        4:
          PInt32u(PtrComp(FDirGamma) + i * FHiResT)^ :=
            Int32u(i shl (FHiResShift - FGammaShift));
      end;
    except
    end;

  // invGamma
  for i := 0 to FHiResSize - 1 do
    try
      case FLoResT of
        1:
          PInt8u(PtrComp(FInvGamma) + i * FLoResT)^ :=
            Int8u(i shr (FHiResShift - FGammaShift));
        2:
          PInt16u(PtrComp(FInvGamma) + i * FLoResT)^ :=
            Int16u(i shr (FHiResShift - FGammaShift));
        4:
          PInt32u(PtrComp(FInvGamma) + i * FLoResT)^ :=
            Int32u(i shr (FHiResShift - FGammaShift));
      end;
    except
    end;
end;

constructor TAggGammaLUT.Create(GammaValue: Double; GammaShift: Int8u = 8;
  HiResShift: Int8u = 8);
begin
  FGammaShift := GammaShift;
  FGammaSize := 1 shl FGammaShift;
  FGammaMask := FGammaSize - 1;

  FHiResShift := HiResShift;
  FHiResSize := 1 shl FHiResShift;
  FHiResMask := FHiResSize - 1;

  FHiResT := FHiResShift div 8;
  FLoResT := FGammaShift div 8;

  AggGetMem(Pointer(FDirGamma), FGammaSize * FHiResT);
  AggGetMem(Pointer(FInvGamma), FHiResSize * FLoResT);

  FGamma := 1;
  GammaChanged;
end;

destructor TAggGammaLUT.Destroy;
begin
  AggFreeMem(Pointer(FDirGamma), FGammaSize * FHiResT);
  AggFreeMem(Pointer(FInvGamma), FHiResSize * FLoResT);

  inherited;
end;

procedure TAggGammaLUT.GammaChanged;
var
  i: Cardinal;
  InverseGamma: Double;
begin
  // dirGamma
  for i := 0 to FGammaSize - 1 do
    try
      case FHiResT of
        1:
          PInt8u(PtrComp(FDirGamma) + i * FHiResT)^ :=
            Int8u(Trunc(Power(i / FGammaMask, FGamma) * FHiResMask + 0.5));
        2:
          PInt16u(PtrComp(FDirGamma) + i * FHiResT)^ :=
            Int16u(Trunc(Power(i / FGammaMask, FGamma) * FHiResMask + 0.5));
        4:
          PInt32u(PtrComp(FDirGamma) + i * FHiResT)^ :=
            Int32u(Trunc(Power(i / FGammaMask, FGamma) * FHiResMask + 0.5));
      end;
    except
    end;

  // invGamma
  if FGamma = 0 then
      FillChar(FInvGamma^, FHiResSize * FLoResT, 0)
  else
    begin
      InverseGamma := 1 / FGamma;

      for i := 0 to FHiResSize - 1 do
        try
          case FLoResT of
            1:
              PInt8u(PtrComp(FInvGamma) + i * FLoResT)^ :=
                Int8u(Trunc(Power(i / FHiResMask, InverseGamma) * FGammaMask + 0.5));
            2:
              PInt16u(PtrComp(FInvGamma) + i * FLoResT)^ :=
                Int16u(Trunc(Power(i / FHiResMask, InverseGamma) * FGammaMask + 0.5));
            4:
              PInt32u(PtrComp(FInvGamma) + i * FLoResT)^ :=
                Int32u(Trunc(Power(i / FHiResMask, InverseGamma) * FGammaMask + 0.5));
          end;
        except
        end;
    end;
end;

function TAggGammaLUT.GetDir(Value: Cardinal): Cardinal;
begin
  case FHiResT of
    1:
      Result := PInt8u(PtrComp(FDirGamma) + Value * FHiResT)^;
    2:
      Result := PInt16u(PtrComp(FDirGamma) + Value * FHiResT)^;
    4:
      Result := PInt32u(PtrComp(FDirGamma) + Value * FHiResT)^;
    else
      Result := 0;
  end;
end;

function TAggGammaLUT.GetInv(Value: Cardinal): Cardinal;
begin
  case FLoResT of
    1:
      Result := PInt8u(PtrComp(FInvGamma) + Value * FLoResT)^;
    2:
      Result := PInt16u(PtrComp(FInvGamma) + Value * FLoResT)^;
    4:
      Result := PInt32u(PtrComp(FInvGamma) + Value * FLoResT)^;
    else
      Result := 0;
  end;
end;

{ TAggGammaLut8 }

constructor TAggGammaLUT8.Create;
var
  i: Cardinal;
begin
  // dirGamma
  for i := 0 to $FF do
      FDirGamma[i] := i;

  // invGamma
  for i := 0 to $FF do
      FInvGamma[i] := i;
end;

constructor TAggGammaLUT8.Create(GammaValue: Double);
begin
  FGamma := GammaValue;
  GammaChanged;
end;

procedure TAggGammaLUT8.GammaChanged;
var
  i: Cardinal;
  InverseGamma: Double;
const
  CScale: Double = 1 / $FF;
begin
  // dirGamma
  for i := 0 to $FF do
      FDirGamma[i] := Int8u(Trunc(Power(i * CScale, FGamma) * $FF + 0.5));

  // invGamma
  if FGamma = 0 then
      FillChar(FInvGamma, 256, 0)
  else
    begin
      InverseGamma := 1 / FGamma;

      for i := 0 to $FF do
          FInvGamma[i] := Int8u(Trunc(Power(i * CScale, InverseGamma) * $FF + 0.5));
    end;
end;

function TAggGammaLUT8.GetDir(Value: Cardinal): Cardinal;
begin
  Result := FDirGamma[Value];
end;

function TAggGammaLUT8.GetInv(Value: Cardinal): Cardinal;
begin
  Result := FInvGamma[Value];
end;

end. 
 
