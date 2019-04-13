{ ****************************************************************************** }
{ * memory Rasterization with AGG support                                      * }
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
unit AggMath;

// Bessel function (besj) was adapted for use in AGG library by Andy Wilk
// Contact: castor.vulgaris@gmail.com

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}

interface

uses
  Math,
  AggBasics,
  AggVertexSequence;

type
  PAggPolyXY = ^TAggPolyXY;
  TAggPolyXY = array [0 .. 0] of TPointDouble;

  PAggStorageXY = ^TAggStorageXY;

  TAggStorageXY = record
    Poly: PAggPolyXY;
    Size: Cardinal;
  end;

const
  CAggIntersectionEpsilon: Double = 1.0E-30;

  // Tables for fast sqrt
const
  CAggSqrtTable: array [0 .. 1023] of Int16u = (0, 2048, 2896, 3547, 4096, 4579,
    5017, 5418, 5793, 6144, 6476, 6792, 7094, 7384, 7663, 7932, 8192, 8444,
    8689, 8927, 9159, 9385, 9606, 9822, 10033, 10240, 10443, 10642, 10837,
    11029, 11217, 11403, 11585, 11765, 11942, 12116, 12288, 12457, 12625, 12790,
    12953, 13114, 13273, 13430, 13585, 13738, 13890, 14040, 14189, 14336, 14482,
    14626, 14768, 14910, 15050, 15188, 15326, 15462, 15597, 15731, 15864, 15995,
    16126, 16255, 16384, 16512, 16638, 16764, 16888, 17012, 17135, 17257, 17378,
    17498, 17618, 17736, 17854, 17971, 18087, 18203, 18318, 18432, 18545, 18658,
    18770, 18882, 18992, 19102, 19212, 19321, 19429, 19537, 19644, 19750, 19856,
    19961, 20066, 20170, 20274, 20377, 20480, 20582, 20684, 20785, 20886, 20986,
    21085, 21185, 21283, 21382, 21480, 21577, 21674, 21771, 21867, 21962, 22058,
    22153, 22247, 22341, 22435, 22528, 22621, 22713, 22806, 22897, 22989, 23080,
    23170, 23261, 23351, 23440, 23530, 23619, 23707, 23796, 23884, 23971, 24059,
    24146, 24232, 24319, 24405, 24491, 24576, 24661, 24746, 24831, 24915, 24999,
    25083, 25166, 25249, 25332, 25415, 25497, 25580, 25661, 25743, 25824, 25905,
    25986, 26067, 26147, 26227, 26307, 26387, 26466, 26545, 26624, 26703, 26781,
    26859, 26937, 27015, 27092, 27170, 27247, 27324, 27400, 27477, 27553, 27629,
    27705, 27780, 27856, 27931, 28006, 28081, 28155, 28230, 28304, 28378, 28452,
    28525, 28599, 28672, 28745, 28818, 28891, 28963, 29035, 29108, 29180, 29251,
    29323, 29394, 29466, 29537, 29608, 29678, 29749, 29819, 29890, 29960, 30030,
    30099, 30169, 30238, 30308, 30377, 30446, 30515, 30583, 30652, 30720, 30788,
    30856, 30924, 30992, 31059, 31127, 31194, 31261, 31328, 31395, 31462, 31529,
    31595, 31661, 31727, 31794, 31859, 31925, 31991, 32056, 32122, 32187, 32252,
    32317, 32382, 32446, 32511, 32575, 32640, 32704, 32768, 32832, 32896, 32959,
    33023, 33086, 33150, 33213, 33276, 33339, 33402, 33465, 33527, 33590, 33652,
    33714, 33776, 33839, 33900, 33962, 34024, 34086, 34147, 34208, 34270, 34331,
    34392, 34453, 34514, 34574, 34635, 34695, 34756, 34816, 34876, 34936, 34996,
    35056, 35116, 35176, 35235, 35295, 35354, 35413, 35472, 35531, 35590, 35649,
    35708, 35767, 35825, 35884, 35942, 36001, 36059, 36117, 36175, 36233, 36291,
    36348, 36406, 36464, 36521, 36578, 36636, 36693, 36750, 36807, 36864, 36921,
    36978, 37034, 37091, 37147, 37204, 37260, 37316, 37372, 37429, 37485, 37540,
    37596, 37652, 37708, 37763, 37819, 37874, 37929, 37985, 38040, 38095, 38150,
    38205, 38260, 38315, 38369, 38424, 38478, 38533, 38587, 38642, 38696, 38750,
    38804, 38858, 38912, 38966, 39020, 39073, 39127, 39181, 39234, 39287, 39341,
    39394, 39447, 39500, 39553, 39606, 39659, 39712, 39765, 39818, 39870, 39923,
    39975, 40028, 40080, 40132, 40185, 40237, 40289, 40341, 40393, 40445, 40497,
    40548, 40600, 40652, 40703, 40755, 40806, 40857, 40909, 40960, 41011, 41062,
    41113, 41164, 41215, 41266, 41317, 41368, 41418, 41469, 41519, 41570, 41620,
    41671, 41721, 41771, 41821, 41871, 41922, 41972, 42021, 42071, 42121, 42171,
    42221, 42270, 42320, 42369, 42419, 42468, 42518, 42567, 42616, 42665, 42714,
    42763, 42813, 42861, 42910, 42959, 43008, 43057, 43105, 43154, 43203, 43251,
    43300, 43348, 43396, 43445, 43493, 43541, 43589, 43637, 43685, 43733, 43781,
    43829, 43877, 43925, 43972, 44020, 44068, 44115, 44163, 44210, 44258, 44305,
    44352, 44400, 44447, 44494, 44541, 44588, 44635, 44682, 44729, 44776, 44823,
    44869, 44916, 44963, 45009, 45056, 45103, 45149, 45195, 45242, 45288, 45334,
    45381, 45427, 45473, 45519, 45565, 45611, 45657, 45703, 45749, 45795, 45840,
    45886, 45932, 45977, 46023, 46069, 46114, 46160, 46205, 46250, 46296, 46341,
    46386, 46431, 46477, 46522, 46567, 46612, 46657, 46702, 46746, 46791, 46836,
    46881, 46926, 46970, 47015, 47059, 47104, 47149, 47193, 47237, 47282, 47326,
    47370, 47415, 47459, 47503, 47547, 47591, 47635, 47679, 47723, 47767, 47811,
    47855, 47899, 47942, 47986, 48030, 48074, 48117, 48161, 48204, 48248, 48291,
    48335, 48378, 48421, 48465, 48508, 48551, 48594, 48637, 48680, 48723, 48766,
    48809, 48852, 48895, 48938, 48981, 49024, 49067, 49109, 49152, 49195, 49237,
    49280, 49322, 49365, 49407, 49450, 49492, 49535, 49577, 49619, 49661, 49704,
    49746, 49788, 49830, 49872, 49914, 49956, 49998, 50040, 50082, 50124, 50166,
    50207, 50249, 50291, 50332, 50374, 50416, 50457, 50499, 50540, 50582, 50623,
    50665, 50706, 50747, 50789, 50830, 50871, 50912, 50954, 50995, 51036, 51077,
    51118, 51159, 51200, 51241, 51282, 51323, 51364, 51404, 51445, 51486, 51527,
    51567, 51608, 51649, 51689, 51730, 51770, 51811, 51851, 51892, 51932, 51972,
    52013, 52053, 52093, 52134, 52174, 52214, 52254, 52294, 52334, 52374, 52414,
    52454, 52494, 52534, 52574, 52614, 52654, 52694, 52734, 52773, 52813, 52853,
    52892, 52932, 52972, 53011, 53051, 53090, 53130, 53169, 53209, 53248, 53287,
    53327, 53366, 53405, 53445, 53484, 53523, 53562, 53601, 53640, 53679, 53719,
    53758, 53797, 53836, 53874, 53913, 53952, 53991, 54030, 54069, 54108, 54146,
    54185, 54224, 54262, 54301, 54340, 54378, 54417, 54455, 54494, 54532, 54571,
    54609, 54647, 54686, 54724, 54762, 54801, 54839, 54877, 54915, 54954, 54992,
    55030, 55068, 55106, 55144, 55182, 55220, 55258, 55296, 55334, 55372, 55410,
    55447, 55485, 55523, 55561, 55599, 55636, 55674, 55712, 55749, 55787, 55824,
    55862, 55900, 55937, 55975, 56012, 56049, 56087, 56124, 56162, 56199, 56236,
    56273, 56311, 56348, 56385, 56422, 56459, 56497, 56534, 56571, 56608, 56645,
    56682, 56719, 56756, 56793, 56830, 56867, 56903, 56940, 56977, 57014, 57051,
    57087, 57124, 57161, 57198, 57234, 57271, 57307, 57344, 57381, 57417, 57454,
    57490, 57527, 57563, 57599, 57636, 57672, 57709, 57745, 57781, 57817, 57854,
    57890, 57926, 57962, 57999, 58035, 58071, 58107, 58143, 58179, 58215, 58251,
    58287, 58323, 58359, 58395, 58431, 58467, 58503, 58538, 58574, 58610, 58646,
    58682, 58717, 58753, 58789, 58824, 58860, 58896, 58931, 58967, 59002, 59038,
    59073, 59109, 59144, 59180, 59215, 59251, 59286, 59321, 59357, 59392, 59427,
    59463, 59498, 59533, 59568, 59603, 59639, 59674, 59709, 59744, 59779, 59814,
    59849, 59884, 59919, 59954, 59989, 60024, 60059, 60094, 60129, 60164, 60199,
    60233, 60268, 60303, 60338, 60373, 60407, 60442, 60477, 60511, 60546, 60581,
    60615, 60650, 60684, 60719, 60753, 60788, 60822, 60857, 60891, 60926, 60960,
    60995, 61029, 61063, 61098, 61132, 61166, 61201, 61235, 61269, 61303, 61338,
    61372, 61406, 61440, 61474, 61508, 61542, 61576, 61610, 61644, 61678, 61712,
    61746, 61780, 61814, 61848, 61882, 61916, 61950, 61984, 62018, 62051, 62085,
    62119, 62153, 62186, 62220, 62254, 62287, 62321, 62355, 62388, 62422, 62456,
    62489, 62523, 62556, 62590, 62623, 62657, 62690, 62724, 62757, 62790, 62824,
    62857, 62891, 62924, 62957, 62991, 63024, 63057, 63090, 63124, 63157, 63190,
    63223, 63256, 63289, 63323, 63356, 63389, 63422, 63455, 63488, 63521, 63554,
    63587, 63620, 63653, 63686, 63719, 63752, 63785, 63817, 63850, 63883, 63916,
    63949, 63982, 64014, 64047, 64080, 64113, 64145, 64178, 64211, 64243, 64276,
    64309, 64341, 64374, 64406, 64439, 64471, 64504, 64536, 64569, 64601, 64634,
    64666, 64699, 64731, 64763, 64796, 64828, 64861, 64893, 64925, 64957, 64990,
    65022, 65054, 65086, 65119, 65151, 65183, 65215, 65247, 65279, 65312, 65344,
    65376, 65408, 65440, 65472, 65504);

  CAggElderBitTable: array [0 .. 255] of Int8 = (0, 0, 1, 1, 2, 2, 2, 2, 3, 3,
    3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5,
    5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
    5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
    6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
    6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 7,
    7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
    7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
    7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
    7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
    7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7);

function CalculatePointLocation(x1, y1, x2, y2, x, y: Double): Double; overload;
function CalculatePointLocation(Point1, Point2: TPointDouble; x, y: Double): Double; overload;

function PointInTriangle(x1, y1, x2, y2, x3, y3, x, y: Double): Boolean; overload;
function PointInTriangle(Point1, Point2, Point3: TPointDouble; x, y: Double): Boolean; overload;

function CalculateDistance(x1, y1, x2, y2: Double): Double; overload;
function CalculateDistance(Point1, Point2: TPointDouble): Double; overload;

function CalculateLinePointDistance(x1, y1, x2, y2, x, y: Double): Double;
function CalculateIntersection(Ax, Ay, Bx, By, Cx, Cy, dx, dy: Double; x, y: PDouble): Boolean;

function IntersectionExists(x1, y1, x2, y2, x3, y3, x4, y4: Double): Boolean;

procedure CalculateOrthogonal(Thickness, x1, y1, x2, y2: Double; x, y: PDouble);
procedure DilateTriangle(x1, y1, x2, y2, x3, y3: Double; x, y: PDouble; d: Double);

function CalculateTriangleArea(x1, y1, x2, y2, x3, y3: Double): Double;
function CalculatePolygonArea(st: PAggStorageXY): Double;
function CalculatePolygonAreaVertexSequence(st: TAggVertexSequence): Double;
function FastSqrt(val: Cardinal): Cardinal;
function Besj(x: Double; n: Integer): Double;
function CrossProduct(x1, y1, x2, y2, x, y: Double): Double;

function Hypot(x, y: Double): Double;
procedure SinCos(Theta: Double; out Sin, Cos: Double);
procedure SinCosScale(Theta: Double; out Sin, Cos: Double; Scale: Double); overload;
procedure SinCosScale(Theta: Double; out Sin, Cos: Double; ScaleSin, ScaleCos: Double); overload;

implementation

function CalculatePointLocation(x1, y1, x2, y2, x, y: Double): Double;
begin
  Result := (x - x2) * (y2 - y1) - (y - y2) * (x2 - x1);
end;

function CalculatePointLocation(Point1, Point2: TPointDouble;
  x, y: Double): Double;
begin
  Result := (x - Point2.x) * (Point2.y - Point1.y) -
    (y - Point2.y) * (Point2.x - Point1.x);
end;

function PointInTriangle(x1, y1, x2, y2, x3, y3, x, y: Double): Boolean;
var
  PntLoc: array [0 .. 2] of Boolean;
begin
  PntLoc[0] := CalculatePointLocation(x1, y1, x2, y2, x, y) < 0.0;
  PntLoc[1] := CalculatePointLocation(x2, y2, x3, y3, x, y) < 0.0;
  PntLoc[2] := CalculatePointLocation(x3, y3, x1, y1, x, y) < 0.0;

  Result := (PntLoc[0] = PntLoc[1]) and (PntLoc[1] = PntLoc[2]) and
    (PntLoc[2] = PntLoc[0]);
end;

function PointInTriangle(Point1, Point2, Point3: TPointDouble; x, y: Double)
  : Boolean; overload;
var
  PntLoc: array [0 .. 2] of Boolean;
begin
  PntLoc[0] := CalculatePointLocation(Point1, Point2, x, y) < 0.0;
  PntLoc[1] := CalculatePointLocation(Point2, Point3, x, y) < 0.0;
  PntLoc[2] := CalculatePointLocation(Point3, Point1, x, y) < 0.0;

  Result := (PntLoc[0] = PntLoc[1]) and (PntLoc[1] = PntLoc[2]) and
    (PntLoc[2] = PntLoc[0]);
end;

function CalculateDistance(x1, y1, x2, y2: Double): Double;
var
  Delta: TPointDouble;
begin
  Delta.x := x2 - x1;
  Delta.y := y2 - y1;

  Result := Hypot(Delta.x, Delta.y);
end;

function CalculateDistance(Point1, Point2: TPointDouble): Double;
var
  Delta: TPointDouble;
begin
  Delta.x := Point2.x - Point1.x;
  Delta.y := Point2.y - Point1.y;

  Result := Hypot(Delta.x, Delta.y);
end;

function CalculateLinePointDistance(x1, y1, x2, y2, x, y: Double): Double;
var
  d: Double;
  Delta: TPointDouble;
begin
  Delta.x := x2 - x1;
  Delta.y := y2 - y1;
  d := Hypot(Delta.x, Delta.y);

  if d < CAggIntersectionEpsilon then
      Result := CalculateDistance(x1, y1, x, y)
  else
      Result := ((x - x2) * Delta.y - (y - y2) * Delta.x) / d;
end;

function CalculateIntersection(Ax, Ay, Bx, By, Cx, Cy, dx, dy: Double;
  x, y: PDouble): Boolean;
var
  r, Num, Den: Double;
begin
  Num := (Ay - Cy) * (dx - Cx) - (Ax - Cx) * (dy - Cy);
  Den := (Bx - Ax) * (dy - Cy) - (By - Ay) * (dx - Cx);

  if Abs(Den) < CAggIntersectionEpsilon then
      Result := False

  else
    begin
      r := Num / Den;
      x^ := Ax + r * (Bx - Ax);
      y^ := Ay + r * (By - Ay);

      Result := True;
    end;
end;

function IntersectionExists(x1, y1, x2, y2, x3, y3, x4, y4: Double): Boolean;
var
  Delta: array [0 .. 1] of TPointDouble;
begin
  Delta[0].x := x2 - x1;
  Delta[0].y := y2 - y1;
  Delta[1].x := x4 - x3;
  Delta[1].y := y4 - y3;

  Result := (((x3 - x2) * Delta[0].y - (y3 - y2) * Delta[0].x < 0.0) <>
    ((x4 - x2) * Delta[0].y - (y4 - y2) * Delta[0].x < 0.0)) and
    (((x1 - x4) * Delta[1].y - (y1 - y4) * Delta[1].x < 0.0) <>
    ((x2 - x4) * Delta[1].y - (y2 - y4) * Delta[1].x < 0.0));
end;

procedure CalculateOrthogonal(Thickness, x1, y1, x2, y2: Double; x, y: PDouble);
var
  d: Double;
  Delta: TPointDouble;
begin
  Delta.x := x2 - x1;
  Delta.y := y2 - y1;
  d := Thickness / Hypot(Delta.x, Delta.y);
  x^ := Delta.y * d;
  y^ := Delta.x * d;
end;

procedure DilateTriangle(x1, y1, x2, y2, x3, y3: Double; x, y: PDouble;
  d: Double);
var
  Loc: Double;
  Delta: array [0 .. 3] of TPointDouble;
begin
  Delta[0] := PointDouble(0);
  Delta[1] := PointDouble(0);
  Delta[2] := PointDouble(0);
  Loc := CalculatePointLocation(x1, y1, x2, y2, x3, y3);

  if Abs(Loc) > CAggIntersectionEpsilon then
    begin
      if CalculatePointLocation(x1, y1, x2, y2, x3, y3) > 0.0 then
          d := -d;

      CalculateOrthogonal(d, x1, y1, x2, y2, @Delta[0].x, @Delta[0].y);
      CalculateOrthogonal(d, x2, y2, x3, y3, @Delta[1].x, @Delta[1].y);
      CalculateOrthogonal(d, x3, y3, x1, y1, @Delta[2].x, @Delta[2].y);
    end;

  x^ := x1 + Delta[0].x;
  inc(x);
  y^ := y1 - Delta[0].y;
  inc(y);
  x^ := x2 + Delta[0].x;
  inc(x);
  y^ := y2 - Delta[0].y;
  inc(y);
  x^ := x2 + Delta[1].x;
  inc(x);
  y^ := y2 - Delta[1].y;
  inc(y);
  x^ := x3 + Delta[1].x;
  inc(x);
  y^ := y3 - Delta[1].y;
  inc(y);
  x^ := x3 + Delta[2].x;
  inc(x);
  y^ := y3 - Delta[2].y;
  inc(y);
  x^ := x1 + Delta[2].x;
  inc(x);
  y^ := y1 - Delta[2].y;
  inc(y);
end;

function CalculateTriangleArea(x1, y1, x2, y2, x3, y3: Double): Double;
begin
  Result := (x1 * y2 - x2 * y1 + x2 * y3 - x3 * y2 + x3 * y1 - x1 * y3) * 0.5;
end;

function CalculatePolygonArea(st: PAggStorageXY): Double;
var
  i: Cardinal;
  v: PPointDouble;
  x, y, Sum, XS, YS: Double;
begin
  Sum := 0.0;
  x := st.Poly[0].x;
  y := st.Poly[0].y;
  XS := x;
  YS := y;

  if st.Size > 0 then
    for i := 1 to st.Size - 1 do
      begin
        v := @st.Poly[i];

        Sum := Sum + (x * v.y - y * v.x);

        x := v.x;
        y := v.y;
      end;

  Result := (Sum + x * YS - y * XS) * 0.5;
end;

function CalculatePolygonAreaVertexSequence(st: TAggVertexSequence): Double;
var
  i: Cardinal;
  v: PAggVertexDistance;
  Pos: TPointDouble;
  Sum, XS, YS: Double;
begin
  Sum := 0.0;
  Pos := PAggVertexDistance(st[0]).Pos;
  XS := Pos.x;
  YS := Pos.y;

  if st.Size > 0 then
    for i := 1 to st.Size - 1 do
      begin
        v := st[i];

        Sum := Sum + (Pos.x * v.Pos.y - Pos.y * v.Pos.x);

        Pos := v.Pos;
      end;

  Result := (Sum + Pos.x * YS - Pos.y * XS) * 0.5;
end;

function FastSqrt(val: Cardinal): Cardinal;
var
  Bit: Integer;
  t, Shift: Cardinal;
begin
  t := val;
  Bit := 0;

  Shift := 11;

  // The following piece of code is just an emulation of the
  // Ix86 assembler command "bsr" (see below). However on old
  // Intels (like Intel MMX 233MHz) this code is about twice
  // faster (sic!) then just one "bsr". On PIII and PIV the
  // bsr is optimized quite well.
  Bit := t shr 24;

  if Bit <> 0 then
      Bit := CAggElderBitTable[Bit] + 24
  else
    begin
      Bit := (t shr 16) and $FF;

      if Bit <> 0 then
          Bit := CAggElderBitTable[Bit] + 16
      else
        begin
          Bit := (t shr 8) and $FF;

          if Bit <> 0 then
              Bit := CAggElderBitTable[Bit] + 8
          else
              Bit := CAggElderBitTable[t];
        end;
    end;

  // This is calculation sqrt itself.
  Bit := Bit - 9;

  if Bit > 0 then
    begin
      Bit := (ShrInt32(Bit, 1)) + (Bit and 1);
      Shift := Shift - Bit;
      val := val shr (Bit shl 1);
    end;

  Result := CAggSqrtTable[val] shr Shift;
end;

// --------------------------------------------------------------------besj
// Function BESJ calculates Bessel function of first kind of order n
// Arguments:
// n - an integer (>=0), the order
// x - value at which the Bessel function is required
// --------------------
// C++ Mathematical Library
// Convereted from equivalent FORTRAN library
// Converetd by Gareth Walker for use by course 392 computational project
// All functions tested and yield the same results as the corresponding
// FORTRAN versions.
//
// If you have any problems using these functions please report them to
// M.Muldoon@UMIST.ac.uk
//
// Documentation available on the web
// http://www.ma.umist.ac.uk/mrm/Teaching/392/libs/392.html
// Version 1.0   8/98
// 29 October, 1999
// --------------------
// Adapted for use in AGG library by Andy Wilk (castor.vulgaris@gmail.com)
// ------------------------------------------------------------------------
function Besj(x: Double; n: Integer): Double;
var
  i, m1, m2, M8, IMAX: Integer;
  d, b, b1, c2, c3, c4, C6: Double;
begin
  if n < 0 then
    begin
      Result := 0;

      Exit;
    end;

  d := 1E-6;
  b := 0;

  if Abs(x) <= d then
    begin
      if n <> 0 then
          Result := 0
      else
          Result := 1;

      Exit;
    end;

  b1 := 0; // b1 is the value from the previous iteration

  // Set up a starting order for recurrence
  m1 := Trunc(Abs(x) + 6);

  if Abs(x) > 5 then
      m1 := Trunc(Abs(1.4 * x + 60 / x));

  m2 := Trunc(n + 2 + Abs(x) * 0.25);

  if m1 > m2 then
      m2 := m1;

  // Apply recurrence down from curent max order
  repeat
    c3 := 0;
    c2 := 1E-30;
    c4 := 0;
    M8 := 1;

    if m2 div 2 * 2 = m2 then
        M8 := -1;

    IMAX := m2 - 2;

    for i := 1 to IMAX do
      begin
        C6 := 2 * (m2 - i) * c2 / x - c3;
        c3 := c2;
        c2 := C6;

        if m2 - i - 1 = n then
            b := C6;

        M8 := -1 * M8;

        if M8 > 0 then
            c4 := c4 + 2 * C6;
      end;

    C6 := 2 * c2 / x - c3;

    if n = 0 then
        b := C6;

    c4 := c4 + C6;
    b := b / c4;

    if Abs(b - b1) < d then
      begin
        Result := b;
        Exit;
      end;

    b1 := b;

    inc(m2, 3);
  until False;
end;

function CrossProduct(x1, y1, x2, y2, x, y: Double): Double;
begin
  Result := (x - x2) * (y2 - y1) - (y - y2) * (x2 - x1);
end;

procedure SinCosDouble(Theta: Double; out Sin, Cos: Double);
begin
  Math.SinCos(Theta, Sin, Cos);
end;

procedure SinCos(Theta: Double; out Sin, Cos: Double);
begin
  Math.SinCos(Theta, Sin, Cos);
end;

function Hypot(x, y: Double): Double;
begin
  Result := Math.Hypot(x, y);
end;

procedure SinCosScale(Theta: Double; out Sin, Cos: Double; Scale: Double);
begin
  SinCos(Theta, Sin, Cos);
  Sin := Sin * Scale;
  Cos := Cos * Scale;
end;

procedure SinCosScale(Theta: Double; out Sin, Cos: Double; ScaleSin, ScaleCos: Double);
begin
  SinCos(Theta, Sin, Cos);
  Sin := Sin * ScaleSin;
  Cos := Cos * ScaleCos;
end;

end.
