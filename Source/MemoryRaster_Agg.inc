constructor TMemoryRaster_AggImage.Create(raster: TMemoryRaster);
begin
  inherited Create(PInt8u(raster.ScanLine[0]), raster.width, raster.height, 4 * raster.width);
end;

procedure TMemoryRaster_AggImage.Attach(raster: TMemoryRaster);
begin
  inherited Attach(PInt8u(raster.ScanLine[0]), raster.width, raster.height, 4 * raster.width);
end;

function TMemoryRaster_Agg2D.GetImageBlendColor: TRasterColor;
begin
  Result := AggColor(FImageBlendColor);
end;

procedure TMemoryRaster_Agg2D.SetImageBlendColor(const Value: TRasterColor);
begin
  inherited ImageBlendColor := AggColor(Value);
end;

function TMemoryRaster_Agg2D.GetFillColor: TRasterColor;
begin
  Result := AggColor(FFillColor);
end;

procedure TMemoryRaster_Agg2D.SetFillColor(const Value: TRasterColor);
begin
  inherited FillColor := AggColor(Value);
end;

function TMemoryRaster_Agg2D.GetLineColor: TRasterColor;
begin
  Result := AggColor(FLineColor);
end;

procedure TMemoryRaster_Agg2D.SetLineColor(const Value: TRasterColor);
begin
  inherited LineColor := AggColor(Value);
end;

procedure TMemoryRaster_Agg2D.Attach(raster: TMemoryRaster);
begin
  inherited Attach(PInt8u(raster.ScanLine[0]), raster.width, raster.height, 4 * raster.width);
end;

procedure TMemoryRaster_Agg2D.FillLinearGradient(x1, y1, x2, y2: Double; c1, c2: TRasterColor; Profile: Double);
begin
  inherited FillLinearGradient(x1, y1, x2, y2, AggColor(c1), AggColor(c2), Profile);
end;

procedure TMemoryRaster_Agg2D.LineLinearGradient(x1, y1, x2, y2: Double; c1, c2: TRasterColor; Profile: Double);
begin
  inherited LineLinearGradient(x1, y1, x2, y2, AggColor(c1), AggColor(c2), Profile);
end;

procedure TMemoryRaster_Agg2D.FillRadialGradient(x, y, r: Double; c1, c2: TRasterColor; Profile: Double);
begin
  inherited FillRadialGradient(x, y, r, AggColor(c1), AggColor(c2), Profile);
end;

procedure TMemoryRaster_Agg2D.LineRadialGradient(x, y, r: Double; c1, c2: TRasterColor; Profile: Double);
begin
  inherited LineRadialGradient(x, y, r, AggColor(c1), AggColor(c2), Profile);
end;

procedure TMemoryRaster_Agg2D.FillRadialGradient(x, y, r: Double; c1, c2, c3: TRasterColor);
begin
  inherited FillRadialGradient(x, y, r, AggColor(c1), AggColor(c2), AggColor(c3));
end;

procedure TMemoryRaster_Agg2D.LineRadialGradient(x, y, r: Double; c1, c2, c3: TRasterColor);
begin
  inherited LineRadialGradient(x, y, r, AggColor(c1), AggColor(c2), AggColor(c3));
end; 
 
 
