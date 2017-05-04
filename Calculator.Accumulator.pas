unit Calculator.Accumulator;

interface

uses
  System.Math,
  System.SysUtils,
  Calculator.Intf,
  Service.Patterns.Nullable;

type
  TAccumulator = class
  private
    FWhole: TNullable<Int64>;
    FFrac: TNullable<Int64>;
    FFracLeadingZeros: Integer;
    FEnteringFraction: Boolean;
    function Whole: Double;
    function Frac: Double;
    class function InternalAddDigit(const AValue: TNullable<Int64>; const ADigit: Byte): Int64;
  public
    function Entering: Boolean;
    function Value: Double;
    function ValueStr: String;
    procedure SetEnteringFraction;
    procedure InvertValueSignal;
    procedure AddDigit(const ADigit: Byte);
    procedure RemoveDigit;
    procedure Reset;
  end;

implementation

{ TAccumulator }

function TAccumulator.Entering: Boolean;
begin
  Result := (not FWhole.IsNull) or (not FFrac.IsNull) or (FFracLeadingZeros > 0) or (FEnteringFraction);
end;

function TAccumulator.Value: Double;
begin
  Result := Whole + Frac;
end;

function TAccumulator.ValueStr: String;
var
  S: String;
begin
  if (not FWhole.IsNull) then
    S := FormatFloat('#,##0', FWhole, FormatSettings)
  else
    S := '0';

  if (FEnteringFraction) then
    S := S + FormatSettings.DecimalSeparator;

  if ((FFracLeadingZeros > 0) or (not FFrac.IsNull)) then
  begin
    if (FFracLeadingZeros > 0) then
      S := S + StringOfChar('0', FFracLeadingZeros);
    if (not FFrac.IsNull) then
      S := S + IntToStr(FFrac.Value);
  end;

  Result := S;
end;

function TAccumulator.Whole: Double;
begin
  if (not FWhole.IsNull) then
    Result := FWhole
  else
    Result := 0;
end;

function TAccumulator.Frac: Double;
begin
  if ((not FFrac.IsNull) and (FFrac <> 0)) then
    Result := FFrac.Value / Power(10, Floor(Log10(FFrac) + FFracLeadingZeros + 1))
  else
    Result := 0;
end;

procedure TAccumulator.SetEnteringFraction;
begin
  FEnteringFraction := True;
end;

class function TAccumulator.InternalAddDigit(const AValue: TNullable<Int64>; const ADigit: Byte): Int64;
begin
  if (AValue.IsNull) then
    Result := ADigit
  else
  begin
    //Testar possível overflow: https://plus.google.com/u/0/+RafaelDipold/posts/RGgs2H2DkzR
    if ((Abs(AValue.Value) * 10 + ADigit) > Abs(AValue.Value)) then
      Result := (AValue.Value * 10) + ADigit
    else
      Result := AValue;
  end;
end;

procedure TAccumulator.InvertValueSignal;
begin
  if (not FWhole.IsNull) then
    FWhole.Value := FWhole.Value * (-1);
end;

procedure TAccumulator.RemoveDigit;
begin
  if (FEnteringFraction) then
  begin
    if (FFrac.IsNull) then
    begin
      if (FFracLeadingZeros = 0) then
      begin
        FEnteringFraction := False;
        RemoveDigit;
      end
      else
        Dec(FFracLeadingZeros);
    end
    else
    begin
      FFrac := Trunc(FFrac.Value / 10);
      if (FFrac = 0) then
        FFrac := SNull;
    end;
  end
  else if (not FWhole.IsNull) then
  begin
    FWhole := Trunc(FWhole.Value / 10);
    if (FWhole = 0) then
      FWhole := SNull;
  end;
end;

procedure TAccumulator.Reset;
begin
  FFracLeadingZeros := 0;
  FEnteringFraction := False;
  FWhole := SNull;
  FFrac := SNull;
end;

procedure TAccumulator.AddDigit(const ADigit: Byte);
begin
  if (FEnteringFraction) then
  begin
    if ((ADigit = 0) and (FFrac.IsNull)) then
    begin
      if (FFracLeadingZeros <= 8) then
        Inc(FFracLeadingZeros);
    end
    else
    begin
      if ((FFrac.IsNull) or (IntToStr(FFrac).Length + FFracLeadingZeros <= 8)) then
        FFrac := InternalAddDigit(FFrac, ADigit);
    end;
  end
  else
    FWhole := InternalAddDigit(FWhole, ADigit);
end;

end.
