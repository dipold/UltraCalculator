unit Service.Patterns.Nullable;

interface

uses
  System.Generics.Defaults,
  System.Rtti, 
  System.SysUtils;

type
  ENullValueException = class(Exception)
  public
    constructor Create;
  end;

  ENullConvertException<T> = class(Exception)
  public
    constructor Create;
  end;

  TNullRecord = record
  end;

  TNullable<T> = record
  private
    FValue: T;
    FHasValue: Boolean;

    class procedure CheckNullOperation(ALeft, ARight: TNullable<T>); static;
    function GetIsNull: Boolean;
    function GetValue: T;
    procedure SetValue(const AValue: T);
    function GetValueOrDefault: T;
  public
    constructor Create(AValue: T); overload;

    property HasValue: Boolean read FHasValue;
    property IsNull: Boolean read GetIsNull;
    property Value: T read GetValue write SetValue;
    property ValueOrDefault: T read GetValueOrDefault;

    class operator Implicit(AValue: TNullRecord): TNullable<T>;

    class operator Implicit(AValue: T): TNullable<T>;
    class operator Implicit(AValue: TNullable<T>): T;

    class operator Explicit(AValue: T): TNullable<T>;
    class operator Explicit(AValue: TNullable<T>): T;

    class operator Equal(ALeft, ARight: TNullable<T>): Boolean;
    class operator NotEqual(ALeft, ARight: TNullable<T>): Boolean;

    class operator GreaterThan(ALeft, ARight: TNullable<T>): Boolean;
    class operator GreaterThanOrEqual(ALeft, ARight: TNullable<T>): Boolean;

    class operator LessThan(ALeft, ARight: TNullable<T>): Boolean;
    class operator LessThanOrEqual(ALeft, ARight: TNullable<T>): Boolean;
  end;

var
  SNull: TNullRecord;

implementation

{ TNullable<T> }

class procedure TNullable<T>.CheckNullOperation(ALeft, ARight: TNullable<T>);
begin
  if ((not ALeft.FHasValue) or (not ARight.FHasValue)) then
    raise ENullValueException.Create;
end;

constructor TNullable<T>.Create(AValue: T);
begin
  FHasValue := False;
  FValue := Value;
end;

class operator TNullable<T>.Implicit(AValue: T): TNullable<T>;
begin
  Result.FValue := AValue;
  Result.FHasValue := True;
end;

class operator TNullable<T>.Implicit(AValue: TNullable<T>): T;
begin
  Result := AValue.GetValue;
end;

class operator TNullable<T>.Equal(ALeft, ARight: TNullable<T>): Boolean;
begin
  if ((ALeft.FHasValue) and (ARight.FHasValue)) then
    Result := TEqualityComparer<T>.Default.Equals(ALeft.FValue, ARight.FValue)
  else
    Result := (ALeft.FHasValue = ARight.FHasValue);
end;

class operator TNullable<T>.Explicit(AValue: T): TNullable<T>;
begin
  Result.FValue := AValue;
  Result.FHasValue := True;
end;

class operator TNullable<T>.Explicit(AValue: TNullable<T>): T;
begin
  if (not AValue.FHasValue) then
    raise ENullConvertException<T>.Create;

  Result := AValue.FValue;
end;

function TNullable<T>.GetIsNull: Boolean;
begin
  Result := not FHasValue;
end;

function TNullable<T>.GetValue: T;
begin
  if not FHasValue then
    raise ENullConvertException<T>.Create;

  Result := FValue;
end;

function TNullable<T>.GetValueOrDefault: T;
begin
  if (FHasValue) then
    Result := FValue
  else
    Result := Default(T);
end;

class operator TNullable<T>.GreaterThan(ALeft, ARight: TNullable<T>): Boolean;
begin
  CheckNullOperation(ALeft, ARight);
  Result := (TComparer<T>.Default.Compare(ALeft, ARight) > 0);
end;

class operator TNullable<T>.GreaterThanOrEqual(ALeft, ARight: TNullable<T>): Boolean;
begin
  CheckNullOperation(ALeft, ARight);
  Result := (TComparer<T>.Default.Compare(ALeft, ARight) >= 0);
end;

class operator TNullable<T>.LessThanOrEqual(ALeft, ARight: TNullable<T>): Boolean;
begin
  CheckNullOperation(ALeft, ARight);
  Result := (TComparer<T>.Default.Compare(ALeft, ARight) <= 0);
end;

class operator TNullable<T>.LessThan(ALeft, ARight: TNullable<T>): Boolean;
begin
  CheckNullOperation(ALeft, ARight);
  Result := (TComparer<T>.Default.Compare(ALeft, ARight) < 0);
end;

class operator TNullable<T>.NotEqual(ALeft, ARight: TNullable<T>): Boolean;
begin
  if ((ALeft.FHasValue) and (ARight.FHasValue)) then
    Result := not TEqualityComparer<T>.Default.Equals(ALeft.FValue, ARight.FValue)
  else
    Result := (ALeft.FHasValue <> ARight.FHasValue);
end;

procedure TNullable<T>.SetValue(const AValue: T);
begin
  FValue := AValue;
  FHasValue := True;
end;

class operator TNullable<T>.Implicit(AValue: TNullRecord): TNullable<T>;
begin
  Result.FHasValue := False;
  Result.FValue := Default(T);
end;

{ ENullValueException }

constructor ENullValueException.Create;
begin
  inherited Create('Nullable: Cannot operate with SNull value.');
end;

{ ENullConvertException<T> }

constructor ENullConvertException<T>.Create;
var
  Context: TRttiContext;
  StrType: string;
begin
  Context := TRttiContext.Create;
  try
    StrType := Context.GetType(TypeInfo(T)).ToString;
  finally
    Context.Free;
  end;

  inherited Create('Nullable: Cannot convert SNull into ' + StrType + '.');
end;

end.
