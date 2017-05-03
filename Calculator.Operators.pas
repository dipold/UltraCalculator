unit Calculator.Operators;

interface

uses
  Calculator.Intf;

type
  TOperatorAdd = class(TInterfacedObject, IOperator)
    function Calc(const A, B: Double): Double;
    function Name: String;
  end;

  TOperatorSubtract = class(TInterfacedObject, IOperator)
    function Calc(const A, B: Double): Double;
    function Name: String;
  end;

  TOperatorMult = class(TInterfacedObject, IOperator)
    function Calc(const A, B: Double): Double;
    function Name: String;
  end;

  TOperatorDiv = class(TInterfacedObject, IOperator)
    function Calc(const A, B: Double): Double;
    function Name: String;
  end;

function CreateOperator(const AOperator: TOperatorEnum): IOperator;

implementation

function CreateOperator(const AOperator: TOperatorEnum): IOperator;
begin
  case AOperator of
    opAdd     : Result := TOperatorAdd.Create;
    opSubtract: Result := TOperatorSubtract.Create;
    opMultiply: Result := TOperatorMult.Create;
    opDivide  : Result := TOperatorDiv.Create;
  else
    Result := nil;
  end;
end;

function TOperatorAdd.Calc(const A, B: Double): Double;
begin
  Result := A + B;
end;

function TOperatorAdd.Name: String;
begin
  Result := '+';
end;

function TOperatorSubtract.Calc(const A, B: Double): Double;
begin
  Result := A - B;
end;

function TOperatorSubtract.Name: String;
begin
  Result := '-';
end;

function TOperatorMult.Calc(const A, B: Double): Double;
begin
  Result := A * B;
end;

function TOperatorMult.Name: String;
begin
  Result := 'x';
end;

function TOperatorDiv.Calc(const A, B: Double): Double;
begin
  Result := A / B;
end;

function TOperatorDiv.Name: String;
begin
  Result := '÷';
end;

end.
