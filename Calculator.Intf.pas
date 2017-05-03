unit Calculator.Intf;

interface

type
  ICalculatorDisplay = interface
    ['{892570F0-459D-4E87-A461-4DDCCC58A384}']
    procedure UpdateUI(const AText: String);
    procedure AddHistory(const AText: String);
  end;

  TOperatorEnum = (opNull, opAdd, opSubtract, opMultiply, opDivide);

  IOperator = interface
    ['{453ECC2F-937C-4423-9654-F2FE9D06CF3D}']
    function Calc(const A, B: Double): Double;
    function Name: String;
  end;

  ICalculator = interface
    ['{94DB130C-7678-4AA4-A0B9-454E3F885117}']
    procedure AddDigit(const ADigit: Byte);
    procedure AddDecimalSeparator;
    procedure ClearAll;
    procedure ClearEntry;
    procedure RemoveLastDigit;
    procedure InvertSignal;
    procedure SetOperator(const AOperator: TOperatorEnum);

    procedure Equals;
  end;

implementation

end.
