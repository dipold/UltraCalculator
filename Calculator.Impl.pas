unit Calculator.Impl;

interface

uses
  Winapi.Messages,
  Winapi.Windows,
  System.SysUtils,
  Vcl.AppEvnts,
  Calculator.Intf,
  Calculator.Operators,
  Calculator.Accumulator,
  Service.Patterns.Nullable;

type
  TCalculator = class(TInterfacedObject, ICalculator)
  private
    FDisplay: ICalculatorDisplay;
    FAppEvents: TApplicationEvents;
    FOperator: IOperator;
    FLeft: TNullable<Double>;
    FRight: TNullable<Double>;
    FResult: TNullable<Double>;
    FAccumulator: TAccumulator;
    procedure AddHistory(const AValue: Double; const AOperator: String = '');
    procedure PromoteAccumulator;
    procedure UpdateUI;
    procedure OnKeyboartEvent(var AMsg: TWMKey; var AHandled: Boolean);
    function FormatNumber(const AValue: Double): String;
  public
    constructor Create(ADisplay: ICalculatorDisplay);
    destructor Destroy; override;

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

{ TCalculator }

procedure TCalculator.InvertSignal;
begin
  FAccumulator.InvertValueSignal;
end;

procedure TCalculator.OnKeyboartEvent(var AMsg: TWMKey; var AHandled: Boolean);
begin
  if (GetKeyState(VK_CONTROL) and 128 = 0) then //Ctrl não pressionado
  begin
    if (GetKeyState(VK_NUMLOCK) and 128 = 0) then //Num Lock ativado
    begin
      case AMsg.CharCode of
        VK_NUMPAD0  : AddDigit(0);
        VK_NUMPAD1  : AddDigit(1);
        VK_NUMPAD2  : AddDigit(2);
        VK_NUMPAD3  : AddDigit(3);
        VK_NUMPAD4  : AddDigit(4);
        VK_NUMPAD5  : AddDigit(5);
        VK_NUMPAD6  : AddDigit(6);
        VK_NUMPAD7  : AddDigit(7);
        VK_NUMPAD8  : AddDigit(8);
        VK_NUMPAD9  : AddDigit(9);
        VK_OEM_COMMA: AddDecimalSeparator;
      end;
    end;

    case AMsg.CharCode of
      VK_DIVIDE   : SetOperator(opDivide);
      VK_MULTIPLY : SetOperator(opMultiply);
      VK_SUBTRACT : SetOperator(opSubtract);
      VK_ADD      : SetOperator(opAdd);
      VK_RETURN   :
      begin
        Equals;
        AHandled := True; //não propaga
      end;
    end;

    if (GetKeyState(VK_SHIFT) and 128 <> 0) then //Shift pressionado
    begin
      case AMsg.CharCode of
        Ord('8')      : SetOperator(opMultiply);
        VK_OEM_PLUS   : SetOperator(opAdd);
      end;
    end
    else
    begin
      case AMsg.CharCode of
        Ord('0')      : AddDigit(0);
        Ord('1')      : AddDigit(1);
        Ord('2')      : AddDigit(2);
        Ord('3')      : AddDigit(3);
        Ord('4')      : AddDigit(4);
        Ord('5')      : AddDigit(5);
        Ord('6')      : AddDigit(6);
        Ord('7')      : AddDigit(7);
        Ord('8')      : AddDigit(8);
        Ord('9')      : AddDigit(9);
        Ord('┴'),
        VK_OEM_MINUS  : SetOperator(opSubtract);
        VK_DECIMAL    : AddDecimalSeparator;
        VK_DELETE     : ClearEntry;
        VK_ESCAPE     : ClearAll;
        VK_OEM_PLUS   : Equals;
      end;
    end;
  end;
end;

procedure TCalculator.ClearAll;
begin
  FResult := SNull;
  FLeft := SNull;
  FRight := SNull;
  SetOperator(opNull);
  FAccumulator.Reset;

	UpdateUI;
end;

procedure TCalculator.ClearEntry;
begin
  FAccumulator.Reset;
	UpdateUI;
end;

constructor TCalculator.Create(ADisplay: ICalculatorDisplay);
begin
  FDisplay := ADisplay;
  FAccumulator := TAccumulator.Create;
  FAppEvents := TApplicationEvents.Create(nil);
  FAppEvents.OnShortCut := OnKeyboartEvent;
  FOperator:= nil;

  UpdateUI;
end;

destructor TCalculator.Destroy;
begin
  FAppEvents.Free;
  FAccumulator.Free;
  inherited;
end;

procedure TCalculator.AddDigit(const ADigit: Byte);
begin
  if (not FResult.IsNull) then
    FResult := SNull;

  FAccumulator.AddDigit(ADigit);

  UpdateUI;
end;

procedure TCalculator.AddDecimalSeparator;
begin
  FAccumulator.SetEnteringFraction;
  UpdateUI;
end;

procedure TCalculator.PromoteAccumulator;
begin
  if (FAccumulator.Entering) then
  begin
    if (FLeft.IsNull) then
    begin
      FLeft := FAccumulator.Value;
      AddHistory(FLeft);
    end
    else
    begin
      Assert(FRight.IsNull);
      FRight := FAccumulator.Value;
      if (Assigned(FOperator)) then
        AddHistory(FRight, FOperator.Name);
    end;

    FAccumulator.Reset;
  end;
end;

procedure TCalculator.RemoveLastDigit;
begin
  FAccumulator.RemoveDigit;
  UpdateUI;
end;

procedure TCalculator.SetOperator(const AOperator: TOperatorEnum);
begin
  if (AOperator = opNull) then
  begin
    FOperator := nil;
  end
  else
  begin
    PromoteAccumulator;

    if ((FResult.IsNull) and (not FLeft.IsNull) and (not FRight.IsNull)) then
    begin
      Equals;
      SetOperator(AOperator);
    end
    else if (not FResult.IsNull) then
    begin
      FLeft := FResult;
      FRight := SNull;
      FResult := SNull;
      SetOperator(AOperator);
    end
    else if (not FLeft.IsNull) then
    begin
      FOperator := CreateOperator(AOperator);
    end;
  end;

  UpdateUI;
end;

procedure TCalculator.Equals;
begin
	PromoteAccumulator;

  if ((not FLeft.IsNull) and (not FRight.IsNull) and (Assigned(FOperator))) then
    FResult := FOperator.Calc(FLeft, FRight)
  else
    FResult := FLeft;

  if (not FResult.IsNull) then
    AddHistory(FResult, '=');

  FRight := SNull;
  FLeft := SNull;

  SetOperator(opNull);

	UpdateUI;
end;

function TCalculator.FormatNumber(const AValue: Double): String;
begin
//  Result := Format('%g', [AValue], FormatSettings);
  Result := FormatFloat('0,.#########', AValue, FormatSettings);
end;

procedure TCalculator.AddHistory(const AValue: Double; const AOperator: String);
begin
  if (AOperator.IsEmpty) then
    FDisplay.AddHistory('');

  FDisplay.AddHistory(FormatNumber(AValue) + AOperator.PadLeft(2));
end;

procedure TCalculator.UpdateUI;
var
  Display: String;
begin
  Display := '0';

  if (not FLeft.IsNull) then
  begin
    Display := FormatNumber(FLeft);

    if (Assigned(FOperator)) then
    begin
      Display := Display + ' ' + FOperator.Name + ' ';

      if (not FRight.IsNull) then
        Display := Display + FormatNumber(FRight)
      else if (FAccumulator.Entering) then
        Display := Display + FAccumulator.ValueStr;
    end;
  end
  else if (FAccumulator.Entering) then
    Display := FAccumulator.ValueStr
  else if (not FResult.IsNull) then
    Display := FormatNumber(FResult);

  FDisplay.UpdateUI(Display);
end;

end.
