program UltraCalculator;

uses
  Vcl.Forms,
  Calculator.UI in 'Calculator.UI.pas' {FCalculator},
  Calculator.Accumulator in 'Calculator.Accumulator.pas',
  Calculator.Impl in 'Calculator.Impl.pas',
  Calculator.Intf in 'Calculator.Intf.pas',
  Calculator.Operators in 'Calculator.Operators.pas',
  Service.Patterns.Nullable in 'Service.Patterns.Nullable.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFCalculator, FCalculator);
  Application.Run;
end.
