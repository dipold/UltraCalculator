unit Calculator.UI;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.Classes, System.ImageList, System.SysUtils, System.Variants,
  Vcl.Controls, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Forms, Vcl.Graphics, Vcl.ImgList, Vcl.StdCtrls,
  Calculator.Intf;

type
  TFCalculator = class(TForm, ICalculatorDisplay)
    gpButtons: TGridPanel;
    btClearLast: TButton;
    btClearAll: TButton;
    btBackspace: TButton;
    btOperatorDiv: TButton;
    btNumber7: TButton;
    btNumber8: TButton;
    btNumber9: TButton;
    btOperatorMult: TButton;
    btNumber4: TButton;
    btNumber5: TButton;
    btNumber6: TButton;
    btOperatorAdd: TButton;
    btNumber1: TButton;
    btNumber2: TButton;
    btNumber3: TButton;
    btOperatorSub: TButton;
    btChangeSignal: TButton;
    btNumber0: TButton;
    btOperatorDecimal: TButton;
    btOperatorEqual: TButton;
    pnHistory: TPanel;
    mmHistory: TMemo;
    pnNumber: TPanel;
    lbNumberText: TLabel;
    pnHistoryActions: TPanel;
    btHistoryClear: TButton;
    btHistoryPrint: TButton;
    ilHistoryActions: TImageList;
    procedure btnNumberClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btOperatorAddClick(Sender: TObject);
    procedure btOperatorSubClick(Sender: TObject);
    procedure btOperatorMultClick(Sender: TObject);
    procedure btOperatorDivClick(Sender: TObject);
    procedure btOperatorDecimalClick(Sender: TObject);
    procedure btOperatorEqualClick(Sender: TObject);
    procedure btChangeSignalClick(Sender: TObject);
    procedure btClearAllClick(Sender: TObject);
    procedure btClearLastClick(Sender: TObject);
    procedure btBackspaceClick(Sender: TObject);
    procedure btHistoryClearClick(Sender: TObject);
    procedure btHistoryPrintClick(Sender: TObject);
  private
    FCalculator: ICalculator;
    procedure UpdateUI(const AText: String);
    procedure AddHistory(const AText: String);
  public
    { Public declarations }
  end;

var
  FCalculator: TFCalculator;

implementation

{$R *.dfm}

uses Calculator.Impl, vcl.printers;

{ TFCalculator }

procedure TFCalculator.btOperatorAddClick(Sender: TObject);
begin
  FCalculator.SetOperator(opAdd);
end;

procedure TFCalculator.btOperatorDecimalClick(Sender: TObject);
begin
  FCalculator.AddDecimalSeparator;
end;

procedure TFCalculator.btOperatorDivClick(Sender: TObject);
begin
  FCalculator.SetOperator(opDivide);
end;

procedure TFCalculator.btOperatorEqualClick(Sender: TObject);
begin
  FCalculator.Equals;
end;

procedure TFCalculator.btOperatorMultClick(Sender: TObject);
begin
  FCalculator.SetOperator(opMultiply);
end;

procedure TFCalculator.btOperatorSubClick(Sender: TObject);
begin
  FCalculator.SetOperator(opSubtract);
end;

procedure TFCalculator.FormCreate(Sender: TObject);
begin
  FCalculator := TCalculator.Create(Self);
  mmHistory.Clear;
end;

procedure TFCalculator.AddHistory(const AText: String);
begin
  mmHistory.Lines.Add(AText);
end;

procedure TFCalculator.UpdateUI(const AText: String);
begin
  lbNumberText.Caption := AText;
end;

procedure TFCalculator.btBackspaceClick(Sender: TObject);
begin
  FCalculator.RemoveLastDigit;
end;

procedure TFCalculator.btChangeSignalClick(Sender: TObject);
begin
  FCalculator.InvertSignal;
end;

procedure TFCalculator.btClearAllClick(Sender: TObject);
begin
  FCalculator.ClearAll;
end;

procedure TFCalculator.btClearLastClick(Sender: TObject);
begin
  FCalculator.ClearEntry;
end;

procedure TFCalculator.btHistoryClearClick(Sender: TObject);
begin
  mmHistory.Clear;
end;

procedure TFCalculator.btHistoryPrintClick(Sender: TObject);
var
  MemoOutput: TextFile;
  Line: String;
begin
  AssignPrn(MemoOutput);
  try
    Printer.Canvas.Font.Name := 'Lucida Console';
    Printer.Canvas.Font.Pitch := fpFixed;

    Rewrite(MemoOutput);
    for Line in mmHistory.Lines do
      WriteLn(MemoOutput, Line.PadLeft(40));
  finally
    CloseFile(MemoOutput);
  end;
end;

procedure TFCalculator.btnNumberClick(Sender: TObject);
var
  Digit: Byte;
begin
  Digit := StrToInt((Sender as TButton).Caption);
  FCalculator.AddDigit(Digit);
end;

end.
