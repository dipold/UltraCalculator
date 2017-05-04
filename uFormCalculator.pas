unit uFormCalculator;

interface

uses
  System.SysUtils, 
  System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, uInterfaces;

type
  TFormCalc = class(TForm, ICalculatorDisplay)
    LayoutDisplay: TLayout;
    LabelDisplay: TLabel;
    GridLayoutButtons: TGridLayout;
    btnSeven: TButton;
    btnEight: TButton;
    btnNine: TButton;
    btnAdd: TButton;
    btnFour: TButton;
    btnFive: TButton;
    btnSix: TButton;
    btnSubtract: TButton;
    btnOne: TButton;
    btnTwo: TButton;
    btnThree: TButton;
    btnMultiply: TButton;
    btnZero: TButton;
    btnDecimalPoint: TButton;
    btnEquals: TButton;
    btnDivide: TButton;
    StyleBook1: TStyleBook;
    procedure btnAddClick(Sender: TObject);
    procedure btnSubtractClick(Sender: TObject);
    procedure btnMultiplyClick(Sender: TObject);
    procedure btnDivideClick(Sender: TObject);
    procedure btnDecimalPointClick(Sender: TObject);
    procedure btnEqualsClick(Sender: TObject);
    procedure btnNumberClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FCalculator: ICalculator;
  public
    // ICalculatorDisplay
    procedure UpdateUI(strText: string);
  end;

var
  FormCalc: TFormCalc;

implementation

{$R *.fmx}

uses uCalculator;

procedure TFormCalc.FormCreate(Sender: TObject);
begin
  FCalculator := TCalculator.Create(self);
end;

procedure TFormCalc.UpdateUI(strText: string);
begin
  LabelDisplay.Text := strText;
end;

procedure TFormCalc.btnAddClick(Sender: TObject);
begin
  FCalculator.SetOperator(opAdd);
end;

procedure TFormCalc.btnSubtractClick(Sender: TObject);
begin
  FCalculator.SetOperator(opSubtract);
end;

procedure TFormCalc.btnMultiplyClick(Sender: TObject);
begin
  FCalculator.SetOperator(opMultiply);
end;

procedure TFormCalc.btnDivideClick(Sender: TObject);
begin
  FCalculator.SetOperator(opDivide);
end;

procedure TFormCalc.btnDecimalPointClick(Sender: TObject);
begin
  FCalculator.AddDecimalSeparator;
end;

procedure TFormCalc.btnEqualsClick(Sender: TObject);
begin
  FCalculator.Equals;
end;

procedure TFormCalc.btnNumberClick(Sender: TObject);
var 
  LDigit: Integer;
begin
  LDigit := (Sender as TButton).Text.ToInteger;
  FCalculator.AddDigit(LDigit);
end;

end.
