unit Unit3;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, System.Generics.Collections;

type
  ICanvas = interface
    procedure DrawLine(X1, Y1, X2, Y2: Integer);
    procedure DrawRectangle(X1, Y1, X2, Y2: Integer);
    procedure DrawEllipse(X1, Y1, X2, Y2: Integer);
    procedure SetPenColor(Color: TColor);
    procedure SetPenWidth(Width: Integer);
  end;

  TShapeClass = class of TShape; // Declare TShapeClass

  TShape = class
  protected
    FColor: TColor;
    FLineWidth: Integer;
    FSelected: Boolean;
    FX1, FY1, FX2, FY2: Integer;
  public
    constructor Create(AColor: TColor; ALineWidth: Integer);
    procedure Draw(Canvas: ICanvas); virtual; abstract;
    function IsPointInside(X, Y: Integer): Boolean; virtual; abstract;
    property Color: TColor read FColor write FColor;
    property LineWidth: Integer read FLineWidth write FLineWidth;
    property Selected: Boolean read FSelected write FSelected;
    property X1: Integer read FX1 write FX1;
    property Y1: Integer read FY1 write FY1;
    property X2: Integer read FX2 write FX2;
    property Y2: Integer read FY2 write FY2;
  end;

  TLine = class(TShape)
  public
    constructor Create(X1, Y1, X2, Y2: Integer; AColor: TColor; ALineWidth: Integer);
    procedure Draw(Canvas: ICanvas); override;
    function IsPointInside(X, Y: Integer): Boolean; override;
  end;

  TRectangle = class(TShape)
  public
    constructor Create(X1, Y1, X2, Y2: Integer; AColor: TColor; ALineWidth: Integer);
    procedure Draw(Canvas: ICanvas); override;
    function IsPointInside(X, Y: Integer): Boolean; override;
  end;

  TEllipse = class(TShape)
  public
    constructor Create(X1, Y1, X2, Y2: Integer; AColor: TColor; ALineWidth: Integer);
    procedure Draw(Canvas: ICanvas); override;
    function IsPointInside(X, Y: Integer): Boolean; override;
  end;

  TCanvasAdapter = class(TInterfacedObject, ICanvas)
  private
    FCanvas: TCanvas;
    FScale: Double;
    FOffsetX, FOffsetY: Integer;
  public
    constructor Create(ACanvas: TCanvas; AScale: Double; AOffsetX, AOffsetY: Integer);
    procedure DrawLine(X1, Y1, X2, Y2: Integer);
    procedure DrawRectangle(X1, Y1, X2, Y2: Integer);
    procedure DrawEllipse(X1, Y1, X2, Y2: Integer);
    procedure SetPenColor(Color: TColor);
    procedure SetPenWidth(Width: Integer);
  end;

  TForm3 = class(TForm)
    pnl1: TPanel;
    btnLine: TButton;
    btnRectangle: TButton;
    btnEllipse: TButton;
    cbColor: TColorBox;
    cbLineWidth: TComboBox;
    PaintBox: TPaintBox;
    procedure FormCreate(Sender: TObject);
    procedure btnLineClick(Sender: TObject);
    procedure btnRectangleClick(Sender: TObject);
    procedure btnEllipseClick(Sender: TObject);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxPaint(Sender: TObject);
    procedure PaintBoxMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure cbColorChange(Sender: TObject);
    procedure cbLineWidthChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FShapes: TObjectList<TShape>;
    FCurrentShape: TShape;
    FDrawing: Boolean;
    FStartX, FStartY: Integer;
    FScale: Double;
    FOffsetX, FOffsetY: Integer;
    FCurrentDrawingTool: TShapeClass;
    procedure DrawShapes;
    procedure SelectShape(X, Y: Integer);
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.dfm}

constructor TShape.Create(AColor: TColor; ALineWidth: Integer);
begin
  FColor := AColor;
  FLineWidth := ALineWidth;
  FSelected := False;
end;

constructor TLine.Create(X1, Y1, X2, Y2: Integer; AColor: TColor; ALineWidth: Integer);
begin
  inherited Create(AColor, ALineWidth);
  FX1 := X1;
  FY1 := Y1;
  FX2 := X2;
  FY2 := Y2;
end;

procedure TLine.Draw(Canvas: ICanvas);
begin
  Canvas.SetPenColor(FColor);
  Canvas.SetPenWidth(FLineWidth);
  Canvas.DrawLine(FX1, FY1, FX2, FY2);
end;

function TLine.IsPointInside(X, Y: Integer): Boolean;
const
  Tolerance = 10;
var
  Dist: Double;
begin
  Dist := Abs((FX2 - FX1) * (FY1 - Y) - (FX1 - X) * (FY2 - FY1)) / Sqrt(Sqr(FX2 - FX1) + Sqr(FY2 - FY1));
  Result := Dist <= Tolerance;
end;

constructor TRectangle.Create(X1, Y1, X2, Y2: Integer; AColor: TColor; ALineWidth: Integer);
begin
  inherited Create(AColor, ALineWidth);
  FX1 := X1;
  FY1 := Y1;
  FX2 := X2;
  FY2 := Y2;
end;

procedure TRectangle.Draw(Canvas: ICanvas);
begin
  Canvas.SetPenColor(FColor);
  Canvas.SetPenWidth(FLineWidth);
  Canvas.DrawRectangle(FX1, FY1, FX2, FY2);
end;

function TRectangle.IsPointInside(X, Y: Integer): Boolean;
begin
  Result := (X >= FX1) and (X <= FX2) and (Y >= FY1) and (Y <= FY2);
end;

constructor TEllipse.Create(X1, Y1, X2, Y2: Integer; AColor: TColor; ALineWidth: Integer);
begin
  inherited Create(AColor, ALineWidth);
  FX1 := X1;
  FY1 := Y1;
  FX2 := X2;
  FY2 := Y2;
end;

procedure TEllipse.Draw(Canvas: ICanvas);
begin
  Canvas.SetPenColor(FColor);
  Canvas.SetPenWidth(FLineWidth);
  Canvas.DrawEllipse(FX1, FY1, FX2, FY2);
end;

function TEllipse.IsPointInside(X, Y: Integer): Boolean;
var
  CenterX, CenterY, RadiusX, RadiusY: Integer;
begin
  CenterX := (FX1 + FX2) div 2;
  CenterY := (FY1 + FY2) div 2;
  RadiusX := Abs(FX2 - FX1) div 2;
  RadiusY := Abs(FY2 - FY1) div 2;
  Result := Sqr((X - CenterX) / RadiusX) + Sqr((Y - CenterY) / RadiusY) <= 1;
end;

constructor TCanvasAdapter.Create(ACanvas: TCanvas; AScale: Double; AOffsetX, AOffsetY: Integer);
begin
  FCanvas := ACanvas;
  FScale := AScale;
  FOffsetX := AOffsetX;
  FOffsetY := AOffsetY;
end;

procedure TCanvasAdapter.DrawLine(X1, Y1, X2, Y2: Integer);
begin
  FCanvas.MoveTo(Round(X1 * FScale) + FOffsetX, Round(Y1 * FScale) + FOffsetY);
  FCanvas.LineTo(Round(X2 * FScale) + FOffsetX, Round(Y2 * FScale) + FOffsetY);
end;

procedure TCanvasAdapter.DrawRectangle(X1, Y1, X2, Y2: Integer);
begin
  FCanvas.Rectangle(Round(X1 * FScale) + FOffsetX, Round(Y1 * FScale) + FOffsetY,
                    Round(X2 * FScale) + FOffsetX, Round(Y2 * FScale) + FOffsetY);
end;

procedure TCanvasAdapter.DrawEllipse(X1, Y1, X2, Y2: Integer);
begin
  FCanvas.Ellipse(Round(X1 * FScale) + FOffsetX, Round(Y1 * FScale) + FOffsetY,
                  Round(X2 * FScale) + FOffsetX, Round(Y2 * FScale) + FOffsetY);
end;

procedure TCanvasAdapter.SetPenColor(Color: TColor);
begin
  FCanvas.Pen.Color := Color;
end;

procedure TCanvasAdapter.SetPenWidth(Width: Integer);
begin
  FCanvas.Pen.Width := Round(Width * FScale);
end;

procedure TForm3.FormCreate(Sender: TObject);
begin
  FShapes := TObjectList<TShape>.Create;
  FScale := 1.0;
  FOffsetX := 0;
  FOffsetY := 0;
end;

procedure TForm3.FormDestroy(Sender: TObject);
begin
  FShapes.Free;
  if FCurrentShape <> nil then
    FCurrentShape.Free;
end;

procedure TForm3.btnLineClick(Sender: TObject);
begin
  FCurrentDrawingTool := TLine;
  FCurrentShape := nil;
  FDrawing := True;
end;

procedure TForm3.btnRectangleClick(Sender: TObject);
begin
  FCurrentDrawingTool := TRectangle;
  FCurrentShape := nil;
  FDrawing := True;
end;

procedure TForm3.btnEllipseClick(Sender: TObject);
begin
  FCurrentDrawingTool := TEllipse;
  FCurrentShape := nil;
  FDrawing := True;
end;

procedure TForm3.PaintBoxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    if FDrawing then
    begin
      FStartX := X;
      FStartY := Y;
    end
    else
    begin
      SelectShape(X, Y);
    end;
  end;
end;

procedure TForm3.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if FDrawing and (FCurrentDrawingTool <> nil) then
  begin
    if FCurrentShape = nil then
    begin
      FCurrentShape := FCurrentDrawingTool.Create(FStartX, FStartY, X, Y, cbColor.Selected, StrToInt(cbLineWidth.Text));
    end
    else
    begin
      FCurrentShape.X2 := X;
      FCurrentShape.Y2 := Y;
    end;
    PaintBox.Invalidate;
  end;
end;

procedure TForm3.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FDrawing and (FCurrentShape <> nil) then
  begin
    FShapes.Add(FCurrentShape);
    FCurrentShape := nil;
    FDrawing := False;
  end;
end;

procedure TForm3.PaintBoxPaint(Sender: TObject);
begin
  DrawShapes;
end;

procedure TForm3.DrawShapes;
var
  Shape: TShape;
  CanvasAdapter: TCanvasAdapter;
begin
  CanvasAdapter := TCanvasAdapter.Create(PaintBox.Canvas, FScale, FOffsetX, FOffsetY);
  try
    for Shape in FShapes do
    begin
      Shape.Draw(CanvasAdapter);
      if Shape.Selected then
      begin
        PaintBox.Canvas.Pen.Color := clRed;
        PaintBox.Canvas.Pen.Width := 1;
        PaintBox.Canvas.Rectangle(Round(Shape.X1 * FScale) + FOffsetX, Round(Shape.Y1 * FScale) + FOffsetY,
                                  Round(Shape.X2 * FScale) + FOffsetX, Round(Shape.Y2 * FScale) + FOffsetY);
      end;
    end;
  finally
    CanvasAdapter.Free;
  end;
end;

procedure TForm3.SelectShape(X, Y: Integer);
var
  Shape: TShape;
  Found: Boolean;
begin
  Found := False;
  for Shape in FShapes do
  begin
    Shape.Selected := False;
  end;
  for Shape in FShapes do
  begin
    if Shape.IsPointInside(X, Y) then
    begin
      Shape.Selected := True;
      Found := True;
      Break;
    end;
  end;
  if not Found then
  begin
    FCurrentShape := nil;
  end;
  PaintBox.Invalidate;
end;

procedure TForm3.PaintBoxMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if WheelDelta > 0 then
    FScale := FScale * 1.1
  else
    FScale := FScale / 1.1;
  FOffsetX := MousePos.X - Round((MousePos.X - FOffsetX) * FScale);
  FOffsetY := MousePos.Y - Round((MousePos.Y - FOffsetY) * FScale);
  PaintBox.Invalidate;
end;

procedure TForm3.cbColorChange(Sender: TObject);
var
  Shape: TShape;
begin
  for Shape in FShapes do
  begin
    if Shape.Selected then
    begin
      Shape.Color := cbColor.Selected;
    end;
  end;
  PaintBox.Invalidate;
end;

procedure TForm3.cbLineWidthChange(Sender: TObject);
var
  Shape: TShape;
begin
  for Shape in FShapes do
  begin
    if Shape.Selected then
    begin
      Shape.LineWidth := StrToInt(cbLineWidth.Text);
    end;
  end;
  PaintBox.Invalidate;
end;

end.
