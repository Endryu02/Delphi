unit Unit5;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Generics.Collections, Math;

type
  T2DPoint = record
    X, Y: Single;
    class operator Subtract(const p1, p2: T2DPoint): T2DPoint;
    class operator Multiply(const d: double; const p: T2DPoint): T2DPoint;
    function Length: double;
    constructor Create(const x, y: double);
  end;

  ICanvas = interface
    procedure SetCurrentColor(c: TColor);
    procedure SetCurrentLineWidth(lw: Integer);
    procedure BeginDraw;
    procedure EndDraw;
    procedure MoveTo(const x, y: Single);
    procedure LineTo(const x, y: Single);
    procedure AddVertex(const p: T2DPoint);
  end;

  TShape = class
  public
    Color: TColor;
    LineWidth: Integer;
    IsSelected: Boolean;
    procedure Draw(const c: ICanvas); virtual; abstract;
    function IsPointInside(const x, y, tol: Single): Boolean; virtual; abstract;
    procedure MoveTo(const dx, dy: Single); virtual; abstract;
  end;

  TLine = class(TShape)
  public
    P1, P2: T2DPoint;
    procedure Draw(const c: ICanvas); override;
    function IsPointInside(const x, y, tol: Single): Boolean; override;
    procedure MoveTo(const dx, dy: Single); override;
  end;

  TRectangle = class(TShape)
  public
    P1, P2: T2DPoint;
    procedure Draw(const c: ICanvas); override;
    function IsPointInside(const x, y, tol: Single): Boolean; override;
    procedure MoveTo(const dx, dy: Single); override;
  end;

  TCircle = class(TShape)
  public
    Center: T2DPoint;
    Radius: Single;
    procedure Draw(const c: ICanvas); override;
    function IsPointInside(const x, y, tol: Single): Boolean; override;
    procedure MoveTo(const dx, dy: Single); override;
  end;

  TCanvasAdapter = class(TInterfacedObject, ICanvas)
  private
    FCanvas: TCanvas;
    fIsStarted: Boolean;
    fxmin, fymin, fxmax, fymax: Single;
    fLockedStyle: boolean;
    procedure SetBounds(const xmin, ymin, xmax, ymax: Single);
    function GetWidth: Integer;
    function GetHeight: Integer;
    function GetScaleFactor: double;
  public
    constructor Create(ACanvas: TCanvas);
    procedure SetCurrentColor(c: TColor);
    procedure SetCurrentLineWidth(lw: Integer);
    procedure BeginDraw;
    procedure EndDraw;
    procedure MoveTo(const x, y: Single);
    procedure LineTo(const x, y: Single);
    procedure AddVertex(const p: T2DPoint);
    procedure DrawRectangle(const x1, y1, x2, y2: Integer);
    procedure DrawEllipse(const x1, y1, x2, y2: Integer);
    function PointToPixel(const p: T2DPoint): TPoint;
    function PixelToPoint(const p: TPoint): T2DPoint;
    function GetCanvas: TCanvas;
    property xmin: single read fxmin write fxmin;
    property ymin: single read fymin write fymin;
    property xmax: single read fxmax write fxmax;
    property ymax: single read fymax write fymax;
    property LockedStyle: boolean read fLockedStyle write fLockedStyle;
    property ScaleFactor: double read GetScaleFactor;
  end;

  TForm5 = class(TForm)
    Panel1: TPanel;
    btnRectangle: TButton;
    btnCircle: TButton;
    btnLine: TButton;
    btnSelection: TButton;
    cmbLineWidth: TComboBox;
    PaintBox: TPaintBox;
    ColorBox: TColorBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnLineClick(Sender: TObject);
    procedure btnRectangleClick(Sender: TObject);
    procedure btnCircleClick(Sender: TObject);
    procedure btnSelectionClick(Sender: TObject);
    procedure cmbLineWidthChange(Sender: TObject);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxPaint(Sender: TObject);
    procedure ColorBoxChange(Sender: TObject);
    procedure PaintBoxMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private
    Shapes: TObjectList<TShape>;
    CurrentTool: string;
    SelectedColor: TColor;
    LineWidth: Integer;
    IsDrawing: Boolean;
    IsSelecting: Boolean;
    StartPoint, EndPoint: T2DPoint;
    SelectedObject: TShape;
    ZoomFactor: Single;
    PanOffset: T2DPoint;
    IsPanning: Boolean;
    StartPanPoint: T2DPoint;
    fCanvas: TCanvasAdapter;
    procedure DrawTemporaryShape(CanvasAdapter: TCanvasAdapter);
    procedure MoveSelectedObject(const dx, dy: Single);
    procedure SelectObject(const x, y: Single);
    function ScreenToWorld(const ScreenPoint: TPoint): T2DPoint;
    function WorldToScreen(const WorldPoint: T2DPoint): TPoint;
  public
  end;

  function PointLineDist(const p, p1, p2: T2DPoint): double;
  function Dot(const p1, p2: T2DPoint): double;

var
  Form5: TForm5;

implementation


function Dot(const p1, p2: T2DPoint): double;
begin
  result := p1.x*p2.x+p1.y*p2.y;
end;

function PointLineDist(const p, p1, p2: T2DPoint): double;
begin
  var v1 := p2-p1;
  var v2 := p-p1;
  var L1 := v1.Length;
  v1 := 1/L1*v1;
  var x := Dot(v1, v2);
  if (x<0) then begin
    result := v2.Length;
  end else if (x>L1) then begin
    result := (p-p2).Length;
  end else begin
    result := (v2-x*v1).Length;
  end;
end;

{$R *.dfm}

{ TCanvasAdapter }

constructor TCanvasAdapter.Create(ACanvas: TCanvas);
begin
  inherited Create;
  FCanvas := ACanvas;
  fIsStarted := False;
  SetBounds(0, 0, FCanvas.ClipRect.Width, FCanvas.ClipRect.Height);
end;

procedure TCanvasAdapter.SetBounds(const xmin, ymin, xmax, ymax: Single);
begin
  fxmin := xmin;
  fymin := ymin;
  fxmax := xmax;
  fymax := ymax;
end;

procedure TCanvasAdapter.SetCurrentColor(c: TColor);
begin
  if fLockedStyle then
    exit;
  if FCanvas <> nil then
    FCanvas.Pen.Color := c;
end;

procedure TCanvasAdapter.SetCurrentLineWidth(lw: Integer);
begin
  if fLockedStyle then
    exit;
  if FCanvas <> nil then
    FCanvas.Pen.Width := lw;
end;

procedure TCanvasAdapter.BeginDraw;
begin
  fIsStarted := False;
end;

procedure TCanvasAdapter.EndDraw;
begin
  fIsStarted := False;
end;

procedure TCanvasAdapter.MoveTo(const x, y: Single);
var
  pp: TPoint;
begin
  pp := PointToPixel(T2DPoint.Create(x, y));
  FCanvas.MoveTo(pp.X, pp.Y);
end;

procedure TCanvasAdapter.LineTo(const x, y: Single);
var
  pp: TPoint;
begin
  pp := PointToPixel(T2DPoint.Create(x, y));
  FCanvas.LineTo(pp.X, pp.Y);
end;

procedure TCanvasAdapter.AddVertex(const p: T2DPoint);
var
  pp: TPoint;
begin
  pp := PointToPixel(p);
  if fIsStarted then
    FCanvas.LineTo(pp.X, pp.Y)
  else
  begin
    FCanvas.MoveTo(pp.X, pp.Y);
    fIsStarted := True;
  end;
end;

procedure TCanvasAdapter.DrawRectangle(const x1, y1, x2, y2: Integer);
begin
  FCanvas.Rectangle(x1, y1, x2, y2);
end;

procedure TCanvasAdapter.DrawEllipse(const x1, y1, x2, y2: Integer);
begin
  FCanvas.Ellipse(x1, y1, x2, y2);
end;

function TCanvasAdapter.PointToPixel(const p: T2DPoint): TPoint;
begin
  Result.X := Round((p.X - fxmin) / (fxmax - fxmin) * GetWidth);
  Result.Y := Round(GetHeight - ((p.Y - fymin) / (fymax - fymin) * GetHeight));
end;

function TCanvasAdapter.PixelToPoint(const p: TPoint): T2DPoint;
begin
  Result.X := fxmin + (p.X / GetWidth) * (fxmax - fxmin);
  Result.Y := fymin + ((GetHeight - p.Y) / GetHeight) * (fymax - fymin);
end;

function TCanvasAdapter.GetWidth: Integer;
begin
  Result := FCanvas.ClipRect.Width;
end;

function TCanvasAdapter.GetHeight: Integer;
begin
  Result := FCanvas.ClipRect.Height;
end;

function TCanvasAdapter.GetScaleFactor: double;
begin
  result := (fxmax-fxmin)/self.GetWidth;
end;

function TCanvasAdapter.GetCanvas: TCanvas;
begin
  Result := FCanvas;
end;

{ TLine }

procedure TLine.Draw(const c: ICanvas);
begin
  c.SetCurrentColor(Color);
  c.SetCurrentLineWidth(LineWidth);
  c.BeginDraw;
  c.MoveTo(P1.X, P1.Y);
  c.LineTo(P2.X, P2.Y);
  c.EndDraw;
end;

function TLine.IsPointInside(const x, y, tol: Single): Boolean;
var
  d: Single;
begin
  d := PointLineDist(T2DPoint.Create(x, y), self.P1, self.P2);
  result := d<tol;
end;

procedure TLine.MoveTo(const dx, dy: Single);
begin
  P1.X := P1.X + dx;
  P1.Y := P1.Y + dy;
  P2.X := P2.X + dx;
  P2.Y := P2.Y + dy;
end;

{ TRectangle }

procedure TRectangle.Draw(const c: ICanvas);
var
  CanvasAdapter: TCanvasAdapter;
begin
end;

function TRectangle.IsPointInside(const x, y, tol: Single): Boolean;
var
  min_x, max_x, min_y, max_y: Single;
begin
  min_x := Min(P1.X, P2.X);
  max_x := Max(P1.X, P2.X);
  min_y := Min(P1.Y, P2.Y);
  max_y := Max(P1.Y, P2.Y);
  Result := (x >= min_x - LineWidth / 2) and (x <= max_x + LineWidth / 2) and
            (y >= min_y - LineWidth / 2) and (y <= max_y + LineWidth / 2);
end;

procedure TRectangle.MoveTo(const dx, dy: Single);
begin
  P1.X := P1.X + dx;
  P1.Y := P1.Y + dy;
  P2.X := P2.X + dx;
  P2.Y := P2.Y + dy;
end;

{ TCircle }

procedure TCircle.Draw(const c: ICanvas);
var
  CanvasAdapter: TCanvasAdapter;
begin
  c.SetCurrentColor(Color);
  c.SetCurrentLineWidth(LineWidth);
  c.BeginDraw;
  const n=64;
  for var i := 0 to n do begin
    var a := i*2*Pi/n;
    c.AddVertex(T2DPoint.Create(Center.x+cos(a)*Radius, Center.y+sin(a)*Radius));
  end;
  c.EndDraw;
end;

function TCircle.IsPointInside(const x, y, tol: Single): Boolean;
begin
  var d := (T2DPoint.Create(x,y)-Center).Length-Radius;
  result := abs(d)<Tol;
end;

procedure TCircle.MoveTo(const dx, dy: Single);
begin
  Center.X := Center.X + dx;
  Center.Y := Center.Y + dy;
end;

{ TForm5 }

procedure TForm5.FormCreate(Sender: TObject);
begin
  Shapes := TObjectList<TShape>.Create;
  SelectedColor := clBlack;
  LineWidth := 1;
  ZoomFactor := 1.0;
  PanOffset.X := 0;
  PanOffset.Y := 0;
  IsPanning := False;
  self.doubleBuffered := true;

  cmbLineWidth.Items.AddStrings(['1', '2', '3', '4', '5']);
  cmbLineWidth.ItemIndex := 0;

  ColorBox.Selected := clBlack;
  fCanvas := TCanvasAdapter.Create(PaintBox.Canvas);
  fCanvas.SetBounds(0, 0, PaintBox.Width, PaintBox.Height);
end;

procedure TForm5.FormDestroy(Sender: TObject);
begin
  Shapes.Free;
end;

procedure TForm5.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  PaintBoxMouseWheel(Sender, Shift, WheelDelta, PaintBox.ScreenToClient(MousePos), Handled);
end;

procedure TForm5.btnLineClick(Sender: TObject);
begin
  CurrentTool := 'Line';
end;

procedure TForm5.btnRectangleClick(Sender: TObject);
begin
  CurrentTool := 'Rectangle';
end;

procedure TForm5.btnCircleClick(Sender: TObject);
begin
  CurrentTool := 'Circle';
end;

procedure TForm5.btnSelectionClick(Sender: TObject);
begin
  CurrentTool := 'Selection';
end;

procedure TForm5.cmbLineWidthChange(Sender: TObject);
begin
  LineWidth := StrToIntDef(cmbLineWidth.Text, 1);
end;

procedure TForm5.PaintBoxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then
  begin
    IsPanning := True;
    StartPanPoint := fCanvas.PixelToPoint(TPoint.Create(X, Y));
  end
  else
  begin
    StartPoint := fCanvas.PixelToPoint(TPoint.Create(X, Y));
    IsDrawing := True;

    if CurrentTool = 'Selection' then
    begin
      IsSelecting := True;
      SelectObject(StartPoint.X, StartPoint.Y);
    end;
  end;
end;

procedure TForm5.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  p: T2DPoint;
begin
  p := fCanvas.PixelToPoint(TPoint.Create(X, Y));
  if IsPanning and (ssRight in Shift) then
  begin
    var dx := -(p.X - StartPanPoint.X);
    var dy := -(p.Y - StartPanPoint.Y);
    fCanvas.SetBounds(fCanvas.xmin+dx, fCanvas.ymin+dy, fCanvas.xmax+dx, fCanvas.ymax+dy);
    PaintBox.Invalidate;
  end
  else if IsSelecting and (SelectedObject <> nil) then
  begin
    MoveSelectedObject(p.x-StartPoint.x, p.y-StartPoint.y);
    StartPoint := p;
    PaintBox.Invalidate;
  end
  else if IsDrawing then
  begin
    EndPoint := p;
    PaintBox.Invalidate;
  end;

  if not IsPanning and not IsSelecting then
    SelectObject(p.X, p.Y);
end;

procedure TForm5.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Shape: TShape;
begin
  if Button = mbRight then
    IsPanning := False
  else
  begin
    IsDrawing := False;
    IsSelecting := False;

    if CurrentTool <> 'Selection' then
    begin
      EndPoint := fCanvas.PixelToPoint(TPoint.Create(X, Y));

      if CurrentTool = 'Line' then
      begin
        Shape := TLine.Create;
        TLine(Shape).P1 := StartPoint;
        TLine(Shape).P2 := EndPoint;
      end
      else if CurrentTool = 'Rectangle' then
      begin
        Shape := TRectangle.Create;
        TRectangle(Shape).P1 := StartPoint;
        TRectangle(Shape).P2 := EndPoint;
      end
      else if CurrentTool = 'Circle' then
      begin
        Shape := TCircle.Create;
        TCircle(Shape).Center := StartPoint;
        TCircle(Shape).Radius := Sqrt(Sqr(EndPoint.X - StartPoint.X) + Sqr(EndPoint.Y - StartPoint.Y));
      end
      else
        Shape := nil;

      if Assigned(Shape) then
      begin
        Shape.Color := SelectedColor;
        Shape.LineWidth := LineWidth;
        Shapes.Add(Shape);
      end;
    end
    else if SelectedObject <> nil then
    begin
      SelectedObject.IsSelected := False;
      SelectedObject := nil;
    end;

    PaintBox.Invalidate;
  end;
end;

procedure TForm5.PaintBoxPaint(Sender: TObject);
var
  CanvasAdapter: TCanvasAdapter;
  Shape: TShape;
  WorldStart, WorldEnd: T2DPoint;
begin
  CanvasAdapter := fCanvas;
  try

    // Draw all shapes
    for Shape in Shapes do
    begin
      Shape.Draw(CanvasAdapter);
      if Shape.IsSelected then
      begin
        CanvasAdapter.SetCurrentColor(clRed);
        CanvasAdapter.SetCurrentLineWidth(2);
        CanvasAdapter.LockedStyle := true;
        Shape.Draw(CanvasAdapter);
        CanvasAdapter.LockedStyle := false;
//        if Shape is TLine then
//        begin
//          WorldStart := TLine(Shape).P1;
//          WorldEnd := TLine(Shape).P2;
//          CanvasAdapter.DrawRectangle(Round(WorldStart.X - 2), Round(WorldStart.Y - 2), Round(WorldEnd.X + 2), Round(WorldEnd.Y + 2));
//        end
//        else if Shape is TRectangle then
//        begin
//          WorldStart := TRectangle(Shape).P1;
//          WorldEnd := TRectangle(Shape).P2;
//          CanvasAdapter.DrawRectangle(Round(WorldStart.X - 2), Round(WorldStart.Y - 2), Round(WorldEnd.X + 2), Round(WorldEnd.Y + 2));
//        end
//        else if Shape is TCircle then
//        begin
//          WorldStart.X := TCircle(Shape).Center.X - TCircle(Shape).Radius;
//          WorldStart.Y := TCircle(Shape).Center.Y - TCircle(Shape).Radius;
//          WorldEnd.X := TCircle(Shape).Center.X + TCircle(Shape).Radius;
//          WorldEnd.Y := TCircle(Shape).Center.Y + TCircle(Shape).Radius;
//          CanvasAdapter.DrawEllipse(Round(WorldStart.X - 2), Round(WorldStart.Y - 2), Round(WorldEnd.X + 2), Round(WorldEnd.Y + 2));
//        end;
      end;
    end;

    // Draw temporary shape if drawing
    if IsDrawing then
      DrawTemporaryShape(CanvasAdapter);
  finally
  end;
end;

procedure TForm5.DrawTemporaryShape(CanvasAdapter: TCanvasAdapter);
var
  Shape: TShape;
begin
  if CurrentTool = 'Line' then
  begin
    Shape := TLine.Create;
    TLine(Shape).P1 := StartPoint;
    TLine(Shape).P2 := EndPoint;
  end
  else if CurrentTool = 'Rectangle' then
  begin
    Shape := TRectangle.Create;
    TRectangle(Shape).P1 := StartPoint;
    TRectangle(Shape).P2 := EndPoint;
  end
  else if CurrentTool = 'Circle' then
  begin
    Shape := TCircle.Create;
    TCircle(Shape).Center := StartPoint;
    TCircle(Shape).Radius := Sqrt(Sqr(EndPoint.X - StartPoint.X) + Sqr(EndPoint.Y - StartPoint.Y));
  end
  else
    Shape := nil;

  if Assigned(Shape) then
  begin
    Shape.Color := SelectedColor;
    Shape.LineWidth := LineWidth;
    Shape.Draw(CanvasAdapter);
    Shape.Free;
  end;
end;

procedure TForm5.ColorBoxChange(Sender: TObject);
begin
  SelectedColor := ColorBox.Selected;
end;

procedure TForm5.SelectObject(const x, y: Single);
var
  i: Integer;
begin
  if SelectedObject<>nil then
    SelectedObject.IsSelected := false;
  var so := SelectedObject;
  SelectedObject := nil;
  var tol := 10*fCanvas.ScaleFactor;
  for i := 0 to Shapes.Count - 1 do
  begin
    if Shapes[i].IsPointInside(x, y, tol) then
    begin
      SelectedObject := Shapes[i];
      SelectedObject.IsSelected := True;
      Break;
    end;
  end;
  if (so<>SelectedObject) then
    PaintBox.Repaint;
end;

procedure TForm5.MoveSelectedObject(const dx, dy: Single);
begin
  if Assigned(SelectedObject) then
  begin
    SelectedObject.MoveTo(dx, dy);
    PaintBox.Invalidate;
  end;
end;

function TForm5.ScreenToWorld(const ScreenPoint: TPoint): T2DPoint;
begin
  result := fCanvas.PixelToPoint(ScreenPoint);
end;

function TForm5.WorldToScreen(const WorldPoint: T2DPoint): TPoint;
begin
  result := fCanvas.PointToPixel(WorldPoint);
end;

procedure TForm5.PaintBoxMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  var p := fCanvas.PixelToPoint(MousePos);
  var k := 1.2;
  if WheelDelta>0 then
    k := 1/k;
  fCanvas.xmin := p.x+k*(fcanvas.xmin-p.x);
  fCanvas.xmax := p.x+k*(fcanvas.xmax-p.x);
  fCanvas.ymin := p.y+k*(fcanvas.ymin-p.y);
  fCanvas.ymax := p.y+k*(fcanvas.ymax-p.y);
  PaintBox.Invalidate;
  Handled := True;
end;

{ T2DPoint }

constructor T2DPoint.Create(const x, y: double);
begin
  self.x := x;
  self.y := y;
end;

function T2DPoint.Length: double;
begin
  result := sqrt(x*x+y*y);
end;

class operator T2DPoint.Multiply(const d: double; const p: T2DPoint): T2DPoint;
begin
  result.x := d*p.x;
  result.y := d*p.y;
end;

class operator T2DPoint.Subtract(const p1, p2: T2DPoint): T2DPoint;
begin
  result.x := p2.x-p1.x;
  result.y := p2.y-p1.y;
end;

end.
