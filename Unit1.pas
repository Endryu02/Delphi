unit Unit1;

interface

uses
  // ����������� ����������� ������� ��� ������ � ������, �������, ���������� ���������� � ��������
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Buttons,
  Vcl.Menus, Vcl.ComCtrls, Vcl.Imaging.jpeg;

type
  TForm1 = class(TForm)
    mm1: TMainMenu;        // ������� ���� ����������
    lbl1: TLabel;          // ����� ��� ����������� ������
    edit1: TEdit;          // ���� ��� ����� � ����������� ����� � �����������
    btn1: TBitBtn;         // ������ ��� ����� ����� � �������� (0-9, +, -, *, / � �.�.)
    btn2: TBitBtn;
    btn3: TBitBtn;
    btn4: TBitBtn;
    btn5: TBitBtn;
    btn6: TBitBtn;
    btn7: TBitBtn;
    btn8: TBitBtn;
    btn9: TBitBtn;
    btn10: TBitBtn;
    btn15: TBitBtn;        // ������ ��� �������� ���������
    btn16: TBitBtn;        // ������ ��� �������� ��������
    btnEqual: TBitBtn;     // ������ ��� ���������� ��������
    btn18: TBitBtn;        // ������ ��� �������� ���������
    btn19: TBitBtn;        // ������ ��� �������� �������
    btn20: TBitBtn;        // ������ ��� ����������� �����
    btn22: TBitBtn;        // ������ ��� ������� ������
    btn24: TBitBtn;        // ������ ��� ���������� �����
    StatusBar1: TStatusBar;// ��������� ������ ��� ����������� ����������
    N1: TMenuItem;         // ������ ���� ��� ��������� ��������
    N2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    N5: TMenuItem;
    N6: TMenuItem;

    // ��������� ��� ��������� �������, ��������� ��� ������� ������ � ������ ����
    procedure FormCreate(Sender: TObject);           // ������������� �����
    procedure btn1Click(Sender: TObject);            // ������ ��� ���� � ��������
    procedure btn2Click(Sender: TObject);
    procedure btn3Click(Sender: TObject);
    procedure btn4Click(Sender: TObject);
    procedure btn5Click(Sender: TObject);
    procedure btn6Click(Sender: TObject);
    procedure btn7Click(Sender: TObject);
    procedure btn8Click(Sender: TObject);
    procedure btn9Click(Sender: TObject);
    procedure btn10Click(Sender: TObject);
    procedure btn22Click(Sender: TObject);           // ������� ������
    procedure btn16Click(Sender: TObject);           // �������� ��������
    procedure btn15Click(Sender: TObject);           // �������� ���������
    procedure btn18Click(Sender: TObject);           // �������� ���������
    procedure btn19Click(Sender: TObject);           // �������� �������
    procedure btn20Click(Sender: TObject);           // ���������� ������
    procedure N4Click(Sender: TObject);              // �������� ���������
    procedure N5Click(Sender: TObject);              // �����������
    procedure N6Click(Sender: TObject);              // �������
    procedure btnEqualClick(Sender: TObject);        // ������� ����������
    procedure btn24Click(Sender: TObject);           // ���������� ���������� �����
  private
    procedure ApplyButtonStyle(Button: TBitBtn);     // ��������� ����� ������
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  kod: char;               // ���������� ��� �������� ������� �������� (+, -, *, / � �.�.)
  x, y, z: real;           // ���������� ��� �������� ����� � ����������
  lastValue: Real;         // ��������� ��������� ��������
  lastOperation: Char;     // ��������� ��������� ��������

implementation

{$R *.dfm}

// ������������� ����� ��� ������� ���������
procedure TForm1.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  // ������������� ��� ����� � ��������� ������
  Color := clWhite;           // ���� ���� �����
  Font.Name := 'Segoe UI';    // ����� ����������
  Font.Size := 10;            // ������ ������

  // ���������� ����� �� ���� ������� �� �����
  for I := 0 to ComponentCount - 1 do
    if Components[I] is TBitBtn then
      ApplyButtonStyle(TBitBtn(Components[I])); // ��������� ����� � ������ ������ ���� TBitBtn
end;

// ��������� ��� ��������� ������ ����� ������
procedure TForm1.ApplyButtonStyle(Button: TBitBtn);
begin
  Button.Font.Name := 'Segoe UI';  // ����� ������
  Button.Font.Size := 10;          // ������ ������ ��� ������
end;


// ���������� ��� ���������
procedure TForm1.btn15Click(Sender: TObject);
begin
  if edit1.Text <> '' then              // ���������, ��� ���� �� ������
  begin
    x := StrToFloat(edit1.Text);        // ����������� ����� � �����
    kod := '-';                         // ������������� ������� �������� �� "-"
    lastOperation := kod;               // ���������� ��������� ��������
    lastValue := 0;                     // ���������� ��������� ��������
    edit1.Clear;                        // ������� ���� ����� ��� ������ �����
  end;
end;

// ���������� ��� ��������
procedure TForm1.btn16Click(Sender: TObject);
begin
  if edit1.Text <> '' then              // ���������, ��� ���� �� ������
  begin
    x := StrToFloat(edit1.Text);        // ����������� ����� � �����
    kod := '+';                         // ������������� ������� �������� �� "+"
    lastOperation := kod;               // ���������� ��������� ��������
    lastValue := 0;                     // ���������� ��������� ��������
    edit1.Clear;                        // ������� ���� ����� ��� ������ �����
  end;
end;

// ���������� ��� ���������
procedure TForm1.btn18Click(Sender: TObject);
begin
  if edit1.Text <> '' then              // ���������, ��� ���� �� ������
  begin
    x := StrToFloat(edit1.Text);        // ����������� ����� � �����
    kod := '*';                         // ������������� ������� �������� �� "*"
    lastOperation := kod;               // ���������� ��������� ��������
    lastValue := 0;                     // ���������� ��������� ��������
    edit1.Clear;                        // ������� ���� ����� ��� ������ �����
  end;
end;

// ���������� ��� �������
procedure TForm1.btn19Click(Sender: TObject);
begin
  if edit1.Text <> '' then              // ���������, ��� ���� �� ������
  begin
    x := StrToFloat(edit1.Text);        // ����������� ����� � �����
    kod := '/';                         // ������������� ������� �������� �� "/"
    lastOperation := kod;               // ���������� ��������� ��������
    lastValue := 0;                     // ���������� ��������� ��������
    edit1.Clear;                        // ������� ���� ����� ��� ������ �����
  end;
end;

// ���������� ����������� �����
procedure TForm1.btn20Click(Sender: TObject);
begin
  x := StrToFloat(edit1.Text);          // ����������� ����� � �����
  edit1.Clear;                          // ������� ���� �����
  z := Sqrt(x);                         // ��������� ���������� ������
  edit1.Text := FloatToStr(z);          // ���������� ���������
end;

// ������� ���� � ����� ���� ����������
procedure TForm1.btn22Click(Sender: TObject);
begin
  edit1.Clear;                          // ������� ���� �����
  x := 0;                               // ���������� �������� ����������
  y := 0;
  z := 0;
  lastValue := 0;
  lastOperation := #0;
  kod := #0;
end;


// ���������� ���������� �����
procedure TForm1.btn24Click(Sender: TObject);
begin
  if Pos(',', edit1.Text) = 0 then      // ���������, ��� � ���� ��� �������
  begin
    if edit1.Text = '' then
      edit1.Text := '0,'                // ���� ���� ������, ��������� "0,"
    else
      edit1.Text := edit1.Text + ',';   // ����� ������ ��������� ������� � �����
  end;
end;

// ������� ���������� ��������
procedure TForm1.btnEqualClick(Sender: TObject);
begin
  if edit1.Text <> '' then              // ���������, ��� ���� �� ������
  begin
    if (kod <> #0) then
      lastOperation := kod;             // ��������� ��������� ��������

    if lastValue = 0 then               // ��������� ��������� ��������
    begin
      y := StrToFloat(edit1.Text);      // ����������� ����� � �����
      lastValue := y;                   // ��������� ��� ��� ���������
    end
    else
      y := lastValue;                   // ���������� ���������� ��������

    // ��������� ��������������� ��������
    case lastOperation of
      '+': z := x + y;
      '-': z := x - y;
      '*': z := x * y;
      '/': if y <> 0 then               // �������� �� ������� �� ����
             z := x / y
           else
           begin
             edit1.Text := 'Error: Division by zero';
             Exit;                      // ������� �� ���������, ���� ������� �� ����
           end;
    end;

    edit1.Text := FloatToStr(z);        // ���������� ���������
    x := z;                             // ��������� ���������
    kod := #0;                          // ���������� ��������
  end;
end;

// ���������� ���� ��� �������� ���������
procedure TForm1.N4Click(Sender: TObject);
begin
  Close;
end;

// ���������� ���� ��� ����������� ������
procedure TForm1.N5Click(Sender: TObject);
begin
  Edit1.CopyToClipboard;                // �������� ���������� ���� � ����� ������
end;

// ���������� ���� ��� ������� ������
procedure TForm1.N6Click(Sender: TObject);
begin
  Edit1.PasteFromClipboard;             // ��������� ����� �� ������ ������ � ����
end;

// ��������� ��� ���������� ���� � ���� ����� ��� ������� ��������������� ������
procedure TForm1.btn1Click(Sender: TObject);
begin
  edit1.Text := edit1.Text + '0';       // ��������� "0" � ��������� ����
end;

procedure TForm1.btn2Click(Sender: TObject);
begin
  edit1.Text := edit1.Text + '1';       // ��������� "1" � ��������� ����
end;

procedure TForm1.btn3Click(Sender: TObject);
begin
  edit1.Text := edit1.Text + '2';       // ��������� "2" � ��������� ����
end;

procedure TForm1.btn4Click(Sender: TObject);
begin
  edit1.Text := edit1.Text + '3';       // ��������� "3" � ��������� ����
end;

procedure TForm1.btn5Click(Sender: TObject);
begin
  edit1.Text := edit1.Text + '4';       // ��������� "4" � ��������� ����
end;

procedure TForm1.btn6Click(Sender: TObject);
begin
  edit1.Text := edit1.Text + '5';       // ��������� "5" � ��������� ����
end;

procedure TForm1.btn7Click(Sender: TObject);
begin
  edit1.Text := edit1.Text + '6';       // ��������� "6" � ��������� ����
end;

procedure TForm1.btn8Click(Sender: TObject);
begin
  edit1.Text := edit1.Text + '7';       // ��������� "7" � ��������� ����
end;

procedure TForm1.btn9Click(Sender: TObject);
begin
  edit1.Text := edit1.Text + '8';       // ��������� "8" � ��������� ����
end;

procedure TForm1.btn10Click(Sender: TObject);
begin
  edit1.Text := edit1.Text + '9';       // ��������� "9" � ��������� ����
end;

end.

