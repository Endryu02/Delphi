unit Unit1;

interface

uses
  // Подключение необходимых модулей для работы с окнами, формами, элементами управления и графикой
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Buttons,
  Vcl.Menus, Vcl.ComCtrls, Vcl.Imaging.jpeg;

type
  TForm1 = class(TForm)
    mm1: TMainMenu;        // Главное меню приложения
    lbl1: TLabel;          // Метка для отображения текста
    edit1: TEdit;          // Поле для ввода и отображения чисел и результатов
    btn1: TBitBtn;         // Кнопки для ввода чисел и операций (0-9, +, -, *, / и т.д.)
    btn2: TBitBtn;
    btn3: TBitBtn;
    btn4: TBitBtn;
    btn5: TBitBtn;
    btn6: TBitBtn;
    btn7: TBitBtn;
    btn8: TBitBtn;
    btn9: TBitBtn;
    btn10: TBitBtn;
    btn15: TBitBtn;        // Кнопка для операции вычитания
    btn16: TBitBtn;        // Кнопка для операции сложения
    btnEqual: TBitBtn;     // Кнопка для выполнения расчетов
    btn18: TBitBtn;        // Кнопка для операции умножения
    btn19: TBitBtn;        // Кнопка для операции деления
    btn20: TBitBtn;        // Кнопка для квадратного корня
    btn22: TBitBtn;        // Кнопка для очистки данных
    btn24: TBitBtn;        // Кнопка для десятичной точки
    StatusBar1: TStatusBar;// Статусная строка для отображения информации
    N1: TMenuItem;         // Пункты меню для различных действий
    N2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    N5: TMenuItem;
    N6: TMenuItem;

    // Процедуры для обработки событий, вызванных при нажатии кнопок и выборе меню
    procedure FormCreate(Sender: TObject);           // Инициализация формы
    procedure btn1Click(Sender: TObject);            // Кнопки для цифр и операций
    procedure btn2Click(Sender: TObject);
    procedure btn3Click(Sender: TObject);
    procedure btn4Click(Sender: TObject);
    procedure btn5Click(Sender: TObject);
    procedure btn6Click(Sender: TObject);
    procedure btn7Click(Sender: TObject);
    procedure btn8Click(Sender: TObject);
    procedure btn9Click(Sender: TObject);
    procedure btn10Click(Sender: TObject);
    procedure btn22Click(Sender: TObject);           // Очистка данных
    procedure btn16Click(Sender: TObject);           // Операция сложения
    procedure btn15Click(Sender: TObject);           // Операция вычитания
    procedure btn18Click(Sender: TObject);           // Операция умножения
    procedure btn19Click(Sender: TObject);           // Операция деления
    procedure btn20Click(Sender: TObject);           // Квадратный корень
    procedure N4Click(Sender: TObject);              // Закрытие программы
    procedure N5Click(Sender: TObject);              // Копирование
    procedure N6Click(Sender: TObject);              // Вставка
    procedure btnEqualClick(Sender: TObject);        // Подсчёт результата
    procedure btn24Click(Sender: TObject);           // Добавление десятичной точки
  private
    procedure ApplyButtonStyle(Button: TBitBtn);     // Установка стиля кнопок
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  kod: char;               // Переменная для хранения текущей операции (+, -, *, / и т.д.)
  x, y, z: real;           // Переменные для хранения чисел и результата
  lastValue: Real;         // Последнее введенное значение
  lastOperation: Char;     // Последняя выбранная операция

implementation

{$R *.dfm}

// Инициализация формы при запуске программы
procedure TForm1.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  // Устанавливаем фон формы и параметры шрифта
  Color := clWhite;           // Цвет фона формы
  Font.Name := 'Segoe UI';    // Шрифт интерфейса
  Font.Size := 10;            // Размер шрифта

  // Применение стиля ко всем кнопкам на форме
  for I := 0 to ComponentCount - 1 do
    if Components[I] is TBitBtn then
      ApplyButtonStyle(TBitBtn(Components[I])); // Применяем стиль к каждой кнопке типа TBitBtn
end;

// Процедура для установки общего стиля кнопок
procedure TForm1.ApplyButtonStyle(Button: TBitBtn);
begin
  Button.Font.Name := 'Segoe UI';  // Шрифт кнопок
  Button.Font.Size := 10;          // Размер шрифта для кнопок
end;


// Обработчик для вычитания
procedure TForm1.btn15Click(Sender: TObject);
begin
  if edit1.Text <> '' then              // Проверяем, что поле не пустое
  begin
    x := StrToFloat(edit1.Text);        // Преобразуем текст в число
    kod := '-';                         // Устанавливаем текущую операцию на "-"
    lastOperation := kod;               // Запоминаем последнюю операцию
    lastValue := 0;                     // Сбрасываем последнее значение
    edit1.Clear;                        // Очищаем поле ввода для нового числа
  end;
end;

// Обработчик для сложения
procedure TForm1.btn16Click(Sender: TObject);
begin
  if edit1.Text <> '' then              // Проверяем, что поле не пустое
  begin
    x := StrToFloat(edit1.Text);        // Преобразуем текст в число
    kod := '+';                         // Устанавливаем текущую операцию на "+"
    lastOperation := kod;               // Запоминаем последнюю операцию
    lastValue := 0;                     // Сбрасываем последнее значение
    edit1.Clear;                        // Очищаем поле ввода для нового числа
  end;
end;

// Обработчик для умножения
procedure TForm1.btn18Click(Sender: TObject);
begin
  if edit1.Text <> '' then              // Проверяем, что поле не пустое
  begin
    x := StrToFloat(edit1.Text);        // Преобразуем текст в число
    kod := '*';                         // Устанавливаем текущую операцию на "*"
    lastOperation := kod;               // Запоминаем последнюю операцию
    lastValue := 0;                     // Сбрасываем последнее значение
    edit1.Clear;                        // Очищаем поле ввода для нового числа
  end;
end;

// Обработчик для деления
procedure TForm1.btn19Click(Sender: TObject);
begin
  if edit1.Text <> '' then              // Проверяем, что поле не пустое
  begin
    x := StrToFloat(edit1.Text);        // Преобразуем текст в число
    kod := '/';                         // Устанавливаем текущую операцию на "/"
    lastOperation := kod;               // Запоминаем последнюю операцию
    lastValue := 0;                     // Сбрасываем последнее значение
    edit1.Clear;                        // Очищаем поле ввода для нового числа
  end;
end;

// Вычисление квадратного корня
procedure TForm1.btn20Click(Sender: TObject);
begin
  x := StrToFloat(edit1.Text);          // Преобразуем текст в число
  edit1.Clear;                          // Очищаем поле ввода
  z := Sqrt(x);                         // Вычисляем квадратный корень
  edit1.Text := FloatToStr(z);          // Отображаем результат
end;

// Очистка поля и сброс всех переменных
procedure TForm1.btn22Click(Sender: TObject);
begin
  edit1.Clear;                          // Очищаем поле ввода
  x := 0;                               // Сбрасываем значения переменных
  y := 0;
  z := 0;
  lastValue := 0;
  lastOperation := #0;
  kod := #0;
end;


// Добавление десятичной точки
procedure TForm1.btn24Click(Sender: TObject);
begin
  if Pos(',', edit1.Text) = 0 then      // Проверяем, что в поле нет запятой
  begin
    if edit1.Text = '' then
      edit1.Text := '0,'                // Если поле пустое, добавляем "0,"
    else
      edit1.Text := edit1.Text + ',';   // Иначе просто добавляем запятую в конец
  end;
end;

// Подсчёт результата операции
procedure TForm1.btnEqualClick(Sender: TObject);
begin
  if edit1.Text <> '' then              // Проверяем, что поле не пустое
  begin
    if (kod <> #0) then
      lastOperation := kod;             // Обновляем последнюю операцию

    if lastValue = 0 then               // Проверяем последнее значение
    begin
      y := StrToFloat(edit1.Text);      // Преобразуем текст в число
      lastValue := y;                   // Сохраняем его как последнее
    end
    else
      y := lastValue;                   // Используем предыдущее значение

    // Выполняем соответствующую операцию
    case lastOperation of
      '+': z := x + y;
      '-': z := x - y;
      '*': z := x * y;
      '/': if y <> 0 then               // Проверка на деление на ноль
             z := x / y
           else
           begin
             edit1.Text := 'Error: Division by zero';
             Exit;                      // Выходим из процедуры, если деление на ноль
           end;
    end;

    edit1.Text := FloatToStr(z);        // Отображаем результат
    x := z;                             // Сохраняем результат
    kod := #0;                          // Сбрасываем операцию
  end;
end;

// Обработчик меню для закрытия программы
procedure TForm1.N4Click(Sender: TObject);
begin
  Close;
end;

// Обработчик меню для копирования текста
procedure TForm1.N5Click(Sender: TObject);
begin
  Edit1.CopyToClipboard;                // Копируем содержимое поля в буфер обмена
end;

// Обработчик меню для вставки текста
procedure TForm1.N6Click(Sender: TObject);
begin
  Edit1.PasteFromClipboard;             // Вставляем текст из буфера обмена в поле
end;

// Процедуры для добавления цифр в поле ввода при нажатии соответствующих кнопок
procedure TForm1.btn1Click(Sender: TObject);
begin
  edit1.Text := edit1.Text + '0';       // Добавляем "0" в текстовое поле
end;

procedure TForm1.btn2Click(Sender: TObject);
begin
  edit1.Text := edit1.Text + '1';       // Добавляем "1" в текстовое поле
end;

procedure TForm1.btn3Click(Sender: TObject);
begin
  edit1.Text := edit1.Text + '2';       // Добавляем "2" в текстовое поле
end;

procedure TForm1.btn4Click(Sender: TObject);
begin
  edit1.Text := edit1.Text + '3';       // Добавляем "3" в текстовое поле
end;

procedure TForm1.btn5Click(Sender: TObject);
begin
  edit1.Text := edit1.Text + '4';       // Добавляем "4" в текстовое поле
end;

procedure TForm1.btn6Click(Sender: TObject);
begin
  edit1.Text := edit1.Text + '5';       // Добавляем "5" в текстовое поле
end;

procedure TForm1.btn7Click(Sender: TObject);
begin
  edit1.Text := edit1.Text + '6';       // Добавляем "6" в текстовое поле
end;

procedure TForm1.btn8Click(Sender: TObject);
begin
  edit1.Text := edit1.Text + '7';       // Добавляем "7" в текстовое поле
end;

procedure TForm1.btn9Click(Sender: TObject);
begin
  edit1.Text := edit1.Text + '8';       // Добавляем "8" в текстовое поле
end;

procedure TForm1.btn10Click(Sender: TObject);
begin
  edit1.Text := edit1.Text + '9';       // Добавляем "9" в текстовое поле
end;

end.

