unit main;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF WINDOWS}Windows, {$ENDIF}Classes, SysUtils, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, ComCtrls, StdCtrls, LCLIntF, Menus, StrUtils, fphttpclient,
  LCLType, math;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnNew: TButton;
    btnOpen: TButton;
    btnSave: TButton;
    btnAbout: TButton;
    btnImport: TButton;
    btnTransform: TButton;
    btnPaper: TButton;
    btnInk: TButton;
    checkRowFirst: TCheckBox;
    listSpriteSize: TComboBox;
    imgPreview: TImage;
    menuInvert: TMenuItem;
    menuFlip: TMenuItem;
    menuShiftRight: TMenuItem;
    menuShiftLeft: TMenuItem;
    menuShiftDown: TMenuItem;
    menuShiftUp: TMenuItem;
    menuMoveColRight: TMenuItem;
    menuMoveColLeft: TMenuItem;
    menuMoveRowUp: TMenuItem;
    menuMoveRowDown: TMenuItem;
    menuRotateA: TMenuItem;
    menuRotateC: TMenuItem;
    menuMirror: TMenuItem;
    menuTransform: TPopupMenu;
    colourPanel: TScrollBox;
    menuEdit: TPopupMenu;
    updateLabel: TLabel;
    OpenDialog1: TOpenDialog;
    updatePanel: TPanel;
    SaveDialog1: TSaveDialog;
    textOutput: TMemo;
    buttonPanel: TPanel;
    infoPanel: TPanel;
    updateTimer: TTimer;
    ToolBar1: TToolBar;
    procedure btnAboutClick(Sender: TObject);
    procedure btnImportClick(Sender: TObject);
    procedure btnInkClick(Sender: TObject);
    procedure btnNewClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure btnPaperClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnTransformClick(Sender: TObject);
    procedure buttonPanelResize(Sender: TObject);
    procedure checkRowFirstChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure ButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure listSpriteSizeChange(Sender: TObject);
    procedure menuFlipClick(Sender: TObject);
    procedure menuInvertClick(Sender: TObject);
    procedure menuMirrorClick(Sender: TObject);
    procedure menuMoveColLeftClick(Sender: TObject);
    procedure menuMoveColRightClick(Sender: TObject);
    procedure menuMoveRowDownClick(Sender: TObject);
    procedure menuMoveRowUpClick(Sender: TObject);
    procedure menuRotateAClick(Sender: TObject);
    procedure menuRotateCClick(Sender: TObject);
    procedure menuShiftDownClick(Sender: TObject);
    procedure menuShiftLeftClick(Sender: TObject);
    procedure menuShiftRightClick(Sender: TObject);
    procedure menuShiftUpClick(Sender: TObject);
    procedure updateLabelClick(Sender: TObject);
    procedure updateTimerTimer(Sender: TObject);
    procedure ColourButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    buttons: Array of Array of TShape;
    pixels: Array of Array of Byte;
    ColourButtons: Array[0..14] of TShape;
    IsSaved: Boolean;
    CurrentFile: String;
    PaperSelect: Boolean;
    InkSelect: Boolean;
    Paper: TColor;
    Ink: TColor;
    ZXColours: Array[0..14] of TColor;
    SpriteWidth: Integer;
    SpriteHeight: Integer;
    procedure GetButtonPosition(const button: TShape; var x: Integer; var y: Integer);
    procedure UpdateViewArea;
    procedure SetButtons;
    procedure UpdateWindowTitle;
    function GetLineValue(l: Integer; p: Integer; c: Integer): Byte;
    procedure SetSpriteSize;
    procedure SetButtonSize;
  public

  end;

const
  APPNAME = 'UDG Plop';
  APPVER = '0.2';
  CURRVER = 20190701;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

uses about;

// http://delphiexamples.com/mathematics/bin2dec.html
function Pow(i, k: Integer): Integer;
var
  j, Count: Integer;
begin
  if k>0 then j:=2
    else j:=1;
  for Count:=1 to k-1 do
    j:=j*2;
  Result:=j;
end;

// http://delphiexamples.com/mathematics/bin2dec.html
function BinToDec(Str: string): Integer;
var
  Len, Res, i: Integer;
  Error: Boolean;
begin
  Error:=False;
  Len:=Length(Str);
  Res:=0;
  for i:=1 to Len do
    if (Str[i]='0')or(Str[i]='1') then
      Res:=Res+Pow(2, Len-i)*StrToInt(Str[i])
    else
    begin
      Error:=True;
      Break;
    end;
  if Error=True then Result:=0
    else Result:=Res;
end;

{$IFDEF MSWINDOWS}
function getWinVer: String;
var
  VerInfo: TOSVersioninfo;
begin
  VerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(VerInfo);
  Result := 'Windows NT '+IntToStr(VerInfo.dwMajorVersion) + '.' + IntToStr(VerInfo.dwMinorVersion)
end;
{$ENDIF}

function HexToTColor(sColor: string): TColor;
var
  r,g,b: Byte;
begin
  r := StrToInt( '$00'+Copy( sColor, 1, 2 ) );
  g := StrToInt( '$00'+Copy( sColor, 3, 2 ) );
  b := StrToInt( '$00'+Copy( sColor, 5, 2 ) );
  Result := RGBToColor(r,g,b);
end;

function ColourContrast(incol: TColor): TColor;
const
  gamma = 2.2;
var
  r,g,b,L: Double;
begin
  r := Red(incol) / 255;
  g := Green(incol) / 255;
  b := Blue(incol) / 255;
  L := 0.2126 * power(R, gamma) + 0.7152 * power(G, gamma) + 0.0722 * power(B, gamma);
  if L > power(0.5, gamma) then Result := clBlack
  else Result := clWhite;
end;

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
var
  c: Integer;
begin
  ZXColours[0] := HexToTColor('000000');
  ZXColours[1] := HexToTColor('0000C0');
  ZXColours[2] := HexToTColor('C00000');
  ZXColours[3] := HexToTColor('C000C0');
  ZXColours[4] := HexToTColor('00C000');
  ZXColours[5] := HexToTColor('00C0C0');
  ZXColours[6] := HexToTColor('C0C000');
  ZXColours[7] := HexToTColor('C0C0C0');
  ZXColours[8] := HexToTColor('0000FF');
  ZXColours[9] := HexToTColor('FF0000');
  ZXColours[10] := HexToTColor('FF00FF');
  ZXColours[11] := HexToTColor('00FF00');
  ZXColours[12] := HexToTColor('00FFFF');
  ZXColours[13] := HexToTColor('FFFF00');
  ZXColours[14] := HexToTColor('FFFFFF');

  PaperSelect := false;
  InkSelect := false;
  Paper := clWhite;
  Ink := clBlack;

  SetSpriteSize;

  for c := 0 to 14 do
  begin
    ColourButtons[c] := TShape.Create(Self);
    ColourButtons[c].Parent := colourPanel;
    ColourButtons[c].Brush.Color := ZXColours[c];
    ColourButtons[c].Width := colourPanel.ClientRect.Width-1;
    ColourButtons[c].Top := c * 27;
    ColourButtons[c].Left := 0;
    ColourButtons[c].Height := 25;
    ColourButtons[c].OnMouseDown := @ColourButtonMouseDown;
  end;
  IsSaved := true;
  CurrentFile := 'Untitled';
  OpenDialog1.Filter := 'UDG Plop Files (*.udgp)|*.udgp';
  SaveDialog1.Filter := 'UDG Plop Files (*.udgp)|*.udgp';
  UpdateWindowTitle;
end;

procedure TfrmMain.btnNewClick(Sender: TObject);
var
  i,j: Integer;
begin
  if not IsSaved then
  begin
     i := messagedlg('This file is unsaved,'+#13#10+'would you like to save it?', mtWarning, mbYesNo, 0);
     if i = mrYes then btnSaveClick(Sender);
  end;
  CurrentFile := 'Untitled';
  IsSaved := true;
  for i := 0 to SpriteWidth-1 do
  begin
    for j := 0 to SpriteHeight-1 do
    begin
      pixels[i,j] := 0;
      buttons[i,j].Brush.Color := Paper;
    end;
  end;
  UpdateViewArea;
  UpdateWindowTitle;
end;

procedure TfrmMain.btnAboutClick(Sender: TObject);
begin
  frmAbout.ShowModal;
end;

procedure TfrmMain.btnImportClick(Sender: TObject);
var
  s,b: String;
  sparts: TStringArray;
  i,j: Integer;
begin
  if not IsSaved then
  begin
    i := messagedlg('This file is unsaved,'+#13#10+'would you like to save it?', mtWarning, mbYesNo, 0);
    if i = mrYes then btnSaveClick(Sender);
  end;
  s := InputBox('Import a sprite','Enter the pixels as 8 integer values seperated by commas','0,0,0,0,0,0,0,0');
  sparts := s.Split(',');
  if High(sparts) = 7 then
  begin
    for i := 0 to 7 do
    begin
      b := IntToBin(StrToIntDef(sparts[i],0),8);
      for j := 1 to 8 do
      begin
        pixels[i,j-1] := StrToInt(b[j]);
      end;
    end;
    UpdateViewArea;
    SetButtons;
    CurrentFile := 'Untitled';
    IsSaved := false;
    UpdateWindowTitle;
  end
  else showmessage('Import failed! Is the data in correct format?');
end;

procedure TfrmMain.btnInkClick(Sender: TObject);
begin
  PaperSelect := false;
  InkSelect := true;
  colourPanel.Left := btnPaper.Left;
  colourPanel.Top := buttonPanel.Top;
  colourPanel.Visible := not colourPanel.Visible;
end;

procedure TfrmMain.btnOpenClick(Sender: TObject);
var
  i,j: Integer;
  fi: TStrings;
  parts: TStringArray;
  b: String;
begin
  if not IsSaved then
  begin
    i := messagedlg('This file is unsaved,'+#13#10+'would you like to save it?', mtWarning, mbYesNo, 0);
    if i = mrYes then btnSaveClick(Sender);
  end;
  if OpenDialog1.Execute then
  begin
    if not FileExists(OpenDialog1.Filename) then exit;
    fi := TStringList.Create;
    fi.LoadFromFile(OpenDialog1.Filename);
    // Support loading original format files
    if Pos('#APPVER',fi.Text) < 1 then
    begin
      if fi.Count > 7 then
      begin
        for i := 0 to 7 do
        begin
          parts := fi[i].Split([' ']);
          if High(parts) = 1 then
          begin
            b := IntToBin(StrToIntDef(parts[1],0),8);
            for j := 1 to 8 do
            begin
              pixels[i,j-1] := StrToInt(b[j]);
            end;
          end
          else
          begin
            for j := 1 to 8 do
            begin
              pixels[i,j-1] := 0;
            end;
          end;
        end;
      end;
    end
    else
    begin
      // New file format
    end;
    fi.Free;
    UpdateViewArea;
    SetButtons;
    CurrentFile := OpenDialog1.FileName;
    IsSaved := true;
    UpdateWindowTitle;
  end;
end;

procedure TfrmMain.btnPaperClick(Sender: TObject);
begin
  PaperSelect := true;
  InkSelect := false;
  colourPanel.Left := btnPaper.Left;
  colourPanel.Top := buttonPanel.Top;
  colourPanel.Visible := not colourPanel.Visible;
end;

procedure TfrmMain.btnSaveClick(Sender: TObject);
var
  outfile: String;
begin
  if (CurrentFile = '') or (CurrentFile = 'Untitled') then
  begin
    if SaveDialog1.Execute then
    begin
      outfile := SaveDialog1.Filename;
    end
    else exit;
  end
  else outfile := CurrentFile;
  if not outfile.EndsWith('.udgp') then outfile := outfile + '.udgp';
  textOutput.Lines.SaveToFile(outfile);
  IsSaved := true;
  if outfile <> CurrentFile then CurrentFile := outfile;
  UpdateWindowTitle;
end;

procedure TfrmMain.btnTransformClick(Sender: TObject);
var
  lowerLeft: TPoint;
begin
  lowerLeft := Classes.Point(btnTransform.Left,btnTransform.Top + btnTransform.Height);
  lowerLeft := ClientToScreen(lowerLeft);
  if SpriteWidth = SpriteHeight then
  begin
    menuRotateC.Enabled := true;
    menuRotateA.Enabled := true;
  end
  else
  begin
    menuRotateC.Enabled := false;
    menuRotateA.Enabled := false;
  end;
  menuTransform.Popup(lowerLeft.X, lowerLeft.Y);
end;

procedure TfrmMain.buttonPanelResize(Sender: TObject);
begin
  SetButtonSize;
end;

procedure TfrmMain.checkRowFirstChange(Sender: TObject);
begin
  UpdateViewArea;
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  i: Integer;
begin
  if not IsSaved then
  begin
    if not IsSaved then
    begin
      i := messagedlg('This file is unsaved,'+#13#10+'would you like to save it?', mtWarning, mbYesNo, 0);
      if i = mrYes then btnSaveClick(Sender);
    end;
  end;
  CanClose := true;
end;

procedure TfrmMain.ButtonMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  cc: TColor;
  i,j: Integer;
  p: TPoint;
begin
  if Button = mbLeft then
  begin
    cc := (Sender as TShape).Brush.Color;
    if cc = Paper then
    begin
      (Sender as TShape).Brush.Color := Ink;
      GetButtonPosition(Sender as TShape,i,j);
      if (i > -1) and (j > -1) then pixels[i,j] := 1;
    end
    else
    begin
      (Sender as TShape).Brush.Color := Paper;
      GetButtonPosition(Sender as TShape,i,j);
      if (i > -1) and (j > -1) then pixels[i,j] := 0;
    end;
    UpdateViewArea;
    IsSaved := false;
    UpdateWindowTitle;
  end;
  if Button = mbRight then
  begin
    if GetCursorPos(p) then
    begin
      GetButtonPosition(Sender as TShape,i,j);
      if i > 0 then menuMoveRowUp.Enabled := true
      else menuMoveRowUp.Enabled := false;
      if j > 0 then menuMoveColLeft.Enabled := true
      else menuMoveColLeft.Enabled := false;
      if i = 7 then menuMoveRowDown.Enabled := false
      else menuMoveRowDown.Enabled := true;
      if j = 7 then menuMoveColRight.Enabled := false
      else menuMoveColRight.Enabled := true;
      menuEdit.Popup(p.X, p.Y);
    end;
  end;
end;

procedure TfrmMain.listSpriteSizeChange(Sender: TObject);
var
  x,y: Integer;
begin
  for x := 0 to High(buttons) do
  begin
    for y := 0 to High(buttons[x]) do
    begin
      buttons[x,y].Free;
    end;
  end;
  SetSpriteSize;
end;

procedure TfrmMain.menuFlipClick(Sender: TObject);
var
  tmp: Array of Array of Byte;
  x,y: Integer;
  j: Integer;
begin
//  tmp := pixels;
  SetLength(tmp, SpriteWidth, SpriteHeight);
  j := SpriteWidth-1;
  for x := 0 to SpriteWidth-1 do
  begin
    for y := 0 to SpriteHeight-1 do
    begin
      tmp[j,y] := pixels[x,y];
    end;
    dec(j);
  end;
  pixels := tmp;
  UpdateViewArea;
  SetButtons;
  IsSaved := false;
end;

procedure TfrmMain.menuInvertClick(Sender: TObject);
var
  x, y: Integer;
begin
  for x := 0 to SpriteWidth-1 do
  begin
    for y := 0 to SpriteHeight-1 do
    begin
      if pixels[x,y] = 1 then pixels[x,y] := 0
      else pixels[x,y] := 1;
    end;
  end;
  UpdateViewArea;
  SetButtons;
  IsSaved := false;
end;

procedure TfrmMain.menuMirrorClick(Sender: TObject);
var
  tmp: Array of Array of Byte;
  x,y: Integer;
  j: Integer;
begin
//  tmp := pixels;
  SetLength(tmp, SpriteWidth, SpriteHeight);
  for x := 0 to SpriteWidth-1 do
  begin
    j := SpriteHeight-1;
    for y := 0 to SpriteHeight-1 do
    begin
      tmp[x,j] := pixels[x,y];
      dec(j);
    end;
  end;
  pixels := tmp;
  UpdateViewArea;
  SetButtons;
  IsSaved := false;
end;

procedure TfrmMain.menuMoveColLeftClick(Sender: TObject);
begin

end;

procedure TfrmMain.menuMoveColRightClick(Sender: TObject);
begin

end;

procedure TfrmMain.menuMoveRowDownClick(Sender: TObject);
begin

end;

procedure TfrmMain.menuMoveRowUpClick(Sender: TObject);
begin

end;

procedure TfrmMain.menuRotateAClick(Sender: TObject);
var
  tmp: Array of Array of Byte;
  x,y,wy: Integer;
begin
  SetLength(tmp, SpriteWidth, SpriteHeight);
  wy := SpriteHeight-1;
  for x := 0 to SpriteWidth-1 do
  begin
    for y := 0 to SpriteHeight-1 do
    begin
      tmp[x,y] := pixels[wy-y,x];
    end;
  end;
  pixels := tmp;
  UpdateViewArea;
  SetButtons;
  IsSaved := false;
end;

procedure TfrmMain.menuRotateCClick(Sender: TObject);
var
  tmp: Array of Array of Byte;
  x,y,t,wx,wy: Integer;
begin
  SetLength(tmp, SpriteWidth, SpriteHeight);
  wx := SpriteWidth-1;
  wy := SpriteHeight-1;
  for x := 0 to 3 do
  begin
    for y := 0 to wx-x do
    begin
      t := pixels[x,y];
      tmp[x,y] := pixels[y,wx-x];
      tmp[y,wx-x] := pixels[wx-x,wy-y];
      tmp[wx-x,wy-y] := pixels[wy-y,x];
      tmp[wy-y,x] := t;
    end;
  end;
  pixels := tmp;
  UpdateViewArea;
  SetButtons;
  IsSaved := false;
end;

procedure TfrmMain.menuShiftDownClick(Sender: TObject);
var
  tmp: Array of Array of Byte;
  x,y: Integer;
begin
  SetLength(tmp, SpriteWidth, SpriteHeight);
  for x := 0 to SpriteWidth-2 do
  begin
    for y := 0 to SpriteHeight-1 do
    begin
      tmp[x+1,y] := pixels[x,y];
    end;
  end;
  for x := 0 to SpriteWidth-1 do tmp[0,x] := 0;
  pixels := tmp;
  UpdateViewArea;
  SetButtons;
  IsSaved := false;
end;

procedure TfrmMain.menuShiftLeftClick(Sender: TObject);
var
  tmp: Array of Array of Byte;
  x,y: Integer;
begin
  SetLength(tmp, SpriteWidth, SpriteHeight);
  for x := 0 to SpriteWidth-1 do
  begin
    for y := 1 to SpriteHeight-1 do
    begin
      tmp[x,y-1] := pixels[x,y];
    end;
  end;
  for x := 0 to SpriteWidth-1 do tmp[x,7] := 0;
  pixels := tmp;
  UpdateViewArea;
  SetButtons;
  IsSaved := false;
end;

procedure TfrmMain.menuShiftRightClick(Sender: TObject);
var
  tmp: Array of Array of Byte;
  x,y: Integer;
begin
  SetLength(tmp, SpriteWidth, SpriteHeight);
  for x := 0 to 7 do
  begin
    for y := 0 to 6 do
    begin
      tmp[x,y+1] := pixels[x,y];
    end;
  end;
  for x := 0 to 7 do tmp[x,0] := 0;
  pixels := tmp;
  UpdateViewArea;
  SetButtons;
  IsSaved := false;
end;

procedure TfrmMain.menuShiftUpClick(Sender: TObject);
var
  tmp: Array of Array of Byte;
  x,y: Integer;
begin
  SetLength(tmp, SpriteWidth, SpriteHeight);
  for x := 1 to 7 do
  begin
    for y := 0 to 7 do
    begin
      tmp[x-1,y] := pixels[x,y];
    end;
  end;
  for x := 0 to 7 do tmp[7,x] := 0;
  pixels := tmp;
  UpdateViewArea;
  SetButtons;
  IsSaved := false;
end;

procedure TfrmMain.updateLabelClick(Sender: TObject);
begin
  OpenURL('https://www.matthewhipkin.co.uk');
  updatePanel.Visible := false;
end;

procedure TfrmMain.ColourButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    if PaperSelect then
    begin
      Paper := (Sender as TShape).Brush.Color;
      btnPaper.Color := (Sender as TShape).Brush.Color;
      btnPaper.Font.Color := ColourContrast((Sender as TShape).Brush.Color);
    end;
    if InkSelect then
    begin
      Ink := (Sender as TShape).Brush.Color;
      btnInk.Color := (Sender as TShape).Brush.Color;
      btnInk.Font.Color := ColourContrast((Sender as TShape).Brush.Color);
    end;
  end;
  colourPanel.Visible := false;
  SetButtons;
end;

procedure TfrmMain.updateTimerTimer(Sender: TObject);
var
  HTTP: TFPHttpClient;
  OS: String;
  response: String;
begin
  updateTimer.Enabled := false;
  {$ifdef Windows}
  OS := getWinVer;
  {$endif}
  {$ifdef Linux}
  OS := 'Linux';
  {$endif}
  {$ifdef FreeBSD}
  OS := 'FreeBSD';
  {$endif}
  {$ifdef Darwin}
  OS := 'OS X';
  {$endif}
  HTTP := TFPHttpClient.Create(nil);
  HTTP.RequestHeaders.Add('User-Agent: Mozilla/5.0 (compatible; '+OS+'; '+APPNAME+' '+APPVER+' ('+IntToStr(CURRVER)+'))');
  response := HTTP.Get('http://www.matthewhipkin.co.uk/udgplop.txt');
  if StrToIntDef(trim(response),-1) > CURRVER then
  begin
    updatePanel.Visible := true;
  end;
  HTTP.Free;
end;

procedure TfrmMain.GetButtonPosition(const button: TShape; var x: Integer; var y: Integer);
var
  i,j: Integer;
begin
  x := -1;
  y := -1;
  for i := 0 to SpriteWidth-1 do
  begin
    for j := 0 to SpriteHeight-1 do
    begin
      if button = buttons[i,j] then
      begin
        x := i;
        y := j;
      end;
    end;
  end;
end;

procedure TfrmMain.UpdateViewArea;
var
  i,j: Integer;
  s: String;
  plines: TStrings;
  bmp: graphics.TBitmap;
begin
  textOutput.Lines.Clear;
  plines := TStringList.Create;
{  for i := 0 to 7 do
  begin
    s := '';
    for j := 0 to 7 do
    begin
      s := s + IntToStr(pixels[i,j]);
    end;
    plines.Add('POKE USR "A"+'+IntToStr(i)+', BIN ' + s);
    s := s + ' ' + IntToStr(GetLineValue(j));
    textOutput.Lines.Add(s);
  end;
  textOutput.Lines.Add('');
  textOutput.Lines.AddStrings(plines);
  textOutput.Lines.Add('');}
  s := 'DATA ';
  if SpriteWidth = 8 then
  begin
    for i := 0 to SpriteHeight-1 do
    begin
      s := s + IntToStr(GetLineValue(i,0,7));
      if i < SpriteHeight-1 then s := s + ', ';
    end;
    textOutput.Lines.Add(s);
  end
  else
  begin
    if (checkRowFirst.Checked) and (SpriteHeight = 16) then
    begin
      for j := 0 to 1 do
      begin
        for i := 0 to SpriteHeight-1 do
        begin
          s := s + IntToStr(GetLineValue(i,j*8,(j*8)+7));
          if i < SpriteHeight-j then s := s + ', ';
        end;
      end;
    end
    else
    begin
      for i := 0 to SpriteHeight-1 do
      begin
        for j := 0 to 1 do
        begin
          s := s + IntToStr(GetLineValue(i,j*8,(j*8)+7));
          if i < SpriteHeight-j then s := s + ', ';
        end;
      end;
    end;
    textOutput.Lines.Add(s);
  end;

  plines.Free;
  bmp := graphics.TBitMap.Create;
  bmp.Width := SpriteWidth;
  bmp.Height := SpriteHeight;
  for i := 0 to SpriteWidth-1 do
  begin
    for j := 0 to SpriteHeight-1 do
    begin
      if pixels[i,j] = 1 then bmp.Canvas.pixels[i,j] := Ink
      else bmp.Canvas.Pixels[i,j] := Paper;
    end;
  end;
  imgPreview.Picture.Assign(bmp);
  bmp.Free;
end;

procedure TfrmMain.SetButtons;
var
  i,j: Integer;
begin
  for i := 0 to SpriteWidth-1 do
  begin
    for j := 0 to SpriteHeight-1 do
    begin
      if pixels[i,j] = 1 then buttons[i,j].Brush.Color := Ink
      else buttons[i,j].Brush.Color := Paper;
    end;
  end;
end;

procedure TfrmMain.UpdateWindowTitle;
begin
  frmMain.Caption := APPNAME + ' [' + CurrentFile;
  if not IsSaved then frmMain.Caption := frmMain.Caption + ' *';
  frmMain.Caption := frmMain.Caption + ']';
  Application.Title := frmMain.Caption;
end;

function TfrmMain.GetLineValue(l: Integer; p: Integer; c: Integer): Byte;
var
  i: Integer;
  s: String;
begin
  Result := 0;
  s := '';
  for i := p to c do
  begin
    s := s + IntToStr(pixels[i,l]);
  end;
  Result := BinToDec(s);
end;

procedure TfrmMain.SetSpriteSize;
var
  x,y: Integer;
begin
  SpriteWidth := 8;
  SpriteHeight := 8;
  if listSpriteSize.ItemIndex = 1 then
  begin
    SpriteWidth := 8;
    SpriteHeight := 16;
  end;
  if listSpriteSize.ItemIndex = 2 then
  begin
    SpriteWidth := 16;
    SpriteHeight := 8;
  end;
  if listSpriteSize.ItemIndex = 3 then
  begin
    SpriteWidth := 16;
    SpriteHeight := 16;
  end;
  SetLength(pixels, SpriteWidth, SpriteHeight);
  SetLength(buttons, SpriteWidth, SpriteHeight);
  for x := 0 to SpriteWidth-1 do
  begin
    for y := 0 to SpriteHeight-1 do
    begin
      buttons[x,y] := TShape.Create(Self);
      buttons[x,y].Parent := buttonPanel;
      {buttons[x,y].Width := bsize;
      buttons[x,y].Height := bsize;
      buttons[x,y].Left := x * (bsize + 2);
      buttons[x,y].Top := y * (bsize + 2);}
      buttons[x,y].OnMouseDown := @ButtonMouseDown;
      buttons[x,y].Brush.Color := Paper;
      buttons[x,y].Pen.Color := Paper;
    end;
  end;
  SetButtonSize;
  for x := 0 to SpriteWidth-1 do
  begin
    for y := 0 to SpriteHeight-1 do
    begin
      pixels[x,y] := 0;
    end;
  end;
  imgPreview.Width := SpriteWidth * 2;
  imgPreview.Height := SpriteHeight * 2;
  if listSpriteSize.ItemIndex = 3 then checkRowFirst.Visible := true
  else checkRowFirst.Visible := false;
end;

procedure TfrmMain.SetButtonSize;
var
  x,y,n,h,c: Integer;
begin
  h := min(buttonPanel.Width,buttonPanel.Height);
  c := max(SpriteWidth,SpriteHeight);
  n := (h div c)-2;
  for x := 0 to SpriteWidth-1 do
  begin
    for y := 0 to SpriteHeight-1 do
    begin
      buttons[x,y].Width := n;
      buttons[x,y].Height := n;
      buttons[x,y].Left := x * (n + 2);
      buttons[x,y].Top := y * (n + 2);
    end;
  end;
end;

end.

