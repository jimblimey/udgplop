unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  StdCtrls;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnNew: TButton;
    btnOpen: TButton;
    btnSave: TButton;
    btnAbout: TButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    textOutput: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    ToolBar1: TToolBar;
    procedure btnAboutClick(Sender: TObject);
    procedure btnNewClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    buttons: Array[0..7,0..7] of TShape;
    pixels: Array[0..7] of Byte;
    IsSaved: Boolean;
    CurrentFile: String;
    procedure GetButtonPosition(const button: TShape; var x: Integer; var y: Integer);
    procedure UpdateViewArea;
    procedure SetButtons;
  public

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

uses about;

function GetBit(const aValue: Byte; const Bit: Byte): Byte;
begin
  Result := (aValue shr Bit) and 1;
end;

procedure SetBit(var aValue: Byte; const Bit: Byte; const Flag: Boolean);
begin
  aValue := (aValue or (1 shl Bit)) xor (Integer(not Flag) shl Bit);
end;

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
var
  x,y,c: Integer;
const
  bsize = 50;
begin
  c := 0;
  for x := 0 to 7 do
  begin
    for y := 0 to 7 do
    begin
      buttons[x,y] := TShape.Create(Self);
      buttons[x,y].Parent := Panel1;
      buttons[x,y].Width := bsize;
      buttons[x,y].Height := bsize;
      buttons[x,y].Left := x * (bsize + 2);
      buttons[x,y].Top := y * (bsize + 2);
      buttons[x,y].OnMouseDown := @ButtonMouseDown;
      buttons[x,y].Brush.Color := clWhite;
      buttons[x,y].Pen.Color := clWhite;
      inc(c);
    end;
  end;
  for c := 0 to 7 do
  begin
    pixels[c] := 0;
  end;
  IsSaved := true;
  CurrentFile := 'Untitled';
  OpenDialog1.Filter := 'UDG Plop Files (*.udgp)|*.udgp';
  SaveDialog1.Filter := 'UDG Plop Files (*.udgp)|*.udgp';
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
  for i := 0 to 7 do
  begin
    pixels[i] := 0;
    for j := 0 to 7 do
    begin
      buttons[i,j].Brush.Color := clWhite;
    end;
  end;
  UpdateViewArea;
end;

procedure TfrmMain.btnAboutClick(Sender: TObject);
begin
  frmAbout.ShowModal;
end;

procedure TfrmMain.btnOpenClick(Sender: TObject);
var
  i: Integer;
  fi: TStrings;
  parts: TStringArray;
begin
  if not IsSaved then
  begin
    i := messagedlg('This file is unsaved,'+#13#10+'would you like to save it?', mtWarning, mbYesNo, 0);
    if i = mrYes then btnSaveClick(Sender)
    else exit;
  end;
  if OpenDialog1.Execute then
  begin
    if not FileExists(OpenDialog1.Filename) then exit;
    fi := TStringList.Create;
    fi.LoadFromFile(OpenDialog1.Filename);
    if fi.Count > 7 then
    begin
      for i := 0 to 7 do
      begin
        parts := fi[i].Split([' ']);
        if High(parts) = 1 then
        begin
          if StrToIntDef(parts[1],-1) > -1 then pixels[i] := StrToInt(parts[1]);
        end
        else pixels[i] := 0;
      end;
    end;
    fi.Free;
    UpdateViewArea;
    SetButtons;
  end;
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
end;

procedure TfrmMain.ButtonMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  cc: TColor;
  i,j: Integer;
begin
  if Button = mbLeft then
  begin
    cc := (Sender as TShape).Brush.Color;
    if cc = clWhite then
    begin
      (Sender as TShape).Brush.Color := clBlack;
      GetButtonPosition(Sender as TShape,i,j);
      textOutput.Lines.Add(IntToStr(i)+','+IntToStr(j));
      if (i > -1) and (j > -1) then SetBit(pixels[i],j,true);
    end
    else
    begin
      (Sender as TShape).Brush.Color := clWhite;
      GetButtonPosition(Sender as TShape,i,j);
      if (i > -1) and (j > -1) then SetBit(pixels[i],j,false);
    end;
    UpdateViewArea;
    IsSaved := false;
  end;
end;

procedure TfrmMain.GetButtonPosition(const button: TShape; var x: Integer; var y: Integer);
var
  i,j: Integer;
begin
  x := -1;
  y := -1;
  for i := 0 to 7 do
  begin
    for j := 0 to 7 do
    begin
      if button = buttons[i,j] then
      begin
        x := j;
        y := i;
      end;
    end;
  end;
end;

procedure TfrmMain.UpdateViewArea;
var
  i,j: Integer;
  s: String;
  plines: TStrings;
begin
  textOutput.Lines.Clear;
  plines := TStringList.Create;
  for i := 0 to 7 do
  begin
    s := '';
    for j := 0 to 7 do
    begin
      s := s + IntToStr(GetBit(pixels[i],j));
    end;
    plines.Add('POKE USR "A"+'+IntToStr(i)+', BIN ' + s);
    s := s + ' ' + IntToStr(pixels[i]);
    textOutput.Lines.Add(s);
  end;
  textOutput.Lines.Add('');
  textOutput.Lines.AddStrings(plines);
  plines.Free;
end;

procedure TfrmMain.SetButtons;
var
  i,j: Integer;
begin
  for i := 0 to 7 do
  begin
    for j := 0 to 7 do
    begin
      if GetBit(pixels[j],i) = 1 then buttons[i,j].Brush.Color := clBlack
      else buttons[i,j].Brush.Color := clWhite;
    end;
  end;
end;

end.

