unit import;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { TfrmImport }

  TfrmImport = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    checkRowsFirst: TCheckBox;
    Label1: TLabel;
    textBytes: TLabeledEdit;
    listSize: TComboBox;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private

  public

  end;

var
  frmImport: TfrmImport;

implementation

{$R *.lfm}

{ TfrmImport }

procedure TfrmImport.btnOKClick(Sender: TObject);
var
  s: String;
  sa: TStringArray;
  cont: Boolean;
  i: Integer;
begin
  cont := true;
  s := textBytes.Text;
  sa := s.Split(',');
  if (listSize.ItemIndex = 0) and (High(sa) <> 7) then cont := false;
  if (listSize.ItemIndex = 1) and (High(sa) <> 15) then cont := false;
  if (listSize.ItemIndex = 2) and (High(sa) <> 15) then cont := false;
  if (listSize.ItemIndex = 3) and (High(sa) <> 31) then cont := false;
  for i := 0 to High(sa) do
  begin
    if (sa[i].ToInteger < 0) and (sa[i].ToInteger > 255) then cont := false;
  end;
  if not cont then
  begin
    messagedlg('The bytes entered do not appear to be a valid sprite', mtError, [mbOK], 0);
    exit;
  end;
  ModalResult := mrOK;
end;

procedure TfrmImport.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

end.

