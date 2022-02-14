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
    ImportItems: TStringArray;
  end;

var
  frmImport: TfrmImport;

implementation

{$R *.lfm}

{ TfrmImport }

procedure TfrmImport.btnOKClick(Sender: TObject);
var
  s: String;
  cont: Boolean;
  i: Integer;
begin
  cont := true;
  s := textBytes.Text;
  ImportItems := s.Split(',');
  if (listSize.ItemIndex = 0) and (High(ImportItems) <> 7) then cont := false;
  if (listSize.ItemIndex = 1) and (High(ImportItems) <> 15) then cont := false;
  if (listSize.ItemIndex = 2) and (High(ImportItems) <> 15) then cont := false;
  if (listSize.ItemIndex = 3) and (High(ImportItems) <> 31) then cont := false;
  for i := 0 to High(ImportItems) do
  begin
    if (StrToIntDef(ImportItems[i],-1) < 0) and (StrToIntDef(ImportItems[i],256) > 255) then cont := false;
  end;
  if not cont then
  begin
    messagedlg('The bytes entered do not appear to be a valid sprite', mtError, [mbOK], 0);
    ModalResult := mrAbort;
  end;
  ModalResult := mrOK;
end;

procedure TfrmImport.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

end.

