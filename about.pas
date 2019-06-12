unit about;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, LCLIntF, ExtCtrls, Buttons;

type

  { TfrmAbout }

  TfrmAbout = class(TForm)
    btnOK: TButton;
    Image1: TImage;
    Image2: TImage;
    imgSF: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure Image2Click(Sender: TObject);
    procedure imgSFClick(Sender: TObject);
    procedure Label5Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmAbout: TfrmAbout;

implementation

{$R *.lfm}

{ TfrmAbout }

procedure TfrmAbout.Image1Click(Sender: TObject);
begin
  OpenURL('http://www.lazarus.freepascal.org');
end;

procedure TfrmAbout.Image2Click(Sender: TObject);
begin
  OpenURL('https://twitter.com/hippy2094');
end;

procedure TfrmAbout.imgSFClick(Sender: TObject);
begin
  OpenURL('https://sourceforge.net/p/udgplop/');
end;

procedure TfrmAbout.Label5Click(Sender: TObject);
begin
  OpenURL('https://www.matthewhipkin.co.uk');
end;

procedure TfrmAbout.btnOKClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TfrmAbout.FormCreate(Sender: TObject);
begin
  Label5.Left := (Label2.Left + Label2.Width) + Label2.Canvas.GetTextWidth(' ');
end;

end.

