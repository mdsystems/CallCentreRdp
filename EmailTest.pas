unit EmailTest;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, SendEmail, IdBaseComponent, IdComponent, IdIOHandler, IdIOHandlerStream;

type
  TForm1 = class(TForm)
    mHTML: TMemo;
    eHost: TLabeledEdit;
    ePort: TLabeledEdit;
    eUserName: TLabeledEdit;
    ePassword: TLabeledEdit;
    eFromName: TLabeledEdit;
    eFromAddress: TLabeledEdit;
    lbContent: TListBox;
    lContent: TLabel;
    cbAuthType: TComboBox;
    cbTlsMode: TComboBox;
    lAuthType: TLabel;
    lTlsMode: TLabel;
    bLoadIni: TButton;
    odIni: TFileOpenDialog;
    bSend: TButton;
    eEmailTo: TLabeledEdit;
    eVoucher: TLabeledEdit;
    eEmailSubject: TLabeledEdit;
    IdIOHandlerStream1: TIdIOHandlerStream;
    lbSubstitutions: TListBox;
    Label1: TLabel;
    procedure bLoadIniClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure bSendClick(Sender: TObject);
  private
    FMessage: TMailMessage;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.bLoadIniClick(Sender: TObject);
begin
  if Assigned(FMessage) and odIni.Execute then begin
    FMessage.LoadIni(odIni.FileName);
    mHTML.Lines.Assign(FMessage.Body);
    eHost.Text := FMessage.MailServerSettings.Host;
    ePort.Text := IntToStr(FMessage.MailServerSettings.Port);
    cbAuthType.ItemIndex := Integer(FMessage.MailServerSettings.AuthType);
    cbTlsMode.ItemIndex := Integer(FMessage.MailServerSettings.TlsMode);
    eUserName.Text := FMessage.MailServerSettings.UserName;
    ePassword.Text := FMessage.MailServerSettings.Password;
    eFromName.Text := FMessage.MailServerSettings.FromName;
    eFromAddress.Text := FMessage.MailServerSettings.FromAddress;
    lbContent.Items.Assign(FMessage.ImageContent);
    lbSubstitutions.Items.Assign(FMessage.Variables);
  end;
end;

procedure TForm1.bSendClick(Sender: TObject);
begin
  FMessage.Subject := eEmailSubject.Text;
  FMessage.ToAddresses := eEmailTo.Text;
  FMessage.SetSubstitution('VOUCHERCODE', eVoucher.Text);
  FMessage.Send;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  s: String;
begin
  FMessage := TMailMessage.Create;
  for s in cAuthTypes do
    cbAuthType.Items.Add(s);
  for s in cTlsTypes do
    cbTlsMode.Items.Add(s);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FMessage);
end;

end.
