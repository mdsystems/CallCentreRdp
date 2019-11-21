{
  Copyright 2015-2019 Micro Data Systems Ltd

  Contact: support@microdata.systems

  Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
  associated documentation files (the "Software"), to deal in the Software without restriction, including
  without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject
  to the following conditions:

  The above copyright notice and this permission notice shall be included in all copies or substantial
  portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT
  LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
  WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
  SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
}
unit TestServer;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, JvExStdCtrls, JvEdit,
  CallServer;

type
  TfTestServer = class(TForm)
    bCreate: TButton;
    bSimCM: TButton;
    eOperator: TJvEdit;
    rgMessageType: TRadioGroup;
    bSetRoute: TButton;
    eRouteName: TJvEdit;
    bFree: TButton;
    mCallLog: TMemo;
    eCallId: TJvEdit;
    procedure bCreateClick(Sender: TObject);
    procedure bFreeClick(Sender: TObject);
    procedure bSetRouteClick(Sender: TObject);
    procedure bSimCMClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure eOperatorChange(Sender: TObject);
  private
    FCallServer: TfCallServer;
    procedure SetControls;
    procedure OnRoutedCall(const ddi, cli, systemName, operatorName, callId, messageString, otherParams: String);
  end;

var
  fTestServer: TfTestServer;

implementation

uses
  CallMessage;

{$R *.dfm}

procedure TfTestServer.bCreateClick(Sender: TObject);
begin
  if not Assigned(FCallServer) then begin
    FCallServer := TfCallServer.Create(Self);
    FCallServer.OnRoutedCall := OnRoutedCall;
    FCallServer.Show;
  end;
  SetControls;
end;

procedure TfTestServer.bFreeClick(Sender: TObject);
begin
  if Assigned(FCallServer) then
    FreeAndNil(FCallServer);
  SetControls;
end;

procedure TfTestServer.bSetRouteClick(Sender: TObject);
begin
  if Assigned(FCallServer) then begin
    FCallServer.RouteName := eRouteName.Text;
    FCallServer.Connect;
  end;
  SetControls;
end;

procedure TfTestServer.bSimCMClick(Sender: TObject);
var
  msg: TCallMessage;
begin
  if Assigned(FCallServer) then begin
    PopulateMessage(msg, TCallMessageType(rgMessageType.ItemIndex), Handle, '', '', eRouteName.Text, '', eOperator.Text, eCallId.Text, '');
    FCallServer.SendMessage(msg);
  end;
  SetControls;
end;

procedure TfTestServer.eOperatorChange(Sender: TObject);
begin
  SetControls;
end;

procedure TfTestServer.FormCreate(Sender: TObject);
begin
  SetControls;
end;

procedure TfTestServer.OnRoutedCall(const ddi, cli, systemName, operatorName, callId, messageString, otherParams: String);
begin
  mCallLog.Lines.Add(Format('Call on ddi %s received from cli %s and other parameters %s', [ddi, cli, otherParams]));
end;

procedure TfTestServer.SetControls;
begin
  bCreate.Enabled := not Assigned(FCallServer);
  bFree.Enabled := Assigned(FCallServer);
  bSetRoute.Enabled := (eRouteName.Text <> '') and Assigned(FCallServer);
  bSimCM.Enabled := (rgMessageType.ItemIndex >= 0) and Assigned(FCallServer);
end;

end.
