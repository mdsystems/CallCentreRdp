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
unit TestDLL;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.AppEvnts, JvExStdCtrls, JvEdit, Vcl.ExtCtrls;

type
  TfTestDLL = class(TForm)
    bTestHandle: TButton;
    lFormHandle: TLabel;
    bSimCM: TButton;
    eDDI: TJvEdit;
    eCLI: TJvEdit;
    eOperator: TJvEdit;
    rgMessageType: TRadioGroup;
    bSetRoute: TButton;
    eRouteName: TJvEdit;
    lAppHandle: TLabel;
    bTestReceive: TButton;
    eCallId: TJvEdit;
    eChannelName: TJvEdit;
    procedure bTestHandleClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure bSimCMClick(Sender: TObject);
    procedure rgMessageTypeClick(Sender: TObject);
    procedure eDDIChange(Sender: TObject);
    procedure bSetRouteClick(Sender: TObject);
    procedure bTestReceiveClick(Sender: TObject);
  private
    procedure SetControls;
  end;

var
  fTestDLL: TfTestDLL;

implementation

{$R *.dfm}

uses
  CallMessage;

const
  cClpRouteName = 'routeName';
  cClpChannelName = 'channelName';

var
  dllHandle: NativeInt;
  TestFetchHandle: function: HWND = nil;
  OpenStatus: procedure = nil;
  CloseStatus: procedure = nil;
  TestDataReceive: procedure = nil;
  SetupRoute: procedure (const rn, cn: String) = nil;
  SimulateRdpAppMessage: procedure (const msg: TCallMessage) = nil;

procedure TfTestDLL.bTestHandleClick(Sender: TObject);
var
  h: HWND;
begin
  if Assigned(TestFetchHandle) then begin
    h := TestFetchHandle;
    if h = Handle then
      ShowMessage('Woop!')
    else
      BringWindowToTop(h);
  end;
end;

procedure TfTestDLL.bTestReceiveClick(Sender: TObject);
begin
  if Assigned(TestDataReceive) then
    TestDataReceive;
end;

procedure TfTestDLL.bSetRouteClick(Sender: TObject);
begin
  if Assigned(SetupRoute) then
    SetupRoute(eRouteName.Text, eChannelName.Text);
end;

procedure TfTestDLL.bSimCMClick(Sender: TObject);
var
  msg: TCallMessage;
begin
  PopulateMessage(msg, TCallMessageType(rgMessageType.ItemIndex), Handle, eDDI.Text, eCLI.Text, eRouteName.Text, '', eOperator.Text, eCallId.Text, '');
  SimulateRdpAppMessage(msg);
end;

procedure TfTestDLL.eDDIChange(Sender: TObject);
begin
  SetControls;
end;

procedure TfTestDLL.FormCreate(Sender: TObject);
begin
  lFormHandle.Caption := IntToStr(Handle);
  lAppHandle.Caption := IntToStr(Application.Handle);
end;

procedure TfTestDLL.FormShow(Sender: TObject);
var
  cmdLine: TStringList;
  i: Integer;
begin
  cmdLine := TStringList.Create;
  try
    for i := 1 to ParamCount do
      cmdLine.Add(ParamStr(i));
    eRouteName.Text := cmdLine.Values[cClpRouteName];
    eChannelName.Text := cmdLine.Values[cClpChannelName];
    SetControls;
  finally
    cmdLine.Free;
  end;
end;

procedure TfTestDLL.rgMessageTypeClick(Sender: TObject);
begin
  SetControls;
end;

procedure TfTestDLL.SetControls;
begin
  bTestHandle.Enabled := True;
  bSetRoute.Enabled := (eRouteName.Text <> '') and (eChannelName.Text <> '') and Assigned(SimulateRdpAppMessage);
  bSimCM.Enabled := (rgMessageType.ItemIndex >= 0) and Assigned(SimulateRdpAppMessage);
  bTestReceive.Enabled := Assigned(TestDataReceive);
end;

initialization
  dllHandle := LoadLibrary('CallManager.dll');
  if dllHandle <> 0 then begin
    TestFetchHandle := GetProcAddress(dllHandle, 'TestFetchHandle');
    OpenStatus := GetProcAddress(dllHandle, 'OpenStatus');
    CloseStatus := GetProcAddress(dllHandle, 'CloseStatus');
    SetupRoute := GetProcAddress(dllHandle, 'SetupRoute');
    SimulateRdpAppMessage := GetProcAddress(dllHandle, 'SimulateRdpAppMessage');
    TestDataReceive := GetProcAddress(dllHandle, 'TestDataReceive');
    if Assigned(OpenStatus) then
      OpenStatus;
  end;

finalization
  if Assigned(CloseStatus) then
    CloseStatus;
  FreeLibrary(dllHandle);

end.
