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
unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdHTTP;

type
  TfMain = class(TForm)
    idHTTP: TIdHTTP;
    eHost: TEdit;
    ePort: TEdit;
    eCLI: TEdit;
    eDDI: TEdit;
    eOperName: TEdit;
    eOperId: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    bClose: TButton;
    bTest: TButton;
    mResponse: TMemo;
    eURL: TEdit;
    eError: TEdit;
    procedure bCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure bTestClick(Sender: TObject);
  private
    function NextCallId():String;
  public
    { Public declarations }
  end;

var
  fMain: TfMain;

implementation

{$R *.dfm}

procedure TfMain.bCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfMain.bTestClick(Sender: TObject);
var
  request: String;
  response: TStringStream;
begin
  mResponse.Clear;
  response := TStringStream.Create('');
  try
    request :=
      Format('http://%s:%s/call.html?callerid=%s&calltracker=%s&agentid=%s&diallednumber=%s&agentnumber=%s',
        [eHost.text, ePort.text, eCLI.Text, NextCallId(), eOperName.Text, eDDI.Text, eOperId.Text]);
    eURL.Text := request;
    try
      idHTTP.Get(request, response);
      mResponse.Lines.Text := response.DataString;
      eError.Text := 'OK';
      eError.Color := clWindow;
    except on e: Exception do
      begin
        eError.Text := e.Message;
        eError.Color := $00C1C1FF;
      end;
    end;
  finally
    response.Free;
  end;
end;

procedure TfMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

function TfMain.NextCallId: String;
begin
  result := IntToStr(Random(High(Integer))) + '.' + Copy(IntToStr(Random(High(Integer))), 1, 4);
end;

end.
