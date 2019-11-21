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
unit ActiveCallPopup;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, JvExStdCtrls, JvListComb, JvExForms, JvScrollBox,
  Vcl.ImgList, CallMessage, Contnrs, Vcl.ExtCtrls, SharedDefinitions, System.ImageList;

type
  TOnSelectCall = procedure (const msg: TCallMessage; const params: TStrings) of object;
  TApprovalMessage = class;
  TApprovalMessageList = class
    FApprovalMessages: TObjectList;
  private
    FSetStatus: TOnStatusEvent;
    function GetCount: Integer;
    function GetNext: TCallMessage;
    procedure DoSetStatus(const level: Integer; const status: String);
    procedure SetSetStatus(const Value: TOnStatusEvent);
  public
    constructor Create;
    destructor Destroy; override;
    procedure ClearAll;
    procedure ClearOldMessages;
    procedure Add(const msg: TCallMessage; const timeout: Integer; const params: TStrings);
    procedure Removed(const msg: TApprovalMessage);
    function CallMessage(idx: Integer): TCallMessage;
    function CallObject(idx: Integer): TApprovalMessage;
    property Count: Integer read GetCount;
    property Next: TCallMessage read GetNext;
    property SetStatus: TOnStatusEvent read FSetStatus write SetSetStatus;
  end;
  TApprovalMessage = class
    FArrivedAt: TDateTime;
  private
    FCallMessage: TCallMessage;
    FApprovalList: TApprovalMessageList;
    FListBox: TJvImageListBox;
    FTimeoutSeconds: Integer;
    FParams: TStrings;
    FSetStatus: TOnStatusEvent;
    function GetTimedOut: Boolean;
    procedure SetParams(const Value: TStrings);
    procedure SetSetStatus(const Value: TOnStatusEvent);
  public
    constructor Create(const list: TApprovalMessageList; const msg: TCallMessage; const timeout: Integer; const params: TStrings);
    destructor Destroy; override;
    procedure ListDestroyed(const list: TApprovalMessageList);
    property ArrivedAt: TDateTime read FArrivedAt;
    property TimeoutSeconds: Integer read FTimeoutSeconds write FTimeoutSeconds;
    property Params: TStrings read FParams write SetParams;
    property CallMessage: TCallMessage read FCallMessage write FCallMessage;
    property ApprovalList: TApprovalMessageList read FApprovalList write FApprovalList;
    property ListBox: TJvImageListBox read FListBox write FListBox;
    property TimedOut: Boolean read GetTimedOut;
    property SetStatus: TOnStatusEvent read FSetStatus write SetSetStatus;
  end;
  TfActiveCallPopup = class(TForm)
    ilMain: TImageList;
    JvScrollBox1: TJvScrollBox;
    lbMain: TJvImageListBox;
    tUpdate: TTimer;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure lbMainDblClick(Sender: TObject);
    procedure lbMainKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tUpdateTimer(Sender: TObject);
  private
    FUpdating: Boolean;
    FOnSelectCall: TOnSelectCall;
    FApprovalMessages: TApprovalMessageList;
    FSetStatus: TOnStatusEvent;
    procedure SelectCall(const callId: Integer);
    function GetCount: Integer;
    procedure UpdateDisplay;
    procedure SetSetStatus(const Value: TOnStatusEvent);
  public
    procedure Add(const msg: TCallMessage; const timeout: Integer; const params: TStrings);
    procedure FetchLastCallInfo(var msg: TCallMessage; params: TStrings);
    property OnSelectCall: TOnSelectCall read FOnSelectCall write FOnSelectCall;
    property Count: Integer read GetCount;
    property SetStatus: TOnStatusEvent read FSetStatus write SetSetStatus;
  end;

const
  cDefaultCallTimeoutSeconds = 10;

implementation

uses
  DateUtils, System.Types;

{$R *.dfm}

procedure TfActiveCallPopup.Add(const msg: TCallMessage; const timeout: Integer; const params: TStrings);
begin
  if Assigned(FApprovalMessages) then begin
    FApprovalMessages.Add(msg, timeout, params);
    tUpdate.Enabled := True;
  end;
end;

procedure TfActiveCallPopup.FetchLastCallInfo(var msg: TCallMessage; params: TStrings);
var
  i: Integer;
  amsg: TApprovalMessage;
begin
  if Assigned(FApprovalMessages) then begin
    i := FApprovalMessages.Count;
    if i > 0 then begin
      amsg := FApprovalMessages.CallObject(i-1);
      if Assigned(amsg) then begin
        msg := amsg.CallMessage;
        params.Assign(amsg.Params);
        FApprovalMessages.ClearAll;
        UpdateDisplay;
      end;
    end;
  end else begin
    msg.identifier := 0;
    params.Clear;
  end;
end;

procedure TfActiveCallPopup.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caHide;
end;

procedure TfActiveCallPopup.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := (not Assigned(FApprovalMessages)) or (FApprovalMessages.Count = 0);
end;

procedure TfActiveCallPopup.FormCreate(Sender: TObject);
begin
  lbMain.DoubleBuffered := True;
  FUpdating := false;
  FApprovalMessages := TApprovalMessageList.Create;
  UpdateDisplay;
end;

procedure TfActiveCallPopup.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FApprovalMessages);
end;

function TfActiveCallPopup.GetCount: Integer;
begin
  if Assigned(FApprovalMessages) then begin
    FApprovalMessages.ClearOldMessages;
    result := FApprovalMessages.Count
  end else
    result := 0;
end;

procedure TfActiveCallPopup.lbMainDblClick(Sender: TObject);
begin
  if lbMain.ItemIndex >= 0 then
    SelectCall(lbMain.ItemIndex);
end;

procedure TfActiveCallPopup.lbMainKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_RETURN) and (Shift = []) and Assigned(FOnSelectCall) and (lbMain.ItemIndex >= 0) then
    SelectCall(lbMain.ItemIndex);
end;

procedure TfActiveCallPopup.SelectCall(const callId: Integer);
begin
  if Assigned(FOnSelectCall) and Assigned(lbMain.Items.Objects[lbMain.ItemIndex]) then begin
    FOnSelectCall(TApprovalMessage(lbMain.Items.Objects[lbMain.ItemIndex]).CallMessage, TApprovalMessage(lbMain.Items.Objects[lbMain.ItemIndex]).Params);
    FApprovalMessages.ClearAll;
    UpdateDisplay;
  end;
end;

procedure TfActiveCallPopup.SetSetStatus(const Value: TOnStatusEvent);
begin
  FSetStatus := Value;
  FApprovalMessages.SetStatus := Value;
end;

procedure TfActiveCallPopup.tUpdateTimer(Sender: TObject);
var
  haveMessages: Boolean;
begin
  haveMessages := false;
  tUpdate.Enabled := False;
  try
    if Assigned(FApprovalMessages) then
      haveMessages := FApprovalMessages.Count > 0;
    UpdateDisplay;
  finally
    tUpdate.Enabled := haveMessages;
  end;
end;

procedure TfActiveCallPopup.UpdateDisplay;
var
  i: Integer;
  itm: TJvImageItem;
  msg: TCallMessage;
  ddi: String;
  cli: String;
  routeName: String;
  systemName: String;
  operatorName: String;
  callId: String;
  messageString: String;
  indx: Integer;
begin
  if not FUpdating then begin
    FUpdating := true;
    try
      indx := lbMain.ItemIndex;
      lbMain.Items.BeginUpdate;
      try
        lbMain.Items.Clear;
        for i := 0 to FApprovalMessages.Count - 1 do begin
          msg := FApprovalMessages.CallMessage(i);
          DecomposeMessage(msg, ddi, cli, routeName, systemName, operatorName, callId, messageString);
          itm := lbMain.Items.Add;
          if cli <> '' then
            cli := ' ' + cli;
          if routeName <> '' then
            routeName := ' ' + routeName;
          if ddi <> '' then
            ddi := ' ' + ddi;
          itm.Text := Format('Call%s for%s via%s', [cli, routeName, ddi]);
          itm.ImageIndex := 0;
          lbMain.Items.Objects[i] := FApprovalMessages.CallObject(i);
          FApprovalMessages.CallObject(i).ListBox := lbMain;
        end;
      finally
        lbMain.Items.EndUpdate;
        if lbMain.Items.Count > 0 then begin
          if indx < lbMain.Items.Count then
            lbMain.ItemIndex := indx;
          Show
        end else
          Hide;
      end;
    finally
      FUpdating := False;
    end;
  end;
end;

{ TApprovalMessageList }

procedure TApprovalMessageList.Add(const msg: TCallMessage; const timeout: Integer; const params: TStrings);
var
  i: Integer;
begin
  if Assigned(FApprovalMessages) then begin
    ClearOldMessages;
    i := FApprovalMessages.Add(TApprovalMessage.Create(Self, msg, timeout, params));
    TApprovalMessage(FApprovalMessages[i]).SetStatus := FSetStatus;
  end;
end;

procedure TApprovalMessageList.ClearAll;
begin
  DoSetStatus(5, 'Removing remaining messages.');
  FApprovalMessages.Clear;
end;

procedure TApprovalMessageList.ClearOldMessages;
var
  i: Integer;
  ddi: String;
  cli: String;
  routeName: String;
  systemName: String;
  operatorName: String;
  callId: String;
  messageString: String;
begin
  i := 0;
  while Assigned(FApprovalMessages) and (i < FApprovalMessages.Count) do begin
    if TApprovalMessage(FApprovalMessages[i]).TimedOut then begin
      DecomposeMessage(TApprovalMessage(FApprovalMessages[i]).CallMessage, ddi, cli, routeName, systemName, operatorName, callId, messageString);
      DoSetStatus(5, Format('Removing message from %s on route %s via %s due to timeout.', [cli, routeName, ddi]));
      FApprovalMessages.Delete(i)
    end else
      inc(i);
  end;
end;

constructor TApprovalMessageList.Create;
begin
  inherited;
  FApprovalMessages := TObjectList.Create;
  FApprovalMessages.OwnsObjects := True;
end;

destructor TApprovalMessageList.Destroy;
begin
  FreeAndNil(FApprovalMessages);
  inherited;
end;

procedure TApprovalMessageList.DoSetStatus(const level: Integer; const status: String);
begin
  if Assigned(FSetStatus) then
    FSetStatus(level, status);
end;

function TApprovalMessageList.GetCount: Integer;
begin
  if Assigned(FApprovalMessages) then begin
    ClearOldMessages;
    result := FApprovalMessages.Count;
  end else
    result := 0;
end;

function TApprovalMessageList.GetNext: TCallMessage;
begin
  if Assigned(FApprovalMessages) then begin
    ClearOldMessages;
    if FApprovalMessages.Count > 0 then
      result := TApprovalMessage(FApprovalMessages[0]).CallMessage
    else
      result.identifier := 0;
  end else
    result.identifier := 0;
end;

procedure TApprovalMessageList.Removed(const msg: TApprovalMessage);
begin
  if Assigned(FApprovalMessages) then
    FApprovalMessages.Remove(msg);
end;

procedure TApprovalMessageList.SetSetStatus(const Value: TOnStatusEvent);
var
  i: Integer;
begin
  FSetStatus := Value;
  if Assigned(FApprovalMessages) then begin
    for i := 0 to FApprovalMessages.Count - 1 do
      TApprovalMessage(FApprovalMessages[i]).SetStatus := Value;
  end;
end;

function TApprovalMessageList.CallMessage(idx: Integer): TCallMessage;
var
  am: TApprovalMessage;
begin
  am := CallObject(idx);
  if Assigned(am) then
    result := am.CallMessage
  else
    result.identifier := 0;
end;

function TApprovalMessageList.CallObject(idx: Integer): TApprovalMessage;
begin
  if Assigned(FApprovalMessages) then begin
    if (idx >= 0) and (idx < FApprovalMessages.Count) then
      result := TApprovalMessage(FApprovalMessages[idx])
    else
      result := nil;
  end else
    result := nil;
end;

{ TApprovalMessage }

constructor TApprovalMessage.Create(const list: TApprovalMessageList; const msg: TCallMessage; const timeout: Integer; const params: TStrings);
begin
  inherited Create;
  FParams := TStringList.Create;
  FArrivedAt := Now;
  FCallMessage := msg;
  FApprovalList := list;
  FTimeoutSeconds := timeout;
  FParams.Assign(params);
end;

destructor TApprovalMessage.Destroy;
var
  i: Integer;
begin
  FreeAndNil(FParams);
  if Assigned(FApprovalList) then
    FApprovalList.Removed(Self);
  if Assigned(FListBox) then begin
    i := FListBox.Items.IndexOfLinkedObject(Self);
    if i >= 0 then
      FListBox.Items.Objects[i] := nil;
  end;
  inherited;
end;

function TApprovalMessage.GetTimedOut: Boolean;
begin
  result := Now > IncSecond(FArrivedAt, FTimeoutSeconds);
end;

procedure TApprovalMessage.ListDestroyed(const list: TApprovalMessageList);
begin
  if List = FApprovalList then
    FApprovalList := nil;
end;

procedure TApprovalMessage.SetParams(const Value: TStrings);
begin
  FParams.Assign(Value);
end;

procedure TApprovalMessage.SetSetStatus(const Value: TOnStatusEvent);
begin
  FSetStatus := Value;
end;

end.
