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
unit CallMessage;

interface

uses
  Windows;

const
  cCallMessageId = 10;
  cTelephoneNoLen = 40;
  cRouteNameLen = 10;
  cSystemNameLen = 7;
  cOperatorNameLen = 20;
  cMessageStringLen = 200;
  cCallIdLen = 30;
  cCallManagerMessageId = 1025;
  cLocateTimeout = 200;
  cConnectTimeout = 500;
  cTimeoutResendDelay = 1; // second
  {$EXTERNALSYM SMTO_ERRORONEXIT}
  SMTO_ERRORONEXIT = $20;
  cWaitSetting = SMTO_NOTIMEOUTIFNOTHUNG + SMTO_ERRORONEXIT + SMTO_NORMAL;
  cMaxRetries = 5;
  cMinLogLevel = 0;
  cMaxLogLevel = 10;

type
  TCallMessageType = (
    cmtNone,
    cmtConnect, cmtConnectAccept, cmtConnectReject,
    cmtDisconnect, cmtDisconnectAccept,
    cmtRdpConnected, cmtRdpDisconnected,
    cmtManagerActive, cmtManagerInactive,
    cmtManagerAccept, cmtCallReceived,
    cmtCallResult, cmtBringToFront);
  TCallMessageTypes = set of TCallMessageType;
  TTelephoneNo = array [0 .. cTelephoneNoLen] of AnsiChar;
  TRouteName = array [0 .. cRouteNameLen] of AnsiChar;
  TSystemName = array [0 .. cSystemNameLen] of AnsiChar;
  TOperatorName = array [0 .. cOperatorNameLen] of AnsiChar;
  TMessageString = array [0 .. cMessageStringLen] of AnsiChar;
  TCallId = array [0 .. cCallIdLen] of AnsiChar;
  TCallMessage = packed record
    identifier: UInt64;
    sequenceNo: UInt64;
    msgType: TCallMessageType;
    handle: UInt64;
    ddi: TTelephoneNo;
    cli: TTelephoneNo;
    routeName: TRouteName;
    systemName: TSystemName;
    operatorName: TOperatorName;
    callId: TCallId;
    messageString: TMessageString;
  end;
  PCallMessage = ^TCallMessage;
  TMessageQueueEntry = class
  private
    FCallMessage: TCallMessage;
    function GetRouteName: String;
    procedure SetRouteName(const Value: String);
  public
    constructor Create(const msg: TCallMessage);
    property CallMessage: TCallMessage read FCallMessage write FCallMessage;
    property RouteName: String read GetRouteName write SetRouteName;
  end;
  TOnCallMessageEvent = procedure(const msg: TCallMessage; const params: String) of object;

procedure PopulateMessage(var
  msg: TCallMessage; msgType: TCallMessageType; handle: HWND;
  const ddi, cli, routeName, systemName, operatorName, callId, messageString: String);

procedure DecomposeMessage(
  const msg: TCallMessage; var ddi, cli, routeName, systemName, operatorName, callId, messageString: String);

const
  cConnectionManagementMessages = [cmtConnect .. cmtRdpDisconnected];
  cCallRoutingMessages = [cmtCallReceived];

implementation

uses
  Math, SharedDefinitions, SysUtils;

var
  GMessageSequence: Integer;

procedure PopulateMessage(
  var msg: TCallMessage; msgType: TCallMessageType; handle: HWND;
  const ddi, cli, routeName, systemName, operatorName, callId, messageString: String);
begin
  FillChar(msg, SizeOf(msg), 0);
  msg.msgType := msgType;
  msg.identifier := cCallManagerMessageId;
  Inc(GMessageSequence);
  msg.sequenceNo := GMessageSequence;
  msg.handle := handle;
  setArray(msg.ddi, ddi, cTelephoneNoLen);
  setArray(msg.cli, cli, cTelephoneNoLen);
  setArray(msg.routeName, UpperCase(routeName), cRouteNameLen);
  setArray(msg.systemName, systemName, cSystemNameLen);
  setArray(msg.operatorName, operatorName, cOperatorNameLen);
  setArray(msg.callId, callId, cCallIdLen);
  setArray(msg.messageString, messageString, cMessageStringLen);
end;

procedure DecomposeMessage(
  const msg: TCallMessage; var ddi, cli, routeName, systemName, operatorName, callId, messageString: String);
begin
  SetStr(ddi, msg.ddi);
  SetStr(cli, msg.cli);
  SetStr(routeName, msg.routeName);
  routeName := UpperCase(routeName);
  SetStr(systemName, msg.systemName);
  SetStr(operatorName, msg.operatorName);
  SetStr(callId, msg.callId);
  SetStr(messageString, msg.messageString);
end;


{ TMessageQueueEntry }

constructor TMessageQueueEntry.Create(const msg: TCallMessage);
begin
  inherited Create;
  FCallMessage := msg;
end;

function TMessageQueueEntry.GetRouteName: String;
begin
  SetStr(result, FCallMessage.routeName);
end;

procedure TMessageQueueEntry.SetRouteName(const Value: String);
begin
  setArray(FCallMessage.routeName, UpperCase(Value), cRouteNameLen);
end;

initialization
  GMessageSequence := 0;
end.
