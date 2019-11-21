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
unit CallManagerStatus;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, Vcl.ExtCtrls, IdContext,
  IdBaseComponent, IdComponent, IdCustomTCPServer,
  IdTCPServer, JvAppInst, CallMessage, Rdp, Contnrs, Vcl.Menus,
  VirtualChannel;

type
  TSetStatusProc = procedure (const level: Integer; const s: String) of object;
  THandleData = record
    processId: ULONG;
    bestHandle: HWND;
  end;
  PHandleData = ^THandleData;
  TfCallManagerStatus = class;
  TRdpChannel = class
  private
    FCallManager: TfCallManagerStatus;
    FHandleData: THandleData;
    FMessageQueue: TObjectList;
    FNextSend: TDateTime;
    FOnSetStatus: TSetStatusProc;
    FRdpHandle: HWND;
    FRouteName: String;
    FRdpChannelName: TRdpChannelName;
    FRetries: Integer;
    FRouterHandle: HWND;
    FCSection: TRTLCriticalSection;
    FCriticalSectionValid: Boolean;
    FCallPacket: TRdpPacket;
    FActivated: Boolean;
    FSpawnedRoute: Boolean;
    procedure DoSendMessages;
    procedure SendRouterMessage(const msg: TCallMessage);
    procedure DoEnterCriticalSection;
    procedure DoLeaveCriticalSection;
  protected
    procedure SetStatus(const level: Integer; const s: String);
    procedure SetupRdpHandle;
    procedure SendPendingCallPacket;
    procedure NotifyActivated;
    procedure NotifyTerminated;
  public
    constructor Create;
    destructor Destroy; override;
    function GetRdpHandle: HWND;
    function CanClose: Boolean;
    procedure Clear;
    procedure HandleRdpCallResult(const ddi, cli, routeName, systemName, operatorName, callId, messageString: String);
    procedure SendRemotePacket(const pkt: TRdpPacket);
    procedure ShowApplication;
  end;
  TRdpChannels = class
  private
    FCallManager: TfCallManagerStatus;
    FOnSetStatus: TSetStatusProc;
    FRdpChannels: TObjectList;
    FWantToClose: Boolean;
  protected
    procedure Add(channel: TRdpChannel);
    function IndexOf(channel: TRdpChannel): Integer; overload;
    function IndexOfRdpChannel(name: TRdpChannelName): Integer; overload;
    function IndexOf(name: String): Integer; overload;
    procedure Remove(const channel: TRdpChannel); overload;
    procedure Remove(const name: TRdpChannelName); overload;
    procedure Remove(const name: String); overload;
    procedure Remove(const channelIndex: Integer); overload;
    procedure SetStatus(const level: Integer; const s: String);
    procedure ProcessRdpMessage(const msg: TCallMessage; const params: String);
  public
    constructor Create;
    destructor Destroy; override;
    function RdpHandle(const routeName: String): HWND;
    function CanClose: Boolean;
    procedure DestroyRoute(const routeName: String);
    procedure DisconnectFromRouter;
    procedure DoSendMessages;
    procedure HandleRdpCallResult(const ddi, cli, routeName, systemName, operatorName, callId, messageString: String);
    procedure HandleRdpConnection(const routeName: String);
    procedure HandleRdpConnectAccept(const routeName: String);
    procedure HandleRdpDisconnection(const routeName: String);
    procedure HandleRdpManagerActive(const routeName: String);
    procedure HandleRdpManagerInactive(const routeName: String);
    procedure HandleRouterMessage(var msg: TWMCopyData);
    procedure ProcessRouterConnect(const routeIndex: Integer; var msg: TCallMessage);
    procedure ProcessRouterConnectAccept(const routeIndex: Integer; const routerHandle: HWND);
    procedure ProcessRouterDisconnect(const routeIndex: Integer; const routerHandle: HWND);
    procedure ProcessRouterDisconnectAccept(const routeIndex: Integer; const routerHandle: HWND);
    procedure ProcessRouterCallReceived(const routeIndex: Integer; const ddi, cli, systemName, callId, operatorName: String);
    procedure SetupRoute(const routeName, rdpChannelName: String; const spawnedRoute: Boolean);
    procedure ShowApplication(const routeIndex: Integer);
  end;
  TfCallManagerStatus = class(TForm)
    N1: TMenuItem;
    pmiExit: TMenuItem;
    pmiShowApplication: TMenuItem;
    pmiShowLog: TMenuItem;
    pmStatus: TPopupMenu;
    StatusMemo: TMemo;
    tiStatus: TTrayIcon;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure pmiShowApplicationClick(Sender: TObject);
    procedure pmiExitClick(Sender: TObject);
    procedure pmiShowLogClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FLogLevel: Integer;
    FRdpChannels: TRdpChannels;
    FWantToClose: Boolean;
    function RdpHandle(const routeName: String): HWND;
    procedure SetCaptions;
    procedure SetLogLevel(const Value: Integer);
  public
    property LogLevel: Integer read FLogLevel write SetLogLevel;
    procedure HandleRouterMessage(var msg: TWMCopyData); message WM_COPYDATA;
    procedure SetStatus(const level: Integer; const s: String);
    procedure SetupRoute(const routeName, rdpChannelName: String; const spawnedRoute: Boolean);
    procedure ShowApplication(const routeIndex: Integer);
  end;

var
  fCallManagerStatus: TfCallManagerStatus;

procedure CloseStatus; stdcall;
procedure OpenStatus; stdcall;
procedure SetupRoute(const routeName, rdpChannelName: String; const spawnedRoute: Boolean);
procedure SimulateRdpAppMessage(const msg: TCallMessage; const params: String);
function TestFetchHandle(const routeName: String): HWND; stdcall;

implementation

uses
  ClientVirtualChannel, DateUtils, Math, SharedDefinitions;

{$R *.dfm}

const
  cShowLogCap = 'Show log';
  cHideLogCap = 'Hide log';
  cCallPacketDelay = 250;

procedure CloseStatus; stdcall;
begin
  if Assigned(fCallManagerStatus) then
    FreeAndNil(fCallManagerStatus);
end;

function EnumLocateRouter(hwnd: THandle; lParam: LPARAM): Boolean; stdcall;
type
  DWPointer = ^DWORD;
var
  pClassName: Array[0..255] of char;
  rc: TRdpChannel;
  msg: TCallMessage;
  cds: TCopyDataStruct;
  msgResult: DWORD_PTR;
begin
  result := true;
  if GetClassName(hWnd, pClassName, SizeOf(pClassName)) <> 0 then begin
    if StrComp(pClassName, 'TfCallRouterMain') = 0 then begin
      if lParam <> 0 then begin
        rc := TRdpChannel(lParam);
        PopulateMessage(msg, cmtConnect, 0, '', '', rc.FRouteName, '', '', '', '');
        msg.handle := rc.FCallManager.Handle;
        cds.dwData := cCallManagerMessageId;
        cds.cbData := SizeOf(TCallMessage);
        cds.lpData := @msg;
        msgResult := 0;
        if SendMessageTimeout(hWnd, WM_COPYDATA, 0, NativeInt(@cds), cWaitSetting, cLocateTimeout, @msgResult) <> 0 then begin
          rc.FRouterHandle := msgResult;
          result := false;
        end;
      end;
    end;
  end;
end;

function FindRdpWindowHandleCallback(handle: THandle; lParam: LPARAM): Boolean; stdcall;

  function isMainWindow(handle: HWND): Boolean;
  begin
    result := (GetWindow(handle, GW_OWNER) = HWND(0)) and (IsWindowVisible(handle) or IsIconic(handle));
  end;

var
  handleData: PHandleData;
  processId: ULONG;
  hwClassName: Array[0..255] of Char;
begin
  if GetClassName(handle, hwClassName, SizeOf(hwClassName)) <> 0 then begin
    if (StrComp(hwClassName, 'TfCallManagerStatus') = 0) then
      result := true
    else begin
      handleData := PHandleData(lParam);
      processId := 0;
      GetWindowThreadProcessId(handle, &processId);
      if (handleData^.processId = processId) and isMainWindow(handle) then begin
        handleData^.bestHandle := handle;
        result := false;
      end else
        result := true
    end;
  end else
    result := true;
end;

procedure OpenStatus; stdcall;
begin
  if not Assigned(fCallManagerStatus) then
    fCallManagerStatus := TfCallManagerStatus.Create(nil);
end;

procedure SetupRoute(const routeName, rdpChannelName: String; const spawnedRoute: Boolean);
begin
  if Assigned(fCallManagerStatus) then
    fCallManagerStatus.SetupRoute(routeName, rdpChannelName, spawnedRoute);
end;

procedure SimulateRdpAppMessage(const msg: TCallMessage; const params: String);
var
  pkt: TRdpPacket;
  pkt2: TRdpPacket;
  buffer: TBytes;
begin
  if Assigned(fCallManagerStatus) and Assigned(fCallManagerStatus.FRdpChannels) then begin
    pkt.Clear;
    pkt2.Clear;
    pkt.msg := msg;
    pkt.ParamStr := params;
    pkt.ToBytes(buffer);
    pkt2.SetFromBytes(buffer);
    fCallManagerStatus.FRdpChannels.ProcessRdpMessage(pkt2.msg, pkt2.ParamStr);
  end;
end;

function TestFetchHandle(const routeName: String): HWND; stdcall;
begin
  openStatus;
  result := fCallManagerStatus.RdpHandle(routeName);
end;


{ TRdpChannel }

constructor TRdpChannel.Create;
begin
  inherited;
  InitializeCriticalSection(FCSection);
  FCriticalSectionValid := True;
  FCallPacket.Clear;
  FRdpHandle := 0;
  FRouterHandle := 0;
  FNextSend := Now;
  FRetries := 0;
  DoEnterCriticalSection;
  try
    FMessageQueue := TObjectList.Create;
    FMessageQueue.OwnsObjects := True;
  finally
    DoLeaveCriticalSection;
  end;
end;

destructor TRdpChannel.Destroy;
var
  msg: TCallMessage;
begin
  if FActivated then begin
    PopulateMessage(msg, cmtRdpDisconnected, FCallManager.Handle, '', '', FRouteName, '', '', '', '');
    SendRouterMessage(msg);
  end;
  SetCallMessageEvent(FRdpChannelName, nil);
  SetOnActivateEvent(FRdpChannelName, nil);
  SetOnTerminateEvent(FRdpChannelName, nil);
  DoEnterCriticalSection;
  try
    FMessageQueue.Clear;
    FreeAndNil(FMessageQueue);
  finally
    DoLeaveCriticalSection;
  end;
  FCriticalSectionValid := False;
  DeleteCriticalSection(FCSection);
  inherited;
end;

function TRdpChannel.CanClose: Boolean;
begin
  result := (FRouterHandle = 0) or not IsWindow(FRouterHandle);
end;

function TRdpChannel.GetRdpHandle: HWND;
begin
  if IsWindow(FRdpHandle) then
    result := FRdpHandle
  else begin
    GetWindowThreadProcessId(FCallManager.Handle, FHandleData.processId);
    FHandleData.bestHandle := 0;
    EnumWindows(@FindRdpWindowHandleCallback, NativeInt(@FHandleData));
    FRdpHandle := FHandleData.bestHandle;
    result := FRdpHandle;
  end;
end;

procedure TRdpChannel.Clear;
begin

end;

procedure TRdpChannel.DoEnterCriticalSection;
begin
  if FCriticalSectionValid then
    EnterCriticalSection(FCSection);
end;

procedure TRdpChannel.DoLeaveCriticalSection;
begin
  if FCriticalSectionValid then
    LeaveCriticalSection(FCSection);
end;

procedure TRdpChannel.DoSendMessages;
var
  cds: TCopyDataStruct;
  cmsg: TCallMessage;
  msgResult: DWORD_PTR;
begin
  SetStatus(5, 'Processing any pending messages');
  DoEnterCriticalSection;
  try
    while Assigned(FMessageQueue) and (FRouterHandle <> 0) and (FMessageQueue.Count > 0) and (Now > FNextSend) do begin
      if IsWindow(FRouterHandle) then begin
        if FRetries < cMaxRetries then begin
          SetStatus(5, Format('Sending message %d to router %d ', [Integer(TMessageQueueEntry(FMessageQueue[0]).CallMessage.msgType), FRouterHandle]));
          TMessageQueueEntry(FMessageQueue[0]).RouteName := FRouteName;
          cds.dwData := cCallManagerMessageId;
          cds.cbData := SizeOf(TCallMessage);
          cmsg := TMessageQueueEntry(FMessageQueue[0]).CallMessage;
          cds.lpData := @cmsg;
          msgResult := 0;
          if SendMessageTimeout(FRouterHandle, WM_COPYDATA, 0, NativeInt(@cds), cWaitSetting, cConnectTimeout, @msgResult) <> 0 then begin
            FRetries := 0;
            if Assigned(FMessageQueue) then
              FMessageQueue.Delete(0);
            SetStatus(5, 'Send successful. Removed message from queue');
          end else begin
            SetStatus(1, Format('Send failed with message code %d', [GetLastError]));
            FNextSend := IncSecond(Now, cTimeoutResendDelay);
            inc(FRetries);
          end;
        end else begin
          SetStatus(5, 'Maximum send retries reached. Clearing manager handle.');
          FRetries := 0;
          FRouterHandle := 0;
        end;
      end else begin
        SetStatus(1, 'No router on original handle. Clearing manager handle');
        FRouterHandle := 0;
        FRetries := 0;
      end;
    end;
  finally
    DoLeaveCriticalSection;
  end;
end;

procedure TRdpChannel.HandleRdpCallResult(const ddi, cli, routeName, systemName, operatorName, callId,
  messageString: String);
var
  msg: TCallMessage;
begin
  if (routeName = FRouteName) then begin
    SetStatus(5, Format('Received RDP call result on route (%s)', [routeName]));
    PopulateMessage(msg, cmtCallResult, FCallManager.Handle, ddi, cli, routeName, systemName, operatorName, callId, messageString);
    SendRouterMessage(msg);
  end else
    SetStatus(1, Format('Received unexpected RDP call result on route (%s). My route is (%s)', [routeName, FRouteName]));
end;

procedure TRdpChannel.NotifyActivated;
var
  msg: TCallMessage;
begin
  if not FActivated then begin
    FActivated := True;
    PopulateMessage(msg, cmtRdpConnected, FCallManager.Handle, '', '', FRouteName, '', '', '', '');
    SendRouterMessage(msg);
  end;
end;

procedure TRdpChannel.NotifyTerminated;
var
  msg: TCallMessage;
begin
  if FActivated then begin
    FActivated := False;
    PopulateMessage(msg, cmtRdpDisconnected, FCallManager.Handle, '', '', FRouteName, '', '', '', '');
    SendRouterMessage(msg);
  end;
end;

procedure TRdpChannel.SendPendingCallPacket;
begin
  if FCallPacket.msg.msgType = cmtCallReceived then begin
    SendRemotePacket(FCallPacket);
    FCallPacket.Clear;
  end;
end;

procedure TRdpChannel.SendRemotePacket(const pkt: TRdpPacket);
begin
  if (FRdpHandle <> 0) and ClientVirtualChannel.ChannelHandleValid(FRdpChannelName) then begin
    SetStatus(5, Format('Writing packet (%d) over channel to remote app.', [Integer(pkt.msg.msgType)]));
    pkt.SetRdpChannelName(FRdpChannelName);
    ClientVirtualChannel.WriteClientChannelPacket(pkt);
    FCallPacket.Clear;
  end else if pkt.msg.msgType = cmtCallReceived then begin
    SetStatus(5, 'Queueing call packet ready for rdp connection.');
    FCallPacket := pkt;
  end else
    SetStatus(5, Format('Ignoring packet (%d) - no rdp channel.', [Integer(pkt.msg.msgType)]));
end;

procedure TRdpChannel.SendRouterMessage(const msg: TCallMessage);

  procedure RemoveDuplicateMessages(const mTypes: TCallMessageTypes);
  var
    i: Integer;
  begin
    i := 0;
    DoEnterCriticalSection;
    try
      while i < FMessageQueue.Count do begin
        if TMessageQueueEntry(FMessageQueue[i]).CallMessage.msgType in mTypes then
          FMessageQueue.Delete(i)
        else
          inc(i);
      end;
    finally
      DoLeaveCriticalSection;
    end;
  end;

begin
  // Connection management is always moved to the front of the message queue and any existing similar messages are
  // removed. Call routing messages cannot be duplicated (makes no sense to have thousands of popping windows - there
  // will only be one call at a time being managed.)
  if msg.msgType in cConnectionManagementMessages then begin
    RemoveDuplicateMessages(cConnectionManagementMessages);
    DoEnterCriticalSection;
    try
      FMessageQueue.Insert(0, TMessageQueueEntry.Create(msg));
    finally
      DoLeaveCriticalSection;
    end;
  end else begin
    if msg.msgType in cCallRoutingMessages then
      RemoveDuplicateMessages(cCallRoutingMessages);
    DoEnterCriticalSection;
    try
      FMessageQueue.Add(TMessageQueueEntry.Create(msg));
    finally
      DoLeaveCriticalSection;
    end;
  end;
  if FRouterHandle = 0 then
    EnumWindows(@EnumLocateRouter, NativeInt(Self))
  else
    DoSendMessages;
end;

procedure TRdpChannel.SetStatus(const level: Integer; const s: String);
begin
  if Assigned(FOnSetStatus) then
    FOnSetStatus(level, s);
end;

procedure TRdpChannel.SetupRdpHandle;
begin
  FRdpHandle := GetRdpHandle;
end;

procedure TRdpChannel.ShowApplication;
var
  h: HWND;
  hClassName: Array[0..255] of Char;
begin
  h := GetRdpHandle;
  SetStatus(5, 'Show application called');
  if IsWindow(h) then begin
    SetStatus(5, 'Rdp handle is a valid window');
    if GetClassName(h, hClassName, SizeOf(hClassName)) <> 0 then begin
      // Attempts to restore a Delphi app, which has a 0 pixel main window
      // and apparently documentation says that it should be restored by
      // sending a WM_SYSCOMMAND message with SC_RESTORE as a parameter. Does
      // not always appear to work if the user clicks the minimise button on the
      // app main window.
      if (StrComp(hClassName, 'TApplication') = 0) then begin
        SetStatus(5, 'Found a Delphi application. Attempting to restore using WM_SYSCOMMAND/SC_RESTORE');
        PostMessage(h, WM_SYSCOMMAND, SC_RESTORE, 0);
      end;
    end;
    if ShowWindow(h, SW_RESTORE) then begin
      SetStatus(5, 'Restored the window. Attempting to it to the foreground');
      if SetForegroundWindow(h) then
        SetStatus(5, 'Brought to foreground')
      else begin
        SetStatus(5, 'Failed to bring to foreground. Attempt to force');
        ForceForegroundWindow(h);
      end;
    end else begin
      SetStatus(5, 'Could not restore the window, trying to show it');
      if ShowWindow(h, SW_SHOWDEFAULT) then begin
        SetStatus(5, 'Show window (default) successful');
        if SetForegroundWindow(h) then
          SetStatus(5, 'Brought to the foreground')
        else begin
          SetStatus(5, 'Cound not bring to the foreground. Attempt to force.');
          ForceForegroundWindow(h);
        end;
      end else begin
        SetStatus(5, 'Show window (default) failed. Attempt to show minimised');
        if ShowWindow(h, SW_MINIMIZE) then begin
          SetStatus(5, 'Calling OpenIcon');
          if OpenIcon(h) then
            SetStatus(5, 'OpenIcon successful')
          else begin
            SetStatus(5, 'OpenIcon failed. Attempt to force');
            ForceForegroundWindow(h);
          end;
        end else begin
          SetStatus(5, 'Show window (minimised) failed. Attempt to force');
          ForceForegroundWindow(h);
        end;
      end;
    end;
  end else
    SetStatus(5, 'Rdp handle returned was not a valid window.');
end;

{ TRdpChannels }

constructor TRdpChannels.Create;
begin
  inherited;
  FRdpChannels := TObjectList.Create;
  FRdpChannels.OwnsObjects := True;
  FWantToClose := False;
end;

destructor TRdpChannels.Destroy;
begin
  FreeAndNil(FRdpChannels);
  inherited;
end;

function TRdpChannels.CanClose: Boolean;
var
  i: Integer;
begin
  result := True;
  if Assigned(FRdpChannels) then begin
    i := 0;
    while result and (i < FRdpChannels.Count) do begin
      result := result and TRdpChannel(FRdpChannels[i]).CanClose;
      inc(i);
    end;
  end;
end;

function TRdpChannels.IndexOf(channel: TRdpChannel): Integer;
begin
  result := FRdpChannels.Count;
  repeat
    Dec(result);
  until (result < 0) or (UpperCase(TRdpChannel(FRdpChannels[result]).FRouteName) = UpperCase(channel.FRouteName));
end;

function TRdpChannels.IndexOfRdpChannel(name: TRdpChannelName): Integer;
var
  s: String;
begin
  SetStr(s, name);
  result := FRdpChannels.Count;
  repeat
    Dec(result);
  until (result < 0) or (UpperCase(String(TRdpChannel(FRdpChannels[result]).FRdpChannelName)) = UpperCase(s));
end;

function TRdpChannels.IndexOf(name: String): Integer;
begin
  result := FRdpChannels.Count;
  repeat
    Dec(result);
  until (result < 0) or (UpperCase(TRdpChannel(FRdpChannels[result]).FRouteName) = UpperCase(name));
end;

function TRdpChannels.RdpHandle(const routeName: String): HWND;
var
  i: Integer;
begin
  i := IndexOf(routeName);
  if i >= 0 then begin
    result := TRdpChannel(FRdpChannels[i]).GetRdpHandle;
    SetStatus(10, Format('Found RDP handle (%d) for route (%s)', [result, routeName]));
  end else begin
    result := 0;
    SetStatus(10, Format('Could not find RDP handle (%s)', [routeName]));
  end;
end;

procedure TRdpChannels.Add(channel: TRdpChannel);
var
  i: Integer;
begin
  if channel.FRouteName <> '' then begin
    i := IndexOf(channel);
    if i < 0 then begin
      i := IndexOf(channel.FRouteName);
      if i < 0 then
        FRdpChannels.Add(channel);
    end;
  end;
end;

procedure TRdpChannels.DestroyRoute(const routeName: string);
var
  pkt: TRdpPacket;
  i: Integer;
  rc: TRdpChannel;
begin
  i := IndexOf(routeName);
  if i >= 0 then begin
    rc := TRdpChannel(FRdpChannels[i]);
    SetStatus(5, Format('Destroying route %s', [routeName]));
    rc.FRdpHandle := 0;
    SetStatus(10, Format('Send disconnect accept to remote app via route (%s)', [routeName]));
    pkt.Clear;
    PopulateMessage(pkt.msg, cmtDisconnectAccept, 0, '', '', rc.FRouteName, '', '', '', '');
    rc.SendRemotePacket(pkt);
  end else
    SetStatus(1, Format('Destroy requested on unknown route (%s)', [routeName]));
end;

procedure TRdpChannels.DisconnectFromRouter;
var
  msg: TCallMessage;
  i: Integer;
  rc: TRdpChannel;
begin
  FWantToClose := True;
  i := 0;
  while i < FRdpChannels.Count do begin
    rc := TRdpChannel(FRdpChannels[i]);
    if IsWindow(rc.FRouterHandle) then begin
      SetStatus(5, Format('Disconnecting from router %s.', [rc.FRouteName]));
      PopulateMessage(msg, cmtDisconnect, FCallManager.Handle, '', '', rc.FRouteName, '', '', '', '');
      rc.SendRouterMessage(msg);
    end else begin
      SetStatus(5, 'Router handle not valid. Already disconnected');
      rc.FRouterHandle := 0;
    end;
    inc(i);
  end;
end;

procedure TRdpChannels.DoSendMessages;
var
  i: Integer;
begin
  for i := 0 to FRdpChannels.Count - 1 do
    TRdpChannel(FRdpChannels[i]).DoSendMessages;
end;

procedure TRdpChannels.HandleRdpCallResult(const ddi, cli, routeName, systemName, operatorName, callId,
  messageString: String);
var
  i: Integer;
begin
  i := IndexOf(routeName);
  if i >= 0 then begin
    SetStatus(5, Format('RDP call result on route (%s)', [routeName]));
    TRdpChannel(FRdpChannels[i]).HandleRdpCallResult(ddi, cli, routeName, systemName, operatorName, callId, messageString);
  end else
    SetStatus(1, Format('RDP call result on unknown route (%s)', [routeName]));
end;

procedure TRdpChannels.HandleRdpConnectAccept(const routeName: String);
var
  i: Integer;
begin
  i := IndexOf(routeName);
  if i >= 0 then begin
    SetStatus(5, Format('RDP connect accept on route (%s)', [routeName]));
    TRdpChannel(FRdpChannels[i]).SendPendingCallPacket;
  end else
    SetStatus(1, Format('Connect accept received for unknown route (%s)', [routeName]));
end;

procedure TRdpChannels.HandleRdpConnection(const routeName: String);
var
  i: Integer;
  pkt: TRdpPacket;
  rc: TRdpChannel;
begin
  i := IndexOf(routeName);
  if i >= 0 then begin
    SetStatus(5, Format('RDP connect on route (%s)', [routeName]));
    rc := TRdpChannel(FRdpChannels[i]);
    SetupRoute(routeName, String(rc.FRdpChannelName), rc.FSpawnedRoute);
    pkt.Clear;
    PopulateMessage(pkt.msg, cmtConnectAccept, 0, '', '', routeName, '', '', '', '');
    rc.SendRemotePacket(pkt);
  end else
    SetStatus(1, Format('Connection received for unknown route (%s)', [routeName]));
end;

procedure TRdpChannels.HandleRdpDisconnection(const routeName: String);
begin
  SetStatus(5, Format('RDP disconnect on route (%s)', [routeName]));
  DestroyRoute(routeName);
end;

procedure TRdpChannels.HandleRdpManagerActive(const routeName: String);
var
  i: Integer;
  pkt: TRdpPacket;
  rc: TRdpChannel;
begin
  i := IndexOf(routeName);
  if i >= 0 then begin
    SetStatus(5, Format('RDP manager active on route (%s)', [routeName]));
    rc := TRdpChannel(FRdpChannels[i]);
    pkt.Clear;
    PopulateMessage(pkt.msg, cmtManagerActive, 0, '', '', routeName, '', '', '', '');
    // Forward message to router
    rc.SendRouterMessage(pkt.msg);
    Sleep(cCallPacketDelay);
    // Send any pending call packets
    rc.SendPendingCallPacket;
  end else
    SetStatus(1, Format('ManagerInactive received for unknown route (%s)', [routeName]));
end;

procedure TRdpChannels.HandleRdpManagerInactive(const routeName: String);
var
  i: Integer;
  pkt: TRdpPacket;
  rc: TRdpChannel;
begin
  i := IndexOf(routeName);
  if i >= 0 then begin
    SetStatus(5, Format('RDP manager inactive on route (%s)', [routeName]));
    rc := TRdpChannel(FRdpChannels[i]);
    pkt.Clear;
    PopulateMessage(pkt.msg, cmtManagerInactive, 0, '', '', routeName, '', '', '', '');
    // Forward message to router
    rc.SendRouterMessage(pkt.msg);
  end else
    SetStatus(1, Format('ManagerInactive received for unknown route (%s)', [routeName]));
end;

procedure TRdpChannels.HandleRouterMessage(var msg: TWMCopyData);
var
  callMsg: TCallMessage;
  ddi: String;
  cli: String;
  routeName: String;
  operatorName: String;
  systemName: String;
  callId: String;
  messageString: String;
  myName: String;
  i: Integer;
begin
  SetStatus(10, Format('Router message received (%d/%d/%d)', [msg.CopyDataStruct.dwData, msg.CopyDataStruct.cbData, SizeOf(callMsg)]));
  if (msg.CopyDataStruct.dwData = cCallManagerMessageId) and (msg.CopyDataStruct.cbData = SizeOf(callMsg)) then begin
    Move(msg.CopyDataStruct.lpData^, callMsg, SizeOf(callMsg));
    CallMessage.DecomposeMessage(callMsg, ddi, cli, routeName, systemName, operatorName, callId, messageString);
    i := IndexOf(routeName);
    if i >= 0 then begin
      msg.Result := FCallManager.Handle;
      if callMsg.msgType = cmtConnect then begin
        SetStatus(5, 'Connect received');
        ProcessRouterConnect(i, callMsg);
      end else if callMsg.msgType = cmtConnectAccept then begin
        SetStatus(5, 'ConnectAccept received');
        ProcessRouterConnectAccept(i, callMsg.handle);
      end else if callMsg.msgType = cmtDisconnect then begin
        SetStatus(5, 'Disconnect received');
        ProcessRouterDisconnect(i, callMsg.handle);
      end else if callMsg.msgType = cmtDisconnectAccept then begin
        SetStatus(5, 'DisconnectAccept received');
        ProcessRouterDisconnectAccept(i, callMsg.handle);
      end else if callMsg.msgType = cmtCallReceived then begin
        SetStatus(5, 'Call received');
        ProcessRouterCallReceived(i, ddi, cli, systemName, callId, operatorName);
      end else if callMsg.msgType = cmtBringToFront then begin
        SetStatus(5, 'BringToFront received');
        ShowApplication(i);
      end else
        SetStatus(1, Format('Unexpected message (%d) received from router', [Integer(callMsg.msgType)]));
    end else begin
      if routeName = '' then
        myName := 'not yet set'
      else
        myName := routeName;
      SetStatus(1, Format('Received message intended for unhandled route (%s)', [myName]));
      msg.Result := 0;
    end;
  end else
    SetStatus(10, Format('Unidentified message received (%d/%d/%d)', [msg.CopyDataStruct.dwData, msg.CopyDataStruct.cbData, SizeOf(callMsg)]));
end;

procedure TRdpChannels.ProcessRdpMessage(const msg: TCallMessage; const params: String);
var
  ddi: String;
  cli: String;
  routeName: String;
  systemName: String;
  operatorName: String;
  messageString: String;
  callId: String;
begin
  SetStatus(5, 'Processing message');
  DecomposeMessage(msg, ddi, cli, routeName, systemName, operatorName, callId, messageString);
  if msg.msgType = cmtConnect then
    // A connect message has been sent from the app running in the RDP session. Send this
    // information to the router so that it knows how to route messages.
    HandleRdpConnection(routeName)
  else if msg.msgType = cmtConnectAccept then
    // A connect message has been sent from the app running in the RDP session. Send this
    // information to the router so that it knows how to route messages.
    HandleRdpConnectAccept(routeName)
  else if msg.MsgType = cmtDisconnect then
    // The app running in the RDP session has closed down, so nothing can now be routed
    // to this session. Notify the router.
    HandleRdpDisconnection(routeName)
  else if msg.msgType = cmtCallResult then
    // The app running in the RDP session has processed a call so here is the result. Send it
    // to the router, in case it needs to do anything.
    HandleRdpCallResult(ddi, cli, routeName, systemName, operatorName, callId, messageString)
  else if msg.msgType = cmtManagerActive then
    // The the remote call manager is now active. Notify the router.
    HandleRdpManagerActive(routeName)
  else if msg.msgType = cmtManagerInactive then
    // The the remote call manager is now inactive. Notify the router.
    HandleRdpManagerInactive(routeName)
  else
    // A message that was not expected was found so complain about it. The RDP app should
    // not have sent this message.
    SetStatus(1, Format('Unexpected message (%d) received from server', [Integer(msg.msgType)]));
end;

procedure TRdpChannels.ProcessRouterCallReceived(const routeIndex: Integer; const ddi, cli, systemName, callId, operatorName: String);
var
  pkt: TRdpPacket;
  rc: TRdpChannel;
begin
  if (routeIndex >= 0) and (routeIndex <= FRdpChannels.Count) then begin
    rc := TRdpChannel(FRdpChannels[routeIndex]);
    pkt.Clear;
    SetStatus(5, Format('Received call notification on route %s', [rc.FRouteName]));
    PopulateMessage(pkt.msg, cmtCallReceived, 0, ddi, cli, rc.FRouteName, systemName, operatorName, callId, '');
    SetStatus(5, Format('DDI %s', [ddi]));
    SetStatus(5, Format('CLI %s', [cli]));
    rc.SendRemotePacket(pkt);
  end else
    SetStatus(1, Format('Received call notification on unknown route index %d', [routeIndex]));
end;

procedure TRdpChannels.ProcessRouterConnect(const routeIndex: Integer; var msg: TCallMessage);
var
  rc: TRdpChannel;
begin
  if (routeIndex >= 0) and (routeIndex < FRdpChannels.Count) then begin
    rc := TRdpChannel(FRdpChannels[routeIndex]);
    if ((rc.FRouterHandle = msg.handle) or (rc.FRouterHandle = 0)) then begin
      SetStatus(5, Format('Received connect on route %s from router handle %d', [rc.FRouteName, msg.handle]));
      if rc.FSpawnedRoute then begin
        rc.FRouterHandle := msg.handle;
        PopulateMessage(msg, cmtConnectAccept, FCallManager.Handle, '', '', rc.FRouteName, '', '', '', '');
        rc.SendRouterMessage(msg);
      end;
    end else
      SetStatus(1, Format('Received connect on different router handle %d', [msg.handle]));
  end else
    SetStatus(1, Format('Received connect on unknown route index %d', [routeIndex]));
end;

procedure TRdpChannels.ProcessRouterConnectAccept(const routeIndex: Integer; const routerHandle: HWND);
var
  rc: TRdpChannel;
begin
  if (routeIndex >= 0) and (routeIndex < FRdpChannels.Count) then begin
    rc := TRdpChannel(FRdpChannels[routeIndex]);
    if (rc.FRouterHandle = routerHandle) or (rc.FRouterHandle = 0) then begin
      SetStatus(5, Format('Router %s with handle %d accepted connection', [rc.FRouteName, routerHandle]));
      rc.FRouterHandle := routerHandle;
    end else
      SetStatus(1, Format('Received connect accept from different router handle %d', [routerHandle]));
  end else
    SetStatus(1, Format('Received connect accept from unknown route index %d', [routeIndex]));
end;

procedure TRdpChannels.ProcessRouterDisconnect(const routeIndex: Integer; const routerHandle: HWND);
var
  msg: TCallMessage;
  rc: TRdpChannel;
begin
  if (routeIndex >= 0) and (routeIndex < FRdpChannels.Count) then begin
    rc := TRdpChannel(FRdpChannels[routeIndex]);
    if rc.FRouterHandle = routerHandle then begin
      SetStatus(5, Format('Received disconnect on route %s from router handle %d', [rc.FRouteName, routerHandle]));
      PopulateMessage(msg, cmtDisconnectAccept, FCallManager.Handle, '', '', rc.FRouteName, '', '', '', '');
      rc.SendRouterMessage(msg);
      rc.FRouterHandle := 0;
    end else
      SetStatus(10, Format('Received disconnect from different router handle %d', [routerHandle]));
  end else
    SetStatus(10, Format('Received disconnect from unknown route id %d', [routeIndex]));
end;

procedure TRdpChannels.ProcessRouterDisconnectAccept(const routeIndex: Integer; const routerHandle: HWND);
var
  rc: TRdpChannel;
begin
  if (routeIndex >= 0) and (routeIndex < FRdpChannels.Count) then begin
    rc := TRdpChannel(FRdpChannels[routeIndex]);
    SetStatus(5, Format('Received disconnect accept on route %s from router handle %d', [rc.FRouteName, routerHandle]));
    rc.FRouterHandle := 0;
    FRdpChannels.Delete(routeIndex);
    if (FRdpChannels.Count = 0) and FWantToClose and Assigned(FCallManager) then begin
      SetStatus(5, 'Closing manager window');
      FCallManager.Close;
    end;
  end else
    SetStatus(10, Format('Disconnect accept on unknown route %d', [routeIndex]));
end;

procedure TRdpChannels.Remove(const channel: TRdpChannel);
var
  i: Integer;
begin
  i := IndexOf(channel);
  if (i >= 0) then
    Remove(i);
end;

procedure TRdpChannels.Remove(const name: String);
var
  i: Integer;
begin
  i := IndexOf(name);
  if (i >= 0) then
    Remove(i);
end;

procedure TRdpChannels.Remove(const name: TRdpChannelName);
var
  i: Integer;
begin
  i := IndexOfRdpChannel(name);
  if (i >= 0) then
    Remove(i);
end;

procedure TRdpChannels.SetStatus(const level: Integer; const s: String);
begin
  if Assigned(FOnSetStatus) then
    FOnSetStatus(level, s);
end;

procedure TRdpChannels.SetupRoute(const routeName, rdpChannelName: String; const spawnedRoute: Boolean);

  procedure SetupEvents(rc: TRdpChannel);
  begin
    SetCallMessageEvent(rc.FRdpChannelName, ProcessRdpMessage);
    SetOnActivateEvent(rc.FRdpChannelName, rc.NotifyActivated);
    SetOnTerminateEvent(rc.FRdpChannelName, rc.NotifyTerminated);
    // It is possible that the channel is already active by now,
    // so check and notify.
    if ChannelHandleValid(rc.FRdpChannelName) then
      rc.NotifyActivated;
  end;

var
  i: Integer;
  rc: TRdpChannel;
begin
  if Length(routeName) > cRouteNameLen then
    SetStatus(1, Format('Route name %s is too long', [routeName]))
  else begin
    i := IndexOf(routeName);
    if i >= 0 then begin
      rc := TRdpChannel(FRdpChannels[i]);
      rc.FOnSetStatus := SetStatus;
      if rc.FRdpHandle <> 0 then
        SetStatus(1, Format('Route %s already set up', [routeName]))
      else begin
        SetChannelName(rc.FRdpChannelName, UpperCase(rdpChannelName));
        SetupEvents(rc);
        rc.SetupRdpHandle;
        rc.FSpawnedRoute := spawnedRoute;
        // Connect to the router.
        if rc.FRouteName <> '' then
          EnumWindows(@EnumLocateRouter, NativeInt(rc));
      end;
    end else begin
      rc := TRdpChannel.Create;
      FRdpChannels.Add(rc);
      rc.FRouteName := UpperCase(routeName);
      SetChannelName(rc.FRdpChannelName, UpperCase(rdpChannelName));
      SetupEvents(rc);
      rc.FCallManager := FCallManager;
      rc.FOnSetStatus := SetStatus;
      rc.SetupRdpHandle;
      rc.FSpawnedRoute := spawnedRoute;
      // Connect to the router.
      if rc.FRouteName <> '' then
        EnumWindows(@EnumLocateRouter, NativeInt(rc));
    end;
  end;
end;

procedure TRdpChannels.ShowApplication(const routeIndex: Integer);
begin
  if (routeIndex >= 0) and (routeIndex < FRdpChannels.Count) then
    TRdpChannel(FRdpChannels[routeIndex]).ShowApplication
  else
    SetStatus(1, Format('Show application for unknown route id (%d)', [routeIndex]));
end;

procedure TRdpChannels.Remove(const channelIndex: Integer);
begin
  if (channelIndex >= 0) and (channelIndex < FRdpChannels.Count) then
    FRdpChannels.Delete(channelIndex);
end;

{ TfCallManagerStatus }

function TfCallManagerStatus.RdpHandle(const routeName: String): HWND;
begin
  if Assigned(FRdpChannels) then
    result := FRdpChannels.RdpHandle(routeName)
  else
    result := 0;
end;

procedure TfCallManagerStatus.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TfCallManagerStatus.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := Assigned(FRdpChannels) and FRdpChannels.CanClose;
  if not CanClose then
    FRdpChannels.DisconnectFromRouter;
end;

procedure TfCallManagerStatus.FormCreate(Sender: TObject);
var
  chNames: TStringList;
  routeNames: TStringList;
  i: Integer;
  spawnerRoute: String;
begin
  FRdpChannels := TRdpChannels.Create;
  FRdpChannels.FCallManager := Self;
  FRdpChannels.FOnSetStatus := SetStatus;
  spawnerRoute := UpperCase(GetEnvironmentVariable(cRouteHandledEnvVar));
  chNames := TStringList.Create;
  try
    routeNames := TStringList.Create;
    try
      FetchRDChannelNames(chNames, routeNames);
      for i := 0 to chNames.Count -1 do
        FRdpChannels.SetupRoute(routeNames[i], chNames[i], UpperCase(routeNames[i]) = spawnerRoute);
    finally
      routeNames.Free;
    end;
  finally
    chNames.Free;
  end;
  FLogLevel := 10;
  FWantToClose := False;
  SetCaptions;
  SetStatus(10, Format('My window handle is %d', [Handle]));
  SetStatus(1, Format('Log level is %d', [FLogLevel]));
end;

procedure TfCallManagerStatus.FormDestroy(Sender: TObject);
begin
  FRdpChannels.DisconnectFromRouter;
  FRdpChannels.FCallManager := nil;
  FreeAndNil(FRdpChannels);
  if FCallManagerStatus = Self then
    fCallManagerStatus := nil;
end;

procedure TfCallManagerStatus.HandleRouterMessage(var msg: TWMCopyData);
begin
  if Assigned(FRdpChannels) then
    FRdpChannels.HandleRouterMessage(msg)
  else
    SetStatus(1, 'Received windows message but no channels are available.');
end;

procedure TfCallManagerStatus.SetCaptions;
begin
  if Visible then
    pmiShowLog.Caption := cHideLogCap
  else
    pmiShowLog.Caption := cShowLogCap
end;

procedure TfCallManagerStatus.SetLogLevel(const Value: Integer);
begin
  FLogLevel := Value;
end;

procedure TfCallManagerStatus.SetStatus(const level: Integer; const s: String);
begin
  if level <= FLogLevel then
    StatusMemo.Lines.Add(s);
end;

procedure TfCallManagerStatus.SetupRoute(const routeName, rdpChannelName: String; const spawnedRoute: Boolean);
begin
  FRdpChannels.SetupRoute(routeName, rdpChannelName, spawnedRoute);
end;

procedure TfCallManagerStatus.ShowApplication(const routeIndex: Integer);
begin
  tiStatus.Visible := False;
  try
    if Assigned(FRdpChannels) then
      FRdpChannels.ShowApplication(routeIndex)
    else
      SetStatus(5, 'Request to show application but no routes available.');
  finally
    tiStatus.Visible := True;
  end;
end;

procedure TfCallManagerStatus.pmiExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfCallManagerStatus.pmiShowApplicationClick(Sender: TObject);
begin
  ShowApplication(0);
end;

procedure TfCallManagerStatus.pmiShowLogClick(Sender: TObject);
begin
  if Visible then
    Hide
  else
    Show;
  SetCaptions;
end;

initialization

finalization
  FreeAndNil(fCallManagerStatus);

end.
