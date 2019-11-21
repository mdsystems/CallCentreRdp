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
unit CallRouterMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ImgList, JvImageList, JvComponentBase, JvTrayIcon, Vcl.ExtCtrls, Vcl.Menus,
  Vcl.StdCtrls, JvExStdCtrls, JvMemo, Vcl.AppEvnts, JvAppInst, Contnrs, CallMessage, Rdp, ActiveCallPopup,
  SharedDefinitions, IdContext, IdGlobal, IdBaseComponent, IdComponent, IdCustomTCPServer,
  IdCustomHTTPServer, IdHTTPServer, IdServerIOHandler, IdSSL, IdSSLOpenSSL;

const
  cTesting = False;

type
  TOnCallMessageEvent = procedure (const msg: TCallMessage) of object;
  TCallManagerApp = class
  private
    FForm: TForm;
    FMessageQueue: TObjectList;
    FSetStatus: TOnStatusEvent;
    FManagerCommand: String;
    FProcessHandle: HWND;
    FRouteName: String;
    FFriendlyName: String;
    FRdpChannel: String;
    FNextSend: TDateTime;
    FRetries: Integer;
    FSpawnedAt: TDateTime;
    FSpawnedProcess: Integer;
    FForwardParameters: Boolean;
    FCallTimeoutSeconds: Integer;
    FShowSysTrayBalloon: Boolean;
    FShowPopupWindow: Boolean;
    FSpawnTimeoutSeconds: Integer;
    FSpawnAtStart: Boolean;
    FExcludeRouterParameters: Boolean;
    FCmdLineParams: TStrings;
    FRdpConnected: Boolean;
    FRdpHost: String;
    FManagerActive: Boolean;
    function SpawnParams: String;
    procedure BringRdpSessionToFront;
    procedure SpawnProcess;
    procedure DoSetStatus(const level: Integer; const status: String);
    procedure DoSendMessages;
    procedure SetSpawnAtStart(const Value: Boolean);
    procedure SetCmdLineParams(const Value: TStrings);
    procedure SetRdpChannel(const Value: String);
    procedure AllowSetForeground;
    procedure SetRdpHost(const Value: String);
  public
    constructor Create(const chanName, mgrCommand: String; const form: TForm; setStatus: TOnStatusEvent);
    destructor Destroy; override;
    procedure SendMessage(const msg: TCallMessage; params: TStrings);
    procedure SetHandle(const handle: HWND);
    procedure ClearHandle(const handle: HWND);
    property RouteName: String read FRouteName;
    property CmdLineParams: TStrings read FCmdLineParams write SetCmdLineParams;
    property ForwardParameters: Boolean read FForwardParameters write FForwardParameters;
    property ExcludeMyParameters: Boolean read FExcludeRouterParameters write FExcludeRouterParameters;
    property ShowSysTrayBalloon: Boolean read FShowSysTrayBalloon write FShowSysTrayBalloon;
    property ShowPopupWindow: Boolean read FShowPopupWindow write FShowPopupWindow;
    property CallTimeoutSeconds: Integer read FCallTimeoutSeconds write FCallTimeoutSeconds;
    property SpawnTimeoutSeconds: Integer read FSpawnTimeoutSeconds write FSpawnTimeoutSeconds;
    property SpawnAtStart: Boolean read FSpawnAtStart write SetSpawnAtStart;
    property FriendlyName: String read FFriendlyName write FFriendlyName;
    property RdpChannel: String read FRdpChannel write SetRdpChannel;
    property RdpHost: String read FRdpHost write SetRdpHost;
  end;
  TManagerSessionList = class
    FSessionList: TObjectList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(var cma: TCallManagerApp);
    function FindSession(const rName: String): TCallManagerApp;
    procedure SetManagerHandle(const rName: String; const rdpHandle: HWND);
    procedure ClearManagerHandle(const rName: String; const rdpHandle: HWND);
    procedure ClearRdpHandle(const rName: String; const procHandle: HWND);
    procedure ManagerActive(const rName: String; const procHandle: HWND);
    procedure ManagerInactive(const rName: String; const procHandle: HWND);
    procedure SetRdpConnected(const rName: String);
    procedure SetRdpDisconnected(const rName: String);
    procedure SendMessage(const rName: String; const msg: TCallMessage; const params: TStrings);
    function ManagerHandle(const rName: String): HWND;
    function Timeout(const rName: String): Integer;
  end;
  TfCallRouterMain = class(TForm)
    tiMain: TTrayIcon;
    pmMain: TPopupMenu;
    miPauseRouting: TMenuItem;
    miShowLog: TMenuItem;
    N1: TMenuItem;
    miExit: TMenuItem;
    mLog: TJvMemo;
    pmLog: TPopupMenu;
    miLogCopytoclipboard: TMenuItem;
    miLogClear: TMenuItem;
    aeMain: TApplicationEvents;
    aiMain: TJvAppInstances;
    idHTTP: TIdHTTPServer;
    idSSL: TIdServerIOHandlerSSLOpenSSL;
    procedure miPauseRoutingClick(Sender: TObject);
    procedure miShowLogClick(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure miLogCopytoclipboardClick(Sender: TObject);
    procedure miLogClearClick(Sender: TObject);
    procedure aeMainMinimize(Sender: TObject);
    procedure aiMainCmdLineReceived(Sender: TObject; CmdLine: TStrings);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure tiMainBalloonClick(Sender: TObject);
    procedure idHTTPCommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
  private
    FRouteMappings: TStringList;
    FCallManagers: TManagerSessionList;
    FRoutingPaused: Boolean;
    FLogLevel: Integer;
    FActiveCallPopup: TfActiveCallPopup;
    FHTTPImmediateCallRedirect: Boolean;
    FShuttingDown: Boolean;
    procedure SetRouteAndRDPSession(var routeName: String; var rdpSession: TCallManagerApp; const routeParam, ddiParam: String);
    procedure BuildCmdLineStrings(cmdLine: TStrings; const excludeMine: Boolean);
    procedure ProcessIniFile;
    procedure SetCaptions;
    procedure ProcessCmdLine(const cmdLine: TStrings);
    procedure SetLogLevel(const Value: Integer);
    procedure SetStatus(const level: Integer; const status: TStrings); overload;
    procedure SetStatus(const level: Integer; const status: String); overload;
    procedure HandleConnect(const routeName: String; const mgrHandle: HWND; var msg: TWMCopyData);
    procedure HandleConnectAccept(const routeName: String; const mgrHandle: HWND);
    procedure HandleConnectReject(const routeName: String; const mgrHandle: HWND);
    procedure HandleDisconnect(const routeName: String; const mgrHandle: HWND);
    procedure HandleDisconnectAccept(const routeName: String; const rdpHandle: HWND);
    procedure HandleCallResult(const routeName: String; const messageString: String);
    procedure HandleRdpConnected(const routeName: String; const mgrHandle: HWND);
    procedure HandleRdpDisconnected(const routeName: String; const mgrHandle: HWND);
    procedure HandleManagerActive(const routeName: String; const mgrHandle: HWND);
    procedure HandleManagerInactive(const routeName: String; const mgrHandle: HWND);
    procedure ApproveMessage(const callMsg: TCallMessage; routeName, ddi, cli: String; params: TStrings);
    procedure ProcessDLLRegistration;
  public
    procedure HandleManagerMessage(var msg: TWMCopyData); message WM_COPYDATA;
    procedure SelectCall(const callMsg: TCallMessage; const params: TStrings); overload;
    procedure MakeCall(const callMsg: TCallMessage; const params: TStrings); overload;
    property LogLevel: Integer read FLogLevel write SetLogLevel;
    procedure WMQueryEndSession(var Message: TWMQueryEndSession); message WM_QUERYENDSESSION;
    procedure WMEndSession(var Message: TWMEndSession); message WM_ENDSESSION;
  end;

var
  fCallRouterMain: TfCallRouterMain;

implementation

{$R *.dfm}

uses
  DateUtils, IniFiles, IdSocketHandle, Math, ShellApi, Shlobj, Registry, System.UITypes;

const
  cMaxLogLines = 1000;
  cSpawnDelay = 30; // seconds
  cStartRoutingCaption = 'Start Routing';
  cPauseRoutingCaption = 'Pause Routing';
  cHideLogCaption = 'Hide Log';
  cShowLogCaption = 'Show Log';
  cOtherSpawnError = 'Error returned when %d starting RDP session.';
  cNoRoutesSection = 'No routes specified in the .ini file.';
  cConfirmClose = 'Are you sure you want to close the call router? Incoming calls may not be routed correctly.';
  cInvalidMessageReceived = 'Invalid pipe message received.';
  cSentDataViaPipe = 'Piped %d bytes to the remote desktop call manager';
  cPipeError = 'Error %d ocurred on pipe to remote desktop call manager';
  cPipeDisconnected = 'Pipe to remote desktop call manager disconnected.';
  cPipeNotConnected = 'Connection to call manager using channel name %s could not be completed.';
  cRouteNameOrMappingRequired = 'The route name is a required parameter if no DDI mapping exists in the .ini file';
  cNoIniFile = 'Could not find the application ini file named ' + cIniFileName + ' in the application directory';
  cOutOfResources = 'The operating system is out of memory or resources.';
  cFileNotFound = 'The specified file was not found.';
  cPathNotFound = 'The specified path was not found.';
  cAccessDenied = 'Windows 95 only: The operating system denied access to the specified file.';
  cNotEnoughMemory = 'Windows 95 only: There was not enough memory to complete the operation.';
  cWrongWindowsVersion = 'Wrong Windows version.';
  cExeInvalid = 'The .EXE file is invalid (non-Win32 .EXE or error in .EXE image).';
  cDifferentOs = 'Application was designed for a different operating system.';
  cDesignedForMsDos = 'Application was designed for MS-DOS 4.0.';
  cRealModeProgram = 'Attempt to load a real-mode program.';
  cSecondInstance = 'Attempt to load a second instance of an application with non-readonly data segments.';
  cCompressedApp = 'Attempt to load a compressed application file.';
  cDllFailure = 'Dynamic-link library (DLL) file failure.';
  cSharingViolation = 'A sharing violation occurred.';
  cInvalidAssociation = 'The filename association is incomplete or invalid.';
  cDdeTimedOut = 'The DDE transaction could not be completed because the request timed out.';
  cDdeFailed = 'The DDE transaction failed.';
  cDdeBusy = 'The DDE transaction could not be completed because other DDE transactions were being processed.';
  cNoAssociation = 'There is no application associated with the given filename extension.';
  cDllNotFound = 'Windows 95 only: The specified dynamic-link library was not found.';
  cSpawnError = 'Unable to launch RDP command ''%s''';
  cHttpSuccess = 200;
  cHttpPageNotFound = 404;
  cPageNotFoundText = 'Call manager page ''%s'' not found.';
  cHttpRouteSuccessText = 'Call routed successfully. This page can be closed.';
  cHttpRouteFailureText = 'The call could not be routed successfully.';
  cHttpRouteSuccessFileName = 'CallRouteSuccess.html';
  cTerminalServicesKey = 'Software\\Microsoft\\Terminal Server Client\\Default\\AddIns\\CallManager';
  cRegistryDLLNameValue = 'Name';
  cTerminalServicesDLLName = 'CallManager.dll';
  cDefaultDDIRoute = 'DEFAULT';

function AllowSetForegroundWindow(dwProcessId: DWORD): BOOL; stdcall; external 'user32.dll';

function EnumLocateCallManager(hwnd: THandle; lParam: LPARAM): Boolean; stdcall;
var
  pClassName: Array[0..255] of char;
  msg: TCallMessage;
  rs: TCallManagerApp;
  cds: TCopyDataStruct;
  msgResult: DWORD_PTR;
begin
  result := true;
  if GetClassName(hwnd, pClassName, SizeOf(pClassName)) <> 0 then begin
    if StrComp(pClassName, 'TfCallManagerStatus') = 0 then begin
      rs := TCallManagerApp(lParam);
      PopulateMessage(msg, cmtConnect, rs.FForm.Handle, '', '', rs.FRouteName, '', '', '', '');
      cds.dwData := cCallManagerMessageId;
      cds.cbData := SizeOf(TCallMessage);
      cds.lpData := @msg;
      msgResult := 0;
      if SendMessageTimeout(hwnd, WM_COPYDATA, 0, NativeInt(@cds), cWaitSetting, cLocateTimeout, @msgResult) <> 0 then begin
        rs.FProcessHandle := msgResult;
        // Continue searching if router did not give its handle back (because we asked for the wrong route)
        result := msgResult = 0;
      end;
    end;
  end;
end;

procedure TfCallRouterMain.aeMainMinimize(Sender: TObject);
begin
  Hide;
  SetCaptions;
end;

procedure TfCallRouterMain.aiMainCmdLineReceived(Sender: TObject; CmdLine: TStrings);
begin
  CmdLine.Add(cClpHideSystrayIcon);
  ProcessCmdLine(CmdLine);
end;

procedure TfCallRouterMain.ApproveMessage(const callMsg: TCallMessage; routeName, ddi, cli: String; params: TStrings);
begin
  if Assigned(FActiveCallPopup) then begin
    tiMain.OnBalloonClick := tiMainBalloonClick;
    FActiveCallPopup.Add(callMsg, FCallManagers.Timeout(routeName), params);
    tiMain.BalloonTitle := 'Incoming call';
    if cli = '' then
      cli := 'unknown number';
    if ddi = '' then
      ddi := 'unknown DDI';
    tiMain.BalloonHint := Format('Call inbound from %s on route %s via %s', [cli, routeName, ddi]);
    tiMain.ShowBalloonHint;
    SetStatus(5, Format('Message added for approval. Call from %s on route %s via %s', [cli, routeName, ddi]));
  end;
end;

procedure TfCallRouterMain.BuildCmdLineStrings(cmdLine: TStrings; const excludeMine: Boolean);
var
  i: Integer;
begin
  for i := 1 to ParamCount do
    cmdLine.Add(ParamStr(i));
end;

procedure TfCallRouterMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  tiMain.Visible := False;
end;

procedure TfCallRouterMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if FShuttingDown then
    CanClose := True
  else
    CanClose := MessageDlg(cConfirmClose, mtConfirmation, mbYesNo, 0, mbNo) = mrYes;
end;

procedure TfCallRouterMain.FormCreate(Sender: TObject);
var
  cmdLine: TStringList;
begin
  FShuttingDown := False;
  FLogLevel := cMaxLogLevel;
  FCallManagers := TManagerSessionList.Create;
  FActiveCallPopup := TfActiveCallPopup.Create(Application);
  FActiveCallPopup.OnSelectCall := SelectCall;
  FActiveCallPopup.SetStatus := SetStatus;
  FRouteMappings := TStringList.Create;
  FRouteMappings.CaseSensitive := False;
  FRouteMappings.Duplicates := dupIgnore;
  FHTTPImmediateCallRedirect := true;
  ProcessIniFile;
  ProcessDLLRegistration;
  cmdLine := TStringList.Create;
  try
    BuildCmdLineStrings(cmdLine, false);
    // If the hide systray icon switch is present, assume that the
    // command line should not be processed, because it will be sent
    // to the currently running program instance.
    if cmdLine.IndexOf(cClpHideSystrayIcon) = -1 then begin
      tiMain.Visible := True;
      tiMain.ShowBalloonHint;
      ProcessCmdLine(cmdLine);
    end;
    SetStatus(5, Format('Router window handle is %d', [Handle]));
    SetStatus(5, Format('Application window handle is %d', [Application.Handle]));
  finally
    cmdLine.Free;
  end;
  DoubleBuffered := True;
end;

procedure TfCallRouterMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FRouteMappings);
  FreeAndNil(FCallManagers);
end;

procedure TfCallRouterMain.HandleCallResult(const routeName: String; const messageString: String);
var
  myHandle: HWnd;
begin
  try
    myHandle := FCallManagers.ManagerHandle(routeName);
    if IsWindow(myHandle) then begin
      SetStatus(10, 'Received call result');
      SetStatus(1, messageString);
    end else
      SetStatus(1, Format('Received unexpected call result for route %s', [routeName]));
  except on e: Exception do
    SetStatus(5, e.Message);
  end;
end;

procedure TfCallRouterMain.HandleConnectAccept(const routeName: String; const mgrHandle: HWND);
begin
  FCallManagers.SetManagerHandle(routeName, mgrHandle);
end;

procedure TfCallRouterMain.HandleConnectReject(const routeName: String; const mgrHandle: HWND);
begin
  FCallManagers.ClearManagerHandle(routeName, mgrHandle);
end;

procedure TfCallRouterMain.HandleConnect(const routeName: String; const mgrHandle: HWND; var msg: TWMCopyData);
var
  amsg: TCallMessage;
  cmdLine: TStringList;
begin
  try
    FCallManagers.SetManagerHandle(routeName, mgrHandle);
    SetStatus(5, 'Handle for rdp session ' + routeName + ' set to ' + IntToStr(mgrHandle));
    if IsWindow(mgrHandle) then begin
      PopulateMessage(amsg, cmtConnectAccept, Self.Handle, '', '', routeName, '', '', '', '');
      SetStatus(5, 'Sending connect accept to manager on route (' + routeName + ')');
      cmdLine := TStringList.Create;
      try
        BuildCmdLineStrings(cmdLine, false);
        FCallManagers.SendMessage(routeName, amsg, cmdLine);
      finally
        cmdLine.Free;
      end;
    end;
  except on e: Exception do
    SetStatus(5, e.Message);
  end;
end;

procedure TfCallRouterMain.HandleDisconnect(const routeName: String; const mgrHandle: HWND);
var
  amsg: TCallMessage;
  myHandle: HWND;
  cmdLine: TStringList;
begin
  try
    if IsWindow(mgrHandle) then begin
      myHandle := FCallManagers.ManagerHandle(routeName);
      if myHandle = mgrHandle then begin
        PopulateMessage(amsg, cmtDisconnectAccept, Self.Handle, '', '', routeName, '', '', '', '');
        SetStatus(5, 'Sending disconnect accept to manager on route ' + routeName);
        cmdLine := TStringList.Create;
        try
          BuildCmdLineStrings(cmdLine, false);
          FCallManagers.SendMessage(routeName, amsg, cmdLine);
          FCallManagers.ClearRdpHandle(routeName, mgrHandle);
        finally
          cmdLine.Free;
        end;
      end else
        SetStatus(5, Format('Rejecting disconnect on route (%s). Disconnecting handle is %d and my handle is %d', [routeName, mgrHandle, myHandle]));
    end else begin
      SetStatus(5, 'Unable to send disconnect accept to manager on route (' + routeName + ')');
      FCallManagers.ClearRdpHandle(routeName, 0);
    end;
  except on e: Exception do
    SetStatus(5, e.Message);
  end;
end;

procedure TfCallRouterMain.HandleDisconnectAccept(const routeName: String; const rdpHandle: HWND);
begin
  SetStatus(5, 'Disconnection request accepted for route (' + routeName + ')');
  try
    FCallManagers.ClearRdpHandle(routeName, rdpHandle);
    SetStatus(5, 'Handle for rdp session ' + routeName + ' cleared');
  except on e: Exception do
    SetStatus(5, e.Message);
  end;
end;

procedure TfCallRouterMain.HandleManagerActive(const routeName: String; const mgrHandle: HWND);
begin
  SetStatus(5, 'ManagerActive for route (' + routeName + ')');
  try
    FCallManagers.ManagerActive(routeName, mgrHandle);
  except on e: Exception do
    SetStatus(5, e.Message);
  end;
end;

procedure TfCallRouterMain.HandleManagerInactive(const routeName: String; const mgrHandle: HWND);
begin
  SetStatus(5, 'ManagerInactive for route (' + routeName + ')');
  try
    FCallManagers.ManagerInactive(routeName, mgrHandle);
  except on e: Exception do
    SetStatus(5, e.Message);
  end;
end;

procedure TfCallRouterMain.HandleManagerMessage(var msg: TWMCopyData);
var
  callMsg: TCallMessage;
  ddi: String;
  cli: String;
  routeName: String;
  operatorName: String;
  systemName: String;
  callId: String;
  messageString: String;
begin
  SetStatus(10, Format('Manager message received (%d/%d/%d)', [msg.CopyDataStruct.dwData, msg.CopyDataStruct.cbData, SizeOf(callMsg)]));
  if (msg.CopyDataStruct.dwData = cCallManagerMessageId) and (msg.CopyDataStruct.cbData = SizeOf(callMsg)) then begin
    Move(msg.CopyDataStruct.lpData^, callMsg, SizeOf(callMsg));
    CallMessage.DecomposeMessage(callMsg, ddi, cli, routeName, systemName, operatorName, callId, messageString);
    try
      if callMsg.msgType = cmtConnect then
        HandleConnect(routeName, callMsg.handle, msg)
      else if callMsg.msgType = cmtConnectAccept then
        HandleConnectAccept(routeName, callMsg.handle)
      else if callMsg.msgType = cmtDisconnect then
        HandleDisconnect(routeName, callMsg.handle)
      else if callMsg.msgType = cmtDisconnectAccept then
        HandleDisconnectAccept(routeName, callMsg.handle)
      else if callMsg.msgType = cmtCallResult then
        HandleCallResult(routeName, messageString)
      else if callMsg.msgType = cmtRdpConnected then
        HandleRdpConnected(routeName, callMsg.handle)
      else if callMsg.msgType = cmtRdpDisconnected then
        HandleRdpDisconnected(routeName, callMsg.handle)
      else if callMsg.msgType = cmtManagerActive then
        HandleManagerActive(routeName, callMsg.handle)
      else if callMsg.msgType = cmtManagerInactive then
        HandleManagerInactive(routeName, callMsg.handle)
      else if callMsg.msgType = cmtConnectReject then
        HandleConnectReject(routeName, callMsg.handle)
      else begin
        SetStatus(1, Format('Unexpected message %d received from call manager', [Integer(callMsg.msgType)]));
      end;
      msg.Result := Handle;
    except
      msg.Result := 0;
    end;
  end;
end;

procedure TfCallRouterMain.HandleRdpConnected(const routeName: String; const mgrHandle: HWND);
var
  myHandle: HWND;
begin
  try
    if IsWindow(mgrHandle) then begin
      myHandle := FCallManagers.ManagerHandle(routeName);
      if myHandle = mgrHandle then begin
        SetStatus(5, 'RdpConnected on route ' + routeName);
        FCallManagers.SetRdpConnected(routeName);
      end else
        raise Exception.Create(Format('Rejecting deactivation on route (%s). Deactivating handle is %d and my handle is %d', [routeName, mgrHandle, myHandle]));
    end else begin
      SetStatus(5, 'Manger lost on route (' + routeName + ')');
      FCallManagers.ClearRdpHandle(routeName, 0);
    end;
  except on e: Exception do
    SetStatus(5, e.Message);
  end;
end;

procedure TfCallRouterMain.HandleRdpDisconnected(const routeName: String; const mgrHandle: HWND);
var
  myHandle: HWND;
begin
  try
    if IsWindow(mgrHandle) then begin
      myHandle := FCallManagers.ManagerHandle(routeName);
      if myHandle = mgrHandle then begin
        SetStatus(5, Format('RdpDisconnected (%d) on route %s', [mgrHandle, routeName]));
        FCallManagers.SetRdpDisconnected(routeName);
      end else if myHandle <> 0 then
        SetStatus(5, Format('Rejecting disconnect on route (%s). Disconnecting handle is %d and my handle is %d', [routeName, mgrHandle, myHandle]));
    end else begin
      SetStatus(5, 'Manger lost on route (' + routeName + ')');
      FCallManagers.ClearRdpHandle(routeName, 0);
    end;
  except on e: Exception do
    SetStatus(5, e.Message);
  end;
end;

procedure TfCallRouterMain.idHTTPCommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo);
var
  callerId: String;
  callTracker: String;
  agentId: String;
  dialledNumber: String;
  agentNumber: String;
  routeName: String;
  systemName: String;
  responseStream: TFileStream;
  callMsg: TCallMessage;
  rdpSession: TCallManagerApp;
begin
  if Uppercase(ARequestInfo.Document) = cCallDocument then begin
    SetStatus(5, 'Call routed via HTTP server');
    callerId := ARequestInfo.Params.Values[cHttpCallerIdParam];
    callTracker := ARequestInfo.Params.Values[cHttpCallTrackerParam];
    agentId := ARequestInfo.Params.Values[cHttpAgentIdParam];
    dialledNumber := ARequestInfo.Params.Values[cHttpDialledNumberParam];
    agentNumber := ARequestInfo.Params.Values[cHttpAgentNumberParam];
    systemName := ARequestInfo.Params.Values[cHttpSystemNameParam];
    SetRouteAndRDPSession(routeName, rdpSession, ARequestInfo.Params.Values[cHttpRouteNameParam], dialledNumber);
    PopulateMessage(callMsg, cmtCallReceived, Self.Handle, dialledNumber, callerId, routeName, systemName, agentId, callTracker, '');
    if FHTTPImmediateCallRedirect then begin
      SetStatus(5, 'Immediate route of call');
      MakeCall(callMsg, ARequestInfo.Params)
    end else begin
      SetStatus(5, 'Route call via UI');
      ApproveMessage(callMsg, routeName, dialledNumber, callerId, ARequestInfo.Params);
    end;
    AResponseInfo.ResponseNo := cHttpSuccess;
    if FileExists(cHttpRouteSuccessFileName) then begin
      // Indy will free the content stream.
      responseStream := TFileStream.Create(cHttpRouteSuccessFileName, fmOpenRead + fmShareDenyNone);
      AResponseInfo.ContentStream := responseStream;
    end else
      AResponseInfo.ContentText := cHttpRouteSuccessText;
  end else begin
    AResponseInfo.ResponseNo := cHttpPageNotFound;
    AResponseInfo.ContentText := Format(cPageNotFoundText, [ARequestInfo.Document]);
  end;
end;

procedure TfCallRouterMain.miExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfCallRouterMain.miLogClearClick(Sender: TObject);
begin
  mLog.Clear;
end;

procedure TfCallRouterMain.miLogCopytoclipboardClick(Sender: TObject);
var
  oss: Integer;
begin
  oss := mLog.SelStart;
  mLog.SelectAll;
  try
    mLog.CopyToClipboard;
  finally
    mLog.SelLength := 0;
    mLog.SelStart := oss;
  end;
end;

procedure TfCallRouterMain.miPauseRoutingClick(Sender: TObject);
begin
  FRoutingPaused := not FRoutingPaused;
  SetCaptions;
end;

procedure TfCallRouterMain.miShowLogClick(Sender: TObject);
begin
  if Visible then
    Hide
  else begin
    Show;
    WindowState := wsNormal;
  end;
  SetCaptions;
end;

procedure TfCallRouterMain.ProcessCmdLine(const cmdLine: TStrings);
var
  ddi: String;
  cli: String;
  routeName: String;
  operatorName: String;
  systemName: String;
  logLevel: String;
  callId: String;
  messageString: String;
  callMsg: TCallMessage;
  rdpSession: TCallManagerApp;
begin
  SetStatus(5, cmdLine);
  ddi := cmdLine.Values[cClpDdiParamName];
  SetRouteAndRDPSession(routeName, rdpSession, cmdLine.Values[cClpRouteName], ddi);
  logLevel := cmdLine.Values[cClpLogLevel];
  if logLevel <> '' then begin
    try
      FLogLevel := Min(Max(StrToInt(logLevel), cMinLogLevel), cMaxLogLevel);
    finally
      FLogLevel := cMinLogLevel;
    end;
  end;
  if Assigned(rdpSession) then begin
    cli := cmdLine.Values[cClpCliParamName];
    operatorName := cmdLine.Values[cClpOperatorName];
    systemName := cmdLine.Values[cClpSystemName];
    messageString := cmdLine.Values[cClpMessageString];
    callId := cmdLine.Values[cClpCallId];
    CallMessage.populateMessage(callMsg, cmtCallReceived, Self.Handle, ddi, cli, routeName, systemName, operatorName, callId, messageString);
    ApproveMessage(callMsg, routeName, ddi, cli, cmdLine);
  end else if (routeName <> '') then
    SetStatus(1, Format('%s is an unknown route. Message not processed.', [routeName]));
end;

procedure TfCallRouterMain.ProcessDLLRegistration;
var
  registry: TRegistry;
  dllName: String;

  procedure WriteDllName;
  begin
    dllName := ExtractFilePath(Application.ExeName) + cTerminalServicesDLLName;
    if FileExists(dllName) then begin
      try
        registry.WriteString(cRegistryDLLNameValue, dllName);
        SetStatus(1, 'Registered remote desktop DLL ''' + dllName + '''');
      except
        SetStatus(1, 'Could not register remote desktop DLL ''' + dllName + '''');
      end;
    end else
      SetStatus(1, 'Could not locate remote desktop DLL ''' + dllName + '''for registration');
  end;

begin
  registry := TRegistry.Create;
  try
    registry.RootKey := HKEY_CURRENT_USER;
    if registry.KeyExists(cTerminalServicesKey) then begin
      try
        registry.OpenKey(cTerminalServicesKey, False);
        SetStatus(3, 'Opened remote desktop registry key');
        dllName := registry.ReadString(cRegistryDLLNameValue);
        if dllName = '' then
          WriteDLLName
        else begin
          if FileExists(dllName) then
            SetStatus(3, 'Remote desktop DLL already registered as ''' + dllName + '''')
          else
            SetStatus(3, 'Remote desktop DLL ''' + dllName + ''' could not be located.');
        end;
      except on e: Exception do
        SetStatus(1, e.Message);
      end;
    end else begin
      SetStatus(1, 'Attempt to create remote desktop registry key');
      if registry.CreateKey(cTerminalServicesKey) then begin
        try
          registry.OpenKey(cTerminalServicesKey, False);
          WriteDllName;
        except on e: Exception do
          SetStatus(1, e.Message);
        end;
      end else
        SetStatus(1, 'Could not create remote desktop registry key to register DLL');
    end;
  finally
    registry.Free;
  end;
end;

procedure TfCallRouterMain.ProcessIniFile;

  procedure AddHttpBindings(bindings: String);
  var
    slb: TStringList;
    slbp: TStringList;
    i: Integer;
    sh: TIdSocketHandle;
  begin
    idHTTP.Bindings.Clear;
    slb := TStringList.Create;
    try
      slb.Delimiter := ';';
      slb.StrictDelimiter := true;
      slb.DelimitedText := bindings;
      slbp := TStringList.Create;
      try
        slbp.StrictDelimiter := true;
        for i := 0 to slb.Count - 1 do begin
          slbp.CommaText := slb[i];
          sh := idHTTP.Bindings.Add;
          sh.IP := slbp.Values[cBindingIPValue];
          sh.Port := ParamToInt(slbp.Values[cBindingPortValue], 80);
          if UpperCase(slbp.Values[cBindingPortValue]) = UpperCase(cIpV6Value) then
            sh.IPVersion := Id_IPv6;
          SetStatus(5, Format('HTTP server bound to %s/%s', [slbp.Values[cBindingIPValue], slbp.Values[cBindingPortValue]]));
        end;
      finally
        slbp.Free;
      end;
    finally
      slb.Free;
    end;

  end;

var
  iFile: TIniFile;
  iniFileName: TFileName;
  sections: TStringList;
  routeInfo: TStringList;
  routes: TStringList;
  httpSettings: TStringList;
  rSession: TCallManagerApp;
  i: Integer;
begin
  iniFileName := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + cIniFileName;
  if FileExists(iniFileName) then begin
    iFile := TIniFile.Create(iniFileName);
    try
      sections := TStringList.Create;
      try
        routes := TStringList.Create;
        try
          routes.CaseSensitive := false;
          routes.Duplicates := dupError;
          iFile.ReadSections(sections);
          for i := 0 to sections.Count - 1 do begin
            if sections[i] = cDDIRoutesSectionName then begin
              iFile.ReadSectionValues(cDDIRoutesSectionName, FRouteMappings);
              FRouteMappings.Sort;
            end else if sections[i] = cHTTPServerSectionName then begin
              httpSettings := TStringList.Create;
              try
                iFile.ReadSectionValues(cHTTPServerSectionName, httpSettings);
                AddHttpBindings(httpSettings.Values[cHttpBindingsValue]);
                if ParamToBool(httpSettings.Values[cUseHTTPSValue], false) then begin
                  idSSL.SSLOptions.CertFile := httpSettings.Values[cCertFileValue];
                  idSSL.SSLOptions.CipherList := httpSettings.Values[cCipherListValue];
                  idSSL.SSLOptions.DHParamsFile := httpSettings.Values[cDHParamsFileValue];
                  idSSL.SSLOptions.KeyFile := httpSettings.Values[cKeyFileValue];
                  idSSL.SSLOptions.RootCertFile := httpSettings.Values[cRootCertFileValue];
                  idHTTP.IOHandler := idSSL;
                end;
                idHTTP.Active := ParamToBool(httpSettings.Values[cHTTPEnabledValue], false);
                if idHTTP.Active then
                  SetStatus(5, 'HTTP server active')
                else
                  SetStatus(5, 'HTTP server inactive');
                FHTTPImmediateCallRedirect := ParamToBool(httpSettings.Values[cHTTPImmediateCallRedirectValue], false);
              finally
                httpSettings.Free;
              end;
            end else begin
              try
                routes.Add(sections[i]);
                routeInfo := TStringList.Create;
                try
                  iFile.ReadSectionValues(sections[i], routeInfo);
                  rSession := TCallManagerApp.Create(sections[i], routeInfo.Values[cCmdLineValue], Self, SetStatus);
                  rSession.ForwardParameters := ParamToBool(routeInfo.Values[cForwardParametersValue], false);
                  rSession.ExcludeMyParameters := ParamToBool(routeInfo.Values[cExcludeRouterParametersValue], true);
                  rSession.ShowSysTrayBalloon := ParamToBool(routeInfo.Values[cShowSysTrayBalloonValue], true);
                  rSession.ShowPopupWindow := ParamToBool(routeInfo.Values[cShowPopupWindowValue], true);
                  rSession.CallTimeoutSeconds := ParamToInt(routeInfo.Values[cCallTimeoutSecondsValue], cDefaultCallTimeoutSeconds);
                  rSession.SpawnTimeoutSeconds := ParamToInt(routeInfo.Values[cSpawnTimeoutSecondsValue], cSpawnDelay);
                  rSession.SpawnAtStart := ParamToBool(routeInfo.Values[cSpawnAtStartValue], false);
                  rSession.FriendlyName := routeInfo.Values[cFriendlyNameValue];
                  rSession.RdpChannel := routeInfo.Values[cRDChannelValue];
                  rSession.RdpHost := routeInfo.Values[cRdHostValue];
                  FCallManagers.Add(rSession);
                finally
                  routeInfo.Free;
                end;
              except
                SetStatus(1, Format('Unable to add route %s. Is there a duplicate entry in the .ini file?', [sections[i]]));
              end;
            end;
          end;
          i := 0;
          while i < FRouteMappings.Count do begin
            if routes.IndexOf(FRouteMappings.ValueFromIndex[i]) >= 0 then begin
              SetStatus(5, Format('Added route mapping for route %s from DDI %s', [FRouteMappings.ValueFromIndex[i], FRouteMappings.Names[i]]));
              inc(i);
            end else begin
              SetStatus(5, Format('Unable to add mapping for non-existent route %s from DDI %s', [FRouteMappings.ValueFromIndex[i], FRouteMappings.Names[i]]));
              FRouteMappings.Delete(i);
            end;
          end;
        finally
          routes.Free;
        end;
      finally
        sections.Free;
      end;
    finally
      iFile.Free;
    end;
  end else
    raise Exception.Create(cNoIniFile);
end;

procedure TfCallRouterMain.SelectCall(const callMsg: TCallMessage; const params: TStrings);
begin
  if callMsg.identifier = cCallManagerMessageId then
    MakeCall(callMsg, params)
  else
    SetStatus(1, 'Call message timed out. Will not route. Click me quicker!');
end;

procedure TfCallRouterMain.MakeCall(const callMsg: TCallMessage; const params: TStrings);
var
  ddi: String;
  cli: String;
  routeName: String;
  operatorName: String;
  systemName: String;
  callId: String;
  messageString: String;
  cMgr: TCallManagerApp;
begin
  DecomposeMessage(callMsg, ddi, cli, routeName, systemName, operatorName, callId, messageString);
  cMgr := FCallManagers.FindSession(routeName);
  if Assigned(cMgr) then begin
    if cli = '' then
      cli := 'unknown number';
    if ddi = '' then
      ddi := 'unknown DDI';
    SetStatus(5, Format('Routing call %s from %s via %s route (%s)', [callId, cli, ddi, routeName]));
    cMgr.AllowSetForeground;
    cMgr.SendMessage(callMsg, params);
  end else
    SetStatus(1, Format('Cannot route message to call manager on route (%s)', [routeName]));
end;

procedure TfCallRouterMain.SetCaptions;
begin
  if FRoutingPaused then
    miPauseRouting.Caption := cStartRoutingCaption
  else
    miPauseRouting.Caption := cPauseRoutingCaption;
  if Visible then
    miShowLog.Caption := cHideLogCaption
  else
    miShowLog.Caption := cShowLogCaption
end;

procedure TfCallRouterMain.SetLogLevel(const Value: Integer);
begin
  FLogLevel := Value;
end;

procedure TfCallRouterMain.SetRouteAndRDPSession(var routeName: String; var rdpSession: TCallManagerApp; const routeParam, ddiParam: String);
var
  i: Integer;
begin
  routeName := UpperCase(routeParam);
  if routeName = '' then begin
    i := FRouteMappings.IndexOfName(ddiParam);
    if i >= 0 then begin
      routeName := FRouteMappings.ValueFromIndex[i];
      rdpSession := FCallManagers.FindSession(routeName);
    end else begin
      i := FRouteMappings.IndexOfName(cDefaultDDIRoute);
      if i >= 0 then begin
        routeName := FRouteMappings.ValueFromIndex[i];
        rdpSession := FCallManagers.FindSession(routeName);
      end else if ddiParam <> '' then
        SetStatus(1, cRouteNameOrMappingRequired);
      rdpSession := nil;
    end;
  end else
    rdpSession := FCallManagers.FindSession(routeName);
end;

procedure TfCallRouterMain.SetStatus(const level: Integer; const status: TStrings);
begin
  if level <= FLogLevel then
    mLog.Lines.AddStrings(status);
end;

procedure TfCallRouterMain.SetStatus(const level: Integer; const status: String);
begin
  if level <= FLogLevel then begin
    while mLog.Lines.Count >= cMaxLogLines do
      mLog.Lines.Delete(0);
    mLog.Lines.Add(status);
  end;
end;

procedure TfCallRouterMain.tiMainBalloonClick(Sender: TObject);
var
  msg: TCallMessage;
  params: TStringList;
begin
  if Assigned(FActiveCallPopup) and (FActiveCallPopup.Count > 0) then begin
    params := TStringList.Create;
    try
      tiMain.OnBalloonClick := nil;
      FActiveCallPopup.FetchLastCallInfo(msg, params);
      SelectCall(msg, params);
    finally
      params.Free;
    end;
  end;
end;

procedure TfCallRouterMain.WMEndSession(var Message: TWMEndSession);
begin
  FShuttingDown := True;
  inherited;
end;

procedure TfCallRouterMain.WMQueryEndSession(var Message: TWMQueryEndSession);
begin
  FShuttingDown := True;
  inherited;
end;

{ TRdpSession }

procedure TCallManagerApp.ClearHandle(const handle: HWND);
begin
  if FProcessHandle = handle then begin
    DoSetStatus(10, Format('Clearing RDP handle %d', [handle]));
    FProcessHandle := handle;
    FManagerActive := False;
  end else
    DoSetStatus(10, Format('Ignoring clear handle request %d', [handle]));
end;

constructor TCallManagerApp.Create(const chanName, mgrCommand: String; const form: TForm; setStatus: TOnStatusEvent);
var
  mgrCmd: String;

  function PublicDocumentsFolder(): String;
  var
    path: array[0 .. Max_Path] of Char;
  begin
    if ShGetSpecialFolderPath(0, path, CSIDL_COMMON_DOCUMENTS, False) then
      result := path
    else
      result := '';
  end;

begin
  inherited Create;
  FRdpConnected := False;
  FMessageQueue := TObjectList.Create;
  FMessageQueue.OwnsObjects := True;
  FCmdLineParams := TStringList.Create;
  FRouteName := chanName;
  FSpawnedProcess := 0;
  if FileExists(mgrCommand) then
    FManagerCommand := mgrCommand
  else begin
    mgrCmd := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + ExtractFileName(mgrCommand);
    if FileExists(mgrCmd) then
      FManagerCommand := mgrCmd
    else begin
      mgrCmd := IncludeTrailingPathDelimiter(GetCurrentDir) + ExtractFileName(mgrCommand);
      if FileExists(mgrCmd) then
        FManagerCommand := mgrCmd
      else begin
        mgrCmd := PublicDocumentsFolder();
        if mgrCmd <> '' then begin
          mgrCmd := IncludeTrailingPathDelimiter(mgrCmd) + ExtractFileName(mgrCommand);
          if FileExists(mgrCmd) then
            FManagerCommand := mgrCmd
          else
            FManagerCommand := '';
        end else
          FManagerCommand := '';
      end;
    end;
  end;
  FSetStatus := setStatus;
  FForm := form;
  FNextSend := Now;
  FRetries := 0;
  FSpawnedAt := 0;
end;

destructor TCallManagerApp.Destroy;
begin
  FreeAndNil(FCmdLineParams);
  FreeAndNil(FMessageQueue);
  inherited;
end;

procedure TCallManagerApp.DoSendMessages;
var
  cds: TCopyDataStruct;
  msgResult: DWORD_PTR;
  cmsg: TCallMessage;
  callMsgFound: Boolean;
begin
  callMsgFound := False;
  while (FProcessHandle <> 0) and (FMessageQueue.Count > 0) and (Now > FNextSend) and (not callMsgFound) do begin
    if IsWindow(FProcessHandle) then begin
      if FRetries < cMaxRetries then begin
        DoSetStatus(10, Format('Routing message to manager handle %d', [FProcessHandle]));
        cds.dwData := cCallManagerMessageId;
        cds.cbData := SizeOf(TCallMessage);
        TMessageQueueEntry(FMessageQueue[0]).RouteName := FRouteName;
        cmsg := TMessageQueueEntry(FMessageQueue[0]).CallMessage;
        if (cMsg.msgType in cConnectionManagementMessages) or (FRdpConnected and FManagerActive) then begin
          cds.lpData := @cmsg;
          msgResult := 0;
          if SendMessageTimeout(FProcessHandle, WM_COPYDATA, 0, NativeInt(@cds), cWaitSetting, cConnectTimeout, @msgResult) <> 0 then begin
            FRetries := 0;
            if FMessageQueue.Count > 0 then
              FMessageQueue.Delete(0);
            DoSetStatus(5, 'Send successful. Removed message from queue');
            if cmsg.msgType = cmtCallReceived then
              BringRdpSessionToFront;
          end else begin
            DoSetStatus(1, Format('Send failed with message code %d', [GetLastError]));
            FNextSend := IncSecond(Now, cTimeoutResendDelay);
            inc(FRetries);
          end;
        end else
          callMsgFound := True;
      end else begin
        DoSetStatus(5, 'Maximum send retries reached. Clearing manager handle.');
        FRetries := 0;
        FProcessHandle := 0;
        FManagerActive := False;
      end;
    end else begin
      DoSetStatus(1, 'No router on original handle. Clearing manager handle');
      FProcessHandle := 0;
      FRetries := 0;
      FManagerActive := False;
    end;
  end;
end;

procedure TCallManagerApp.DoSetStatus(const level: Integer; const status: String);
begin
  if Assigned(FSetStatus) then
    FSetStatus(level, status);
end;

procedure TCallManagerApp.AllowSetForeground;
begin
  if FSpawnedProcess <> 0 then begin
    if AllowSetForegroundWindow(FSpawnedProcess) then
      DoSetStatus(5, 'Authorised setting foreground window')
    else
      DoSetStatus(5, Format('Unable to authorise setting foreground window (%d)', [GetLastError]));
  end else
    DoSetStatus(5, Format('Unable to authorise setting foreground window when process id is unknown (%d)', [GetLastError]));
end;

procedure TCallManagerApp.BringRdpSessionToFront;
var
  msg: TCallMessage;
begin
  AllowSetForeground;
  DoSetStatus(5, 'Bringing rdp session to front');
  FillChar(msg, SizeOf(msg), 0);
  msg.msgType := cmtBringToFront;
  FMessageQueue.Add(TMessageQueueEntry.Create(msg))
end;

procedure TCallManagerApp.SendMessage(const msg: TCallMessage; params: TStrings);

  procedure RemoveDuplicateMessages(const mTypes: TCallMessageTypes);
  var
    i: Integer;
  begin
    i := 0;
    while i < FMessageQueue.Count do begin
      if TMessageQueueEntry(FMessageQueue[i]).CallMessage.msgType in mTypes then
        FMessageQueue.Delete(i)
      else
        inc(i);
    end;
  end;

begin
  // Connection management is always moved to the front of the message queue and any existing similar messages are
  // removed. Call routing messages cannot be duplicated (makes no sense to have thousands of popping windows - there
  // will only be one call at a time being managed.)
  if msg.msgType in cConnectionManagementMessages then begin
    RemoveDuplicateMessages(cConnectionManagementMessages);
    FMessageQueue.Insert(0, TMessageQueueEntry.Create(msg))
  end else begin
    if msg.msgType in cCallRoutingMessages then
      RemoveDuplicateMessages(cCallRoutingMessages);
    FMessageQueue.Add(TMessageQueueEntry.Create(msg));
  end;
  if IsWindow(FProcessHandle) then begin
    DoSetStatus(10, 'Manager known. Start sending');
    DoSendMessages
  end else begin
    DoSetStatus(10, 'Searching for manager');
    FProcessHandle := 0;
    EnumWindows(@EnumLocateCallManager, NativeInt(Self));
    if IsWindow(FProcessHandle) then begin
      DoSetStatus(10, 'Found manager. Start sending');
      AllowSetForeground;
      DoSendMessages;
    end else begin
      DoSetStatus(10, 'Manager not found. Spawn new one');
      if FForwardParameters then
        CmdLineParams := params
      else
        CmdLineParams.Clear;
      SpawnProcess;
    end;
  end;
end;

procedure TCallManagerApp.SetCmdLineParams(const Value: TStrings);
begin
  FCmdLineParams.Assign(Value);
end;

procedure TCallManagerApp.SetHandle(const handle: HWND);
begin
  DoSetStatus(10, Format('Setting RDP handle %d', [handle]));
  if IsWindow(handle) then begin
    DoSetStatus(10, 'Valid handle');
    if (handle <> FProcessHandle) then begin
      if IsWindow(FProcessHandle) then
         raise Exception.CreateFmt('Rejecting new connection for handle %d as my existing one %d is still valid.', [handle, FProcessHandle])
       else begin
         FProcessHandle := handle;
         FManagerActive := False;
       end;
    end;
  end else begin
    if handle <> 0 then
      DoSetStatus(10, Format('Invalid handle %d', [handle]));
    FProcessHandle := 0;
    FManagerActive := False;
  end;
end;

procedure TCallManagerApp.SetRdpChannel(const Value: String);
begin
  // TO DO - Need any setup here?
  FRdpChannel := Value;
end;

procedure TCallManagerApp.SetRdpHost(const Value: String);
begin
  // TO DO - Need any setup here?
  FRdpHost := Value;
end;

procedure TCallManagerApp.SetSpawnAtStart(const Value: Boolean);
begin
  if FSpawnAtStart <> Value then begin
    FSpawnAtStart := Value;
    if FSpawnAtStart then
      SpawnProcess;
  end;
end;

function TCallManagerApp.SpawnParams: String;
var
  i: Integer;
begin
  result := '';
  if FForwardParameters then begin
    for i := 0 to FCmdLineParams.Count - 1 do begin
      if not (((FCmdLineParams.Names[i] = cClpHideSystrayIcon) or
          (FCmdLineParams.Names[i] = cClpDdiParamName) or
          (FCmdLineParams.Names[i] = cClpCliParamName) or
          (FCmdLineParams.Names[i] = cClpRouteName) or
          (FCmdLineParams.Names[i] = cClpOperatorName) or
          (FCmdLineParams.Names[i] = cClpSystemName) or
          (FCmdLineParams.Names[i] = cClpMessageString)) and FExcludeRouterParameters) then begin
        if result <> '' then
          result := result + ' ';
        result := result + FCmdLineParams[i];
      end;
    end;
  end;
end;

procedure TCallManagerApp.SpawnProcess;
var
  er: Integer;
  clp: String;
begin
  if IsWindow(FProcessHandle) then begin
    DoSetStatus(10, Format('RDP session already active with handle %d', [FProcessHandle]));
    AllowSetForeground;
  end else if Now > IncSecond(FSpawnedAt, FSpawnTimeoutSeconds) then begin
    if FProcessHandle <> 0 then begin
      DoSetStatus(10, Format('RDP window handle %d invalid. Resetting.', [FProcessHandle]));
      FProcessHandle := 0;
      FSpawnedProcess := 0;
      FRdpConnected := False;
      FManagerActive := False;
    end;
    if FManagerCommand <> '' then begin
      DoSetStatus(10, Format('Spawning remote desktop command ', [FManagerCommand]));
      clp := SpawnParams;
      SetEnvironmentVariableA(cRouteHandledEnvVar, PAnsiChar(AnsiString(FRouteName)));
      er := ShellExecute(FProcessHandle, 'open', PWideChar(FManagerCommand), PWideChar(clp), '', SW_SHOWNORMAL);
      if er < 33 then begin
        DoSetStatus(1, Format(cSpawnError, [FManagerCommand]));
        case er of
          0: DoSetStatus(1, cOutOfResources);
          2: DoSetStatus(1, cFileNotFound);
          3: DoSetStatus(1, cPathNotFound);
          5: DoSetStatus(1, cAccessDenied);
          8: DoSetStatus(1, cNotEnoughMemory);
          10: DoSetStatus(1, cWrongWindowsVersion);
          11: DoSetStatus(1, cExeInvalid);
          12: DoSetStatus(1, cDifferentOs);
          13: DoSetStatus(1, cDesignedForMsDos);
          15: DoSetStatus(1, cRealModeProgram);
          16: DoSetStatus(1, cSecondInstance);
          19: DoSetStatus(1, cCompressedApp);
          20: DoSetStatus(1, cDllFailure);
          26: DoSetStatus(1, cSharingViolation);
          27: DoSetStatus(1, cInvalidAssociation);
          28: DoSetStatus(1, cDdeTimedOut);
          29: DoSetStatus(1, cDdeFailed);
          30: DoSetStatus(1, cDdeBusy);
          31: DoSetStatus(1, cNoAssociation);
          32: DoSetStatus(1, cDllNotFound);
        else
          DoSetStatus(1, Format(cOtherSpawnError, [er]));
        end;
      end else begin
        FSpawnedProcess := er;
        FSpawnedAt := Now;
        DoSetStatus(10, Format('Spawned ', [FManagerCommand]));
        DoSendMessages;
      end;
    end else
      DoSetStatus(1, 'No application specified. Cannot spawn process.');
  end else
    DoSetStatus(10, 'Not spawning new application yet - another may be running shortly');
end;

{ TRdpSessionList }

procedure TManagerSessionList.Add(var cma: TCallManagerApp);
begin
  FSessionList.Add(cma);
end;

procedure TManagerSessionList.ClearManagerHandle(const rName: String; const rdpHandle: HWND);
var
  cma: TCallManagerApp;
begin
  cma := FindSession(rName);
  if Assigned(cma) then
    cma.ClearHandle(rdpHandle)
  else
    raise Exception.Create('Unable to locate rdp session (' + rName + ')');
end;

procedure TManagerSessionList.ClearRdpHandle(const rName: String; const procHandle: HWND);
var
  rdps: TCallManagerApp;
begin
  rdps := FindSession(rName);
  if Assigned(rdps) then begin
    if procHandle <> rdps.FProcessHandle then
      raise Exception.Create('Attempt to clear handle failed for route (' + rName + ')')
    else begin
      rdps.DoSetStatus(5, Format('Clearing rdp handle for route %s.', [rName]));
      rdps.SetHandle(0);
      rdps.FRdpConnected := False;
      rdps.FProcessHandle := 0;
      rdps.FManagerActive := False;
    end;
  end else
    raise Exception.Create('Unable to locate rdp session (' + rName + ')');
end;

constructor TManagerSessionList.Create;
begin
  inherited Create;
  FSessionList := TObjectList.Create;
  FSessionList.OwnsObjects := True;
end;

destructor TManagerSessionList.Destroy;
begin
  FreeAndNil(FSessionList);
  inherited;
end;

function TManagerSessionList.FindSession(const rName: String): TCallManagerApp;
var
  i: Integer;
begin
  i := 0;
  result := nil;
  while (i < FSessionList.Count) and (result = nil) do begin
    if Uppercase(TCallManagerApp(FSessionList[i]).RouteName) = Uppercase(rName) then
      result := TCallManagerApp(FSessionList[i])
    else
      inc(i);
  end;
end;

procedure TManagerSessionList.ManagerActive(const rName: String; const procHandle: HWND);
var
  rdps: TCallManagerApp;
  msg: TCallMessage;
  mqe: TMessageQueueEntry;
begin
  rdps := FindSession(rName);
  if Assigned(rdps) then begin
    if (procHandle <> 0) and (procHandle <> rdps.FProcessHandle) then
      raise Exception.Create('Attempt to set manager active failed for route (' + rName + ')')
    else begin
      rdps.DoSetStatus(5, Format('Set manager active for route %s.', [rName]));
      rdps.FManagerActive := True;
      FillChar(msg, SizeOf(msg), 0);
      msg.msgType := cmtManagerAccept;
      mqe := TMessageQueueEntry.Create(msg);
      mqe.RouteName := rName;
      rdps.DoSetStatus(8, Format('Acknowlege manager active for route %s.', [rName]));
      rdps.FMessageQueue.Add(mqe);
      rdps.DoSetStatus(10, Format('Send pending messages for route %s.', [rName]));
      rdps.DoSendMessages;
    end;
  end else
    raise Exception.Create('Unable to locate rdp session (' + rName + ')');
end;

function TManagerSessionList.ManagerHandle(const rName: String): HWND;
var
  cma: TCallManagerApp;
begin
  cma := FindSession(rName);
  if Assigned(cma) then
    result := cma.FProcessHandle
  else
    raise Exception.Create('Unable to locate rdp session (' + rName + ')');
end;

procedure TManagerSessionList.ManagerInactive(const rName: String; const procHandle: HWND);
var
  rdps: TCallManagerApp;
begin
  rdps := FindSession(rName);
  if Assigned(rdps) then begin
    if (procHandle <> 0) and (procHandle <> rdps.FProcessHandle) then
      raise Exception.Create('Attempt to set manager inactive failed for route (' + rName + ')')
    else begin
      rdps.DoSetStatus(5, Format('Set manager inactive for route %s.', [rName]));
      rdps.FManagerActive := False;
    end;
  end else
    raise Exception.Create('Unable to locate rdp session (' + rName + ')');
end;

procedure TManagerSessionList.SendMessage(const rName: String; const msg: TCallMessage; const params: TStrings);
var
  cma: TCallManagerApp;
begin
  cma := FindSession(rName);
  if Assigned(cma) then
    cma.SendMessage(msg, params)
  else
    raise Exception.Create('Unable to locate rdp session (' + rName + ')');
end;

procedure TManagerSessionList.SetManagerHandle(const rName: String; const rdpHandle: HWND);
var
  cma: TCallManagerApp;
begin
  cma := FindSession(rName);
  if Assigned(cma) then
    cma.SetHandle(rdpHandle)
  else
    raise Exception.Create('Unable to locate rdp session (' + rName + ')');
end;

procedure TManagerSessionList.SetRdpConnected(const rName: String);
var
  cma: TCallManagerApp;
begin
  cma := FindSession(rName);
  if Assigned(cma) then begin
    cma.FRdpConnected := True;
    cma.DoSendMessages;
  end else
    raise Exception.Create('Unable to locate rdp session (' + rName + ')');
end;

procedure TManagerSessionList.SetRdpDisconnected(const rName: String);
var
  cma: TCallManagerApp;
begin
  cma := FindSession(rName);
  if Assigned(cma) then begin
    cma.FRdpConnected := False;
    cma.FProcessHandle := 0;
    cma.FManagerActive := False;
  end else
    raise Exception.Create('Unable to locate rdp session (' + rName + ')');
end;

function TManagerSessionList.Timeout(const rName: String): Integer;
var
  cma: TCallManagerApp;
begin
  cma := FindSession(rName);
  if Assigned(cma) then
    result := cma.CallTimeoutSeconds
  else
    Result := cDefaultCallTimeoutSeconds;
end;

end.
