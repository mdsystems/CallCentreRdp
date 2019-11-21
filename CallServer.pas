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
unit CallServer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, CallMessage, Rdp,
  Contnrs, SharedDefinitions, VirtualChannel;

type
  PHandle = ^THandle;
  TReadThread = class;
  TWriteThread = class;
  TRoutedCallEvent = procedure (const ddi, cli, systemName, operatorName, callId, messageString, otherParams: String) of object;
  TfCallServer = class(TForm)
    mStatus: TMemo;
    procedure bWriteClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure bNotepadClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FReadThread: TReadThread;
    FWriteThread: TWriteThread;
    FChannel: THandle;
    FConnected: Boolean;
    FLogLevel: Integer;
    FPacket: TRdpPacket;
    FRouteName: String;
    FRdpChannelName: TRdpChannelName;
    FRoutingEnabled: Boolean;
    FOnRoutedCall: TRoutedCallEvent;
    FLoggingEnabled: Boolean;
    function CurrentLogFileName: String;
    procedure SetLogLevel(const Value: Integer);
    procedure CloseVirtualChannel;
    procedure OnPacketRead;
    procedure SetRouteName(const Value: String);
    procedure RouteCall(const msg: TCallMessage);
    procedure SetConnected(const Value: Boolean);
    function GetRdpChannelName: String;
    procedure SetRdpChannelName(const Value: String);
    procedure SetLoggingEnabled(const Value: Boolean);
  public
    procedure SetStatus(const level: Integer; const s: String);
    procedure Connect;
    procedure Disconnect;
    procedure WaitForDisconnect;
    procedure SendMessage(const msg: TCallMessage);
    property LogLevel: Integer read FLogLevel write SetLogLevel;
    property LoggingEnabled: Boolean read FLoggingEnabled write SetLoggingEnabled;
    property RouteName: String read FRouteName write SetRouteName;
    property RdpChannelName: String read GetRdpChannelName write SetRdpChannelName;
    property Connected: Boolean read FConnected write SetConnected;
    property RoutingEnabled: Boolean read FRoutingEnabled;
    property OnRoutedCall: TRoutedCallEvent read FOnRoutedCall write FOnRoutedCall;
  end;
  TPacketObject = class
    FRdpPacket: TRdpPacket;
  public
    constructor Create(const pkt: TRdpPacket);
    property Packet: TRdpPacket read FRdpPacket;
  end;
  TPacketThread = class (TThread)
  private
    FActive: Boolean;
    FBuffer: TBytes;
    FStatus: String;
    FLevel: Integer;
    function GetPacketsWaiting: Boolean;
    function GetCurrentPacket: TRdpPacket;
  protected
    FError: Integer;
    FPackets: TObjectList;
    FForm: TfCallServer;
    FChannel: PHandle;
    FOnStatus: TOnStatusEvent;
    procedure DoOnStatus;
  public
    constructor Create(form: TfCallServer; channel: PHandle; onStatus: TOnStatusEvent);
    destructor Destroy; override;
    procedure Execute; override;
    procedure DoProcessing(var bufferActive: Boolean); virtual; abstract;
    procedure SetStatus(const level: Integer; const status: String);
    procedure Purge;
    property Error: Integer read FError;
    property PacketsWaiting: Boolean read GetPacketsWaiting;
    property CurrentPacket: TRdpPacket read GetCurrentPacket;
    property Active: Boolean read FActive;
  end;
  TOnPacketReadEvent = procedure of object;
  TReadThread = class (TPacketThread)
  private
    FBufferLen: DWORD;
    FBufferOffset: DWORD;
    FOnPacketRead: TOnPacketReadEvent;
    procedure DoOnPacketRead;
  public
    constructor Create(form: TfCallServer; channel: PHandle; onStatus: TOnStatusEvent; onPacketRead: TOnPacketReadEvent);
    function ReadPacket: TRdpPacket;
    procedure DoProcessing(var bufferActive: Boolean); override;
  end;
  TWriteThread = class (TPacketThread)
    procedure SendPacket(const pkt: TRdpPacket);
    procedure DoProcessing(var bufferActive: Boolean); override;
  end;

implementation

uses
  DateUtils, ShellAPI;

{$R *.dfm}

const
  cMaxLogLines = 10240;
  cRouteNameTooLong = 'The route name must be %d characters or less';
  cLogFolderName = 'Logs';
  cLogFileExtension = '.log';
  cDisconnectTimeout = 3;
  cDefaultLogLevel = 10;

var
  gCSection: TRTLCriticalSection;

function WTSVirtualChannelOpenEx(sessionId: DWORD; pVirtualName: PAnsiChar; flags: DWORD): THandle; stdcall; external 'WtsApi32.dll';
function WTSVirtualChannelClose(hChannelHandle: THandle): Boolean; stdcall; external 'WtsApi32.dll';
function WTSVirtualChannelRead(hChannelHandle: THandle; timeOut: ULONG; buffer: PByte; bufferSize: ULONG; var pBytesRead: ULONG): Boolean; stdcall; external 'WtsApi32.dll';
function WTSVirtualChannelWrite(hChannelHandle: THandle; buffer: PByte; length: ULONG; pUserData: PBytes): Boolean; stdcall; external 'WtsApi32.dll';
function WTSVirtualChannelPurgeInput(hChannelHandle: THandle): Boolean; stdcall; external 'WtsApi32.dll';
function WTSVirtualChannelPurgeOutput(hChannelHandle: THandle): Boolean; stdcall; external 'WtsApi32.dll';

procedure TfCallServer.CloseVirtualChannel;
var
  err: Integer;
begin
  if WTSVirtualChannelClose(FChannel) then begin
    FConnected := False;
    FWriteThread.Purge;
    FReadThread.Purge;
    FReadThread.Terminate;
    FWriteThread.Terminate;
    while FReadThread.Active or FWriteThread.Active do
      Application.ProcessMessages;
    SetStatus(1, 'Channel disconnected');
  end else begin
    err := GetLastError;
    SetStatus(1, Format('Channel disconnect failed! Error = %d.', [err]));
  end;
end;

procedure TfCallServer.Connect;
var
  err: Integer;
  pkt: TRdpPacket;
begin
  if FConnected then
    SetStatus(1, 'Connection requested when already requested')
  else begin
    if (FRouteName <> '') and (FRdpChannelName[0] <> #0) then begin
      FChannel := WTSVirtualChannelOpenEx(WTS_CURRENT_SESSION, FRdpChannelName, 0);
      if (FChannel = 0) then begin
        err := GetLastError;
        SetStatus(1, Format('Channel connect failed! Error = %d.', [err]));
      end else begin
        FConnected := True;
        SetStatus(5, 'Channel connected. Attempting to start routing');
        pkt.Clear;
        PopulateMessage(pkt.msg, cmtManagerActive, Handle, '', '', FRouteName, '', '', '', '');
        FWriteThread.SendPacket(pkt);
      end
    end else
      SetStatus(1, 'Channel connect failed because no route or RDP channel is set');
  end;
end;

function TfCallServer.CurrentLogFileName: String;
var
  logDir: String;
begin
  logDir := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + cLogFolderName;
  if ForceDirectories(logDir) then
    result := IncludeTrailingPathDelimiter(logDir) + FormatDateTime('YYYYMMDDHHMI', Now) + cLogFileExtension
  else
    result := FormatDateTime('YYYYMMDDHHMI', Now) + cLogFileExtension;
end;

procedure TfCallServer.Disconnect;
var
  pkt: TRdpPacket;
begin
  if FConnected then begin
    if FRoutingEnabled then begin
      pkt.Clear;
      PopulateMessage(pkt.msg, cmtManagerInactive, Handle, '', '', FRouteName, '', '', '', '');
      FWriteThread.SendPacket(pkt);
    end else
      CloseVirtualChannel;
  end;
end;

procedure TfCallServer.FormCreate(Sender: TObject);
begin
  FPacket.Clear;
  FReadThread := TReadThread.Create(Self, @FChannel, SetStatus, OnPacketRead);
  FWriteThread := TWriteThread.Create(Self, @FChannel, SetStatus);
  FChannel := 0;
  FConnected := false;
  FLogLevel := cDefaultLogLevel;
  FRouteName := '';
  FRoutingEnabled := false;
  FOnRoutedCall := nil;
end;

procedure TfCallServer.FormDestroy(Sender: TObject);
begin
  WaitForDisconnect;
  FreeAndNil(FReadThread);
  FreeAndNil(FWriteThread);
end;

function TfCallServer.GetRdpChannelName: String;
begin
  SharedDefinitions.SetStr(result, FRdpChannelName);
end;

procedure TfCallServer.OnPacketRead;
var
  packet: TRdpPacket;
begin
  SetStatus(5, 'Processing inbound packet');
  packet.Copy(FReadThread.CurrentPacket);
  if packet.msg.msgType = cmtDisconnectAccept then begin
    SetStatus(1, 'Disconnect accept received');
    CloseVirtualChannel;
  end else if packet.msg.msgType = cmtConnectAccept then begin
    SetStatus(1, 'Connect accept received. Routing enabled');
    FRoutingEnabled := True;
  end else if packet.msg.msgType = cmtManagerAccept then begin
    SetStatus(1, 'Manager accept received. Routing enabled');
    FRoutingEnabled := True;
  end else if packet.msg.msgType = cmtCallReceived then begin
    SetStatus(1, 'Call received. Routing call.');
    FRoutingEnabled := True;
    RouteCall(packet.msg)
  end else if packet.msg.msgType = cmtCallResult then begin
    SetStatus(1, 'Unexpected call result message received');
    FRoutingEnabled := True;
  end;
  // If the packet is not a call result, then send a response to indicate completion.
  if packet.msg.msgType <> cmtCallResult then begin
    SetStatus(5, 'Sending response');
    packet.Clear;
    PopulateMessage(packet.msg, cmtCallResult, Handle, '', '', routeName, '', '', '', 'Processed message');
    FWriteThread.SendPacket(packet);
  end;
end;

procedure TfCallServer.RouteCall(const msg: TCallMessage);
var
  ddi: String;
  cli: String;
  routeName: String;
  systemName: String;
  operatorName: String;
  callId: String;
  messageString: String;
begin
  DecomposeMessage(msg, ddi, cli, routeName, systemName, operatorName, callId, messageString);
  if routeName = FRouteName then begin
    SetStatus(1, Format('Routing call from ddi ''%s'' with cli ''%s''', [ddi, cli]));
    if Assigned(FOnRoutedCall) then
      FOnRoutedCall(ddi, cli, systemName, operatorName, callId, messageString, '')
    else
      SetStatus(5, 'No routing occurred as routing method not set');
  end else
    SetStatus(1, 'Route for call does not match my route');
end;

procedure TfCallServer.SetRdpChannelName(const Value: String);
begin
  SharedDefinitions.SetArray(FRdpChannelName, Value, SizeOf(FRdpChannelName));
  SetStatus(1, Format('RDP channel name ''%s'' set', [Value]));
end;

procedure TfCallServer.SetRouteName(const Value: String);
var
  oldConnected: Boolean;
begin
  if FRouteName <> UpperCase(Value) then begin
    if Length(Value) > cRouteNameLen then
      raise Exception.Create(Format(cRouteNameTooLong, [cRouteNameLen]))
    else begin
      oldConnected := FConnected;
      if FConnected then
        Disconnect;
      FRouteName := UpperCase(Value);
      SetStatus(1, Format('Route name ''%s'' set', [Value]));
      if oldConnected then
        Connect;
    end;
  end;
end;

procedure TfCallServer.SendMessage(const msg: TCallMessage);
var
  packet: TRdpPacket;
begin
  packet.Clear;
  packet.msg := msg;
  FWriteThread.SendPacket(packet);
end;

procedure TfCallServer.SetConnected(const Value: Boolean);
begin
  FConnected := Value;
end;

procedure TfCallServer.SetLoggingEnabled(const Value: Boolean);
begin
  if FLoggingEnabled <> Value then begin
    if FLoggingEnabled then
      FLogLevel := cMinLogLevel - 1
    else
      FLogLevel := cDefaultLogLevel;
    FLoggingEnabled := Value;
  end;
end;

procedure TfCallServer.SetLogLevel(const Value: Integer);
begin
  if FLogLevel <> Value then begin
    FLogLevel := Value;
    FLoggingEnabled := FLogLevel >= 0;
  end;
end;

procedure TfCallServer.SetStatus(const level: Integer; const s: String);
begin
  if level <= FLogLevel then begin
    if (mStatus.Lines.Count = cMaxLogLines) then begin
      if LoggingEnabled then
        mStatus.Lines.SaveToFile(CurrentLogFileName);
      mStatus.Lines.Clear;
    end;
    mStatus.Lines.Add(DateTimeToStr(Now) + ' ' + s);
  end;
end;

procedure TfCallServer.WaitForDisconnect;
var
  startTime: TDateTime;
begin
  startTime := Now;
  if FConnected then begin
    Disconnect;
    while FConnected and (SecondsBetween(Now, startTime) <= cDisconnectTimeout) do
      Application.ProcessMessages;
  end;
end;

procedure TfCallServer.bNotepadClick(Sender: TObject);
begin
  ShellExecute(0, 'open', 'Notepad.exe', '', '', SW_SHOWNORMAL);
end;

procedure TfCallServer.bWriteClick(Sender: TObject);
begin
  
end;

{ TMessageThread }

constructor TPacketThread.Create(form: TfCallServer; channel: PHandle; onStatus: TOnStatusEvent);
begin
  FPackets := TObjectList.Create;
  FPackets.OwnsObjects := true;
  FForm := form;
  FChannel := channel;
  FOnStatus := onStatus;
  SetStatus(10, ClassName + ' created');
  inherited Create(False);
end;

destructor TPacketThread.Destroy;
begin
  FreeAndNil(FPackets);
  FForm := nil;
  inherited;
end;

procedure TPacketThread.DoOnStatus;
begin
  if Assigned(FOnStatus) then
    FOnStatus(FLevel, FStatus);
end;

procedure TPacketThread.Execute;
var
  bufferActive: Boolean;
begin
  FActive := True;
  try
    while not Terminated do begin
      SetStatus(10, ClassName + ' processing');
      DoProcessing(bufferActive);
      SetStatus(10, ClassName + ' processing complete');
      if not bufferActive then
        Sleep(500);
    end;
  finally
    SetStatus(10, ClassName + ' processing terminated');
    FActive := False;
  end;
end;

function TPacketThread.GetCurrentPacket: TRdpPacket;
begin
  SetStatus(10, ClassName + ' fetching current packet');
  EnterCriticalSection(gCSection);
  try
    if FPackets.Count > 0 then begin
      result := TPacketObject(FPackets[0]).Packet;
      FPackets.Delete(0);
    end else
      FillChar(result, SizeOf(result), 0);
  finally
    LeaveCriticalSection(gCSection);
  end;
  SetStatus(10, ClassName + ' fetched current packet');
end;

function TPacketThread.GetPacketsWaiting: Boolean;
begin
  SetStatus(10, ClassName + ' packets waiting check');
  EnterCriticalSection(gCSection);
  try
    result := FPackets.Count > 0;
  finally
    LeaveCriticalSection(gCSection);
  end;
  SetStatus(10, ClassName + ' packets waiting check complete');
end;

procedure TPacketThread.Purge;
begin
  EnterCriticalSection(gCSection);
  try
    FPackets.Clear;
    SetLength(FBuffer, 0);
  finally
    LeaveCriticalSection(gCSection);
    SetStatus(5, ClassName + ' packets purged');
  end;
end;

procedure TPacketThread.SetStatus(const level: Integer; const status: String);
begin
  EnterCriticalSection(gCSection);
  try
    FLevel := level;
    FStatus := status;
  finally
    LeaveCriticalSection(gCSection);
    Synchronize(DoOnStatus);
  end;
end;

{ TReadThread }

constructor TReadThread.Create(form: TfCallServer; channel: PHandle; onStatus: TOnStatusEvent; onPacketRead: TOnPacketReadEvent);
begin
  inherited Create(form, channel, onStatus);
  FBufferLen := 0;
  FBufferOffset := 0;
  FOnPacketRead := onPacketRead;
end;

procedure TReadThread.DoOnPacketRead;
begin
  SetStatus(10, ClassName + ' processing read packet');
  if Assigned(FOnPacketRead) then
    FOnPacketRead;
end;

procedure TReadThread.DoProcessing(var bufferActive: Boolean);
var
  bytesRead: Cardinal;
  havePacket: Boolean;
  pkt: TRdpPacket;
begin
  havePacket := false;
  SetStatus(10, ClassName + ' currently processing');
  EnterCriticalSection(gCSection);
  try
    if (FBufferLen = 0) and (FBufferOffset = 0) then begin
      if WTSVirtualChannelRead(FChannel^, 0, @FBufferLen, SizeOf(FBufferLen), bytesRead) then begin
        if bytesRead = SizeOf(FBufferLen) then begin
          FError := 0;
          SetLength(FBuffer, FBufferLen);
          bufferActive := True;
        end else begin
          FError := GetLastError;
          FBufferLen := 0;
          FBufferOffset := 0;
          SetLength(FBuffer, FBufferLen);
          WTSVirtualChannelPurgeInput(FChannel^);
          bufferActive := False;
        end;
      end else begin
        FError := GetLastError;
        FBufferLen := 0;
        FBufferOffset := 0;
        SetLength(FBuffer, FBufferLen);
        WTSVirtualChannelPurgeInput(FChannel^);
        bufferActive := False;
      end;
    end else if WTSVirtualChannelRead(FChannel^, 0, @FBuffer[FBufferOffset], FBufferLen - FBufferOffset, bytesRead) then begin
      FError := 0;
      inc(FBufferOffset, bytesRead);
      if FBufferOffset = FBufferLen then begin
        pkt.SetFromBytes(FBuffer);
        FPackets.Add(TPacketObject.Create(pkt));
        FBufferOffset := 0;
        FBufferLen := 0;
        SetLength(FBuffer, 0);
        havePacket := true;
        bufferActive := False;
      end else
        bufferActive := True;
    end else begin
      FError := GetLastError;
      bufferActive := False;
    end;
  finally
    LeaveCriticalSection(gCSection);
    if havePacket then
      Synchronize(DoOnPacketRead);
    SetStatus(10, ClassName + ' current processing complete');
  end;
end;

function TReadThread.ReadPacket: TRdpPacket;
begin
  result := CurrentPacket;
end;

{ TWriteThread }

procedure TWriteThread.DoProcessing(var bufferActive: Boolean);
var
  bytesWritten: DWORD;
  totalWritten: DWORD;
  bufLen: DWORD;
  tBuffer: TBytes;
begin
  SetStatus(10, 'Write thread process started');
  EnterCriticalSection(gCSection);
  try
    if Assigned(FPackets) and (FPackets.Count > 0) then begin
      TPacketObject(FPackets[0]).Packet.ToBytes(tBuffer);
      bufLen := Length(tBuffer);
      SetLength(FBuffer, bufLen + SizeOf(bufLen));
      Move(bufLen, FBuffer[0], SizeOf(bufLen));
      Move(tBuffer[0], FBuffer[SizeOf(bufLen)], bufLen);
      bufLen := Length(FBuffer);
      totalWritten := 0;
      FError := CHANNEL_RC_OK;
      repeat
        SetStatus(10, Format('Processing data in write loop. Total %d. Written %d. Error %d', [bufLen, totalWritten, FError]));
        if WTSVirtualChannelWrite(FChannel^, @FBuffer[totalWritten], bufLen - totalWritten, @bytesWritten) then begin
          if bytesWritten = bufLen then begin
            FPackets.Delete(0);
            bufferActive := false;
          end else
            bufferActive := true;
          inc(totalWritten, bytesWritten);
        end else begin
          FError := GetLastError;
          bufferActive := false;
        end;
      until (totalWritten >= bufLen) or (FError <> CHANNEL_RC_OK);
    end;
  finally
    LeaveCriticalSection(gCSection);
  end;
  SetStatus(10, Format('Write thread process complete rc=%d', [FError]));
end;

{ TPacketObject }

constructor TPacketObject.Create(const pkt: TRdpPacket);
begin
  inherited Create;
  FRdpPacket.Copy(pkt);
end;

procedure TWriteThread.SendPacket(const pkt: TRdpPacket);
begin
  SetStatus(5, 'Write thread queueing packet');
  EnterCriticalSection(gCSection);
  try
    FPackets.Add(TPacketObject.Create(pkt));
    SetStatus(3, 'Packet queued');
    SetStatus(5, Format('Packet type %d on route %s.', [Integer(pkt.msg.msgType), pkt.msg.routeName]));
  finally
    LeaveCriticalSection(gCSection);
  end;
end;

initialization
  InitializeCriticalSection(gCSection);

finalization
  DeleteCriticalSection(gCSection);

end.

