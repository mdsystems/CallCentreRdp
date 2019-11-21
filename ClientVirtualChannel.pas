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
unit ClientVirtualChannel;

interface

uses
  CallMessage, ChannelData, Rdp, SharedDefinitions, VirtualChannel, Windows;

type
  TChannelsData = array [0 .. 0] of TChannelData;
  PChannelsData = ^TChannelsData;
  TOnEntryEvent = procedure;
  TOnChannelActiveEvent = procedure(const rdpChannel: TRdpChannelName) of object;

procedure Initialise;
function VirtualChannelEntry(const ePoints: TChannelEntryPoints): Boolean; stdcall;
procedure WriteClientChannelPacket(pkt: TRdpPacket);
procedure SetCallMessageEvent(const name: TrdpChannelName; handler: TOnCallMessageEvent);
procedure SetOnActivateEvent(const name: TRdpChannelName; handler: TOnConnectEvent);
procedure SetOnTerminateEvent(const name: TRdpChannelName; handler: TOnConnectEvent);
procedure SetChannelName(var cName: TRdpChannelName; const str: String);
function ChannelHandleValid(const name: TRdpChannelName): Boolean;

var
  OnEntry: TOnEntryEvent;
  OnStatus: TOnStatusEvent;

implementation

uses
  AnsiStrings, Classes, Math, SysUtils;

type
  TChannelEvents = (ceInitialized = 0, ceConnected = 1, ceV1Connected = 2, ceDisconnected = 3, ceTerminated = 4,
    ceDataReceived = 10, ceWriteComplete = 11, ceWriteCancelled = 12);
  TChannelReturnCodes = (crOk = 0, crAlreadyInitialized, crNotInitialized, crAlreadyConnected, crNotConnected,
    crTooManyChannels, crBadChannel, crBadChannelHandle, crNoBuffer, crBadInitHandle, crNotOpen, crBadProc, crNoMemory,
    crUnknownChannelName, crAlreadyOpen, crNotInVirtualChannelEntry, crNullData, crZeroLength);

const
  events: array [TChannelEvents] of string = ('Initialized', 'Connected', 'V1 Connected', 'Disconnected', 'Terminated',
    'Unknown 5', 'Unknown 6', 'Unknown 7', 'Unknown 8', 'Unknown 9', 'Data Received', 'Write Complete',
    'Write Cancelled');

var
  entryPoints: TChannelEntryPoints;
  channels: PChannels;
  channelsData: PChannelsData;
  channelCount: Integer;

procedure SetChannelName(var cName: TRdpChannelName; const str: String);
var
  sLen: Integer;
  aStr: AnsiString;
begin
  sLen := Length(str);
  if sLen > SizeOf(TRdpChannelName) then
    sLen := SizeOf(TRdpChannelName);
  FillChar(cName, SizeOf(cName), 0);
  if sLen > 0 then begin
    aStr := AnsiString(str);
    Move(aStr[1], cName[0], Min(Length(aStr), sLen));
  end;
end;

procedure SetStatus(const level: Integer; const str: string);
begin
  if Assigned(OnStatus) then
    OnStatus(level, str);
end;

procedure ProcessRequest(channelIndex: Integer);
var
  pkt: TRdpPacket;
begin
  if (channelIndex >= 0) and (channelIndex < channelCount) then begin
    pkt.Clear;
    channelsData^[channelIndex].ReadPacketFromInputBuffer(pkt, OnStatus);
    if Assigned(channelsData^[channelIndex].OnCallMessage) then
      channelsData^[channelIndex].OnCallMessage(pkt.msg, pkt.ParamStr);
  end;
end;

function ChannelName(channelIndex: Integer): TRdpChannelName;
begin
  if (channelIndex >= 0) and (channelIndex < channelCount) then
    Move(channels^[channelIndex].rdChannelName[0], result[0], SizeOf(TRdpChannelName))
  else
    FillChar(result[0], SizeOf(TRdpChannelName), 0);
end;

function ChannelIndex(handle: DWORD; ignoreHandle: Boolean): Integer; overload;
begin
  result := channelCount;
  repeat
    Dec(result);
  until (result < 0) or ((channelsData^[result].Handle = handle) and (ignoreHandle or channelsData^[result].HandleValid));
end;

function ChannelIndex(name: TRdpChannelName; ignoreHandle: Boolean): Integer; overload;
begin
  result := channelCount;
  repeat
    Dec(result);
  until (result < 0) or ((channels^[result].rdChannelName = name) and (ignoreHandle or channelsData^[result].HandleValid));
end;

function ChannelIndex(handle: DWORD): Integer; overload;
begin
  result := ChannelIndex(handle, false);
end;

function ChannelIndex(name: TRdpChannelName): Integer; overload;
begin
  result := ChannelIndex(name, false);
end;

function ChannelHandleValid(const name: TRdpChannelName): Boolean;
begin
  result := ChannelIndex(name, false) >= 0;
end;

procedure SetCallMessageEvent(const name: TRdpChannelName; handler: TOnCallMessageEvent);
var
  i: Integer;
begin
  i := ChannelIndex(name, True);
  if i >= 0 then
    channelsData^[i].OnCallMessage := handler;
end;

procedure SetOnActivateEvent(const name: TRdpChannelName; handler: TOnConnectEvent);
var
  i: Integer;
begin
  i := ChannelIndex(name, True);
  if i >= 0 then
    channelsData^[i].OnActivate := handler;
end;

procedure SetOnTerminateEvent(const name: TRdpChannelName; handler: TOnConnectEvent);
var
  i: Integer;
begin
  i := ChannelIndex(name, True);
  if i >= 0 then
    channelsData^[i].OnTerminate := handler;
end;

procedure VirtualChannelOpenEvent(openHandle: DWORD; event: UINT; pData: PByte; dataLength: UINT32; totalLength: UINT32;
  dataFlags: UINT32); stdcall;
var
  cIndex: Integer;
  e: TChannelEvents;
begin
  cIndex := ChannelIndex(openHandle);
  if cIndex = -1 then
    SetStatus(1, Format('Unknown channel handle = %d', [openHandle]))
  else begin
    e := TChannelEvents(event);
    if ((e >= low(events)) and (e <= high(events))) then
      SetStatus(5, events[e])
    else
      SetStatus(10, Format('Unknown event id = %d', [event]));
    case e of
      ceDataReceived: begin
          SetStatus(10, Format('Open Handle = %d DataLength = %d TotalLength = %d Flags = %d',
            [openHandle, channelsData^[cIndex].InputBufferLength, channelsData^[cIndex].InputBufferOffset, dataFlags]));
          if ((dataFlags and cChannelFlagFirst) <> 0) then begin
            channelsData^[cIndex].ResetInputBuffer(totalLength);
            SetStatus(10, Format('Set data length %d', [totalLength]));
          end;
          if channelsData^[cIndex].InputBufferLength = 0 then begin
            SetStatus(1, 'Internal error. No buffer allocated.');
          end else begin
            SetStatus(10, Format('Appending %d bytes to buffer', [dataLength]));
            channelsData^[cIndex].AddToInputBuffer(pData, dataLength, OnStatus);
            if ((dataFlags and cChannelFlagLast) <> 0) then begin
              SetStatus(10, 'Buffer should be complete');
              if channelsData^[cIndex].InputBufferFull then
                ProcessRequest(cIndex)
              else
                SetStatus(1, 'Input buffer not full, though last data packet was indicated');
            end;
          end;
        end;

      ceWriteComplete: begin
          SetStatus(10, Format('Open Handle = %d Write complete', [openHandle]));
          channelsData^[cIndex].ClearOutputBuffer;
        end;

      ceWriteCancelled: begin
          SetStatus(10, Format('Open Handle = %d Write cancelled', [openHandle]));
          channelsData^[cIndex].ClearOutputBuffer;
        end;
    end;
  end;
end;

procedure VirtualChannelInitEvent(pInitHandle: THandle; event: UINT; pData: Pointer; dataLength: UINT); stdcall;

  procedure OpenChannels;
  var
    i: Integer;
    stat: UINT;
    handle: DWORD;
  begin
    // TODO - Obtain channel list and allocate memory.
    for i := 0 to channelCount - 1 do begin
      SetStatus(5, Format('Opening channel %s', [channels^[i].rdChannelName]));
      channelsData^[i].ClearInputBuffer;
      handle := channelsData^[i].Handle;
      stat := entryPoints.virtualChannelOpen(pInitHandle, handle, channels^[i].rdChannelName, VirtualChannelOpenEvent);
      channelsData^[i].Handle := handle;
      if (TChannelReturnCodes(stat) <> crOk) then
        SetStatus(1, Format('Open failed. Status = %d', [stat]))
      else begin
        SetStatus(10, Format('Open handle = %d', [handle]));
        if Assigned(channelsData^[i].OnActivate) then
          channelsData^[i].OnActivate;
      end;
    end;
  end;

var
  i: Integer;
  e: TChannelEvents;
begin
  e := TChannelEvents(event);
  if ((e >= low(events)) and (e <= high(events))) then
    SetStatus(5, events[e])
  else
    SetStatus(1, Format('Unknown event id = %d', [event]));
  case e of
    ceConnected: begin
        SetStatus(5, Format('Server name = %s length = %d', [string(PChar(pData)), dataLength]));
        OpenChannels;
      end;
    ceTerminated: begin
        for i := 0 to channelCount - 1 do begin
          if Assigned(channelsData^[i].OnTerminate) then
            channelsData^[i].OnTerminate;
          channelsData^[i].ClearInputBuffer;
        end;
      end;
  end;
end;

function VirtualChannelEntry(const ePoints: TChannelEntryPoints): Boolean; stdcall;
// This is the main (and only) entry point to the DLL.  This gets called when
// the remote desktop client tries to load this DLL to initialize the virtual
// channel.
var
  stat: UINT;
  i: Integer;
begin
  // TODO Need channel count to be set here and channel names populated.
  if Assigned(OnEntry) then
    OnEntry;
  entryPoints := ePoints;
  stat := entryPoints.virtualChannelInit(gInitHandle, channels, channelCount, cVirtualChannelVersionWin2K,
    VirtualChannelInitEvent);
  Result := (TChannelReturnCodes(stat) = crOk);
  SetStatus(5, Format('Init status = %d', [stat]));
  for i := 0 to channelCount -1 do begin
    if ((channels^[i].options and cChanOptInitialised) = 0) then begin
      SetStatus(1, Format('Channel %s not initialized', [channels^[i].rdChannelName]));
      Result := False;
    end;
  end;
end;

procedure WriteClientChannelPacket(pkt: TRdpPacket);
var
  stat: UINT;
  obSize: Cardinal;
  cIndex: Integer;
begin
  cIndex := ChannelIndex(pkt.rdpChannelName);
  if cIndex = -1 then
    SetStatus(1, 'Unable to identify RDP channel for packet.')
  else begin
    if channelsData^[cIndex].HandleValid then begin
      SetStatus(10, 'VChannel active. Sending packet');
      channelsData^[cIndex].WritePacketToOutputBuffer(pkt, OnStatus);
      obSize := channelsData^[cIndex].OutputBufferLength;
      if Assigned(OnStatus) then
        OnStatus(10, Format('OpenHandle = %d Writing %d bytes', [channelsData^[cIndex].Handle, obSize]));
      stat := entryPoints.virtualChannelWrite(channelsData^[cIndex].Handle, channelsData^[cIndex].OutputBufferData, obSize, nil);
      if (TChannelReturnCodes(stat) <> crOk) then begin
        if Assigned(OnStatus) then
          OnStatus(1, Format('Write failed! Error = %d.', [stat]));
      end;
      SetStatus(10, 'Sent packet');
    end else
      SetStatus(1, 'VChannel handle not set. Cannot communicate with RDP session.');
  end;
end;

procedure Initialise;
var
  channelList: TStringList;
  allocSize: Integer;
  i: Integer;
begin
  channels := nil;
  channelsData := nil;
  ZeroMemory(@entryPoints, SizeOf(entryPoints));
  channelList := TStringList.Create;
  try
    FetchRDChannelNames(channelList, nil);
    channelCount := channelList.Count;
    if channelCount > 0 then begin
      allocSize := SizeOf(TChannelDef) * channelCount;
      GetMem(channels, allocSize);
      ZeroMemory(channels, allocSize);
      allocSize := SizeOf(TChannelData) * channelCount;
      GetMem(channelsData, allocSize);
      ZeroMemory(channelsData, allocSize);
      for i := 0 to channelCount - 1 do begin
        channelsData^[i] := TChannelData.Create;
        AnsiStrings.StrPLCopy(channels^[i].rdChannelName, AnsiString(channelList[i]), SizeOf(channels^[i].rdChannelName));
      end;
    end;
  finally
    channelList.Free;
  end;
  gInitHandle := 0;
  OnStatus := nil;
end;

procedure Finalise;
var
  i: Integer;
begin
  for i := 0 to channelCount - 1 do
    FreeAndNil(channelsData[i]);
  FreeMem(channels);
  FreeMem(channelsData);
end;

initialization

finalization
  Finalise;

end.
