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
unit ChannelData;

interface

uses
  CallMessage, Rdp, SharedDefinitions, System.SysUtils, VirtualChannel, Windows;

type
  TChannelData = class
  private
    FHandle: DWORD;
    FInputBuffer: TBytes;
    FInputBufferOffset: DWORD;
    FOutputBuffer: TBytes;
    FOutputBufferOffset: DWORD;
    FOnCallMessage: TOnCallMessageEvent;
    FOnTerminate: TOnConnectEvent;
    FOnActivate: TOnConnectEvent;
    procedure AddToBuffer(var buffer: TBytes; const data: PByte; const dataLen: DWORD; var offset: DWORD);
  public
    constructor Create;
    destructor Destroy; override;
    function HandleValid: Boolean;
    function InputBufferLength: DWORD;
    function InputBufferOffset: DWORD;
    function InputBufferFull: Boolean;
    function InputBufferData: PByte;
    function OutputBufferLength: DWORD;
    function OutputBuffferOffset: DWORD;
    function OutputBufferFull: Boolean;
    function OutputBufferData: PByte;
    procedure ClearInputBuffer;
    procedure ClearOutputBuffer;
    procedure ResetInputBuffer(const len: Integer);
    procedure ResetOutputBuffer(const len: Integer);
    procedure AddToInputBuffer(const data: PByte; dataLen: DWORD; OnStatus: TOnStatusEvent);
    procedure AddToOutputBuffer(const data: PByte; dataLen: DWORD; OnStatus: TOnStatusEvent);
    procedure WritePacketToOutputBuffer(const packet: TRdpPacket; OnStatus: TOnStatusEvent);
    procedure ReadPacketFromInputBuffer(var packet: TRdpPacket; OnStatus: TOnStatusEvent);
    property Handle: DWORD read FHandle write FHandle;
    property OnCallMessage: TOnCallMessageEvent read FOnCallMessage write FOnCallMessage;
    property OnActivate: TOnConnectEvent read FOnActivate write FOnActivate;
    property OnTerminate: TOnConnectEvent read FOnTerminate write FOnTerminate;
  end;

implementation

{ TChannelData }

procedure TChannelData.AddToBuffer(var buffer: TBytes; const data: PByte; const dataLen: DWORD; var offset: DWORD);
begin
  if dataLen + offset > DWORD(Length(buffer)) then
    raise Exception.Create('Buffer overflow')
  else begin
    Move(data^, buffer[offset], dataLen);
    inc(offset, dataLen);
  end;
end;

procedure TChannelData.AddToInputBuffer(const data: PByte; dataLen: DWORD; OnStatus: TOnStatusEvent);
begin
  try
    if Assigned(OnStatus) then
      OnStatus(10, Format('Appending to input buffer at offset %d', [FInputBufferOffset]));
    AddToBuffer(FInputBuffer, data, dataLen, FInputBufferOffset);
  except on e: Exception do
    if Assigned(OnStatus) then
      OnStatus(1, 'Unable to append to channel input: ' + e.Message)
    else
      raise;
  end;
end;

procedure TChannelData.AddToOutputBuffer(const data: PByte; dataLen: DWORD; OnStatus: TOnStatusEvent);
begin
  try
    if Assigned(OnStatus) then
      OnStatus(10, Format('Appending to output buffer at offset %d', [FOutputBufferOffset]));
    AddToBuffer(FOutputBuffer, data, dataLen, FOutputBufferOffset);
  except on e: Exception do
    if Assigned(OnStatus) then
      OnStatus(1, 'Unable to append to channel output: ' + e.Message)
    else
      raise;
  end;
end;

function TChannelData.InputBufferData: PByte;
begin
  result := @FInputBuffer[0];
end;

function TChannelData.InputBufferFull: Boolean;
begin
  result := FInputBufferOffset >= DWORD(Length(FInputBuffer));
end;

procedure TChannelData.ClearInputBuffer;
begin
  FInputBufferOffset := 0;
  SetLength(FInputBuffer, 0);
end;

procedure TChannelData.ClearOutputBuffer;
begin
  FOutputBufferOffset := 0;
  SetLength(FOutputBuffer, 0);
end;

constructor TChannelData.Create;
begin
  inherited Create;
  FHandle := 0;
  FInputBufferOffset := 0;
  SetLength(FInputBuffer, 0);
  FOutputBufferOffset := 0;
  SetLength(FOutputBuffer, 0);
  FOnCallMessage := nil;
  FOnActivate := nil;
  FOnTerminate := nil;
end;

destructor TChannelData.Destroy;
begin
  SetLength(FInputBuffer, 0);
  SetLength(FOutputBuffer, 0);
  inherited;
end;

function TChannelData.HandleValid: Boolean;
begin
  Result := FHandle <> 0;
end;

function TChannelData.InputBufferLength: DWORD;
begin
  result := Length(FInputBuffer);
end;

function TChannelData.InputBufferOffset: DWORD;
begin
  result := FInputBufferOffset;
end;

function TChannelData.OutputBufferData: PByte;
begin
  result := @FOutputBuffer[0]
end;

function TChannelData.OutputBufferFull: Boolean;
begin
  result := FOutputBufferOffset >= DWORD(Length(FOutputBuffer));
end;

function TChannelData.OutputBufferLength: DWORD;
begin
  result := Length(FOutputBuffer);
end;

function TChannelData.OutputBuffferOffset: DWORD;
begin
  result := FOutputBufferOffset;
end;

procedure TChannelData.ReadPacketFromInputBuffer(var packet: TRdpPacket; OnStatus: TOnStatusEvent);
var
  tbSize: DWORD;
  tBuffer: TBytes;
begin
  try
    if Length(FInputBuffer) >= SizeOf(tbSize) then begin
      Move(FInputBuffer[0], tbSize, SizeOf(tbSize));
      if Assigned(OnStatus) then
        OnStatus(10, Format('Data length %d', [tbSize]));
      SetLength(tBuffer, tbSize);
      Move(FInputBuffer[SizeOf(tbSize)], tBuffer[0], tbSize);
      packet.SetFromBytes(tBuffer);
    end else begin
      if Assigned(OnStatus) then
        OnStatus(10, Format('Buffer length too short to contain a data length %d', [Length(FInputBuffer)]));
      packet.Clear;
    end;
    ClearInputBuffer;
  except on E:Exception do
    if Assigned(OnStatus) then
      OnStatus(1, 'Unable to extract packet from buffer: ' + e.Message)
    else
      raise;
  end;

  {
    if Length(VChannel.InputBuffer) = SizeOf(pkt) then begin
    Move(VChannel.InputBuffer[0], pkt, SizeOf(pkt));
    SetStatus(10, 'Packet type ' + IntToStr(Integer(pkt.PacketType)));
    case pkt.PacketType of
    ptCallMessage:
    begin
    if Assigned(OnCallMessage) then
    OnCallMessage(pkt.msg);
    end;
    else
    SetStatus(10, 'Input buffer length invalid');
    end;
    VChannel.InputBufferSize := 0;
    SetLength(VChannel.InputBuffer, 0);
    end;
  }
end;

procedure TChannelData.ResetInputBuffer(const len: Integer);
begin
  if len <> Length(FInputBuffer) then
    SetLength(FInputBuffer, len);
  FInputBufferOffset := 0;
end;

procedure TChannelData.ResetOutputBuffer(const len: Integer);
begin
  if len <> Length(FOutputBuffer) then
    SetLength(FOutputBuffer, len);
  FOutputBufferOffset := 0;
end;

procedure TChannelData.WritePacketToOutputBuffer(const packet: TRdpPacket; OnStatus: TOnStatusEvent);
var
  obSize: DWORD;
  tBuffer: TBytes;
begin
  packet.ToBytes(tBuffer);
  obSize := Length(tBuffer);
  SetLength(FOutputBuffer, SizeOf(obSize) + obSize);
  Move(obSize, FOutputBuffer[0], SizeOf(obSize));
  Move(tBuffer[0], FOutputBuffer[SizeOf(obSize)], obSize);
end;

end.
