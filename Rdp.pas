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
unit Rdp;

interface

uses
  CallMessage, SharedDefinitions, SysUtils, Types, VirtualChannel, Windows;

type
  TExtraData = record
    dataLen: DWORD;
    data: TBytes;
  end;
  TRdpPacket = record
  private
    FParams: TExtraData;
    FRdpChannelName: TRdpChannelName;
    procedure SetParams(const params: TExtraData);
    function GetParamStr: String;
    procedure SetParamStr(const Value: String);
  public
    msg: TCallMessage;
    procedure Copy(const pkt: TRdpPacket);
    procedure ToBytes(var bytes: TBytes);
    procedure SetFromBytes(bytes: TBytes);
    procedure SetRdpChannelName(cn: String); overload;
    procedure SetRdpChannelName(cn: TRdpChannelName); overload;
    procedure Clear;
    property rdpChannelName: TRdpChannelName read FRdpChannelName;
    property ParamStr: String read GetParamStr write SetParamStr;
  end;

var
  gInitHandle: THandle;

implementation

{ TRdpPacket }

procedure TRdpPacket.Clear;
begin
  FParams.dataLen := 0;
  FillChar(FRdpChannelName, SizeOf(FRdpChannelName), 0);
  SetLength(FParams.data, 0);
  ZeroMemory(@msg, SizeOf(msg));
end;

procedure TRdpPacket.Copy(const pkt: TRdpPacket);
begin
  SetParams(pkt.FParams);
  FRdpChannelName := pkt.FRdpChannelName;
  msg := pkt.msg;
end;

procedure TRdpPacket.SetFromBytes(bytes: TBytes);
var
  bLen: Cardinal;
begin
  bLen := Length(bytes);
  if bLen >= SizeOf(FParams.dataLen) then begin
    Move(bytes[0], FParams.dataLen, SizeOf(FParams.dataLen));
    if FParams.dataLen + SizeOf(FParams.dataLen) > bLen then
      raise Exception.CreateFmt('Buffer insufficient to populate packet params (%d/%d)', [bLen, FParams.dataLen])
    else if FParams.dataLen + SizeOf(FParams.dataLen) + SizeOf(msg) > bLen then
      raise Exception.CreateFmt('Buffer insufficient to populate packet message (%d/%d/%d)', [bLen, FParams.dataLen, SizeOf(msg)])
    else begin
      SetLength(FParams.data, FParams.dataLen);
      if FParams.dataLen > 0 then
        Move(bytes[SizeOf(FParams.dataLen)], FParams.data[0], FParams.dataLen);
      Move(bytes[FParams.dataLen + SizeOf(FParams.dataLen)], msg, SizeOf(msg));
    end;
  end else
    raise Exception.Create('Buffer insufficient to populate packet length');
end;

function TRdpPacket.GetParamStr: String;
begin
  SharedDefinitions.SetStr(result, FParams.data);
end;

procedure TRdpPacket.SetParams(const params: TExtraData);
begin
  FParams.dataLen := params.dataLen;
  SetLength(FParams.data, params.dataLen);
  Move(params.data, FParams.data, params.dataLen);
end;

procedure TRdpPacket.SetParamStr(const Value: String);
begin
  FParams.dataLen := Length(Value);
  SetLength(FParams.data, FParams.dataLen);
  SharedDefinitions.SetArray(FParams.data, Value, FParams.dataLen);
end;

procedure TRdpPacket.SetRdpChannelName(cn: String);
begin
  SharedDefinitions.SetArray(FRdpChannelName, cn, SizeOf(FRdpChannelName));
end;

procedure TRdpPacket.SetRdpChannelName(cn: TRdpChannelName);
begin
  Move(cn[0], FRdpChannelName[0], SizeOf(TRdpChannelName));
end;

procedure TRdpPacket.ToBytes(var bytes: TBytes);
begin
  SetLength(bytes, FParams.dataLen + SizeOf(FParams.dataLen) + SizeOf(msg));
  Move(FParams.dataLen, bytes[0], SizeOf(FParams.dataLen));
  if FParams.dataLen > 0 then
    Move(FParams.data[0], bytes[SizeOf(FParams.dataLen)], FParams.dataLen);
  Move(msg, bytes[SizeOf(FParams.dataLen) + FParams.dataLen], SizeOf(msg));
end;

end.
