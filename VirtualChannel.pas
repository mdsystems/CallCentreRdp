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
unit VirtualChannel;

interface

uses
  Classes, Windows, SharedDefinitions;

const
  cVirtualChannelVersionWin2K = 1;
  cMaxPacketDataLength = 4096;
  cChannelFlagFirst = $01;
  cChannelFlagLast = $02;

  cChanOptInitialised = $80000000;

  CO_ENCRYPT_RDP = $40000000;
  CO_ENCRYPT_SC = $20000000;
  CO_ENCRYPT_CS = $10000000;
  CO_PRIORITY_HIGH = $08000000;
  CO_PRIORITY_MEDIUM = $04000000;
  CO_PRIORITY_LOW = $02000000;
  CO_COMPRESS_RDP = $00800000;
  CO_COMPRESS = $00400000;
  CO_SHOW_PROTOCOL = $00200000;

  CHANNEL_RC_OK = 0;
  CHANNEL_RC_ALREADY_INITIALIZED = 1;
  CHANNEL_RC_NOT_INITIALIZED = 2;
  CHANNEL_RC_ALREADY_CONNECTED = 3;
  CHANNEL_RC_NOT_CONNECTED = 4;
  CHANNEL_RC_TOO_MANY_CHANNELS = 5;
  CHANNEL_RC_BAD_CHANNEL = 6;
  CHANNEL_RC_BAD_CHANNEL_HANDLE = 7;
  CHANNEL_RC_NO_BUFFER = 8;
  CHANNEL_RC_BAD_INIT_HANDLE = 9;
  CHANNEL_RC_NOT_OPEN = 10;
  CHANNEL_RC_BAD_PROC = 11;
  CHANNEL_RC_NO_MEMORY = 12;
  CHANNEL_RC_UNKNOWN_CHANNEL_NAME = 13;
  CHANNEL_RC_ALREADY_OPEN = 14;
  CHANNEL_RC_NOT_IN_VIRTUALCHANNELENTRY = 15;
  CHANNEL_RC_NULL_DATA = 16;
  CHANNEL_RC_ZERO_LENGTH = 17;
  CHANNEL_RC_PENDING_WRITE = 18;

  VIRTUAL_CHANNEL_VERSION_WIN2000 = 1;

  WTS_CURRENT_SERVER_HANDLE = 0;
  WTS_CURRENT_SERVER = WTS_CURRENT_SERVER_HANDLE;
  WTS_CURRENT_SESSION = DWORD(-1);

  cRDChannelNameLen = 7;

type
  TRdpChannelName = array [0 .. cRDChannelNameLen] of AnsiChar;
  TOnConnectEvent = procedure of object;
  TChannelDef = record
    rdChannelName: TRdpChannelName;
    options: ULONG
  end;
  TChannels = array [0 .. 0] of TChannelDef;
  PChannels = ^TChannels;
  TVirtualChannelInitEvent =
    procedure(pInitHandle: THandle; event: UINT; pData: Pointer; dataLength: UINT);
    stdcall;
  TVirtualChannelInit =
    function(var ppInitHandle: THandle; pChannel: PChannels; channelCount: Integer; versionRequested: Integer;
      pChannelInitEventProc: TVirtualChannelInitEvent): UINT;
    stdcall;
  TVirtualChannelOpenEvent =
    procedure(openHandle: DWORD; event: UINT; pData: PByte; dataLength: UINT32; totalLength: UINT32; dataFlags: UINT32);
    stdcall;
  TVirtualChannelOpen =
    function(pInitHandle: THandle; var pOpenHandle: DWORD; pRDChannelName: PAnsiChar;
      pChannelOpenEventProc: TVirtualChannelOpenEvent): UINT;
    stdcall;
  TVirtualChannelClose =
    function(openHandle: DWORD): UINT;
    stdcall;
  TVirtualChannelWrite =
    function(openHandle: DWORD; pData: Pointer; dataLength: ULONG; pUserData: Pointer): UINT;
    stdcall;
  TChannelEntryPoints = record
    cbSize: DWORD;
    protocolVersion: DWORD;
    virtualChannelInit: TVirtualChannelInit;
    virtualChannelOpen: TVirtualChannelOpen;
    virtualChannelCose: TVirtualChannelClose;
    virtualChannelWrite: TVirtualChannelWrite;
  end;

function IniFileName: String;

procedure FetchRDChannelNames(rdChannelNames, routeNames: TStringList);
// rdChannelNames and routeNames must be instantiated before calling this procedure. They is managed by the caller.

implementation

uses
  IniFiles, Sysutils;


function IniFileName: String;
var
  cret: integer;
  ModName: array[0 .. MAX_PATH - 1] of Char;
begin
  Windows.GetModuleFileName(HInstance, ModName, SizeOf(ModName));
  cret := GetLongPathName(ModName, ModName, SizeOf(ModName));
  SetString(Result, ModName, cret);
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(Result)) + cIniFileName;
end;

procedure FetchRDChannelNames(rdChannelNames, routeNames: TStringList);
var
  iFile: TIniFile;
  ifName: TFileName;
  sections: TStringList;
  routeInfo: TStringList;
  routes: TStringList;
  i: Integer;
begin
  ifName := IniFileName;
  if FileExists(ifName) then begin
    iFile := TIniFile.Create(ifName);
    try
      sections := TStringList.Create;
      try
        routes := TStringList.Create;
        try
          routes.CaseSensitive := false;
          routes.Duplicates := dupIgnore;
          iFile.ReadSections(sections);
          if Assigned(rdChannelNames) then
            rdChannelNames.Clear;
          if Assigned(routeNames) then
            routeNames.Clear;
          for i := 0 to sections.Count - 1 do begin
            if (sections[i] <> cDDIRoutesSectionName) and (sections[i] <> cHTTPServerSectionName) then begin
              routes.Add(sections[i]);
              routeInfo := TStringList.Create;
              try
                rdChannelNames.Duplicates := dupIgnore;
                iFile.ReadSectionValues(sections[i], routeInfo);
                if Assigned(rdChannelNames) then begin
                  rdChannelNames.Add(Uppercase(routeInfo.Values[cRDChannelValue]));
                  rdChannelNames.Sort;
                end;
                if Assigned(routeNames) then begin
                  routeNames.Add(Uppercase(sections[i]));
                  routeNames.Sort;
                end;
              finally
                routeInfo.Free;
              end;
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
    raise Exception.Create(Format(cNoIniFile, [ifName]));
end;

end.
