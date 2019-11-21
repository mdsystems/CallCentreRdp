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
unit SharedDefinitions;

interface

uses
  Windows, SysUtils;

type
  PBytes = ^TBytes;
  TOnStatusEvent = procedure (const level: Integer; const status: String) of object;
  {$IFDEF VER180}
  UINT32 = Cardinal;
  {$ENDIF}

const
  cIniFileName = 'CallRouter.ini';
  cDDIRoutesSectionName = 'DDIRoutes';
  cHTTPServerSectionName = 'HTTPServer';
  cCmdLineValue = 'CommandLine';
  cForwardParametersValue = 'ForwardParameters';
  cExcludeRouterParametersValue = 'ExcludeRouterParametersValue';
  cShowSysTrayBalloonValue = 'ShowSysTrayBalloon';
  cShowPopupWindowValue = 'ShowPopupWindow';
  cCallTimeoutSecondsValue = 'CallTimeoutSeconds';
  cSpawnTimeoutSecondsValue = 'SpawnTimeoutSeconds';
  cSpawnAtStartValue = 'SpawnAtStart';
  cFriendlyNameValue = 'FriendlyName';
  cRDChannelValue = 'RDChannel';
  cRdHostValue = 'RDHost';
  cUseHTTPSValue = 'UseHTTPS';
  cCertFileValue = 'CertFile';
  cCipherListValue = 'CipherList';
  cDHParamsFileValue = 'DHParamsFile';
  cKeyFileValue = 'KeyFile';
  cRootCertFileValue = 'RootCertFile';
  cHttpBindingsValue = 'Bindings';
  cBindingIPValue = 'IP';
  cBindingPortValue = 'Port';
  cIpV6Value = 'IPV6';
  cHTTPEnabledValue = 'Enabled';
  cHTTPImmediateCallRedirectValue = 'ImmediateCallRedirect';
  cCallDocument = '/CALL.HTML';
  cClpHideSystrayIcon = '/HideSysTrayIcon';
  cClpLogLevel = 'LogLevel';
  cClpDdiParamName = 'DDI';
  cClpCliParamName = 'CLI';
  cClpRouteName = 'RouteName';
  cClpOperatorName = 'OperName';
  cClpSystemName = 'SysName';
  cClpMessageString = 'Message';
  cClpCallId = 'CallId';
  cHttpCallerIdParam = 'callerid';
  cHttpCallTrackerParam = 'calltracker';
  cHttpAgentIdParam = 'agentid';
  cHttpDialledNumberParam = 'diallednumber';
  cHttpAgentNumberParam = 'agentnumber';
  cHttpRouteNameParam = 'routename';
  cHttpSystemNameParam = 'systemname';
  cNoIniFile = 'Unable to locate ini file %s';
  cRouteHandledEnvVar = 'ROUTEHANDLED';

function ParamToBool(const param: String; const default: Boolean): Boolean;
function ParamToInt(const param: String; const default: Integer): Integer;

procedure SetStr(var str: String; const aoc: TBytes); overload;
procedure SetStr(var str: String; const aoc: array of AnsiChar); overload;
procedure SetArray(var aoc: TBytes; const str: String; const maxLen: Integer); overload;
procedure SetArray(var aoc: array of AnsiChar; const str: String; const maxLen: Integer); overload;

function ForceForegroundWindow(lnHWND: HWND): Boolean;

{$IFDEF VER180}
function GetLongPathName(ShortPathName: PChar; LongPathName: PChar; cchBuffer: Integer): Integer; stdcall; external kernel32 name 'GetLongPathNameW';
{$ENDIF}


implementation

uses
  AnsiStrings, Math;

procedure SetStr(var str: String; const aoc: TBytes);
var
  l: Integer;
begin
  l := Length(aoc);
  if l > 0 then
    str := String(AnsiStrings.StrPas(PAnsiChar(@aoc[0])))
  else
    str := '';
end;

procedure SetStr(var str: String; const aoc: array of AnsiChar);
var
  l: Integer;
begin
  l := Length(aoc);
  if l > 0 then
    str := String(AnsiStrings.StrPas(aoc))
  else
    str := '';
end;

procedure SetArray(var aoc: TBytes; const str: String; const maxLen: Integer);
var
  sLen: Integer;
  aStr: AnsiString;
begin
  FillChar(aoc, SizeOf(aoc), 0);
  sLen := Length(str);
  if sLen > 0 then begin
    aStr := AnsiString(str);
    Move(aStr[1], aoc[0], Min(Length(aStr), maxLen));
  end;
end;

procedure SetArray(var aoc: array of AnsiChar; const str: String; const maxLen: Integer);
var
  sLen: Integer;
  aStr: AnsiString;
begin
  sLen := Length(str);
  if sLen > 0 then begin
    aStr := AnsiString(str);
    Move(aStr[1], aoc[0], Min(Length(aStr), maxLen));
  end;
end;

function ForceForegroundWindow(lnHWND: HWND): Boolean;
var
  ForeThread: Cardinal;
  AppThread: Cardinal;
begin
  ForeThread := GetWindowThreadProcessId(GetForegroundWindow(), NIL);
  AppThread := GetCurrentThreadId();
  if ForeThread <> AppThread then begin
    if AttachThreadInput(ForeThread, AppThread, True) then begin
      if BringWindowToTop(lnHWND) then
        result := ShowWindow(lnHWND, 0)
      else
        result := False;
      AttachThreadInput(ForeThread, AppThread, False);
    end else
      result := False;
  end else begin
    if BringWindowToTop(lnHWND) then
      result := ShowWindow(lnHWND, 0)
    else
      result := False;
  end;
end;

function ParamToBool(const param: String; const default: Boolean): Boolean;
begin
  if UpperCase(param) = 'Y' then
    result := true
  else if UpperCase(param) = 'N' then
    result := false
  else if UpperCase(param) = 'YES' then
    result := true
  else if UpperCase(param) = 'NO' then
    result := false
  else if param = '1' then
    result := true
  else
    result := default;
end;

function ParamToInt(const param: String; const default: Integer): Integer;
begin
  try
    result := StrToInt(param);
  except
    result := default;
  end;
end;

end.
