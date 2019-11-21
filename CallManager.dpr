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
library CallManager;

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

uses
  SysUtils,
  Classes,
  Types,
  Windows,
  Math,
  CallManagerStatus in 'CallManagerStatus.pas' {fCallManagerStatus},
  Rdp in 'Rdp.pas',
  CallMessage in 'CallMessage.pas',
  ClientVirtualChannel in 'ClientVirtualChannel.pas',
  ChannelData in 'ChannelData.pas',
  SharedDefinitions in 'SharedDefinitions.pas',
  VirtualChannel in 'VirtualChannel.pas';

{$R *.res}

exports ClientVirtualChannel.VirtualChannelEntry;
exports CallManagerStatus.TestFetchHandle;
exports CallManagerStatus.OpenStatus;
exports CallManagerStatus.CloseStatus;
exports CallManagerStatus.SetupRoute;
exports CallManagerStatus.SimulateRdpAppMessage;

procedure OnEntry;
begin
  ClientVirtualChannel.Initialise;
  fCallManagerStatus := TfCallManagerStatus.Create(nil);
  ClientVirtualChannel.OnStatus := fCallManagerStatus.SetStatus;
  fCallManagerStatus.SetStatus(5, 'OnEntry ' + DateTimeToStr(Now));
  fCallManagerStatus.SetStatus(5, 'Ini file is ' + IniFileName);
end;

procedure OnTerminate;
begin
  ClientVirtualChannel.OnStatus := nil;
end;

begin
  ClientVirtualChannel.OnEntry := OnEntry;
end.
