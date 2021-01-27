unit cli_keyboard;

{$mode objfpc}{$H+}

{ ***************************************************************************
  Monitor Keyboard-Event in Thread
  Return ASCII-Code
  ---------------------------------
  Usage:

  procedure TDemo.KeyChar(c: char);
  begin
    if c = 'q' then Terminate();
  end;

  procedure TDemo.DoRun;
  begin
    kbd:= TKeyMonitor.Create();
    kbd.onKeyCharRcvd:= @KeyChar;
    ....
    while not terminated do begin
      CheckSynchronize(1000);
      ...
    end; //mainloop
    //FreeOnTerminate !!!
  ***************************************************************************
}

interface

uses
  Classes, SysUtils, crt;



type

//Callback type
TKeyChar = procedure(c: char) of object;

TKeyMonitor = class (TThread)
protected
  procedure Execute(); override;
private
  KeyChar: char;
  FOnKeyCharRcvd: TKeyChar;
  procedure KeyCharRcvd;

public
  property onKeyCharRcvd: TKeyChar read FOnKeyCharRcvd write FOnKeyCharRcvd;
  constructor Create();
  destructor  Destroy(); override;

end;

implementation

constructor TKeyMonitor.Create();
begin
  inherited Create(false);
  FreeOnTerminate:= true;
  onKeyCharRcvd:= nil;
end;

destructor  TKeyMonitor.Destroy();
begin
  inherited Destroy();
end;

procedure TKeyMonitor.Execute();
begin
  while not Terminated do begin
    //blocked reading
    KeyChar:= ReadKey();
    synchronize(@KeyCharRcvd);
  end;
end;

//Notifier
procedure TKeyMonitor.KeyCharRcvd;
begin
  if Assigned(FOnKeyCharRcvd) then FOnKeyCharRcvd(KeyChar);
end;

end.

