unit cli_keyboard;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, crt;



type

//Callback type
TKeyChar = procedure(c: char) of object;

TKeyboard = class (TThread)
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

constructor TKeyboard.Create();
begin
  inherited Create(false);
  FreeOnTerminate:= true;
  onKeyCharRcvd:= nil;
end;

destructor  TKeyBoard.Destroy();
begin
  inherited Destroy();
end;

procedure TKeyBoard.Execute();
begin
  while not Terminated do begin
    KeyChar:= ReadKey();
    synchronize(@KeyCharRcvd);
  end;
end;

//Notifier
procedure TKeyBoard.KeyCharRcvd;
begin
  if Assigned(FOnKeyCharRcvd) then FOnKeyCharRcvd(KeyChar);
end;

end.

