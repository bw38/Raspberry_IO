program demo;

{$mode objfpc}{$H+}

uses
 // {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
 // {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  tuxserial, cli_keyboard, fpTimer
  ;

type

TDemo = class(TCustomApplication)
protected
  procedure DoRun; override;
private
  uart: TSerial;
  kbd: TKeyBoard;
  timer1: TFpTimer;
  test: integer;
  procedure RxData(rxLine: array of Byte);
  procedure KeyChar(c: char);

  procedure Timer1Event(Sender: TObject);
public
  constructor Create(TheOwner: TComponent); override;
  destructor Destroy; override;
  procedure WriteHelp; virtual;
end;

procedure TDemo.KeyChar(c: char);
begin
  if c = 'q' then Terminate();
end;

procedure TDemo.RxData(rxLine: array of Byte);
var i: integer;
begin
  for i:= 0 to length(rxLine)-1 do write(format('%.02X', [rxLine[i]]), '.');
  writeln('***', length(rxLine));
  test:= test + length(rxLine);
end;

procedure TDemo.Timer1Event(Sender: TObject);
begin
  writeln('Bytes read: ', test);
end;

procedure TDemo.DoRun;
var
  ErrorMsg: String;
  err, n, i: integer;
  b: TLine_State;
  ctrl: TCtrl_Line;
  buf: array of byte;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h', 'help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  //Read StdIn-Char in Thread
  kbd:= TKeyBoard.Create();
  kbd.onKeyCharRcvd:= @KeyChar;

  //Timer-Example
  timer1:= TFpTimer.Create(self);
  timer1.OnTimer:= @Timer1Event;
  timer1.Interval:= 3000;
  timer1.Enabled:= true;

  test:= 0;
  //Readback-Loop - Jumper Tx<->-Rx
  uart:= TSerial.Create('/dev/ttyAMA0');
  uart.onDataRcvd:= @RxData;            // !!! mode objfpc => "@" !!!
  uart.Baudrate:= Bd115200;
  uart.DataBits:= CS_8;
  uart.Parity:= paNone;
  uart.FlowControl:= fcNone;

  err:= uart.open();
  if err < 0 then begin
    writeln('Error: open serial port');
    Terminate;
    exit;
  end;
  writeln('Err: ', err);
  n:= uart.Flush();

  ctrl:= clRTS;

  setlength(buf, 10000);
  for i:= 0 to length(buf)-1 do buf[i]:= i and $FF; // ord('A')+i;

  n:= uart.SetCtrl(ctrl, lsOn);
  uart.WriteData(buf);
  n:= uart.SetCtrl(ctrl, lsOff);

  while not terminated do begin
    CheckSynchronize(1000); //Sycronize Serial and readkey / ssh-input

  end; //mainloop
  writeln ('Terminate Program');

  uart.Free;
  kbd.Free;

  Terminate;
end;

constructor TDemo.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TDemo.Destroy;
begin
  inherited Destroy;
end;

procedure TDemo.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: TDemo;
begin
  Application:=TDemo.Create(nil);
  Application.Title:='UART Test';
  Application.Run;
  Application.Free;
end.

