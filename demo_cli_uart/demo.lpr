program demo;

//Example using raspberry serial port

{$mode objfpc}{$H+}

uses
 // {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
 // {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, fpTimer,
  tuxserial, cli_keyboard, gpiolib, gpioisr
  ;

type
TDemo = class(TCustomApplication)
protected
  procedure DoRun; override;
private
  uart: TSerial;
  kbd: TKeyMonitor;
  gpio: TGpioMap;

  timer1: TFpTimer;
  test: integer;
  procedure RxData(rxLine: array of Byte);
  procedure KeyChar(c: char);

  procedure Timer1Event(Sender: TObject);
public
  constructor Create(TheOwner: TComponent); override;
  destructor Destroy; override;
end;

//Example-Timer
procedure TDemo.KeyChar(c: char);
begin
  if c = 'q' then Terminate();
end;

//Read serial data - syncronized with mainthread
//Loop back fram written data
procedure TDemo.RxData(rxLine: array of Byte);
var i: integer;
begin
  for i:= 0 to length(rxLine)-1 do write(format('%.02X', [rxLine[i]]), '.');
  writeln('***', length(rxLine)); //length per read block
  test:= test + length(rxLine);   //overall read bytes
end;

procedure TDemo.Timer1Event(Sender: TObject);
begin
  writeln('Bytes read: ', test);
end;

procedure TDemo.DoRun;
var
  err, i: integer;
  b: TLine_State;
  ctrl: TCtrl_Line;
  buf: array of byte;
begin

  //Read StdIn-Char in Thread ('q' => Terminate)
  kbd:= TKeyMonitor.Create();
  kbd.onKeyCharRcvd:= @KeyChar;

  //Timer-Example
  timer1:= TFpTimer.Create(self);
  timer1.OnTimer:= @Timer1Event;
  timer1.Interval:= 3000;
  timer1.Enabled:= true;

  //Mux BCM16(CTS0) & BCM17(RTS0) to Alt3
  gpio:= TGpioMap.Create;
  gpio.SetFSel(16, fsel_alt3);
  gpio.SetValue(16, 1);
  gpio.SetFSel(17, fsel_alt3);
  gpio.SetValue(17, 1);
  gpio.Free;

  test:= 0;
  //Readback-Loop - Jumper Tx<->-Rx
  uart:= TSerial.Create('/dev/ttyAMA0');
  uart.onDataRcvd:= @RxData;            // !!! mode objfpc => "@" !!!
  //default properties
  //uart.Baudrate:= Bd115200;
  //uart.DataBits:= CS_8;
  //uart.Parity:= paNone;
  //uart.FlowControl:= fcNone;

  err:= uart.open();
  if err < 0 then begin
    writeln('Error: open serial port');
    Terminate;
    exit;
  end;

  uart.Flush();

  ctrl:= clRTS; //Test Osizilloscope, write blocking on large blocks
{
  setlength(buf, 10000);     //Tx-Test-Buffer
  for i:= 0 to length(buf)-1 do buf[i]:= i and $FF;

  uart.SetCtrl(ctrl, lsOn);
  uart.WriteData(buf);
  uart.SetCtrl(ctrl, lsOff);
 }

 uart.WriteString('Hello World');


  while not terminated do begin
    CheckSynchronize(1000); //Syncronize - Serial and readkey(ssh-input)

  end; //mainloop
  writeln ('Terminate Program');

  uart.Free;
//  kbd.Free;

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


var
  Application: TDemo;
begin
  Application:=TDemo.Create(nil);
  Application.Title:='UART Test';
  Application.Run;
  Application.Free;
end.

