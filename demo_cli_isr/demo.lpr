program demo;

//Example using raspberry serial port

{$mode objfpc}{$H+}

uses
 // {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
 // {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, fpTimer,
  cli_keyboard, gpiolib, gpioisr
  ;

type
TDemo = class(TCustomApplication)
protected
  procedure DoRun; override;
private
  kbd: TKeyBoard;
  gpio: TGpioMap;
  timer1: TFpTimer;
  isr: TGpioCallback;

  test: integer;
  procedure KeyChar(c: char);
  procedure Timer1Event(Sender: TObject);

  procedure EdgeDetected(r, f: LongWord);
public
  constructor Create(TheOwner: TComponent); override;
  destructor Destroy; override;
end;

//Example-Timer
procedure TDemo.KeyChar(c: char);
begin
  if c = 'q' then Terminate();
end;

procedure TDemo.Timer1Event(Sender: TObject);
begin
  writeln('XXXXX');
end;

procedure TDemo.EdgeDetected(r, f: LongWord);
begin
  writeln('######');
end;

procedure TDemo.DoRun;
var
  err, i: integer;
begin

  //Read StdIn-Char in Thread ('q' => Terminate)
  kbd:= TKeyBoard.Create();
  kbd.onKeyCharRcvd:= @KeyChar;

  //Timer-Example
  timer1:= TFpTimer.Create(self);
  timer1.OnTimer:= @Timer1Event;
  timer1.Interval:= 3000;
//  timer1.Enabled:= true;

  //Mux BCM17 & 27 as input / pullup
  gpio:= TGpioMap.Create;
  gpio.SetFSel(17, fsel_input);
  gpio.SetPull(17, pull_Up);
  gpio.SetFSel(27, fsel_input);
  gpio.SetPull(27, pull_Up);
  gpio.Free;

  test:= 0;

  isr:= TGpioCallback.Create();
  isr.onEdgeDetected:= @EdgeDetected;
  err:= isr.AddGpio(17, edBoth);
  writeln('AddErr: ', err);
  isr.Open(); //Start Thread


  while not terminated do begin
    CheckSynchronize(1000); //Syncronize - Serial and readkey(ssh-input)

  end; //mainloop
  writeln ('Terminate Program');
  isr.Free;

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

