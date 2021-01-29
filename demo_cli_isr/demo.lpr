program demo;

{
  Example GPIO-Monitoring - similar ISR
  Rotation-Encoder ALPS conected to GPIO 17 + 27 / GND

  https://tech.alpsalpine.com/prod/e/pdf/encoder/incremental/ec20a/ec20a.pdf
}

{$mode objfpc}{$H+}

uses
 // {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
 // {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, fpTimer,
  cli_keyboard, gpiolib, gpioisr
  ;

const
  TermA = 17; //GPIO Pins Rotary Encoder Alps
  TermB = 27;

type
TDemo = class(TCustomApplication)
protected
  procedure DoRun; override;
private
  kbd: TKeyMonitor;
  gpio: TGpioMap;
  gpio_mon: TGpioMonitor;

  toutRotation: TFpTimer;
  Fvalue: integer;
  Rotation: integer;

  procedure SetValue(v: integer);
  procedure KeyDetected(c: char);
  procedure RotTimeout(Sender: TObject);
  procedure EdgeDetected(ev: TGpioEvent);
public
  property Value: integer read FValue write SetValue;
  constructor Create(TheOwner: TComponent); override;
  destructor Destroy; override;
end;

procedure TDemo.SetValue(v: integer);
begin
  FValue:= v;
  // **** userprogram ****
  writeln(Value, #13);
end;

//Monitor Keyboard- / ssh- Events
procedure TDemo.KeyDetected(c: char);
begin
  case c of
    'q',#3: Terminate();
    '0'   : Value:= 0;
  end;
end;

//50ms without rotationchange
procedure TDemo.RotTimeout(Sender: TObject);
begin
  toutRotation.Enabled:= false;
  Rotation:= 0;
end;

procedure TDemo.EdgeDetected(ev: TGpioEvent);
begin
  if (ev.gpio = TermB) and (Rotation = 0) then begin
    if gpio.GetValue(TermA) = gpio.GetValue(TermB)
      then Rotation:= -1
      else Rotation:= 1;
    Value:= Value + Rotation;
    toutRotation.Enabled:= true;
  end;
  if (ev.gpio = TermA) and (Rotation <> 0) then Rotation:= 0;
end;


procedure TDemo.DoRun;
begin

  //Read StdIn-Char in Thread ('q' => Terminate)
  kbd:= TKeyMonitor.Create();
  kbd.onKeyCharRcvd:= @KeyDetected;

  //corr direction change
  toutRotation:= TFpTimer.Create(Self);
  toutRotation.OnTimer:= @RotTimeOut;
  toutRotation.Interval:= 50;
  toutRotation.Enabled:= false;;

  //Mux BCM17 & 27 as input & pullup
  gpio:= TGpioMap.Create;
  gpio.SetFSel(TermA, fsel_input);
  gpio.SetPull(TermA, pull_Up);
  gpio.SetFSel(TermB, fsel_input);
  gpio.SetPull(TermB, pull_Up);

  gpio_mon:= TGpioMonitor.Create();
  gpio_mon.onEdgeDetected:= @EdgeDetected;
  gpio_mon.useSynchronize:= false;
  gpio_mon.Delay:= 3;
  gpio_mon.AddGpio(TermA, edBoth);
  gpio_mon.AddGpio(TermB, edBoth);
  gpio_mon.Open(); //Start Thread

  Value:= 0;
  Rotation:= 0;

  while not terminated do begin
    CheckSynchronize(1000); //Syncronize - Serial and readkey(ssh-input)

  end; //mainloop
  writeln ('Terminate Program');

  gpio_mon.Free;
  gpio.Free;

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

