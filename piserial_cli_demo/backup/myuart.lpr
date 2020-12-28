program myuart;

{$mode objfpc}{$H+}

uses
 // {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
 // {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  piuartlib
  ;

type

TRPi_UART = class(TCustomApplication)
protected
  procedure DoRun; override;
private
  uart: TSerial;
public
  constructor Create(TheOwner: TComponent); override;
  destructor Destroy; override;
  procedure WriteHelp; virtual;
end;

{ TRPi_UART }

procedure TRPi_UART.DoRun;
var
  ErrorMsg: String;
  err, n, i: integer;
  b: TLogical_State;
  ctrl: TCtrl_Line;
  buf: array of byte;
  rbuf: array[0..256] of byte;
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

  uart:= TSerial.Create('/dev/ttyUSB0');
  err:= uart.open(Bd2400, '8N1');
  if err < 0 then begin
    writeln('Error: open serial port');
    Terminate;
    exit;
  end;

  n:= uart.Flush();
  n:= uart.NumRead();

  ctrl:= ctrl_dtr;
{
  b:= uart.GetCtrl(ctrl);

  n:= uart.SetCtrl(ctrl, log_on);
  b:= uart.GetCtrl(ctrl);
  n:= uart.SetCtrl(ctrl, log_off);
  b:= uart.GetCtrl(ctrl);
  n:= uart.SetCtrl(ctrl, log_on);
  b:= uart.GetCtrl(ctrl);
  n:= uart.SetCtrl(ctrl, log_off);
  b:= uart.GetCtrl(ctrl);
 }
  setlength(buf, 64);
  for i:= 0 to 63 do buf[i]:= ord('A')+i;

  n:= uart.SetCtrl(ctrl, log_off);
  uart.WriteBuf(buf, 64);
  n:= uart.SetCtrl(ctrl, log_on);

  sleep(10000);

//  n:= uart.NumRead();

//  n:= uart.ReadBuf(buf, 10);

//  uart.close();
  uart.Free;

  Terminate;
end;

constructor TRPi_UART.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TRPi_UART.Destroy;
begin
  inherited Destroy;
end;

procedure TRPi_UART.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: TRPi_UART;
begin
  Application:=TRPi_UART.Create(nil);
  Application.Title:='UART Test';
  Application.Run;
  Application.Free;
end.

