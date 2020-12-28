unit piuartlib;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  BaseUnix, Unix, Linux, TermIO
  ;

{$include serial_def.inc}

type
TCtrl_Line = (             // DTE - Data Terminal
  ctrl_dtr = TIOCM_DTR,    // --> Data Terminal Ready
  ctrl_rts = TIOCM_RTS,    // --> Request To Send
  ctrl_cts = TIOCM_CTS,    // <-- Clear To Send
  ctrl_dsr = TIOCM_DSR     // <-- Data Set Ready
 );

TLogical_State = (  //RS232-Pegel      FTDI232
  log_off = 0,      //-3V .. -25V      3V
  log_on  = 1       //+3V .. +25V      0V
);


type
//Callback type
TDataReceived = procedure(line: ansistring) of object;

{
TBlockRead = class (TThread)
protected
  procedure Execute; override;
private
  hDevice: THandle;
  FuseSynchronize: boolean;
  FOnDataReceived: TDataReceived;
public
  property onDataRcvd: TDataReceived read FOnDataReceived write FOnDataReceived;
  property useSynchronize: boolean write FuseSynchronize;

  constructor Create(dev: THandle);
  destructor  Destroy(); override;
end;
}

TSerial = class  (TThread)
protected
  procedure Execute; override;

private
  FDevice: string;
  FuseSynchronize: boolean;
  FOnDataReceived: TDataReceived;

  hDevice: THandle;
  tios: Termios;
  rxLine: string;
//  blockread: TBlockRead;
  function config(bd: TBaudrate; frame: string): integer;
  procedure DataReceived;
public
  property Device: string read FDevice;

  property onDataRcvd: TDataReceived read FOnDataReceived write FOnDataReceived;
  property useSynchronize: boolean write FuseSynchronize;

  constructor Create(dev: string);
  Destructor Destroy; override;

  function  Open(bd: TBaudrate; frame: string = '8N1'): integer;
//  function  Open(cbd: LongWord; frame: string = '8N1'): integer; overload;
//  procedure Close();

  function NumRead(): integer;
  function Flush(): integer;

  function SetCtrl(ctrl_line: TCtrl_Line; state: TLogical_State): integer;
  function GetCtrl(ctrl_line: TCtrl_Line): TLogical_State;

  function WriteBuf(const buf: array of byte; len:integer): integer;
//  function ReadBuf (var buf: array of byte; len:integer): integer;

end;

implementation

constructor TSerial.Create(dev: string);
begin
  inherited Create(true);   //Start after sucessfull Open
  FDevice:= dev;
  FreeOnTerminate := true;
  FOnDataReceived:= nil;
end;

destructor TSerial.Destroy;
begin
  if hDevice >= 0 then fpclose(hDevice);
  inherited Destroy();
end;


//  https://blog.mbedded.ninja/programming/operating-systems/linux/linux-serial-ports-using-c-cpp/
function TSerial.config(bd: TBaudrate; frame: string): integer;
var
  parity, sbits: char;
  cs: TCharSize;
begin
  result:= -1;

  cs:= cs_8;
  parity:= 'N';
  sbits:= '1';

  if length(frame)>0 then case frame[1] of
    '5' : cs:= cs_5;
    '6' : cs:= cs_6;
    '7' : cs:= cs_7;
  end;
  if length(frame)>1 then
    parity:= UpperCase(frame)[2];
    if not (parity in ['E', 'O']) then parity:= 'N';

  if length(frame)>2 then
    if frame[3] = '2' then sbits:= '2';


  hDevice:= THandle(fpOpen(FDevice, O_RDWR{ or O_SYNC or O_NOCTTY or O_NDELAY}));
  //ReadBuf back the current tios of the device
  if fpIOCtl(hDevice, TCGETS, @tios) <> 0 then exit; //return -1

  //control modes c_cflag
  //Parity
  case parity of
    'N':  begin
            tios.c_cflag:= tios.c_cflag and not PARENB; //clear enable bit
          end;
    'O':  begin
            tios.c_cflag:= tios.c_cflag or PARENB;      //set enable bit
            tios.c_cflag:= tios.c_cflag or PARODD;      //set odd bit
          end;
    'E':  begin
            tios.c_cflag:= tios.c_cflag or PARENB;      //set enable bit
            tios.c_cflag:= tios.c_cflag and not PARODD; //clear odd bit
          end;
  end;
  //Stopbits
  if sbits = '2'
    then tios.c_cflag:= tios.c_cflag or CSTOPB          //two stopbits
    else tios.c_cflag:= tios.c_cflag and not CSTOPB;    //one stopbit
  //Charactersize
  tios.c_cflag:= tios.c_cflag and not CSIZE;            //clear all the size bits
  tios.c_cflag:= tios.c_cflag or integer(cs);           //set size
  //HW Flowcontrol
  tios.c_cflag:= tios.c_cflag and not CRTSCTS;          //Disable RTS/CTS
  //Enable the receiver and set local mode
  tios.c_cflag:= (tios.c_cflag  or CLOCAL or CREAD);
  //Baudrate, In- & Out-Speed same bits in c_cflag
  tios.c_cflag:= tios.c_cflag and not CBAUD;            //clear all speed bits
  tios.c_cflag:= tios.c_cflag or integer(bd);           //set speed bits

  //Local Modes (c_lflag)
  //disable Canonical Mode (Do not wait for CR)
  tios.c_lflag:= tios.c_lflag and not ICANON;
  //disable Echo Mode
  tios.c_lflag:= tios.c_lflag and not ECHO;
  //disable interpretation of INTR, QUIT and SUSP
  tios.c_lflag:= tios.c_lflag and not ISIG;

  //Input Modes (c_iflag)
  //disable SW flow ctrl
  tios.c_iflag:= tios.c_iflag and not (IXON or IXOFF or IXANY);
  //disable any special handling of received bytes
  tios.c_iflag:= tios.c_iflag and
    not (IGNBRK or BRKINT or PARMRK or ISTRIP or INLCR or IGNCR or ICRNL);

  //Output Modes (c_oflag)
  //Prevent special interpretation of output bytes (e.g. newline chars)
  tios.c_oflag:= tios.c_oflag and not OPOST;
  //Prevent conversion of newline to carriage return/line feed
  tios.c_oflag:= tios.c_oflag and not ONLCR;

  //VMIN and VTIME (c_cc)
  tios.c_cc[VMIN]:=  0;     //Return for every received character
  tios.c_cc[VTIME]:= 50;    //Rx-Timeout n*0.1sek
  result:= 0;
end;

//Open wirh standard baudrate
function  TSerial.Open(bd: TBaudrate; frame: string = '8N1'): integer;
begin
  result:= -1;
  if config(bd, frame) = 0 then begin
    if fpIOCtl(hDevice, TCSETS, @tios) = 0 then begin //0 => success
//      blockread:= TBlockRead.Create(hDevice);
      self.Start;
      result:= 0;
    end;
  end;
end;

{
//Open with custom baudrate   !!! don't work !!!
function  TSerial.Open(cbd: LongWord; frame: string = '8N1'): integer;
begin
  result:= -1;
   if config(BD_EX, frame) = 0 then begin
    cfg_tty.c_ispeed:= cbd;
    cfg_tty.c_ospeed:= cbd;
    result:= fpIOCtl(hDevice, TCSETS, @cfg_tty); //0 => success
  end;
end;
}
{
procedure TSerial.close();
begin

end;
}

function TSerial.Flush(): integer;
begin
  result:= fpIOCtl(hDevice, TCFLSH, nil);   //nil ??
end;

function TSerial.NumRead(): integer;
begin
  fpIOCtl(hDevice, FIONREAD, @result);
end;

function TSerial.SetCtrl(ctrl_line: TCtrl_Line;  state: TLogical_State): integer;
var ctrl, err: integer;
begin
  err:= fpIOCtl(hDevice, TIOCMGET, @ctrl);
  if state = log_on
    then ctrl:= ctrl or integer(ctrl_line)
    else ctrl:= ctrl and not integer(ctrl_line);
  err:= err or fpIOCtl(hDevice, TIOCMSET, @ctrl);
  result:= err;
end;

function TSerial.GetCtrl(ctrl_line: TCtrl_Line): TLogical_State;
var ctrl: integer;
begin
  fpIOCtl(hDevice, TIOCMGET, @ctrl);
  if (ctrl and integer(ctrl_line)) <> 0
    then result:= log_on
    else result:= log_off;
end;



function TSerial.WriteBuf(const buf: array of byte; len:integer): integer;
begin
  result:= fpWrite(hDevice, buf, len);
end;

//Notifier
procedure TSerial.DataReceived;
begin
  if Assigned(FOnDataReceived) then FOnDataReceived(rxLine);
end;

procedure TSerial.Execute();
var
  i, n: integer;
  buf: array [0..255] of byte;
begin
  while not terminated do begin
    n:= fpRead(hDevice, buf, 255);
    for i:= 0 to n-1 do write(buf[i], '.');
    writeln('#');
  end;
end;

//-----------------------------------------------------------------------------

end.

