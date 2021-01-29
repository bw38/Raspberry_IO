unit tuxserial;

{******************************************************************************
  Serial Interface for Linux / PiOS
  ----------------------------------
  Usage:
  procedure TDemo.RxData(rxLine: array of Byte);
  begin
    ...
  end;

  procedure TDemo.DoRun;
  begin
    uart:= TSerial.Create('/dev/ttyAMA0');
    uart.onDataRcvd:= @RxData;   //Callback Addr
    uart.Baudrate:= Bd115200;
    uart.DataBits:= CS_8;
    uart.Parity:= paNone;
    uart.FlowControl:= fcNone;
    if uart.Open = 0 then begin
      ....
      while not terminated do begin
        CheckSynchronize(1000);
        ...
      end; //mainloop
    end;
    uart.Free;
  end;
  *****************************************************************************
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, termio, BaseUnix;


//Standarddefinitionen der Seriellen Schnittstellen
//übernommen aus termio.inc

//Standardbaurates
//Termios definiert nicht alle Baudrates in verschiedenen Prozessor-Plattformen
type

TBaudrate = (
   Bd0      = $0000000,
   Bd50     = $0000001,
   Bd75     = $0000002,
   Bd110    = $0000003,
   Bd134    = $0000004,
   Bd150    = $0000005,
   Bd200    = $0000006,
   Bd300    = $0000007,
   Bd600    = $0000008,
   Bd1200   = $0000009,
   Bd1800   = $000000A,
   Bd2400   = $000000B,
   Bd4800   = $000000C,
   Bd9600   = $000000D,
   Bd19200  = $000000E,
   Bd38400  = $000000F,
   Bd_EX    = $0001000,    //customer baudrate
   Bd57600  = $0001001,
   Bd115200 = $0001002,
   Bd230400 = $0001003,
   Bd460800 = $0001004,
   Bd500000 = $0001005,
   Bd576000 = $0001006,
   Bd921600 = $0001007,
   Bd1000000= $0001008,
   Bd1152000= $0001009,
   Bd1500000= $000100A,
   Bd2000000= $000100B,
   Bd2500000= $000100C,
   Bd3000000= $000100D,
   Bd3500000= $000100E,
   Bd4000000= $000100F
);

// Bit per Character
TDataBits = (
  CS_5   = $0000000,
  CS_6   = $0000010,
  CS_7   = $0000020,
  CS_8   = $0000030
);

TFlowControl=(fcNone,fcHardware); //xon/xoff not implemented

TParity=(paNone, paOdd, paEven);  //Parity doesn't work  on miniUART

TStopBits=(sbOne,sbTwo); 					//only one Stopbit possible on miniUART


TCtrl_Line = (          // DTE - Data Terminal
  clDTR = TIOCM_DTR,    // --> Data Terminal Ready
  clRTS = TIOCM_RTS,    // --> Request To Send
  clCTS = TIOCM_CTS,    // <-- Clear To Send
  clDSR = TIOCM_DSR     // <-- Data Set Ready
 );

TLine_State = (         //RS232-Pegel      FTDI232
  lsOff = 0,            //-3V .. -25V      3V
  lsOn  = 1             //+3V .. +25V      0V
);


type
//Callback type
TDataReceived = procedure(rxLine: array of Byte) of object;


type
TSerial = class(TThread)
	private
  	FfdDev: integer;
    strDev: string;
    FBaudrate: TBaudRate;
    FDataBits: TDataBits;
    FParity:   TParity;
    FStopbits: TStopbits;

    FFlowControl: TFlowControl;
    FRxTimeout : integer;
    FuseSynchronize: boolean;

    FOnDataReceived: TDataReceived;
    RxLine: array of Byte;
    procedure DataReceived;
  protected
    procedure Execute; override;

  public
    property fdDev: integer read FfdDev;  //FileDescriptor
    property Baudrate: TBaudrate read FBaudrate write FBaudrate;
    property DataBits: TDataBits read FDataBits write FDataBits;
    property Parity: TParity read FParity write FParity;
		property Stopbits: TStopbits read FStopbits write FStopbits;
    property FlowControl: tFlowControl read FFLowControl write FFLowControl;

    property onDataRcvd: TDataReceived read FOnDataReceived write FOnDataReceived;
    property UseSynchronize: boolean write FuseSynchronize;

    property device: string read strDev;

    constructor Create(dev: string);
    destructor Destroy; override;

    function Open(): integer;
    function Flush(): integer;
    function WriteData(buf: array of Byte): integer;
    function WriteString(s: string): integer;

    function SetCtrl(cl: TCtrl_Line;  ls: TLine_State): integer;
    function GetCtrl(cl: TCtrl_Line): TLine_State;

 end;

implementation

constructor TSerial.Create(dev: string);
begin
  inherited create(true); 	//do not start thread immediatly

  FfdDev:= -1;
  strDev:= dev;
  //Set default Parameters for UART0 115200bd 8N1
  //change before open
  Baudrate:= Bd115200;
  DataBits:= cs_8;
  Parity:= paNone;
  Stopbits:= sbOne;
  FlowControl:= fcNone;
  UseSynchronize:= true;  //switch off only in console-Programs

  onDataRcvd:= nil;
  FreeOnTerminate := false;
end;

destructor TSerial.Destroy;
var
  flags: integer;
  tios: TermIOS;
begin
  if fdDev >= -1 then begin
    Terminate;
    //unblock reading
    flags:= fpFcntl(fdDev, F_GETFL);
    fpFcntl(fdDev, F_SETFL, flags or O_NONBLOCK);
    //set blocktime to 0.1sek
    fpIOCtl(fdDev, TCGETS, @tios);
    tios.c_cc[VTIME]:= 1;
    fpIOCtl(FfdDev, TCSETS, @tios);
    self.WaitFor;
    fpClose(fdDev);
  end;
  inherited Destroy;
end;

// open serial port
// every property has to be choosen
// Return 0 => success
function TSerial.Open(): integer;
var tios: TermIOS;
begin
  result:= -1;
  FfdDev:= THandle(fpOpen(strDev, O_RDWR{ or O_SYNC or O_NOCTTY or O_NDELAY}));
  if fdDev < 0 then exit;

  //ReadBuf back the current tios of the strDev
  if fpIOCtl(fdDev, TCGETS, @tios) <> 0 then exit; //return -1

  //control modes c_cflag
  //Parity
  case parity of
    paNone :  begin
                tios.c_cflag:= tios.c_cflag and not PARENB; //clear enable bit
              end;
    paOdd :   begin
                tios.c_cflag:= tios.c_cflag or PARENB;      //set enable bit
                tios.c_cflag:= tios.c_cflag or PARODD;      //set odd bit
              end;
    paEven:   begin
                tios.c_cflag:= tios.c_cflag or PARENB;      //set enable bit
                tios.c_cflag:= tios.c_cflag and not PARODD; //clear odd bit
              end;
  end;
  //Stopbits
  if StopBits = sbTwo
    then tios.c_cflag:= tios.c_cflag or CSTOPB          //two stopbits
    else tios.c_cflag:= tios.c_cflag and not CSTOPB;    //one stopbit
  //Charsize
  tios.c_cflag:= tios.c_cflag and not CSIZE;            //clear all the size bits
  tios.c_cflag:= tios.c_cflag or integer(DataBits);     //set size
  //RTS - CTS - Flowcontrol
  if FlowControl = fcHardware
    then tios.c_cflag:= tios.c_cflag or CRTSCTS
    else tios.c_cflag:= tios.c_cflag and not CRTSCTS;   //Disable RTS/CTS

  //Enable the receiver and set local mode
  tios.c_cflag:= (tios.c_cflag  or CLOCAL or CREAD);
  //Baudrate, In- & Out-Speed same bits in c_cflag
  tios.c_cflag:= tios.c_cflag and not CBAUD;            //clear all speed bits
  tios.c_cflag:= tios.c_cflag or integer(BaudRate);           //set speed bits

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
  tios.c_cc[VTIME]:= 100;   //Rx-Timeout n*0.1sek

  if fpIOCtl(FfdDev, TCSETS, @tios) = 0 then begin //0 => success
    self.Start;
    Start;						//Start Reading - Thread
    result:= 0;       //return success
  end;
end;

function TSerial.Flush(): integer;
begin
  result:= fpIOCtl(fdDev, TCFLSH, nil);   //nil ??
end;


//Data Receive in Background
procedure TSerial.Execute;
var
  buf: array of byte;
  cnt, i: integer;
begin
  SetLength(buf, 255);
  Flush();
  while not terminated do begin
    cnt:= fpRead(fdDev, buf[0], length(buf));
    if (cnt > 0) then begin
      RxLine:= copy(buf, 0, cnt);
      //notify Mainthread
      if FuseSynchronize
        then synchronize(@DataReceived)
      	else @DataReceived;
    end;
  end;
end;

//Notifier
procedure TSerial.DataReceived;
begin
  if Assigned(FOnDataReceived) then FOnDataReceived(RxLine);
end;


//Data Send
//write up to 4k data, more will block until written
function TSerial.WriteData(buf: array of Byte): integer;
begin
  if fdDev > 0 then result:= fpWrite(fdDev, buf, length(buf));
end;


//String schreiben
function TSerial.WriteString(s: string): integer;
var
  buf: array of byte;
  i: integer;
begin
  setlength(buf, length(s));
  for i:= 1 to length(s) do buf[i-1]:= byte(s[i]);
  result:= WriteData(buf);
end;


//CTRL-IO
function TSerial.SetCtrl(cl: TCtrl_Line;  ls: TLine_State): integer;
var ctrl, err: integer;
begin
  err:= fpIOCtl(fdDev, TIOCMGET, @ctrl);
  if ls = lsOn
    then ctrl:= ctrl or integer(cl)
    else ctrl:= ctrl and not integer(cl);
  err:= err or fpIOCtl(fdDev, TIOCMSET, @ctrl);
  result:= err;
end;

function TSerial.GetCtrl(cl: TCtrl_Line): TLine_State;
var ctrl: integer;
begin
  fpIOCtl(fdDev, TIOCMGET, @ctrl);
  if (ctrl and integer(cl)) <> 0
    then result:= lsOn
    else result:= lsOff;
end;



end.

