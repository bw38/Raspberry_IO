unit piSerial;

{
http://wiki.freepascal.org/Multithreaded_Application_Tutorial/de
}


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, termio, BaseUnix;

{
type
  TBaudRate=
    (B50, B75, B110, B300, B600, B1200, B2400, B4800,
     B9600, B19200, B38400, B57600, B115200, B230400,
     B460800, B921600);
const
  ConstsBaud: array[TBaudRate] of integer=
    ( 50,  75,  110,  300,  600,  1200,  2400,  4800,
      9600,  19200,  38400,  57600,  115200,  230400,
      460800, 921600);
  //weitere Baudrates -> s. wiringSerial.c

type
  TDataBits=(db8, db7, db6, db5);   //db6 & db5 doesn't work  on UART1
  TParity=(paNone, paOdd, paEven);  //Parity doesn't work  on UART1
  TStopBits=(sbOne,sbTwo); 					//only one Stopbit possible on UART1

}

{$include serial_def.inc}




type
	//Callback type
	TDataReceived = procedure(rxLine: array of Byte) of object;


type
TRS232 = class(TThread)
  protected
    RxLine: array of Byte;
    procedure DataReceived;
	private
  	Ffd: integer;
//    FCntRx: integer;

    FBaudrate: TBaudRate;
    FDataBits: TDataBits;
    FParity:   TParity;
    FStopbits: TStopbits;

    FFlowControl: TFlowControl;
    FRxTimeout : integer;
    FuseSynchronize: boolean;

    FOnDataReceived: TDataReceived;


  protected
    procedure Execute; override;

  public
    Device: string;

    property fdDev: integer read Ffd;  //FileDescriptor
    property Baudrate: TBaudrate read FBaudrate write FBaudrate;
    property DataBits: TDataBits read FDataBits write FDataBits;
    property Parity: TParity read FParity write FParity;
		property Stopbits: TStopbits read FStopbits write FStopbits;

    property FlowControl: tFlowControl read FFLowControl write FFLowControl;
    property RxTimeout: integer read FRxTimeout write FRxTimeout;	//x * 0.1s

    property onDataRcvd: TDataReceived read FOnDataReceived write FOnDataReceived;
    property useSynchronize: boolean write FuseSynchronize;
//    property CntRx: integer read FCntRx;

    constructor Create(dev: string);
    destructor Destroy; override;

    function Open(): integer;
    function Flush(): integer;
    function NumRead(): integer;
    function WriteData(buf: array of Byte): integer;

    procedure SuspendTx(active: boolean);

    procedure SetRTS(State: Boolean);
    function  GetCTS(): Boolean;
 end;

implementation

constructor TRS232.Create(dev: string);
begin
  inherited create(true); 	//do not start thread immediatly

  Ffd:= -1;
  Device:= dev;
  //Set default Parameters for UART0 115200bd 8N1
  //change before open
  Baudrate:= Bd115200;
  DataBits:= cs_8;
  Parity:= paNone;
  Stopbits:= sbOne;
  FlowControl:= fcNone;

  RxTimeout:= 10;	//Timeout n * 0.1sec

  onDataRcvd:= nil;
  FreeOnTerminate := true;
end;

destructor TRS232.Destroy;
begin
  if fdDev >= -1 then begin
    Flush();
    fpClose(fdDev);
  end;
  inherited Destroy;
end;


function TRS232.Open(): integer;
var tios: TermIOS;
begin

  {
  bd:= ConstsBaud[Baudrate];

  Ffd:= serialOpen(pchar(Device), bd);
  tn:= ttyName(fd);

  //UART has to be activated und Pin-mapped outside !

  if pos('ttyAMA0', tn) > 0 then begin //UART0 map
    if FlowControl = fcHardware then begin
      pinModeAlt(0,  amAlt3); //RTS0
      pinModeAlt(27, amAlt3);	//CTS0
    end;
  end else if pos('ttyS0', tn) > 0 then begin //UART1 map
    if FlowControl = fcHardware then begin
      pinModeAlt(0,  amAlt5); //RTS1
      pinModeAlt(27, amAlt5);	//CTS1
    end;
  end;

  tcgetattr(fd, tios);             //unit termio
  tios.c_cc[VTIME]:= RxTimeout;    //rx timeout  n * 0.1sec

  tios.c_cflag := tios.c_cflag and not CSIZE;
  case DataBits of
    db5: tios.c_cflag:=tios.c_cflag or CS5;
    db6: tios.c_cflag:=tios.c_cflag or CS6;
    db7: tios.c_cflag:=tios.c_cflag or CS7;
    db8: tios.c_cflag:=tios.c_cflag or CS8;
  end;

  //Parity not available on UART1 (mini-UART)
  tios.c_cflag:= tios.c_cflag and not PARENB;		//Parity off
  case Parity of  //default none Parity
    paOdd:  tios.c_cflag:= tios.c_cflag or PARENB or PARODD;  // Enable + odd parity
    paEven: tios.c_cflag:= tios.c_cflag or PARENB;  // Enable + not odd (even) parity
  end;

  //One or Two Stopbits (only one for UART1)
  tios.c_cflag:= tios.c_cflag and not CSTOPB;		//one Stopbit
  if Stopbits = sbTwo then
     tios.c_cflag:= tios.c_cflag or CSTOPB;

  //RTS - CTS - Flowcontrol
  tios.c_cflag:= tios.c_cflag and not CRTSCTS;  //fcNone
  if FlowControl = fcHardware then 
    tios.c_cflag:= tios.c_cflag or CRTSCTS;

  if tcsetattr(fd, TCSANOW, tios) <> 0 then begin  //set immidiately
    result:= -1;
    exit;
  end;
  }

  Ffd:= THandle(fpOpen(Device, O_RDWR{ or O_SYNC or O_NOCTTY or O_NDELAY}));
  //ReadBuf back the current tios of the device
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
  //HW Flowcontrol
  tios.c_cflag:= tios.c_cflag and not CRTSCTS;          //Disable RTS/CTS
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
  tios.c_cc[VTIME]:= RxTimeout;    //Rx-Timeout n*0.1sek

  if fpIOCtl(Ffd, TCSETS, @tios) = 0 then begin //0 => success
    self.Start;
    FuseSynchronize:= true; //switch off only in console-Programs
    Start;						//Start Reading - Thread
    SuspendTx(false); //Don't block Tx on Start
    result:= fdDev;      //return File-Descrictor
  end;
end;

function TRS232.Flush(): integer;
begin
  result:= fpIOCtl(fdDev, TCFLSH, nil);   //nil ??
end;

function TRS232.NumRead(): integer;
begin
  fpIOCtl(fdDev, FIONREAD, @result);
end;


//true blocks Tx until false (independet of Flowcontrol)
procedure TRS232.SuspendTx(active: boolean);
begin
  if fdDev > 0 then TCFlow(fdDev, integer(not active));
end;

//Data Receive in Background
procedure TRS232.Execute;
var
  buf: array of byte;
  cnt, i: integer;
begin
  SetLength(buf, 255);
  while not terminated do begin
    cnt:= fpRead(fdDev, buf[0], length(buf));
    if (cnt >= 0) then begin       // -1 for Timeout
      setlength(RxLine, cnt);
      RxLine:= copy(buf, 0, cnt);
//      for i:= 0 to cnt-1 do RxLine[i]:= buf[i];
      //notify Mainthread
      if FuseSynchronize
        then synchronize(@DataReceived)
      	else @DataReceived;
    end;
  end;
end;

//Notifier
procedure TRS232.DataReceived;
begin
  if Assigned(FOnDataReceived) then FOnDataReceived(RxLine);
end;


//Data Send
//write up to 4k data, more will block until written
function TRS232.WriteData(buf: array of Byte): integer;
begin
  if fdDev > 0 then result:= fpWrite(fdDev, buf, length(buf));
end;


//Set Status (BCM.17)
// true =>  RTSlow
// false => RTShigh
procedure TRS232.SetRTS(State: Boolean);
const RTS: Cardinal = TIOCM_RTS;
begin
  if State
    then fpioctl(fdDev, TIOCMBIS, @RTS)
    else fpioctl(fdDev, TIOCMBIC, @RTS);
end;

//Get Status (BCM.16)
// CTSlow  => true;
// CTShigh => false;
function TRS232.GetCTS(): Boolean;
var Flags: Cardinal;
begin
  fpioctl(fdDev, TIOCMGET, @Flags);
  Result := (Flags and TIOCM_CTS) <> 0;
end;

end.

