unit gpioisr;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BaseUnix, Unix, Linux;


type

TEdgeDetect = (
  edNone, edRising, edFalling, edBoth
);


//Callback type
TGpioEdgeDetected = procedure(falling, rising: LongWord) of object;

TGpioCallback = class (TThread)
protected
  procedure Execute(); override;
private
  FEdgeDetected: TGpioEdgeDetected;
  FuseSynchronize: boolean;
  epfd: cint;
  procedure DataReceived;
public
  property onEdgeDetected: TGpioEdgeDetected read FEdgeDetected write FEdgeDetected;
  property useSynchronize: boolean read FuseSynchronize write FuseSynchronize;

  constructor Create();
  destructor  Destroy(); override;

  function  AddGpio(gpio: integer; edge: TEdgeDetect): integer;
  procedure Open();

  function  IsInHeader(gpio: integer): boolean;
end;

implementation

const
//accepted GPIO
gpio_header = [0..31];

constructor TGpioCallback.Create();
begin
  inherited Create(true); //do nit start immediately
  onEdgeDetected:= nil;
  FreeOnterminate:= false;
  useSynchronize:= true;
  epfd:= epoll_create(1);  //open an epoll file descriptor, size ignored
end;

destructor  TGpioCallback.Destroy();
begin
  //sysfs cleaning !!!!
  fpclose(epfd);
  Terminate();
  inherited Destroy();
end;

// --------------------------------------------------------------------------
//Add to WatchList
function TGpioCallBack.AddGpio(gpio: integer; edge: TEdgeDetect): integer;
var
  fd, err: cint32;
  s: string;
  ev: EPoll_Event;
begin
  result:= -1;
  if not IsInHeader(gpio) then exit;

  s:= IntToStr(gpio);
  {
  //close, if exported
  fd:= fpopen('/sys/class/gpio/unexport', O_WRONLY);

  fpwrite(fd, PChar(s), length(s)+1);
  fpclose(fd);
  }

  if not FileExists('/sys/class/gpio/gpio'+s) then begin
    //create sys-directory
    fd:= fpopen('/sys/class/gpio/export', O_WRONLY);
    if fd < 0 then exit;  //return -1
    fpwrite(fd, PChar(s), length(s));
    fpclose(fd);
    sleep(50);           //Wait for creation
  end;

  //set input
  fd:= fpopen('/sys/class/gpio/gpio'+s+'/direction', O_RDWR or O_NONBLOCK);
  if fd < 0 then exit;
  fpwrite(fd, PChar('in'), 3);

  //Set wtching edge
  fd:= fpopen('/sys/class/gpio/gpio'+s+'/edge', O_RDWR or O_NONBLOCK);
  case edge of
    edNone:    fpwrite(fd, PChar('none'), 5);
    edRising:  fpwrite(fd, PChar('rising'), 7);
    edFalling: fpwrite(fd, PChar('falling'), 8);
    edBoth:    fpwrite(fd, PChar('both'), 5);
  end;
  fpclose(fd);

  //set -> wait for change value
  fd:= fpopen('/sys/class/gpio/gpio'+s+'/value', O_RDWR or O_NONBLOCK);
  if fd < 0 then exit;
  err:= fpread(fd, nil, 1);

  ev.events:= EPOLLPRI;
  ev.Data.fd:= fd;
  writeln('FD: ', fd);
  result:= epoll_ctl(epfd, EPOLL_CTL_ADD, fd, @ev); // return 0 => on success


end;

procedure TGpioCallback.Open();
begin
  Start;
end;

procedure TGpioCallback.Execute();
var
  n: cint;
  i, j: integer;
  events: array[0..63] of EPoll_Event;
  buf: array [0..64] of byte;
begin
  while not terminated do begin
      n:= epoll_wait(epfd, @events, 64, 1000);
      if n > 0 then begin
        for i:= 0 to n-1 do
 //       writeln('YYY: ', n,  ' | ', format('%.8X', [events[i].Events]), ' | ', events[i].Data.fd);
        fpread(events[i].Data.fd, @buf[0], 1);
        write(events[i].Data.fd, ' ',events[i].Data.u32, ' ', events[i].Data.u64, ' ---> ' );
        //notify Mainthread
        if FuseSynchronize
          then synchronize(@DataReceived)
      	  else @DataReceived;
      end;
  end;
end;


//Notifier
procedure TGpioCallback.DataReceived;
begin
  if Assigned(onEdgeDetected) then onEdgeDetected(11,22);
end;


function TGpioCallback.IsInHeader(gpio: integer): boolean;
begin
  result:= gpio in gpio_header;
end;

//*****************************************************************************

{
procedure TGpioCallback.WaitForEvent(gpio: integer);
var epfdx, fd, n, i: cint;
    ev:  EPoll_Event;
    events: array[0..64] of EPoll_Event;
    s: string;
    buf: array [0..64] of byte;
begin
  epfdx:= epoll_create(1);

  fd:= fpopen('/sys/class/gpio/unexport', O_WRONLY);
  s:= IntToStr(gpio);
  fpwrite(fd, PChar(s), length(s)+1);
  fpclose(fd);

  fd:= fpopen('/sys/class/gpio/export', O_WRONLY);
  s:= IntToStr(gpio);
  fpwrite(fd, PChar(s), length(s));
  fpclose(fd);
  sleep(50);

  fd:= fpopen('/sys/class/gpio/gpio'+s+'/edge', O_RDWR or O_NONBLOCK);
  fpwrite(fd, 'falling', 7);
  fpclose(fd);

  fd:= fpopen('/sys/class/gpio/gpio'+s+'/value', O_RDWR or O_NONBLOCK);

  if fd < 0 then begin
    writeln('Open File failed');
  end else begin

    ev.events:= EPOLLPRI;
    ev.data.fd:= fd;
    writeln('FD: ', fd);
    n:= epoll_ctl(epfdx, EPOLL_CTL_ADD, fd, @ev);

    n:= fpread(fd, nil, 1);

    while not terminated do begin
      n:= epoll_wait(epfdx, @events, 64, 1000);
      for i:= 0 to n-1 do
        writeln('YYY: ', n,  ' | ', format('%.8X', [events[i].Events]), ' | ', events[i].Data.fd);
 //     fplseek (fd, 0, SEEK_SET);
      n:= fpread(events[i].Data.fd, @buf[0], 1);

    end;



  end;



  fpclose(epfdx);
end;
}


end.

