unit gpioisr;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BaseUnix, Unix, Linux;


type

TEdgeDetect = (
  edNone, edRising, edFalling, edBoth
);

TGpioEvent = record
  gpio: integer;
  fd: cint32;
  timestamp: QWord;
  value: integer;
end;

//Callback type
TGpioEdgeDetected = procedure(ev: TGpioEvent) of object;

TGpioMonitor = class (TThread)
protected
  procedure Execute(); override;
private
  FEdgeDetected: TGpioEdgeDetected;
  FuseSynchronize: boolean;
  FDelay: integer;
  epfd: cint;
  gpios: array[0..31] of cint32;  //map gpio to fd
  gpio_event: TGpioEvent;

  procedure DataReceived;
public
  property onEdgeDetected: TGpioEdgeDetected read FEdgeDetected write FEdgeDetected;
  property useSynchronize: boolean read FuseSynchronize write FuseSynchronize;
  property Delay: integer read FDelay write FDelay;

  constructor Create();
  destructor  Destroy(); override;

  function  AddGpio(gpio: integer; edge: TEdgeDetect): integer;
  function  ModGpio(gpio: integer; edge: TEdgeDetect): integer;
  function  DelGPio(gpio: integer): integer;

  procedure Open();

  function  IsInHeader(gpio: integer): boolean;
end;

implementation

const
//accepted GPIO
gpio_header = [0..31];

constructor TGpioMonitor.Create();
var i: integer;
begin
  inherited Create(true); //do nit start immediately
  onEdgeDetected:= nil;
  FreeOnterminate:= false;
  useSynchronize:= true;
  epfd:= epoll_create(1);  //open an epoll file descriptor, size ignored
  for i:= 0 to length(gpios)-1 do  gpios[i]:= -1;
  FDelay:= 0;
end;

destructor  TGpioMonitor.Destroy();
var i: integer;
begin
  Terminate();
  //Timeout epoll_wait
  WaitFor();
  //cleaning sysfs
  for i:= 0 to length(gpios)-1 do DelGpio(i);
  fpclose(epfd);
  inherited Destroy();
end;

// --------------------------------------------------------------------------
//Add to WatchList
function TGpioMonitor.AddGpio(gpio: integer; edge: TEdgeDetect): integer;
var
  fd: cint32;
  s: string;
  ev: EPoll_Event;
begin
  result:= -1;
  if not IsInHeader(gpio) then exit;

  s:= IntToStr(gpio);
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

  //Set watching edge
  fd:= fpopen('/sys/class/gpio/gpio'+s+'/edge', O_RDWR or O_NONBLOCK);
  case edge of
    edNone:    fpwrite(fd, PChar('none'), 5);
    edRising:  fpwrite(fd, PChar('rising'), 7);
    edFalling: fpwrite(fd, PChar('falling'), 8);
    edBoth:    fpwrite(fd, PChar('both'), 5);
  end;
  fpclose(fd);

  //set -> wait for change value
  fd:= fpopen('/sys/class/gpio/gpio'+s+'/value', O_RDONLY or O_NONBLOCK);
  if fd < 0 then exit;
  fpread(fd, nil, 1);

  ev.events:= EPOLLPRI;
  ev.Data.u32:= gpio;
  writeln('FD: ', fd);
  result:= epoll_ctl(epfd, EPOLL_CTL_ADD, fd, @ev); // return 0 => on success
  gpios[gpio]:= fd;

end;

//Change Edge (or add gpio)
function TGpioMonitor.ModGpio(gpio: integer; edge: TEdgeDetect): integer;
var
  s: string;
  fd: cint;
  res: integer;
begin
  result:= -1;
  if not IsInHeader(gpio) then exit;
  if gpios[gpio] < 0 then AddGpio(gpio, edge);

  //Set watching edge
  s:= IntToStr(gpio);
  fd:= fpopen('/sys/class/gpio/gpio'+s+'/edge', O_RDWR or O_NONBLOCK);
  case edge of
    edNone:    res:= fpwrite(fd, PChar('none'), 5);
    edRising:  res:= fpwrite(fd, PChar('rising'), 7);
    edFalling: res:= fpwrite(fd, PChar('falling'), 8);
    edBoth:    res:= fpwrite(fd, PChar('both'), 5);
  end;
  fpclose(fd);
  if res > 0 then result:= 0;
end;

//Remove from watchlist, close FileDescriptor and unexport from sysfs
function TGpioMonitor.DelGPio(gpio: integer): integer;
var
  fd: cint;
  s: string;
begin
  result:= -1;
  if not IsInHeader(gpio) then exit;

  fd:= gpios[gpio];
  if fd < 0 then exit;

  result:= epoll_ctl(epfd, EPOLL_CTL_DEL, fd, nil) or
    fpClose(fd);

  //unexprt GPIO
  fd:= fpopen('/sys/class/gpio/unexport', O_WRONLY);
  s:= IntToStr(gpio);
  fpwrite(fd, PChar(s), length(s)+1);
  fpclose(fd);

end;

procedure TGpioMonitor.Open();
begin
  Start;
end;

procedure TGpioMonitor.Execute();
var
  n: cint;
  i, v: integer;
  events: array[0..3] of EPoll_Event;
  fd: cint32;
  cbuf: array [0..1] of char;
  ts: QWord;
begin
  while not terminated do begin
    n:= epoll_wait(epfd, @events, 4, 1000); //Blocking Destroy !!!
    //not sure if more then one event
    for i:= 0 to n-1 do begin
      fd:= gpios[events[i].Data.u32];
      ts:= GetTickCount64();
      fpread(fd, @cbuf[0], 2);        //2 Char => '0' or '1' + #10
      v:= byte(cbuf[0]) and 1;        //ASCII to int
      fplseek (fd, 0, SEEK_SET);      //for next read

      gpio_event.gpio:= events[i].Data.u32; //gpio-nr
      gpio_event.fd:= fd;
      gpio_event.timestamp:= ts;
      gpio_event.value:= v;
      //Delay Notifing, e.g. for debouncing
      while GetTickCount64() < ts + Delay do sleep(1);
      //notify Mainthread
      if FuseSynchronize
        then synchronize(@DataReceived)
       else @DataReceived;
    end;
  end;
end;


//Notifier
procedure TGpioMonitor.DataReceived;
begin
  if Assigned(onEdgeDetected) then onEdgeDetected(gpio_event);
end;


function TGpioMonitor.IsInHeader(gpio: integer): boolean;
begin
  result:= gpio in gpio_header;
end;



end.

