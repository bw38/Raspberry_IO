unit gpiolib;

{$mode objfpc}{$H+}

//{$linklib c}


interface

uses Classes, SysUtils, BaseUnix, Unix, Linux  ;


const
  gpioilb_version = '0.7.0';


type
  TSoC = record
    FamilyName: string;
    DeviceName: string;
  end;


  TSoC_Type = (
    soc_BCM2835, soc_BCM2836, soc_BCM2837, soc_BCM2711,
    soc_max_num
  );

const
  SOC_NUM = integer(soc_max_num); //Anzahl der wählbaren SoC-Typen
  SOC_UNDEFINED = TSoC_Type(soc_max_num);

  SoCs: array [0..SOC_NUM-1] of TSoC = (
  (FamilyName:'BCM2708'; DeviceName:'BCM2835'),  // -> Zero, Zero W, A, B, A+, B+
  (FamilyName:'BCM2709'; DeviceName:'BCM2836'),  // -> 2B
  (FamilyName:'BCM2710'; DeviceName:'BCM2837'),  // -> 2B*, 3B, 3A+, 3B+
  (FamilyName:'BCM2711'; DeviceName:'BCM2711')   // -> 4B
);


  //Offset-Adr div 4 - Ausrichtung an LongWord
  GPFSELX: array[0..5] of integer = (0,1,2,3,4,5);  //($00, $04, $08, $0C, $10, $14);
  GPSETX : array[0..1] of integer = (7,8);          //($1C, $20); //Read only
  GPCLRX : array[0..1] of integer = (10,11);        //($28, $2C); ((Read only
  GPLEVX : array[0..1] of integer = (13,14);        //($34, $38);

  GPEDSX : array[0..1] of integer = (16,17);        //($40, $44);
  GPRENX : array[0..1] of integer = (19,20);        //($4C, $50);
  GPFENX : array[0..1] of integer = (22,23);        //($58, $5C);
  GPHENX : array[0..1] of integer = (25,26);        //($64, $68);
  GPLENX : array[0..1] of integer = (28,29);        //($70, $74);
  GPARENX: array[0..1] of integer = (30,31);        //($7C, $80);
  GPAFENX: array[0..1] of integer = (33,34);        //($88, $8C);

  //Pull up/dwn bis PRi3
  GPPUDX : array[0..0] of integer = (37);
  GPPUDCLKX:
           array[0..1] of integer = (38, 39);
  //Pull up/dwn RPi4
  GPIO_PUP_PDN_CNTRL_REGX:
           array[0..3] of integer = (57, 58, 59, 60);

// -----------------------------------------------------------------------------


type
  TFSel = (
    fsel_input, fsel_output,
    fsel_alt5, fsel_alt4, fsel_alt0, fsel_alt1, fsel_alt2, fsel_alt3,
    fsel_undefined
  );

  //Wertigkeit  der RPi4-Pull-Steuerung
  //bis RPi3 => none, dwn, up, res (s. SetPull)
  TPull = (
    pull_none, pull_up, pull_dwn, pull_reserved,
    pull_undefined
  );

 pLongWord = ^LongWord;

TGpioMap = class
private
  Fgpiomap: pLongWord;
  Fsoc_typ: TSoc_Type;

  function  gpio_map_init(): TSoC_Type;
  procedure gpio_unmap();



public
  property SoCTyp: TSoC_Type read Fsoc_typ;
  property GpioMap: pLongWord read Fgpiomap;

  constructor create;
  destructor  destroy; override;

  function  GetSocName(): string;
  function  GetFamilyName(): string;

  procedure SetFSel(gpio: integer; fsel: TFSel);
  function  GetFSel(gpio: integer): TFSel;

  procedure SetValue(gpio: integer; lev: integer);
  function  GetValue(gpio: integer): integer;

  procedure SetPull(gpio: integer; pull: TPull);
  function  GetPull(gpio: integer): TPull;

  function  IsInHeader(gpio: integer): boolean;
end;


implementation


const
//Info-File mit SoC-Informationen als String
SOC_INFO =  '/sys/firmware/devicetree/base/compatible';

MAP_SIZE = 4096;

//accepted GPIO
gpio_header = [0..31];


constructor TGpioMap.create();
begin
  inherited create();
  Fgpiomap:= nil;
  Fsoc_typ:= gpio_map_init();
end;

destructor TGpioMap.destroy();
begin
  gpio_unmap();
  inherited destroy();
end;

function TGpioMap.gpio_map_init(): TSoC_Type;
var tf: TextFile;
    s: string;
    i: integer;
    fd: cint;
    soc: TSoC_Type;
begin
  AssignFile(tf, SOC_INFO);
  reset(tf);
  readln(tf, s);
  CloseFile(tf);
  s:= UpperCase(s);
  soc:= SOC_UNDEFINED;

  for i:= 0 to SOC_NUM-1 do
    if pos(SoCs[i].DeviceName, s) > 0 then begin
      soc:= TSoC_Type(i);
      break;
    end;

  gpio_unmap();

  fd:= fpopen('/dev/gpiomem', O_RDWR or O_SYNC);
  if fd < 0 then begin
    writeln('Device opening failed');
    soc:= SOC_UNDEFINED;
  end else
  Fgpiomap:= fpmmap(nil, MAP_SIZE, PROT_READ or PROT_WRITE, MAP_SHARED, fd, 0);
  if Fgpiomap = MAP_FAILED then begin
    writeln('GPIO mapping failed');
    soc:= SOC_UNDEFINED;
	end;
  fpclose(fd);

  result:= soc;
end;


procedure TGpioMap.gpio_unmap();
begin
  if Fgpiomap <> nil then begin
    fpmunmap(Fgpiomap, MAP_SIZE);
    Fgpiomap:= nil;
  end;
end;

// -----------------------------------------------------------------------------

function  TGpioMap.GetSocName(): string;
begin
  if Fsoc_typ = SOC_UNDEFINED
    then result:= '???'
    else result:= SoCs[integer(Fsoc_typ)].DeviceName;
end;

function TGpioMap.GetFamilyName(): string;
begin
  if Fsoc_typ = SOC_UNDEFINED
    then result:= '???'
    else result:= SoCs[integer(Fsoc_typ)].FamilyName;
end;

function  TGpioMap.IsInHeader(gpio: integer): boolean;
begin
  result:= gpio in gpio_header;
end;

//-----------------------------------------------------------------------------


procedure TGPioMap.SetFsel(gpio: integer; fsel: TFSel);
var
  reg, offs: integer;
  plw: ^LongWord;
  lw: LongWord;
begin
  if (Fgpiomap = nil) or (fsel = fsel_undefined) or (not IsInHeader(gpio)) then exit;
  reg:= gpio div 10;
  offs:= gpio mod 10;
  plw:= Fgpiomap + GPFSELX[reg];
  lw:= plw^;
  lw:= lw and (not(7 shl (offs*3)));  //3 Bit maskieren 0
  lw:= lw or (integer(fsel) shl (offs*3));
  plw^:= lw;
end;

function TGpioMap.GetFSel(gpio: integer): TFSel;
var
  reg, offs: integer;
  plw: ^LongWord;
  lw: LongWord;
begin
  result:= fsel_undefined;
  if (Fgpiomap = nil) or (not IsInHeader(gpio)) then exit;
  reg:= gpio div 10;
  offs:= gpio mod 10;
  plw:= Fgpiomap + GPFSELX[reg];
  lw:= plw^;
  result:= TFSel((lw shr (offs*3)) and 7);
end;


// ----------------------------------------------------------------------------
//Setzen eines Ausgangs
//lev nur 0 / 1 zulässig
//SET- und CLEAR-Register write only
procedure TGpioMap.SetValue(gpio: integer; lev: integer);
var
  lws, lwc: LongWord;
  plw: ^LongWord;
begin
  if (Fgpiomap = nil)  or (not IsInHeader(gpio)) or (not lev in [0..1]) then exit;
  //alle zugänglichen Pin in Reg0
  case lev of
    0: (Fgpiomap + GPCLRX[0])^:= 1 shl gpio;   //Einzelbit
    1: (Fgpiomap + GPSETX[0])^:= 1 shl gpio;
  end;
end;


function  TGpioMap.GetValue(gpio: integer) : integer;
begin
  result:= -1;
  if (Fgpiomap = nil) or (not IsInHeader(gpio)) then exit;
  //alle zulässigen Pin in Reg0
  result:= ((Fgpiomap + GPLEVX[0])^ shr gpio) and 1;
end;



procedure TGpioMap.SetPull(gpio: integer; pull: TPull);
var
  reg, offs: integer;
  lw: LongWord;
  plw, plwc: ^LongWord;
begin
  if (Fgpiomap = nil) or (not IsInHeader(gpio)) then exit;

  if (Fsoc_typ in [soc_bcm2835, soc_bcm2836, soc_bcm2837]) then begin //Raspberry 1..3
    plw:= Fgpiomap + GPPUDX[0];
    plwc:= Fgpiomap + GPPUDCLKX[0];
    case pull of    //unterschiedliche Wertigkeit zum Pi4
      pull_none: plw^:= 0;
      pull_dwn : plw^:= 1;
      pull_up  : plw^:= 2;
    end;
    sleep(1);
    plwc^:= (1 shl gpio);
    sleep(1);
    plw^:= 0;
    plwc^:= 0; //plwc^ and (not(1 shl gpio));
  end else

  if (Fsoc_typ in [soc_bcm2711]) then begin //Raspberry 4
    reg:= gpio div 16;    //16 GPIO je pull-Register
    offs:= gpio mod 16;

    plw:= Fgpiomap + GPIO_PUP_PDN_CNTRL_REGX[reg];
    lw:= plw^;
    lw:= lw and (not(3 shl (offs*2)));  //2 Bit maskieren 0
    lw:= lw or (integer(pull) shl (offs*2));
    plw^:= lw;
  end else {if then} begin
    //Behandlung zukünftiger Typen
  end;
end;



function TGpioMap.GetPull(gpio: integer): TPull;
var
  reg, offs: integer;
  lw: LongWord;
  plw: ^LongWord;
begin
  result:= pull_undefined;
  if (Fgpiomap = nil) or (not IsInHeader(gpio)) or
     (SoCTyp in [soc_bcm2835, soc_bcm2836, soc_bcm2837]) //RPi1-3 kein Rücklesen möglich
    then exit;
    reg:= gpio div 16;    //16 GPIO je Pull-Register
    offs:= gpio mod 16;

    plw:= Fgpiomap + GPIO_PUP_PDN_CNTRL_REGX[reg];
    lw:= plw^;
    result:= TPull((lw shr (offs*2)) and 3);
end;





end.

