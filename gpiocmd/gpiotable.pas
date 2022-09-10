unit gpiotable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  gpiolib;


{$include 'bcm2835_alt_func_tab.inc'}
{$include 'bcm2711_alt_func_tab.inc'}
{$include 'P8Header.inc'}

type

TGpioTable = class
private
  gpio: TGpioMap;
  function GetFuncName(pin: integer; fsel: TFSel): string;
  function GetPullName(pin: integer): string;
  function GetModeName(pin: integer; fsel: TFsel): string;
  function GetValName(pin: integer): string;
  function GetBCMName( pin: integer): string;
public
  constructor create();
  destructor  Destroy; override;

  procedure Run();

end;

implementation

constructor TGpioTable.create();
begin
  inherited create;
end;

destructor TGpioTable.Destroy;
begin
  inherited destroy;
end;

procedure TGpioTable.Run();
const
  ColSZ: array[0..5] of string = ('8','3','4','1','3','2');
  headline0 = '  + --------+------------+---+-----+-----------+-----+---+------+---------------+';
  headline1 = '  | Function| Pull| FSel |Val| BCM |   Header  | BCM |Val| FSel |Pull |Function |';

var
  i, pin: integer;
  s, line: string;
  fsel: TFSel;

begin
  gpio:= TGpioMap.create;
  if gpio.SoCTyp <> SOC_UNDEFINED then begin

    writeln ('  SoC-Typ: ', gpio.GetSocName(), ' [', gpio.GetFamilyName(), ']');
    writeln(headline0);
    writeln(headline1);
    writeln(headline0);

    for i:= 1 to 20 do begin
      line:= '  |';
     //ungerade Spalte 01..39
      pin:= i*2 - 1;
      fsel:= gpio.GetFSel(P8_Header[pin]);
      s:= GetFuncName(pin, fsel);
      line:= line + format('%'+colsz[0]+'s', [s]) + ' | ';

      s:= GetPullName(pin);
      line:= line + format('%'+colsz[1]+'s', [s]) + ' | ';

      s:= GetModeName(pin, fsel);
      line:= line + format('%'+colsz[2]+'s', [s]) + ' | ';

      s:= GetValName(pin);
      line:= line + format('%'+colsz[3]+'s', [s]) + ' | ';

      s:= GetBCMName(pin);
      line:= line + format('%'+colsz[4]+'s', [s]) + ' | ';

      s:= IntToStr(pin);
      line:= line + format('%'+colsz[5]+'s', [s]) + ' o o ';

      //gerade Spalte 02..40
      inc(pin);

      s:= IntToStr(pin);
      line:= line + format('%-'+colsz[5]+'s', [s]) + ' | ';;

      s:= GetBCMName(pin);
      line:= line + format('%-'+colsz[4]+'s', [s]) + ' | ';

      s:= GetValName(pin);
      line:= line + format('%-'+colsz[3]+'s', [s]) + ' | ';

      fsel:= gpio.GetFSel(P8_Header[pin]);
      s:= GetModeName(pin, fsel);
      line:= line + format('%-'+colsz[2]+'s', [s]) + ' | ';

      s:= GetPullName(pin);
      line:= line + format('%-'+colsz[1]+'s', [s]) + ' | ';

      s:= GetFuncName(pin, fsel);
      line:= line + format('%-'+colsz[0]+'s', [s]) + '|';

      writeln (line);
    end;
    writeln(headline0);
  end;
  gpio.Free;
end;


// ----------------------------------------------------------------------------

//pin: P8-Headernummer
//fsel: 0..7
function TGpioTable.GetFuncName(pin: integer; fsel: TFSel): string;
var
  gp: integer;
  s: string;
begin
  result:= '???';
  if (pin > high(P8_Header)) or (gpio.SoCTyp = SOC_UNDEFINED) then exit;
  gp:= P8_Header[pin];
  if gp < 0
    then s:= ' '
    else case gpio.SoCTyp of
      soc_bcm2835,
      soc_bcm2836,
      soc_bcm2837:  //Pi1-3
        begin
          case fsel of
            fsel_input,
            fsel_output: s:= 'GPIO ' + IntToStr(gp);
            fsel_alt0:   s:= bcm2835_func_tab[gp, 0];
            fsel_alt1:   s:= bcm2835_func_tab[gp, 1];
            fsel_alt2:   s:= bcm2835_func_tab[gp, 2];
            fsel_alt3:   s:= bcm2835_func_tab[gp, 3];
            fsel_alt4:   s:= bcm2835_func_tab[gp, 4];
            fsel_alt5:   s:= bcm2835_func_tab[gp, 5];
          end;
        end;
      soc_bcm2711:  //Pi4
        begin
          case fsel of
            fsel_input,
            fsel_output: s:= 'GPIO ' + IntToStr(gp);
            fsel_alt0:   s:= bcm2711_func_tab[gp, 0];
            fsel_alt1:   s:= bcm2711_func_tab[gp, 1];
            fsel_alt2:   s:= bcm2711_func_tab[gp, 2];
            fsel_alt3:   s:= bcm2711_func_tab[gp, 3];
            fsel_alt4:   s:= bcm2711_func_tab[gp, 4];
            fsel_alt5:   s:= bcm2711_func_tab[gp, 5];
          end;
        end;
    end;
  if length(s) > 8 then s:= copy(s, 1, 6) + '..';
  result:= s;
end;

function TGpioTable.GetPullName(pin: integer): string;
var
  gp: integer;
  s: string;
begin
  result:= '???';
  if (pin > high(P8_Header)) or (gpio.SoCTyp = SOC_UNDEFINED) then exit;
  gp:= P8_Header[pin];
  //keine Anzeige bei Spannungs-Pins
  //keine Auswertung bei RPi1-3 möglich
  if (gp < 0) or(gpio.SoCTyp in [soc_bcm2835, soc_bcm2836, soc_bcm2837])
    then s:= ''
    else case gpio.GetPull(gp) of  //nur Pi4
      pull_none: s:= '-';
      pull_up  : s:= 'UP';
      pull_dwn : s:= 'DWN';
    end;
  result:= s;
end;

function TGpioTable.GetModeName(pin: integer; fsel: TFSel): string;
var
  gp: integer;
  s: string;
begin
  result:= '???';
  if (pin > high(P8_Header)) or (gpio.SoCTyp = SOC_UNDEFINED) then exit;
  gp:= P8_Header[pin];
  if gp < 0
    then s:= ''
    else case fsel of
        fsel_input:  s:= 'IN';
        fsel_output: s:= 'OUT';
        fsel_alt0:   s:= 'ALT0';
        fsel_alt1:   s:= 'ALT1';
        fsel_alt2:   s:= 'ALT2';
        fsel_alt3:   s:= 'ALT3';
        fsel_alt4:   s:= 'ALT4';
        fsel_alt5:   s:= 'ALT5';
        fsel_undefined: s:= '???';
    end;
  result:= s;
end;

function TGpioTable.GetValName(pin: integer): string;
var
  gp: integer;
  s: string;
begin
  result:= '?';
  if (pin > high(P8_Header)) or (gpio.SoCTyp = SOC_UNDEFINED) then exit;
  gp:= P8_Header[pin];
  if gp < 0
    then s:= ''
    else s:= IntToStr(gpio.GetValue(gp));
  result:= s;
end;

function TGpioTable.GetBCMName(pin: integer): string;
var
  gp: integer;
  s: string;
begin
  result:= '???';
  if (pin > high(P8_Header)) or (gpio.SoCTyp = SOC_UNDEFINED) then exit;
  gp:= P8_Header[pin];
  case gp of
    -3 : s:= '5V0';
    -2 : s:= '3V3';
    -1 : s:= 'GND';
    else begin
      s:= IntToStr(gp);
      if length(s) = 1 then s:= '0' + s;
    end;
  end;
  result:= s;
end;




end.

