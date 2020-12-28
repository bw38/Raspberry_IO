program gpiocmd;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, crt,
  gpiolib, gpiotable;


const
 gpiocmd_version = '0.8.1';


type
  TGpio = class(TCustomApplication)
  protected
    procedure DoRun; override;
  private
    gpio: TGpioMap;
    table: TGpioTable;

  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;



procedure TGpio.DoRun;
const
 hl0 = ' +-------------+----------+----------+----------+----------+----------+----------+';
 hl1 = ' |   RegBank   |   Reg0   |    Reg1  |    Reg2  |   Reg3   |   Reg4   |   Reg5   |';
var
  ErrorMsg: String;
  n, pin, err, x, i: integer;
  s: string;
  fsel: TFSel;
  pull: TPull;
  gp: TGpioMap;
  p: pLongWord;
begin
  {
  // quick check parameters
  ErrorMsg:=CheckOptions('h', 'help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;
  }
  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if HasOption('t', 'table') then begin
    table:= TGpioTable.create();
    table.Run();
    table.Free;
    Terminate;
    Exit;
	end;

  // Function Select
  // gpio -fsel pin fsel
  // gpio -fsel 17 alt5
  if HasOption('f', 'fsel') then begin
    err:= 0;
    gp:= TGpioMap.create;
    n:= GetParamCount();
    if n = 3 then begin
      //1.Arg Pin-Nummer
      try
        pin:= StrToInt(GetParams(2));
        if not gpio.IsInHeader(pin) then err:= err or 1;
      except
        err:= err or 2;
      end;
      s:= LowerCase(GetParams(3));
      if (s = 'in') or (s = 'input') then fsel:= fsel_input else
      if (s = 'out') or (s = 'output') then fsel:= fsel_output else
      if (s = 'alt0') then fsel:= fsel_alt0 else
      if (s = 'alt1') then fsel:= fsel_alt1 else
      if (s = 'alt2') then fsel:= fsel_alt2 else
      if (s = 'alt3') then fsel:= fsel_alt3 else
      if (s = 'alt4') then fsel:= fsel_alt4 else
      if (s = 'alt5') then fsel:= fsel_alt5 else
      err:= err or 4
    end else err:= err or 8;
    if err = 0
      then gp.SetFSel(pin, fsel)
      else writeln ('Falsche(r) Parameter - ErrCode:', err);
    gp.Free;
    Terminate;
    Exit;
  end;

  // Set Pull None/Up/Down
  // gpio -pull pin fsel
  // gpio -pull 17 up
  if HasOption('p', 'pull') then begin
    err:= 0;
    gp:= TGpioMap.create;
    n:= GetParamCount();
    if n = 3 then begin
      //1.Arg Pin-Nummer
      try
        pin:= StrToInt(GetParams(2));
        if not gpio.IsInHeader(pin) then err:= err or 1;
      except
        err:= err or 2;
      end;
      s:= LowerCase(GetParams(3));
      if (s = 'no') or (s = 'none') then pull:= pull_none else
      if (s = 'dn') or (s = 'dwn') or (s = 'down') then pull:= pull_dwn else
      if (s = 'up') then pull:= pull_up else
      err:= err or 4
    end else err:= err or 8;
    if err = 0
      then gp.SetPull(pin, pull)
      else writeln ('Falsche(r) Parameter - ErrCode:', err);
    gp.Free;
    Terminate;
    Exit;
  end;


  // Write value
  // gpio -write pin int
  // gpio -write 17 1
  if HasOption('w', 'write') then begin
    err:= 0;
    gp:= TGpioMap.create;
    n:= GetParamCount();
    if n = 3 then begin
      //1.Arg Pin-Nummer
      try
        pin:= StrToInt(GetParams(2));
        if not gpio.IsInHeader(pin) then err:= err or 1;
      except
        err:= err or 2;
      end;
      s:= GetParams(3);
      if (s = '0') then x:= 0 else
      if (s = '1') then x:= 1 else
      err:= err or 4
    end else err:= err or 8;
    if err = 0
      then gp.SetValue(pin, x)
      else writeln ('Falsche(r) Parameter - ErrCode:', err);
    gp.Free;
    Terminate;
    Exit;
  end;

  // Read value
  // gpio -read pin [pin pin ...]
  // gpio -read 17
  if HasOption('r', 'read') then begin
    gp:= TGpioMap.create;
    n:= GetParamCount();
    if n >= 2 then begin
      for i:= 2 to n do begin
        try
          pin:= StrToInt(GetParams(i));
          if gpio.IsInHeader(pin)
            then write(' ', gp.GetValue(pin))
            else write(' X');
        except
          write(' X');
        end;
      end;
    end  else writeln ('Falsche(r) Parameter - ErrCode:', -1);
    gp.Free;
    writeln();
    Terminate;
    Exit;
  end;

  // Read value
  // gpio -read pin [pin pin ...]
  // gpio -read 17
  if HasOption('d', 'dump') then begin
    gp:= TGpioMap.create;
    p:= gp.GpioMap;

    writeln(hl0);
    writeln(hl1);
    writeln(hl0);

    write(' GPFSEL[0..5]  | ');
    for i:= 0 to high(GPFSELX) do
      write( Format('%.8X | ', [(p+GPFSELX[i])^]) );
    writeln();

// Write only
    write(' *GPSET[0..1]  | ');
    for i:= 0 to high(GPSETX) do
      write( Format('%.8X | ', [(p+GPSETX[i])^]) );
    writeln();

    write(' *GPCLR[0..1]  | ');
    for i:= 0 to high(GPCLRX) do
      write( Format('%.8X | ', [(p+GPCLRX[i])^]) );
    writeln();

    write(' GPLEV[0..1]   | ');
    for i:= 0 to high(GPLEVX) do
      write( Format('%.8X | ', [(p+GPLEVX[i])^]) );
    writeln();

    write(' GPEDS[0..1]   | ');
    for i:= 0 to high(GPEDSX) do
      write( Format('%.8X | ', [(p+GPEDSX[i])^]) );
    writeln();

    write(' GPREN[0..1]   | ');
    for i:= 0 to high(GPRENX) do
      write( Format('%.8X | ', [(p+GPRENX[i])^]) );
    writeln();

    write(' GPFEN[0..1]   | ');
    for i:= 0 to high(GPFENX) do
      write( Format('%.8X | ', [(p+GPFENX[i])^]) );
    writeln();

    write(' GPHEN[0..1]   | ');
    for i:= 0 to high(GPHENX) do
      write( Format('%.8X | ', [(p+GPHENX[i])^]) );
    writeln();

    write(' GPLEN[0..1]   | ');
    for i:= 0 to high(GPLENX) do
      write( Format('%.8X | ', [(p+GPLENX[i])^]) );
    writeln();

    write(' GPAREN[0..1]  | ');
    for i:= 0 to high(GPARENX) do
      write( Format('%.8X | ', [(p+GPARENX[i])^]) );
    writeln();

    write(' GPAFEN[0..1]  | ');
    for i:= 0 to high(GPAFENX) do
      write( Format('%.8X | ', [(p+GPAFENX[i])^]) );
    writeln();

    //RPi 1..3
    if gp.SoCTyp in [soc_bcm2835, soc_bcm2836, soc_bcm2837] then begin
      write(' GPPUD[0]      | ');
      for i:= 0 to high(GPPUDX) do
        write( Format('%.8X | ', [(p+GPPUDX[i])^]) );
      writeln();
      write(' GPPUDCLK[0..1]| ');
      for i:= 0 to high(GPPUDCLKX) do
        write( Format('%.8X | ', [(p+GPPUDCLKX[i])^]) );
      writeln();
    end;

    //RPi 4
    if gp.SoCTyp in [soc_bcm2711] then begin
      write(' GPPUPDN[0..3] | ');
      for i:= 0 to high(GPIO_PUP_PDN_CNTRL_REGX) do
        write( Format('%.8X | ', [(p+GPIO_PUP_PDN_CNTRL_REGX[i])^]) );
      writeln();
    end;
    writeln(hl0);
    writeln(#10#13' *) - Write only');
    gp.Free;
    writeln();
    Terminate;
    Exit;
  end;

  if HasOption('v', 'version') then begin
    writeln('gpio Commandline tool: ', gpiocmd_version);
    writeln('gpiolib fpc-library:   ', gpioilb_version);
    writeln();
    Terminate;
    Exit;
  end;

  writeln ('Unbekannte Parameter -> Hilfe: gpio -h');


  Terminate;
end;

constructor TGpio.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TGpio.Destroy;
begin
  inherited Destroy;
end;

procedure TGpio.WriteHelp;
begin
  writeln('GPIO Kommandozeilen-Werkzeug'#10#13);
  writeln('use: gpio -arg gpio [value]');
  writeln('args:');
  writeln('  -r, -read   : Lesen eines oder mehrerer GPIO');
  writeln('  -w, -write  : Schreiben eines GPIO-Ausg. | Value: [0, 1]');
  writeln('  -f, -fsel   : Setzen einer GPIO-Funktion | Value: [in, input, out, output, alt0(..5)]');
  writeln('  -p, -pull   : PullUp/Dwn-Widerstände     | Value: [no, none, up, dn, dwn, down]');
  writeln('  -t, -table  : Übersicht GPIO');
  writeln('  -d, -dump   : Hexdump GPIO-Register');
  writeln('  -v, -version: Versionsangaben');
  writeln('  -h, -help   : Hilfe');
  writeln('gpio:');
  writeln(' 0..31 - numerischer Wert BCM');
end;

var
  Application: TGpio;
begin
  Application:= TGpio.Create(nil);
  Application.Title:='GPIO Table';
  Application.Run;
  Application.Free;
end.

