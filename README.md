# gpio



Tool for watching and controlling Raspberry Pi - GPIO

https://github.com/bw38/Raspberry_IO/tree/master/gpiocmd)/binary/gpio
written in freepascal / lazarus


## Install on Pi

wget https://github.com/bw38/Raspberry_IO/raw/master/gpiocmd/binary/gpio
chmod +x gpio
sudo chown root:root gpio
sudo mv gpio /usr/local/bin



## usage

gpio -arg gpio [value]
args:
  -r, -read   : Lesen eines oder mehrerer GPIO
  -w, -write  : Schreiben eines GPIO-Ausg. | value: [0, 1]
  -f, -fsel   : Setzen einer GPIO-Funktion | value: [in, input, out, output, alt0(..5)]
  -p, -pull   : PullUp/Dwn-Widerstände     | value: [no, none, up, dn, dwn, down]
  -t, -table  : Übersicht GPIO
  -d, -dump   : Hexdump GPIO-Register
  -v, -version: Versionsangaben
  -h, -help   : Hilfe
gpio:
  0..31 - numerischer Wert BCM
