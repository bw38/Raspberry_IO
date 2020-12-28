//Standarddefinitionen der Seriellen Schnittstellen
//übernommen aus termio.inc

//Standardbaurates
//Termios definiert nicht alle Baudrates in verscjiedenen Prozessor-Plattformen
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

TFlowControl=(fcNone,fcHardware);   //xon/xoff not implemented

TParity=(paNone, paOdd, paEven);  //Parity doesn't work  on miniUART

TStopBits=(sbOne,sbTwo); 					//only one Stopbit possible on miniUART
