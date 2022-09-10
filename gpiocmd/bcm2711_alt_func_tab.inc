const
bcm2711_func_tab: array[0..31, 0..5] of string = (
  ('SDA0',       'SA5',         'PCLK',      'SPI3_CE0_N',     'TXD2',         'SDA6'),
  ('SCL0',       'SA4',         'DE',        'SPI3_MISO',      'RXD2',         'SCL6'),
  ('SDA1',       'SA3',         'LCD_VSYNC', 'SPI3_MOSI',      'CTS2',         'SDA3'),
  ('SCL1',       'SA2',         'LCD_HSYNC', 'SPI3_SCLK',      'RTS2',         'SCL3'),
  ('GPCLK0',     'SA1',         'DPI_D0',    'SPI4_CE0_N',     'TXD3',         'SDA3'),
  ('GPCLK1',     'SA0',         'DPI_D1',    'SPI4_MISO',      'RXD3',         'SCL3'),
  ('GPCLK2',     'SOE_N/SE',    'DPI_D2',    'SPI4_MOSI',      'CTS3',         'SDA4'),
  ('SPI0_CE1_N', 'SWE_N/SRW_N', 'DPI_D3',    'SPI4_SCLK',      'RTS3',         'SCL4'),
  ('SPI0_CE0_N', 'SD0',         'DPI_D4',    'BSCSL/CE_N',     'TXD4',         'SDA4'),
  ('SPI0_MISO',  'SD1',         'DPI_D5',    'BSCSL/MISO',     'RXD4',         'SCL4'),
  ('SPI0_MOSI',  'SD2',         'DPI_D6',    'BSCSL SDA/MOSI', 'CTS4',         'SDA5'),
  ('SPI0_SCLK',  'SD3',         'DPI_D7',    'BSCSL SCL/SCLK', 'RTS4',         'SCL5'),
  ('PWM0_0',     'SD4',         'DPI_D8',    'SPI5_CE0_N',     'TXD5',         'SDA5'),
  ('PWM0_1',     'SD5',         'DPI_D9',    'SPI5_MISO',      'RXD5',         'SCL5'),
  ('TXD0',       'SD6',         'DPI_D10',   'SPI5_MOSI',      'CTS5',         'TXD1'),
  ('RXD0',       'SD7',         'DPI_D11',   'SPI5_SCLK',      'RTS5',         'RXD1'),
  ('reserved',   'SD8',         'DPI_D12',   'CTS0',           'SPI1_CE2_N',   'CTS1'),
  ('reserved',   'SD9',         'DPI_D13',   'RTS0',           'SPI1_CE1_N',   'RTS1'),
  ('PCM_CLK',    'SD10',        'DPI_D14',   'SPI6_CE0_N',     'SPI1_CE0_N',   'PWM0_0'),
  ('PCM_FS',     'SD11',        'DPI_D15',   'SPI6_MISO',      'SPI1_MISO',    'PWM0_1'),
  ('PCM_DIN',    'SD12',        'DPI_D16',   'SPI6_MOSI',      'SPI1_MOSI',    'GPCLK0'),
  ('PCM_DOUT',   'SD13',        'DPI_D17',   'SPI6_SCLK',      'SPI1_SCLK',    'GPCLK1'),
  ('SD0_CLK',    'SD14',        'DPI_D18',   'SD1_CLK',        'ARM_TRST',     'SDA6'),
  ('SD0_CMD',    'SD15',        'DPI_D19',   'SD1_CMD',        'ARM_RTCK',     'SCL6'),
  ('SD0_DAT0',   'SD16',        'DPI_D20',   'SD1_DAT0',       'ARM_TDO',      'SPI3_CE1_N'),
  ('SD0_DAT1',   'SD17',        'DPI_D21',   'SD1_DAT1',       'ARM_TCK',      'SPI4_CE1_N'),
  ('SD0_DAT2',   'reserved',    'DPI_D22',   'SD1_DAT2',       'ARM_TDI',      'SPI5_CE1_N'),
  ('SD0_DAT3',   'reserved',    'DPI_D23',   'SD1_DAT3',       'ARM_TMS',      'SPI6_CE1_N'),
  ('SDA0',       'SA5',         'PCM_CLK',   'reserved',       'MII_A_RX_ERR', 'RGMII_MDIO'),
  ('SCL0',       'SA4',         'PCM_FS',    'reserved',       'MII_A_TX_ERR', 'RGMII_MDC'),
  ('reserved',   'SA3',         'PCM_DIN',   'CTS0',           'MII_A_CRS',    'CTS1'),
  ('reserved',  'SA2',          'PCM_DOUT',  'RTS0',           'MII_A_COL',    'RTS1')
);