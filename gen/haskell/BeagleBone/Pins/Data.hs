-- Generated file. http://creativecommons.org/publicdomain/zero/1.0/

module BeagleBone.Pins.Data
       (BBPin(..), MPUPin(..), MPUPinSignal(..), Signal(..),
        SignalType(..), BBPinId(..), MPUPinId(..), SignalId(..), bbPins,
        mpuPins, signals)
       where
import qualified Data.Map as Map
 
data BBPin = BBPin{bbpId :: BBPinId, bbpName :: String,
                   bbpMPUPinId :: Maybe MPUPinId}
           deriving (Eq, Ord, Show, Read)
 
data MPUPin = MPUPin{mpId :: MPUPinId, mpLinuxName :: Maybe String,
                     mpSignals :: Map.Map SignalId MPUPinSignal}
            deriving (Eq, Ord, Show, Read)
 
data MPUPinSignal = MPUPinSignal{mpsMode :: Maybe Integer,
                                 mpsSignalId :: SignalId}
                  deriving (Eq, Ord, Show, Read)
 
data Signal = Signal{sId :: SignalId, sType :: SignalType,
                     sGPIONum :: Maybe Integer, sLinuxPWMName :: Maybe String}
            deriving (Eq, Ord, Show, Read)
 
data SignalType = A
                | I
                | O
                | IO
                | IOD
                | PWR
                | GND
                deriving (Eq, Ord, Bounded, Enum, Show, Read)
 
data BBPinId = BB_P8_1
             | BB_P8_2
             | BB_P8_3
             | BB_P8_4
             | BB_P8_5
             | BB_P8_6
             | BB_P8_7
             | BB_P8_8
             | BB_P8_9
             | BB_P8_10
             | BB_P8_11
             | BB_P8_12
             | BB_P8_13
             | BB_P8_14
             | BB_P8_15
             | BB_P8_16
             | BB_P8_17
             | BB_P8_18
             | BB_P8_19
             | BB_P8_20
             | BB_P8_21
             | BB_P8_22
             | BB_P8_23
             | BB_P8_24
             | BB_P8_25
             | BB_P8_26
             | BB_P8_27
             | BB_P8_28
             | BB_P8_29
             | BB_P8_30
             | BB_P8_31
             | BB_P8_32
             | BB_P8_33
             | BB_P8_34
             | BB_P8_35
             | BB_P8_36
             | BB_P8_37
             | BB_P8_38
             | BB_P8_39
             | BB_P8_40
             | BB_P8_41
             | BB_P8_42
             | BB_P8_43
             | BB_P8_44
             | BB_P8_45
             | BB_P8_46
             | BB_P9_1
             | BB_P9_2
             | BB_P9_3
             | BB_P9_4
             | BB_P9_5
             | BB_P9_6
             | BB_P9_7
             | BB_P9_8
             | BB_P9_9
             | BB_P9_10
             | BB_P9_11
             | BB_P9_12
             | BB_P9_13
             | BB_P9_14
             | BB_P9_15
             | BB_P9_16
             | BB_P9_17
             | BB_P9_18
             | BB_P9_19
             | BB_P9_20
             | BB_P9_21
             | BB_P9_22
             | BB_P9_23
             | BB_P9_24
             | BB_P9_25
             | BB_P9_26
             | BB_P9_27
             | BB_P9_28
             | BB_P9_29
             | BB_P9_30
             | BB_P9_31
             | BB_P9_32
             | BB_P9_33
             | BB_P9_34
             | BB_P9_35
             | BB_P9_36
             | BB_P9_37
             | BB_P9_38
             | BB_P9_39
             | BB_P9_40
             | BB_P9_41
             | BB_P9_42
             | BB_P9_43
             | BB_P9_44
             | BB_P9_45
             | BB_P9_46
             | BB_USR0
             | BB_USR1
             | BB_USR2
             | BB_USR3
             deriving (Eq, Ord, Bounded, Enum, Show, Read)
 
data MPUPinId = MPU_AIN0
              | MPU_AIN1
              | MPU_AIN2
              | MPU_AIN3
              | MPU_AIN4
              | MPU_AIN5
              | MPU_AIN6
              | MPU_AIN7
              | MPU_CAP_VBB_MPU
              | MPU_CAP_VDD_RTC
              | MPU_CAP_VDD_SRAM_CORE
              | MPU_CAP_VDD_SRAM_MPU
              | MPU_DDR_A0
              | MPU_DDR_A1
              | MPU_DDR_A2
              | MPU_DDR_A3
              | MPU_DDR_A4
              | MPU_DDR_A5
              | MPU_DDR_A6
              | MPU_DDR_A7
              | MPU_DDR_A8
              | MPU_DDR_A9
              | MPU_DDR_A10
              | MPU_DDR_A11
              | MPU_DDR_A12
              | MPU_DDR_A13
              | MPU_DDR_A14
              | MPU_DDR_A15
              | MPU_DDR_BA0
              | MPU_DDR_BA1
              | MPU_DDR_BA2
              | MPU_DDR_CASn
              | MPU_DDR_CK
              | MPU_DDR_CKE
              | MPU_DDR_CKn
              | MPU_DDR_CSn0
              | MPU_DDR_D0
              | MPU_DDR_D1
              | MPU_DDR_D2
              | MPU_DDR_D3
              | MPU_DDR_D4
              | MPU_DDR_D5
              | MPU_DDR_D6
              | MPU_DDR_D7
              | MPU_DDR_D8
              | MPU_DDR_D9
              | MPU_DDR_D10
              | MPU_DDR_D11
              | MPU_DDR_D12
              | MPU_DDR_D13
              | MPU_DDR_D14
              | MPU_DDR_D15
              | MPU_DDR_DQM0
              | MPU_DDR_DQM1
              | MPU_DDR_DQS0
              | MPU_DDR_DQS1
              | MPU_DDR_DQSn0
              | MPU_DDR_DQSn1
              | MPU_DDR_ODT
              | MPU_DDR_RASn
              | MPU_DDR_RESETn
              | MPU_DDR_VREF
              | MPU_DDR_VTP
              | MPU_DDR_WEn
              | MPU_ECAP0_IN_PWM0_OUT
              | MPU_EMU0
              | MPU_EMU1
              | MPU_EXTINTn
              | MPU_EXT_WAKEUP
              | MPU_GPMC_A0
              | MPU_GPMC_A1
              | MPU_GPMC_A2
              | MPU_GPMC_A3
              | MPU_GPMC_A4
              | MPU_GPMC_A5
              | MPU_GPMC_A6
              | MPU_GPMC_A7
              | MPU_GPMC_A8
              | MPU_GPMC_A9
              | MPU_GPMC_A10
              | MPU_GPMC_A11
              | MPU_GPMC_AD0
              | MPU_GPMC_AD1
              | MPU_GPMC_AD2
              | MPU_GPMC_AD3
              | MPU_GPMC_AD4
              | MPU_GPMC_AD5
              | MPU_GPMC_AD6
              | MPU_GPMC_AD7
              | MPU_GPMC_AD8
              | MPU_GPMC_AD9
              | MPU_GPMC_AD10
              | MPU_GPMC_AD11
              | MPU_GPMC_AD12
              | MPU_GPMC_AD13
              | MPU_GPMC_AD14
              | MPU_GPMC_AD15
              | MPU_GPMC_ADVn_ALE
              | MPU_GPMC_BEn0_CLE
              | MPU_GPMC_BEn1
              | MPU_GPMC_CLK
              | MPU_GPMC_CSn0
              | MPU_GPMC_CSn1
              | MPU_GPMC_CSn2
              | MPU_GPMC_CSn3
              | MPU_GPMC_OEn_REn
              | MPU_GPMC_WAIT0
              | MPU_GPMC_WEn
              | MPU_GPMC_WPn
              | MPU_I2C0_SCL
              | MPU_I2C0_SDA
              | MPU_LCD_AC_BIAS_EN
              | MPU_LCD_DATA0
              | MPU_LCD_DATA1
              | MPU_LCD_DATA2
              | MPU_LCD_DATA3
              | MPU_LCD_DATA4
              | MPU_LCD_DATA5
              | MPU_LCD_DATA6
              | MPU_LCD_DATA7
              | MPU_LCD_DATA8
              | MPU_LCD_DATA9
              | MPU_LCD_DATA10
              | MPU_LCD_DATA11
              | MPU_LCD_DATA12
              | MPU_LCD_DATA13
              | MPU_LCD_DATA14
              | MPU_LCD_DATA15
              | MPU_LCD_HSYNC
              | MPU_LCD_PCLK
              | MPU_LCD_VSYNC
              | MPU_MCASP0_ACLKR
              | MPU_MCASP0_ACLKX
              | MPU_MCASP0_AHCLKR
              | MPU_MCASP0_AHCLKX
              | MPU_MCASP0_AXR0
              | MPU_MCASP0_AXR1
              | MPU_MCASP0_FSR
              | MPU_MCASP0_FSX
              | MPU_MDC
              | MPU_MDIO
              | MPU_MII1_COL
              | MPU_MII1_CRS
              | MPU_MII1_RXD0
              | MPU_MII1_RXD1
              | MPU_MII1_RXD2
              | MPU_MII1_RXD3
              | MPU_MII1_RX_CLK
              | MPU_MII1_RX_DV
              | MPU_MII1_RX_ER
              | MPU_MII1_TXD0
              | MPU_MII1_TXD1
              | MPU_MII1_TXD2
              | MPU_MII1_TXD3
              | MPU_MII1_TX_CLK
              | MPU_MII1_TX_EN
              | MPU_MMC0_CLK
              | MPU_MMC0_CMD
              | MPU_MMC0_DAT0
              | MPU_MMC0_DAT1
              | MPU_MMC0_DAT2
              | MPU_MMC0_DAT3
              | MPU_PMIC_POWER_EN
              | MPU_PWRONRSTn
              | MPU_RESERVED
              | MPU_RMII1_REF_CLK
              | MPU_RTC_KALDO_ENn
              | MPU_RTC_PWRONRSTn
              | MPU_RTC_XTALIN
              | MPU_RTC_XTALOUT
              | MPU_SPI0_CS0
              | MPU_SPI0_CS1
              | MPU_SPI0_D0
              | MPU_SPI0_D1
              | MPU_SPI0_SCLK
              | MPU_TCK
              | MPU_TDI
              | MPU_TDO
              | MPU_TMS
              | MPU_TRSTn
              | MPU_UART0_CTSn
              | MPU_UART0_RTSn
              | MPU_UART0_RXD
              | MPU_UART0_TXD
              | MPU_UART1_CTSn
              | MPU_UART1_RTSn
              | MPU_UART1_RXD
              | MPU_UART1_TXD
              | MPU_USB0_CE
              | MPU_USB0_DM
              | MPU_USB0_DP
              | MPU_USB0_DRVVBUS
              | MPU_USB0_ID
              | MPU_USB0_VBUS
              | MPU_USB1_CE
              | MPU_USB1_DM
              | MPU_USB1_DP
              | MPU_USB1_DRVVBUS
              | MPU_USB1_ID
              | MPU_USB1_VBUS
              | MPU_VDDA1P8V_USB0
              | MPU_VDDA1P8V_USB1
              | MPU_VDDA3P3V_USB0
              | MPU_VDDA3P3V_USB1
              | MPU_VDDA_ADC
              | MPU_VDDS
              | MPU_VDDSHV1
              | MPU_VDDSHV2
              | MPU_VDDSHV3
              | MPU_VDDSHV4
              | MPU_VDDSHV5
              | MPU_VDDSHV6
              | MPU_VDDS_DDR
              | MPU_VDDS_OSC
              | MPU_VDDS_PLL_CORE_LCD
              | MPU_VDDS_PLL_DDR
              | MPU_VDDS_PLL_MPU
              | MPU_VDDS_RTC
              | MPU_VDDS_SRAM_CORE_BG
              | MPU_VDDS_SRAM_MPU_BB
              | MPU_VDD_CORE
              | MPU_VDD_MPU
              | MPU_VDD_MPU_MON
              | MPU_VPP
              | MPU_VREFN
              | MPU_VREFP
              | MPU_VSS
              | MPU_VSSA_ADC
              | MPU_VSSA_USB
              | MPU_VSS_OSC
              | MPU_VSS_RTC
              | MPU_WARMRSTn
              | MPU_XDMA_EVENT_INTR0
              | MPU_XDMA_EVENT_INTR1
              | MPU_XTALIN
              | MPU_XTALOUT
              deriving (Eq, Ord, Bounded, Enum, Show, Read)
 
data SignalId = Sig_AIN0
              | Sig_AIN1
              | Sig_AIN2
              | Sig_AIN3
              | Sig_AIN4
              | Sig_AIN5
              | Sig_AIN6
              | Sig_AIN7
              | Sig_CAP_VBB_MPU
              | Sig_CAP_VDD_RTC
              | Sig_CAP_VDD_SRAM_CORE
              | Sig_CAP_VDD_SRAM_MPU
              | Sig_EMU0
              | Sig_EMU1
              | Sig_EMU2
              | Sig_EMU3
              | Sig_EMU4
              | Sig_ENZ_KALDO_1P8V
              | Sig_EXT_WAKEUP
              | Sig_I2C0_SCL
              | Sig_I2C0_SDA
              | Sig_I2C1_SCL
              | Sig_I2C1_SDA
              | Sig_I2C2_SCL
              | Sig_I2C2_SDA
              | Sig_OSC0_IN
              | Sig_OSC0_OUT
              | Sig_OSC1_IN
              | Sig_OSC1_OUT
              | Sig_PMIC_POWER_EN
              | Sig_RTC_porz
              | Sig_TCK
              | Sig_TDI
              | Sig_TDO
              | Sig_TMS
              | Sig_USB0_CE
              | Sig_USB0_DM
              | Sig_USB0_DP
              | Sig_USB0_DRVVBUS
              | Sig_USB0_ID
              | Sig_USB0_VBUS
              | Sig_USB1_CE
              | Sig_USB1_DM
              | Sig_USB1_DP
              | Sig_USB1_DRVVBUS
              | Sig_USB1_ID
              | Sig_USB1_VBUS
              | Sig_VDDA1P8V_USB0
              | Sig_VDDA1P8V_USB1
              | Sig_VDDA3P3V_USB0
              | Sig_VDDA3P3V_USB1
              | Sig_VDDA_ADC
              | Sig_VDDS
              | Sig_VDDSHV1
              | Sig_VDDSHV2
              | Sig_VDDSHV3
              | Sig_VDDSHV4
              | Sig_VDDSHV5
              | Sig_VDDSHV6
              | Sig_VDDS_DDR
              | Sig_VDDS_OSC
              | Sig_VDDS_PLL_CORE_LCD
              | Sig_VDDS_PLL_DDR
              | Sig_VDDS_PLL_MPU
              | Sig_VDDS_RTC
              | Sig_VDDS_SRAM_CORE_BG
              | Sig_VDDS_SRAM_MPU_BB
              | Sig_VDD_CORE
              | Sig_VDD_MPU
              | Sig_VDD_MPU_MON
              | Sig_VPP
              | Sig_VREFN
              | Sig_VREFP
              | Sig_VSS
              | Sig_VSSA_ADC
              | Sig_VSSA_USB
              | Sig_VSS_OSC
              | Sig_VSS_RTC
              | Sig_clkout1
              | Sig_clkout2
              | Sig_dcan0_rx
              | Sig_dcan0_tx
              | Sig_dcan1_rx
              | Sig_dcan1_tx
              | Sig_ddr_a0
              | Sig_ddr_a1
              | Sig_ddr_a2
              | Sig_ddr_a3
              | Sig_ddr_a4
              | Sig_ddr_a5
              | Sig_ddr_a6
              | Sig_ddr_a7
              | Sig_ddr_a8
              | Sig_ddr_a9
              | Sig_ddr_a10
              | Sig_ddr_a11
              | Sig_ddr_a12
              | Sig_ddr_a13
              | Sig_ddr_a14
              | Sig_ddr_a15
              | Sig_ddr_ba0
              | Sig_ddr_ba1
              | Sig_ddr_ba2
              | Sig_ddr_casn
              | Sig_ddr_ck
              | Sig_ddr_cke
              | Sig_ddr_csn0
              | Sig_ddr_d0
              | Sig_ddr_d1
              | Sig_ddr_d2
              | Sig_ddr_d3
              | Sig_ddr_d4
              | Sig_ddr_d5
              | Sig_ddr_d6
              | Sig_ddr_d7
              | Sig_ddr_d8
              | Sig_ddr_d9
              | Sig_ddr_d10
              | Sig_ddr_d11
              | Sig_ddr_d12
              | Sig_ddr_d13
              | Sig_ddr_d14
              | Sig_ddr_d15
              | Sig_ddr_dqm0
              | Sig_ddr_dqm1
              | Sig_ddr_dqs0
              | Sig_ddr_dqs1
              | Sig_ddr_dqsn0
              | Sig_ddr_dqsn1
              | Sig_ddr_nck
              | Sig_ddr_odt
              | Sig_ddr_rasn
              | Sig_ddr_resetn
              | Sig_ddr_vref
              | Sig_ddr_vtp
              | Sig_ddr_wen
              | Sig_eCAP0_in_PWM0_out
              | Sig_eCAP1_in_PWM1_out
              | Sig_eCAP2_in_PWM2_out
              | Sig_eQEP0A_in
              | Sig_eQEP0B_in
              | Sig_eQEP0_index
              | Sig_eQEP0_strobe
              | Sig_eQEP1A_in
              | Sig_eQEP1B_in
              | Sig_eQEP1_index
              | Sig_eQEP1_strobe
              | Sig_eQEP2A_in
              | Sig_eQEP2B_in
              | Sig_eQEP2_index
              | Sig_eQEP2_strobe
              | Sig_ehrpwm0A
              | Sig_ehrpwm0B
              | Sig_ehrpwm0_synci
              | Sig_ehrpwm0_synco
              | Sig_ehrpwm0_tripzone_input
              | Sig_ehrpwm1A
              | Sig_ehrpwm1B
              | Sig_ehrpwm1_tripzone_input
              | Sig_ehrpwm2A
              | Sig_ehrpwm2B
              | Sig_ehrpwm2_tripzone_input
              | Sig_gmii1_col
              | Sig_gmii1_crs
              | Sig_gmii1_rxclk
              | Sig_gmii1_rxd0
              | Sig_gmii1_rxd1
              | Sig_gmii1_rxd2
              | Sig_gmii1_rxd3
              | Sig_gmii1_rxdv
              | Sig_gmii1_rxerr
              | Sig_gmii1_txclk
              | Sig_gmii1_txd0
              | Sig_gmii1_txd1
              | Sig_gmii1_txd2
              | Sig_gmii1_txd3
              | Sig_gmii1_txen
              | Sig_gmii2_col
              | Sig_gmii2_crs
              | Sig_gmii2_rxclk
              | Sig_gmii2_rxd0
              | Sig_gmii2_rxd1
              | Sig_gmii2_rxd2
              | Sig_gmii2_rxd3
              | Sig_gmii2_rxdv
              | Sig_gmii2_rxerr
              | Sig_gmii2_txclk
              | Sig_gmii2_txd0
              | Sig_gmii2_txd1
              | Sig_gmii2_txd2
              | Sig_gmii2_txd3
              | Sig_gmii2_txen
              | Sig_gpio0_0
              | Sig_gpio0_1
              | Sig_gpio0_2
              | Sig_gpio0_3
              | Sig_gpio0_4
              | Sig_gpio0_5
              | Sig_gpio0_6
              | Sig_gpio0_7
              | Sig_gpio0_8
              | Sig_gpio0_9
              | Sig_gpio0_10
              | Sig_gpio0_11
              | Sig_gpio0_12
              | Sig_gpio0_13
              | Sig_gpio0_14
              | Sig_gpio0_15
              | Sig_gpio0_16
              | Sig_gpio0_17
              | Sig_gpio0_18
              | Sig_gpio0_19
              | Sig_gpio0_20
              | Sig_gpio0_21
              | Sig_gpio0_22
              | Sig_gpio0_23
              | Sig_gpio0_26
              | Sig_gpio0_27
              | Sig_gpio0_28
              | Sig_gpio0_29
              | Sig_gpio0_30
              | Sig_gpio0_31
              | Sig_gpio1_0
              | Sig_gpio1_1
              | Sig_gpio1_2
              | Sig_gpio1_3
              | Sig_gpio1_4
              | Sig_gpio1_5
              | Sig_gpio1_6
              | Sig_gpio1_7
              | Sig_gpio1_8
              | Sig_gpio1_9
              | Sig_gpio1_10
              | Sig_gpio1_11
              | Sig_gpio1_12
              | Sig_gpio1_13
              | Sig_gpio1_14
              | Sig_gpio1_15
              | Sig_gpio1_16
              | Sig_gpio1_17
              | Sig_gpio1_18
              | Sig_gpio1_19
              | Sig_gpio1_20
              | Sig_gpio1_21
              | Sig_gpio1_22
              | Sig_gpio1_23
              | Sig_gpio1_24
              | Sig_gpio1_25
              | Sig_gpio1_26
              | Sig_gpio1_27
              | Sig_gpio1_28
              | Sig_gpio1_29
              | Sig_gpio1_30
              | Sig_gpio1_31
              | Sig_gpio2_0
              | Sig_gpio2_1
              | Sig_gpio2_2
              | Sig_gpio2_3
              | Sig_gpio2_4
              | Sig_gpio2_5
              | Sig_gpio2_6
              | Sig_gpio2_7
              | Sig_gpio2_8
              | Sig_gpio2_9
              | Sig_gpio2_10
              | Sig_gpio2_11
              | Sig_gpio2_12
              | Sig_gpio2_13
              | Sig_gpio2_14
              | Sig_gpio2_15
              | Sig_gpio2_16
              | Sig_gpio2_17
              | Sig_gpio2_18
              | Sig_gpio2_19
              | Sig_gpio2_20
              | Sig_gpio2_21
              | Sig_gpio2_22
              | Sig_gpio2_23
              | Sig_gpio2_24
              | Sig_gpio2_25
              | Sig_gpio2_26
              | Sig_gpio2_27
              | Sig_gpio2_28
              | Sig_gpio2_29
              | Sig_gpio2_30
              | Sig_gpio2_31
              | Sig_gpio3_0
              | Sig_gpio3_1
              | Sig_gpio3_2
              | Sig_gpio3_3
              | Sig_gpio3_4
              | Sig_gpio3_5
              | Sig_gpio3_6
              | Sig_gpio3_7
              | Sig_gpio3_8
              | Sig_gpio3_9
              | Sig_gpio3_10
              | Sig_gpio3_13
              | Sig_gpio3_14
              | Sig_gpio3_15
              | Sig_gpio3_16
              | Sig_gpio3_17
              | Sig_gpio3_18
              | Sig_gpio3_19
              | Sig_gpio3_20
              | Sig_gpio3_21
              | Sig_gpmc_a0
              | Sig_gpmc_a1
              | Sig_gpmc_a2
              | Sig_gpmc_a3
              | Sig_gpmc_a4
              | Sig_gpmc_a5
              | Sig_gpmc_a6
              | Sig_gpmc_a7
              | Sig_gpmc_a8
              | Sig_gpmc_a9
              | Sig_gpmc_a10
              | Sig_gpmc_a11
              | Sig_gpmc_a12
              | Sig_gpmc_a13
              | Sig_gpmc_a14
              | Sig_gpmc_a15
              | Sig_gpmc_a16
              | Sig_gpmc_a17
              | Sig_gpmc_a18
              | Sig_gpmc_a19
              | Sig_gpmc_a20
              | Sig_gpmc_a21
              | Sig_gpmc_a22
              | Sig_gpmc_a23
              | Sig_gpmc_a24
              | Sig_gpmc_a25
              | Sig_gpmc_a26
              | Sig_gpmc_a27
              | Sig_gpmc_ad0
              | Sig_gpmc_ad1
              | Sig_gpmc_ad2
              | Sig_gpmc_ad3
              | Sig_gpmc_ad4
              | Sig_gpmc_ad5
              | Sig_gpmc_ad6
              | Sig_gpmc_ad7
              | Sig_gpmc_ad8
              | Sig_gpmc_ad9
              | Sig_gpmc_ad10
              | Sig_gpmc_ad11
              | Sig_gpmc_ad12
              | Sig_gpmc_ad13
              | Sig_gpmc_ad14
              | Sig_gpmc_ad15
              | Sig_gpmc_advn_ale
              | Sig_gpmc_be0n_cle
              | Sig_gpmc_be1n
              | Sig_gpmc_clk
              | Sig_gpmc_csn0
              | Sig_gpmc_csn1
              | Sig_gpmc_csn2
              | Sig_gpmc_csn3
              | Sig_gpmc_csn4
              | Sig_gpmc_csn5
              | Sig_gpmc_csn6
              | Sig_gpmc_dir
              | Sig_gpmc_oen_ren
              | Sig_gpmc_wait0
              | Sig_gpmc_wait1
              | Sig_gpmc_wen
              | Sig_gpmc_wpn
              | Sig_lcd_ac_bias_en
              | Sig_lcd_data0
              | Sig_lcd_data1
              | Sig_lcd_data2
              | Sig_lcd_data3
              | Sig_lcd_data4
              | Sig_lcd_data5
              | Sig_lcd_data6
              | Sig_lcd_data7
              | Sig_lcd_data8
              | Sig_lcd_data9
              | Sig_lcd_data10
              | Sig_lcd_data11
              | Sig_lcd_data12
              | Sig_lcd_data13
              | Sig_lcd_data14
              | Sig_lcd_data15
              | Sig_lcd_data16
              | Sig_lcd_data17
              | Sig_lcd_data18
              | Sig_lcd_data19
              | Sig_lcd_data20
              | Sig_lcd_data21
              | Sig_lcd_data22
              | Sig_lcd_data23
              | Sig_lcd_hsync
              | Sig_lcd_memory_clk
              | Sig_lcd_pclk
              | Sig_lcd_vsync
              | Sig_mcasp0_aclkr
              | Sig_mcasp0_aclkx
              | Sig_mcasp0_ahclkr
              | Sig_mcasp0_ahclkx
              | Sig_mcasp0_axr0
              | Sig_mcasp0_axr1
              | Sig_mcasp0_axr2
              | Sig_mcasp0_axr3
              | Sig_mcasp0_fsr
              | Sig_mcasp0_fsx
              | Sig_mcasp1_aclkr
              | Sig_mcasp1_aclkx
              | Sig_mcasp1_ahclkr
              | Sig_mcasp1_ahclkx
              | Sig_mcasp1_axr0
              | Sig_mcasp1_axr1
              | Sig_mcasp1_axr2
              | Sig_mcasp1_axr3
              | Sig_mcasp1_fsr
              | Sig_mcasp1_fsx
              | Sig_mdio_clk
              | Sig_mdio_data
              | Sig_mmc0_clk
              | Sig_mmc0_cmd
              | Sig_mmc0_dat0
              | Sig_mmc0_dat1
              | Sig_mmc0_dat2
              | Sig_mmc0_dat3
              | Sig_mmc0_dat4
              | Sig_mmc0_dat5
              | Sig_mmc0_dat6
              | Sig_mmc0_dat7
              | Sig_mmc0_pow
              | Sig_mmc0_sdcd
              | Sig_mmc0_sdwp
              | Sig_mmc1_clk
              | Sig_mmc1_cmd
              | Sig_mmc1_dat0
              | Sig_mmc1_dat1
              | Sig_mmc1_dat2
              | Sig_mmc1_dat3
              | Sig_mmc1_dat4
              | Sig_mmc1_dat5
              | Sig_mmc1_dat6
              | Sig_mmc1_dat7
              | Sig_mmc1_sdcd
              | Sig_mmc1_sdwp
              | Sig_mmc2_clk
              | Sig_mmc2_cmd
              | Sig_mmc2_dat0
              | Sig_mmc2_dat1
              | Sig_mmc2_dat2
              | Sig_mmc2_dat3
              | Sig_mmc2_dat4
              | Sig_mmc2_dat5
              | Sig_mmc2_dat6
              | Sig_mmc2_dat7
              | Sig_mmc2_sdcd
              | Sig_mmc2_sdwp
              | Sig_nNMI
              | Sig_nRESETIN_OUT
              | Sig_nTRST
              | Sig_porz
              | Sig_pr1_ecap0_ecap_capin_apwm_o
              | Sig_pr1_edc_latch0_in
              | Sig_pr1_edc_latch1_in
              | Sig_pr1_edc_sync0_out
              | Sig_pr1_edc_sync1_out
              | Sig_pr1_edio_data_in0
              | Sig_pr1_edio_data_in1
              | Sig_pr1_edio_data_in2
              | Sig_pr1_edio_data_in3
              | Sig_pr1_edio_data_in4
              | Sig_pr1_edio_data_in5
              | Sig_pr1_edio_data_in6
              | Sig_pr1_edio_data_in7
              | Sig_pr1_edio_data_out0
              | Sig_pr1_edio_data_out1
              | Sig_pr1_edio_data_out2
              | Sig_pr1_edio_data_out3
              | Sig_pr1_edio_data_out4
              | Sig_pr1_edio_data_out5
              | Sig_pr1_edio_data_out6
              | Sig_pr1_edio_data_out7
              | Sig_pr1_edio_latch_in
              | Sig_pr1_edio_sof
              | Sig_pr1_mdio_data
              | Sig_pr1_mdio_mdclk
              | Sig_pr1_mii0_col
              | Sig_pr1_mii0_crs
              | Sig_pr1_mii0_rxd0
              | Sig_pr1_mii0_rxd1
              | Sig_pr1_mii0_rxd2
              | Sig_pr1_mii0_rxd3
              | Sig_pr1_mii0_rxdv
              | Sig_pr1_mii0_rxer
              | Sig_pr1_mii0_rxlink
              | Sig_pr1_mii0_txd0
              | Sig_pr1_mii0_txd1
              | Sig_pr1_mii0_txd2
              | Sig_pr1_mii0_txd3
              | Sig_pr1_mii0_txen
              | Sig_pr1_mii1_col
              | Sig_pr1_mii1_crs
              | Sig_pr1_mii1_rxd0
              | Sig_pr1_mii1_rxd1
              | Sig_pr1_mii1_rxd2
              | Sig_pr1_mii1_rxd3
              | Sig_pr1_mii1_rxdv
              | Sig_pr1_mii1_rxer
              | Sig_pr1_mii1_rxlink
              | Sig_pr1_mii1_txd0
              | Sig_pr1_mii1_txd1
              | Sig_pr1_mii1_txd2
              | Sig_pr1_mii1_txd3
              | Sig_pr1_mii1_txen
              | Sig_pr1_mii_mr0_clk
              | Sig_pr1_mii_mr1_clk
              | Sig_pr1_mii_mt0_clk
              | Sig_pr1_mii_mt1_clk
              | Sig_pr1_pru0_pru_r30_0
              | Sig_pr1_pru0_pru_r30_1
              | Sig_pr1_pru0_pru_r30_2
              | Sig_pr1_pru0_pru_r30_3
              | Sig_pr1_pru0_pru_r30_4
              | Sig_pr1_pru0_pru_r30_5
              | Sig_pr1_pru0_pru_r30_6
              | Sig_pr1_pru0_pru_r30_7
              | Sig_pr1_pru0_pru_r30_8
              | Sig_pr1_pru0_pru_r30_9
              | Sig_pr1_pru0_pru_r30_10
              | Sig_pr1_pru0_pru_r30_11
              | Sig_pr1_pru0_pru_r30_12
              | Sig_pr1_pru0_pru_r30_13
              | Sig_pr1_pru0_pru_r30_14
              | Sig_pr1_pru0_pru_r30_15
              | Sig_pr1_pru0_pru_r31_0
              | Sig_pr1_pru0_pru_r31_1
              | Sig_pr1_pru0_pru_r31_2
              | Sig_pr1_pru0_pru_r31_3
              | Sig_pr1_pru0_pru_r31_4
              | Sig_pr1_pru0_pru_r31_5
              | Sig_pr1_pru0_pru_r31_6
              | Sig_pr1_pru0_pru_r31_7
              | Sig_pr1_pru0_pru_r31_8
              | Sig_pr1_pru0_pru_r31_9
              | Sig_pr1_pru0_pru_r31_10
              | Sig_pr1_pru0_pru_r31_11
              | Sig_pr1_pru0_pru_r31_12
              | Sig_pr1_pru0_pru_r31_13
              | Sig_pr1_pru0_pru_r31_14
              | Sig_pr1_pru0_pru_r31_15
              | Sig_pr1_pru0_pru_r31_16
              | Sig_pr1_pru1_pru_r30_0
              | Sig_pr1_pru1_pru_r30_1
              | Sig_pr1_pru1_pru_r30_2
              | Sig_pr1_pru1_pru_r30_3
              | Sig_pr1_pru1_pru_r30_4
              | Sig_pr1_pru1_pru_r30_5
              | Sig_pr1_pru1_pru_r30_6
              | Sig_pr1_pru1_pru_r30_7
              | Sig_pr1_pru1_pru_r30_8
              | Sig_pr1_pru1_pru_r30_9
              | Sig_pr1_pru1_pru_r30_10
              | Sig_pr1_pru1_pru_r30_11
              | Sig_pr1_pru1_pru_r30_12
              | Sig_pr1_pru1_pru_r30_13
              | Sig_pr1_pru1_pru_r30_14
              | Sig_pr1_pru1_pru_r30_15
              | Sig_pr1_pru1_pru_r31_0
              | Sig_pr1_pru1_pru_r31_1
              | Sig_pr1_pru1_pru_r31_2
              | Sig_pr1_pru1_pru_r31_3
              | Sig_pr1_pru1_pru_r31_4
              | Sig_pr1_pru1_pru_r31_5
              | Sig_pr1_pru1_pru_r31_6
              | Sig_pr1_pru1_pru_r31_7
              | Sig_pr1_pru1_pru_r31_8
              | Sig_pr1_pru1_pru_r31_9
              | Sig_pr1_pru1_pru_r31_10
              | Sig_pr1_pru1_pru_r31_11
              | Sig_pr1_pru1_pru_r31_12
              | Sig_pr1_pru1_pru_r31_13
              | Sig_pr1_pru1_pru_r31_14
              | Sig_pr1_pru1_pru_r31_15
              | Sig_pr1_pru1_pru_r31_16
              | Sig_pr1_uart0_cts_n
              | Sig_pr1_uart0_rts_n
              | Sig_pr1_uart0_rxd
              | Sig_pr1_uart0_txd
              | Sig_rgmii1_rclk
              | Sig_rgmii1_rctl
              | Sig_rgmii1_rd0
              | Sig_rgmii1_rd1
              | Sig_rgmii1_rd2
              | Sig_rgmii1_rd3
              | Sig_rgmii1_tclk
              | Sig_rgmii1_tctl
              | Sig_rgmii1_td0
              | Sig_rgmii1_td1
              | Sig_rgmii1_td2
              | Sig_rgmii1_td3
              | Sig_rgmii2_rclk
              | Sig_rgmii2_rctl
              | Sig_rgmii2_rd0
              | Sig_rgmii2_rd1
              | Sig_rgmii2_rd2
              | Sig_rgmii2_rd3
              | Sig_rgmii2_tclk
              | Sig_rgmii2_tctl
              | Sig_rgmii2_td0
              | Sig_rgmii2_td1
              | Sig_rgmii2_td2
              | Sig_rgmii2_td3
              | Sig_rmii1_crs_dv
              | Sig_rmii1_refclk
              | Sig_rmii1_rxd0
              | Sig_rmii1_rxd1
              | Sig_rmii1_rxerr
              | Sig_rmii1_txd0
              | Sig_rmii1_txd1
              | Sig_rmii1_txen
              | Sig_rmii2_crs_dv
              | Sig_rmii2_refclk
              | Sig_rmii2_rxd0
              | Sig_rmii2_rxd1
              | Sig_rmii2_rxerr
              | Sig_rmii2_txd0
              | Sig_rmii2_txd1
              | Sig_rmii2_txen
              | Sig_spi0_cs0
              | Sig_spi0_cs1
              | Sig_spi0_d0
              | Sig_spi0_d1
              | Sig_spi0_sclk
              | Sig_spi1_cs0
              | Sig_spi1_cs1
              | Sig_spi1_d0
              | Sig_spi1_d1
              | Sig_spi1_sclk
              | Sig_tclkin
              | Sig_testout
              | Sig_timer4
              | Sig_timer5
              | Sig_timer6
              | Sig_timer7
              | Sig_uart0_ctsn
              | Sig_uart0_rtsn
              | Sig_uart0_rxd
              | Sig_uart0_txd
              | Sig_uart1_ctsn
              | Sig_uart1_dcdn
              | Sig_uart1_dsrn
              | Sig_uart1_dtrn
              | Sig_uart1_rin
              | Sig_uart1_rtsn
              | Sig_uart1_rxd
              | Sig_uart1_txd
              | Sig_uart2_ctsn
              | Sig_uart2_rtsn
              | Sig_uart2_rxd
              | Sig_uart2_txd
              | Sig_uart3_ctsn
              | Sig_uart3_rtsn
              | Sig_uart3_rxd
              | Sig_uart3_txd
              | Sig_uart4_ctsn
              | Sig_uart4_rtsn
              | Sig_uart4_rxd
              | Sig_uart4_txd
              | Sig_uart5_ctsn
              | Sig_uart5_rtsn
              | Sig_uart5_rxd
              | Sig_uart5_txd
              | Sig_xdma_event_intr0
              | Sig_xdma_event_intr1
              | Sig_xdma_event_intr2
              deriving (Eq, Ord, Bounded, Enum, Show, Read)
 
bbPins :: Map.Map BBPinId BBPin
bbPins
  = Map.fromList
      [(BB_P8_1, BBPin BB_P8_1 "GND" Nothing),
       (BB_P8_2, BBPin BB_P8_2 "GND" Nothing),
       (BB_P8_3, BBPin BB_P8_3 "GPIO1_6" (Just MPU_GPMC_AD6)),
       (BB_P8_4, BBPin BB_P8_4 "GPIO1_7" (Just MPU_GPMC_AD7)),
       (BB_P8_5, BBPin BB_P8_5 "GPIO1_2" (Just MPU_GPMC_AD2)),
       (BB_P8_6, BBPin BB_P8_6 "GPIO1_3" (Just MPU_GPMC_AD3)),
       (BB_P8_7, BBPin BB_P8_7 "TIMER4" (Just MPU_GPMC_ADVn_ALE)),
       (BB_P8_8, BBPin BB_P8_8 "TIMER7" (Just MPU_GPMC_OEn_REn)),
       (BB_P8_9, BBPin BB_P8_9 "TIMER5" (Just MPU_GPMC_BEn0_CLE)),
       (BB_P8_10, BBPin BB_P8_10 "TIMER6" (Just MPU_GPMC_WEn)),
       (BB_P8_11, BBPin BB_P8_11 "GPIO1_13" (Just MPU_GPMC_AD13)),
       (BB_P8_12, BBPin BB_P8_12 "GPIO1_12" (Just MPU_GPMC_AD12)),
       (BB_P8_13, BBPin BB_P8_13 "EHRPWM2B" (Just MPU_GPMC_AD9)),
       (BB_P8_14, BBPin BB_P8_14 "GPIO0_26" (Just MPU_GPMC_AD10)),
       (BB_P8_15, BBPin BB_P8_15 "GPIO1_15" (Just MPU_GPMC_AD15)),
       (BB_P8_16, BBPin BB_P8_16 "GPIO1_14" (Just MPU_GPMC_AD14)),
       (BB_P8_17, BBPin BB_P8_17 "GPIO0_27" (Just MPU_GPMC_AD11)),
       (BB_P8_18, BBPin BB_P8_18 "GPIO2_1" (Just MPU_GPMC_CLK)),
       (BB_P8_19, BBPin BB_P8_19 "EHRPWM2A" (Just MPU_GPMC_AD8)),
       (BB_P8_20, BBPin BB_P8_20 "GPIO1_31" (Just MPU_GPMC_CSn2)),
       (BB_P8_21, BBPin BB_P8_21 "GPIO1_30" (Just MPU_GPMC_CSn1)),
       (BB_P8_22, BBPin BB_P8_22 "GPIO1_5" (Just MPU_GPMC_AD5)),
       (BB_P8_23, BBPin BB_P8_23 "GPIO1_4" (Just MPU_GPMC_AD4)),
       (BB_P8_24, BBPin BB_P8_24 "GPIO1_1" (Just MPU_GPMC_AD1)),
       (BB_P8_25, BBPin BB_P8_25 "GPIO1_0" (Just MPU_GPMC_AD0)),
       (BB_P8_26, BBPin BB_P8_26 "GPIO1_29" (Just MPU_GPMC_CSn0)),
       (BB_P8_27, BBPin BB_P8_27 "GPIO2_22" (Just MPU_LCD_VSYNC)),
       (BB_P8_28, BBPin BB_P8_28 "GPIO2_24" (Just MPU_LCD_PCLK)),
       (BB_P8_29, BBPin BB_P8_29 "GPIO2_23" (Just MPU_LCD_HSYNC)),
       (BB_P8_30, BBPin BB_P8_30 "GPIO2_25" (Just MPU_LCD_AC_BIAS_EN)),
       (BB_P8_31, BBPin BB_P8_31 "UART5_CTSN" (Just MPU_LCD_DATA14)),
       (BB_P8_32, BBPin BB_P8_32 "UART5_RTSN" (Just MPU_LCD_DATA15)),
       (BB_P8_33, BBPin BB_P8_33 "UART4_RTSN" (Just MPU_LCD_DATA13)),
       (BB_P8_34, BBPin BB_P8_34 "UART3_RTSN" (Just MPU_LCD_DATA11)),
       (BB_P8_35, BBPin BB_P8_35 "UART4_CTSN" (Just MPU_LCD_DATA12)),
       (BB_P8_36, BBPin BB_P8_36 "UART3_CTSN" (Just MPU_LCD_DATA10)),
       (BB_P8_37, BBPin BB_P8_37 "UART5_TXD" (Just MPU_LCD_DATA8)),
       (BB_P8_38, BBPin BB_P8_38 "UART5_RXD" (Just MPU_LCD_DATA9)),
       (BB_P8_39, BBPin BB_P8_39 "GPIO2_12" (Just MPU_LCD_DATA6)),
       (BB_P8_40, BBPin BB_P8_40 "GPIO2_13" (Just MPU_LCD_DATA7)),
       (BB_P8_41, BBPin BB_P8_41 "GPIO2_10" (Just MPU_LCD_DATA4)),
       (BB_P8_42, BBPin BB_P8_42 "GPIO2_11" (Just MPU_LCD_DATA5)),
       (BB_P8_43, BBPin BB_P8_43 "GPIO2_8" (Just MPU_LCD_DATA2)),
       (BB_P8_44, BBPin BB_P8_44 "GPIO2_9" (Just MPU_LCD_DATA3)),
       (BB_P8_45, BBPin BB_P8_45 "GPIO2_6" (Just MPU_LCD_DATA0)),
       (BB_P8_46, BBPin BB_P8_46 "GPIO2_7" (Just MPU_LCD_DATA1)),
       (BB_P9_1, BBPin BB_P9_1 "GND" Nothing),
       (BB_P9_2, BBPin BB_P9_2 "GND" Nothing),
       (BB_P9_3, BBPin BB_P9_3 "DC_3.3V" Nothing),
       (BB_P9_4, BBPin BB_P9_4 "DC_3.3V" Nothing),
       (BB_P9_5, BBPin BB_P9_5 "VDD_5V" Nothing),
       (BB_P9_6, BBPin BB_P9_6 "VDD_5V" Nothing),
       (BB_P9_7, BBPin BB_P9_7 "SYS_5V" Nothing),
       (BB_P9_8, BBPin BB_P9_8 "SYS_5V" Nothing),
       (BB_P9_9, BBPin BB_P9_9 "PWR_BUT" Nothing),
       (BB_P9_10, BBPin BB_P9_10 "SYS_RESETn" (Just MPU_WARMRSTn)),
       (BB_P9_11, BBPin BB_P9_11 "UART4_RXD" (Just MPU_GPMC_WAIT0)),
       (BB_P9_12, BBPin BB_P9_12 "GPIO1_28" (Just MPU_GPMC_BEn1)),
       (BB_P9_13, BBPin BB_P9_13 "UART4_TXD" (Just MPU_GPMC_WPn)),
       (BB_P9_14, BBPin BB_P9_14 "EHRPWM1A" (Just MPU_GPMC_A2)),
       (BB_P9_15, BBPin BB_P9_15 "GPIO1_16" (Just MPU_GPMC_A0)),
       (BB_P9_16, BBPin BB_P9_16 "EHRPWM1B" (Just MPU_GPMC_A3)),
       (BB_P9_17, BBPin BB_P9_17 "I2C1_SCL" (Just MPU_SPI0_CS0)),
       (BB_P9_18, BBPin BB_P9_18 "I2C1_SDA" (Just MPU_SPI0_D1)),
       (BB_P9_19, BBPin BB_P9_19 "I2C2_SCL" (Just MPU_UART1_RTSn)),
       (BB_P9_20, BBPin BB_P9_20 "I2C2_SDA" (Just MPU_UART1_CTSn)),
       (BB_P9_21, BBPin BB_P9_21 "UART2_TXD" (Just MPU_SPI0_D0)),
       (BB_P9_22, BBPin BB_P9_22 "UART2_RXD" (Just MPU_SPI0_SCLK)),
       (BB_P9_23, BBPin BB_P9_23 "GPIO1_17" (Just MPU_GPMC_A1)),
       (BB_P9_24, BBPin BB_P9_24 "UART1_TXD" (Just MPU_UART1_TXD)),
       (BB_P9_25, BBPin BB_P9_25 "GPIO3_21" (Just MPU_MCASP0_AHCLKX)),
       (BB_P9_26, BBPin BB_P9_26 "UART1_RXD" (Just MPU_UART1_RXD)),
       (BB_P9_27, BBPin BB_P9_27 "GPIO3_19" (Just MPU_MCASP0_FSR)),
       (BB_P9_28, BBPin BB_P9_28 "SPI1_CS0" (Just MPU_MCASP0_AHCLKR)),
       (BB_P9_29, BBPin BB_P9_29 "SPI1_D0" (Just MPU_MCASP0_FSX)),
       (BB_P9_30, BBPin BB_P9_30 "SPI1_D1" (Just MPU_MCASP0_AXR0)),
       (BB_P9_31, BBPin BB_P9_31 "SPI1_SCLK" (Just MPU_MCASP0_ACLKX)),
       (BB_P9_32, BBPin BB_P9_32 "VADC" Nothing),
       (BB_P9_33, BBPin BB_P9_33 "AIN4" (Just MPU_AIN4)),
       (BB_P9_34, BBPin BB_P9_34 "AGND" Nothing),
       (BB_P9_35, BBPin BB_P9_35 "AIN6" (Just MPU_AIN6)),
       (BB_P9_36, BBPin BB_P9_36 "AIN5" (Just MPU_AIN5)),
       (BB_P9_37, BBPin BB_P9_37 "AIN2" (Just MPU_AIN2)),
       (BB_P9_38, BBPin BB_P9_38 "AIN3" (Just MPU_AIN3)),
       (BB_P9_39, BBPin BB_P9_39 "AIN0" (Just MPU_AIN0)),
       (BB_P9_40, BBPin BB_P9_40 "AIN1" (Just MPU_AIN1)),
       (BB_P9_41, BBPin BB_P9_41 "CLKOUT2" (Just MPU_XDMA_EVENT_INTR1)),
       (BB_P9_42, BBPin BB_P9_42 "GPIO0_7" (Just MPU_ECAP0_IN_PWM0_OUT)),
       (BB_P9_43, BBPin BB_P9_43 "GND" Nothing),
       (BB_P9_44, BBPin BB_P9_44 "GND" Nothing),
       (BB_P9_45, BBPin BB_P9_45 "GND" Nothing),
       (BB_P9_46, BBPin BB_P9_46 "GND" Nothing),
       (BB_USR0, BBPin BB_USR0 "GPIO1_21" (Just MPU_GPMC_A5)),
       (BB_USR1, BBPin BB_USR1 "GPIO1_22" (Just MPU_GPMC_A6)),
       (BB_USR2, BBPin BB_USR2 "GPIO1_23" (Just MPU_GPMC_A7)),
       (BB_USR3, BBPin BB_USR3 "GPIO1_24" (Just MPU_GPMC_A8))]
 
mpuPins :: Map.Map MPUPinId MPUPin
mpuPins
  = Map.fromList
      [(MPU_AIN0,
        MPUPin MPU_AIN0 (Just "ain0")
          (Map.fromList [(Sig_AIN0, MPUPinSignal (Just 0) Sig_AIN0)])),
       (MPU_AIN1,
        MPUPin MPU_AIN1 (Just "ain1")
          (Map.fromList [(Sig_AIN1, MPUPinSignal (Just 0) Sig_AIN1)])),
       (MPU_AIN2,
        MPUPin MPU_AIN2 (Just "ain2")
          (Map.fromList [(Sig_AIN2, MPUPinSignal (Just 0) Sig_AIN2)])),
       (MPU_AIN3,
        MPUPin MPU_AIN3 (Just "ain3")
          (Map.fromList [(Sig_AIN3, MPUPinSignal (Just 0) Sig_AIN3)])),
       (MPU_AIN4,
        MPUPin MPU_AIN4 (Just "ain4")
          (Map.fromList [(Sig_AIN4, MPUPinSignal (Just 0) Sig_AIN4)])),
       (MPU_AIN5,
        MPUPin MPU_AIN5 (Just "ain5")
          (Map.fromList [(Sig_AIN5, MPUPinSignal (Just 0) Sig_AIN5)])),
       (MPU_AIN6,
        MPUPin MPU_AIN6 (Just "ain6")
          (Map.fromList [(Sig_AIN6, MPUPinSignal (Just 0) Sig_AIN6)])),
       (MPU_AIN7,
        MPUPin MPU_AIN7 (Just "ain7")
          (Map.fromList [(Sig_AIN7, MPUPinSignal (Just 0) Sig_AIN7)])),
       (MPU_CAP_VBB_MPU,
        MPUPin MPU_CAP_VBB_MPU Nothing
          (Map.fromList
             [(Sig_CAP_VBB_MPU, MPUPinSignal Nothing Sig_CAP_VBB_MPU)])),
       (MPU_CAP_VDD_RTC,
        MPUPin MPU_CAP_VDD_RTC Nothing
          (Map.fromList
             [(Sig_CAP_VDD_RTC, MPUPinSignal Nothing Sig_CAP_VDD_RTC)])),
       (MPU_CAP_VDD_SRAM_CORE,
        MPUPin MPU_CAP_VDD_SRAM_CORE Nothing
          (Map.fromList
             [(Sig_CAP_VDD_SRAM_CORE,
               MPUPinSignal Nothing Sig_CAP_VDD_SRAM_CORE)])),
       (MPU_CAP_VDD_SRAM_MPU,
        MPUPin MPU_CAP_VDD_SRAM_MPU Nothing
          (Map.fromList
             [(Sig_CAP_VDD_SRAM_MPU,
               MPUPinSignal Nothing Sig_CAP_VDD_SRAM_MPU)])),
       (MPU_DDR_A0,
        MPUPin MPU_DDR_A0 Nothing
          (Map.fromList [(Sig_ddr_a0, MPUPinSignal (Just 0) Sig_ddr_a0)])),
       (MPU_DDR_A1,
        MPUPin MPU_DDR_A1 Nothing
          (Map.fromList [(Sig_ddr_a1, MPUPinSignal (Just 0) Sig_ddr_a1)])),
       (MPU_DDR_A2,
        MPUPin MPU_DDR_A2 Nothing
          (Map.fromList [(Sig_ddr_a2, MPUPinSignal (Just 0) Sig_ddr_a2)])),
       (MPU_DDR_A3,
        MPUPin MPU_DDR_A3 Nothing
          (Map.fromList [(Sig_ddr_a3, MPUPinSignal (Just 0) Sig_ddr_a3)])),
       (MPU_DDR_A4,
        MPUPin MPU_DDR_A4 Nothing
          (Map.fromList [(Sig_ddr_a4, MPUPinSignal (Just 0) Sig_ddr_a4)])),
       (MPU_DDR_A5,
        MPUPin MPU_DDR_A5 Nothing
          (Map.fromList [(Sig_ddr_a5, MPUPinSignal (Just 0) Sig_ddr_a5)])),
       (MPU_DDR_A6,
        MPUPin MPU_DDR_A6 Nothing
          (Map.fromList [(Sig_ddr_a6, MPUPinSignal (Just 0) Sig_ddr_a6)])),
       (MPU_DDR_A7,
        MPUPin MPU_DDR_A7 Nothing
          (Map.fromList [(Sig_ddr_a7, MPUPinSignal (Just 0) Sig_ddr_a7)])),
       (MPU_DDR_A8,
        MPUPin MPU_DDR_A8 Nothing
          (Map.fromList [(Sig_ddr_a8, MPUPinSignal (Just 0) Sig_ddr_a8)])),
       (MPU_DDR_A9,
        MPUPin MPU_DDR_A9 Nothing
          (Map.fromList [(Sig_ddr_a9, MPUPinSignal (Just 0) Sig_ddr_a9)])),
       (MPU_DDR_A10,
        MPUPin MPU_DDR_A10 Nothing
          (Map.fromList [(Sig_ddr_a10, MPUPinSignal (Just 0) Sig_ddr_a10)])),
       (MPU_DDR_A11,
        MPUPin MPU_DDR_A11 Nothing
          (Map.fromList [(Sig_ddr_a11, MPUPinSignal (Just 0) Sig_ddr_a11)])),
       (MPU_DDR_A12,
        MPUPin MPU_DDR_A12 Nothing
          (Map.fromList [(Sig_ddr_a12, MPUPinSignal (Just 0) Sig_ddr_a12)])),
       (MPU_DDR_A13,
        MPUPin MPU_DDR_A13 Nothing
          (Map.fromList [(Sig_ddr_a13, MPUPinSignal (Just 0) Sig_ddr_a13)])),
       (MPU_DDR_A14,
        MPUPin MPU_DDR_A14 Nothing
          (Map.fromList [(Sig_ddr_a14, MPUPinSignal (Just 0) Sig_ddr_a14)])),
       (MPU_DDR_A15,
        MPUPin MPU_DDR_A15 Nothing
          (Map.fromList [(Sig_ddr_a15, MPUPinSignal (Just 0) Sig_ddr_a15)])),
       (MPU_DDR_BA0,
        MPUPin MPU_DDR_BA0 Nothing
          (Map.fromList [(Sig_ddr_ba0, MPUPinSignal (Just 0) Sig_ddr_ba0)])),
       (MPU_DDR_BA1,
        MPUPin MPU_DDR_BA1 Nothing
          (Map.fromList [(Sig_ddr_ba1, MPUPinSignal (Just 0) Sig_ddr_ba1)])),
       (MPU_DDR_BA2,
        MPUPin MPU_DDR_BA2 Nothing
          (Map.fromList [(Sig_ddr_ba2, MPUPinSignal (Just 0) Sig_ddr_ba2)])),
       (MPU_DDR_CASn,
        MPUPin MPU_DDR_CASn Nothing
          (Map.fromList
             [(Sig_ddr_casn, MPUPinSignal (Just 0) Sig_ddr_casn)])),
       (MPU_DDR_CK,
        MPUPin MPU_DDR_CK Nothing
          (Map.fromList [(Sig_ddr_ck, MPUPinSignal (Just 0) Sig_ddr_ck)])),
       (MPU_DDR_CKE,
        MPUPin MPU_DDR_CKE Nothing
          (Map.fromList [(Sig_ddr_cke, MPUPinSignal (Just 0) Sig_ddr_cke)])),
       (MPU_DDR_CKn,
        MPUPin MPU_DDR_CKn Nothing
          (Map.fromList [(Sig_ddr_nck, MPUPinSignal (Just 0) Sig_ddr_nck)])),
       (MPU_DDR_CSn0,
        MPUPin MPU_DDR_CSn0 Nothing
          (Map.fromList
             [(Sig_ddr_csn0, MPUPinSignal (Just 0) Sig_ddr_csn0)])),
       (MPU_DDR_D0,
        MPUPin MPU_DDR_D0 Nothing
          (Map.fromList [(Sig_ddr_d0, MPUPinSignal (Just 0) Sig_ddr_d0)])),
       (MPU_DDR_D1,
        MPUPin MPU_DDR_D1 Nothing
          (Map.fromList [(Sig_ddr_d1, MPUPinSignal (Just 0) Sig_ddr_d1)])),
       (MPU_DDR_D2,
        MPUPin MPU_DDR_D2 Nothing
          (Map.fromList [(Sig_ddr_d2, MPUPinSignal (Just 0) Sig_ddr_d2)])),
       (MPU_DDR_D3,
        MPUPin MPU_DDR_D3 Nothing
          (Map.fromList [(Sig_ddr_d3, MPUPinSignal (Just 0) Sig_ddr_d3)])),
       (MPU_DDR_D4,
        MPUPin MPU_DDR_D4 Nothing
          (Map.fromList [(Sig_ddr_d4, MPUPinSignal (Just 0) Sig_ddr_d4)])),
       (MPU_DDR_D5,
        MPUPin MPU_DDR_D5 Nothing
          (Map.fromList [(Sig_ddr_d5, MPUPinSignal (Just 0) Sig_ddr_d5)])),
       (MPU_DDR_D6,
        MPUPin MPU_DDR_D6 Nothing
          (Map.fromList [(Sig_ddr_d6, MPUPinSignal (Just 0) Sig_ddr_d6)])),
       (MPU_DDR_D7,
        MPUPin MPU_DDR_D7 Nothing
          (Map.fromList [(Sig_ddr_d7, MPUPinSignal (Just 0) Sig_ddr_d7)])),
       (MPU_DDR_D8,
        MPUPin MPU_DDR_D8 Nothing
          (Map.fromList [(Sig_ddr_d8, MPUPinSignal (Just 0) Sig_ddr_d8)])),
       (MPU_DDR_D9,
        MPUPin MPU_DDR_D9 Nothing
          (Map.fromList [(Sig_ddr_d9, MPUPinSignal (Just 0) Sig_ddr_d9)])),
       (MPU_DDR_D10,
        MPUPin MPU_DDR_D10 Nothing
          (Map.fromList [(Sig_ddr_d10, MPUPinSignal (Just 0) Sig_ddr_d10)])),
       (MPU_DDR_D11,
        MPUPin MPU_DDR_D11 Nothing
          (Map.fromList [(Sig_ddr_d11, MPUPinSignal (Just 0) Sig_ddr_d11)])),
       (MPU_DDR_D12,
        MPUPin MPU_DDR_D12 Nothing
          (Map.fromList [(Sig_ddr_d12, MPUPinSignal (Just 0) Sig_ddr_d12)])),
       (MPU_DDR_D13,
        MPUPin MPU_DDR_D13 Nothing
          (Map.fromList [(Sig_ddr_d13, MPUPinSignal (Just 0) Sig_ddr_d13)])),
       (MPU_DDR_D14,
        MPUPin MPU_DDR_D14 Nothing
          (Map.fromList [(Sig_ddr_d14, MPUPinSignal (Just 0) Sig_ddr_d14)])),
       (MPU_DDR_D15,
        MPUPin MPU_DDR_D15 Nothing
          (Map.fromList [(Sig_ddr_d15, MPUPinSignal (Just 0) Sig_ddr_d15)])),
       (MPU_DDR_DQM0,
        MPUPin MPU_DDR_DQM0 Nothing
          (Map.fromList
             [(Sig_ddr_dqm0, MPUPinSignal (Just 0) Sig_ddr_dqm0)])),
       (MPU_DDR_DQM1,
        MPUPin MPU_DDR_DQM1 Nothing
          (Map.fromList
             [(Sig_ddr_dqm1, MPUPinSignal (Just 0) Sig_ddr_dqm1)])),
       (MPU_DDR_DQS0,
        MPUPin MPU_DDR_DQS0 Nothing
          (Map.fromList
             [(Sig_ddr_dqs0, MPUPinSignal (Just 0) Sig_ddr_dqs0)])),
       (MPU_DDR_DQS1,
        MPUPin MPU_DDR_DQS1 Nothing
          (Map.fromList
             [(Sig_ddr_dqs1, MPUPinSignal (Just 0) Sig_ddr_dqs1)])),
       (MPU_DDR_DQSn0,
        MPUPin MPU_DDR_DQSn0 Nothing
          (Map.fromList
             [(Sig_ddr_dqsn0, MPUPinSignal (Just 0) Sig_ddr_dqsn0)])),
       (MPU_DDR_DQSn1,
        MPUPin MPU_DDR_DQSn1 Nothing
          (Map.fromList
             [(Sig_ddr_dqsn1, MPUPinSignal (Just 0) Sig_ddr_dqsn1)])),
       (MPU_DDR_ODT,
        MPUPin MPU_DDR_ODT Nothing
          (Map.fromList [(Sig_ddr_odt, MPUPinSignal (Just 0) Sig_ddr_odt)])),
       (MPU_DDR_RASn,
        MPUPin MPU_DDR_RASn Nothing
          (Map.fromList
             [(Sig_ddr_rasn, MPUPinSignal (Just 0) Sig_ddr_rasn)])),
       (MPU_DDR_RESETn,
        MPUPin MPU_DDR_RESETn Nothing
          (Map.fromList
             [(Sig_ddr_resetn, MPUPinSignal (Just 0) Sig_ddr_resetn)])),
       (MPU_DDR_VREF,
        MPUPin MPU_DDR_VREF Nothing
          (Map.fromList
             [(Sig_ddr_vref, MPUPinSignal (Just 0) Sig_ddr_vref)])),
       (MPU_DDR_VTP,
        MPUPin MPU_DDR_VTP Nothing
          (Map.fromList [(Sig_ddr_vtp, MPUPinSignal (Just 0) Sig_ddr_vtp)])),
       (MPU_DDR_WEn,
        MPUPin MPU_DDR_WEn Nothing
          (Map.fromList [(Sig_ddr_wen, MPUPinSignal (Just 0) Sig_ddr_wen)])),
       (MPU_ECAP0_IN_PWM0_OUT,
        MPUPin MPU_ECAP0_IN_PWM0_OUT (Just "ecap0_in_pwm0_out")
          (Map.fromList
             [(Sig_eCAP0_in_PWM0_out,
               MPUPinSignal (Just 0) Sig_eCAP0_in_PWM0_out),
              (Sig_gpio0_7, MPUPinSignal (Just 7) Sig_gpio0_7),
              (Sig_mmc0_sdwp, MPUPinSignal (Just 5) Sig_mmc0_sdwp),
              (Sig_pr1_ecap0_ecap_capin_apwm_o,
               MPUPinSignal (Just 3) Sig_pr1_ecap0_ecap_capin_apwm_o),
              (Sig_spi1_cs1, MPUPinSignal (Just 2) Sig_spi1_cs1),
              (Sig_spi1_sclk, MPUPinSignal (Just 4) Sig_spi1_sclk),
              (Sig_uart3_txd, MPUPinSignal (Just 1) Sig_uart3_txd),
              (Sig_xdma_event_intr2,
               MPUPinSignal (Just 6) Sig_xdma_event_intr2)])),
       (MPU_EMU0,
        MPUPin MPU_EMU0 (Just "emu0")
          (Map.fromList
             [(Sig_EMU0, MPUPinSignal (Just 0) Sig_EMU0),
              (Sig_gpio3_7, MPUPinSignal (Just 7) Sig_gpio3_7)])),
       (MPU_EMU1,
        MPUPin MPU_EMU1 (Just "emu1")
          (Map.fromList
             [(Sig_EMU1, MPUPinSignal (Just 0) Sig_EMU1),
              (Sig_gpio3_8, MPUPinSignal (Just 7) Sig_gpio3_8)])),
       (MPU_EXTINTn,
        MPUPin MPU_EXTINTn Nothing
          (Map.fromList [(Sig_nNMI, MPUPinSignal (Just 0) Sig_nNMI)])),
       (MPU_EXT_WAKEUP,
        MPUPin MPU_EXT_WAKEUP Nothing
          (Map.fromList
             [(Sig_EXT_WAKEUP, MPUPinSignal (Just 0) Sig_EXT_WAKEUP)])),
       (MPU_GPMC_A0,
        MPUPin MPU_GPMC_A0 (Just "gpmc_a0")
          (Map.fromList
             [(Sig_ehrpwm1_tripzone_input,
               MPUPinSignal (Just 6) Sig_ehrpwm1_tripzone_input),
              (Sig_gmii2_txen, MPUPinSignal (Just 1) Sig_gmii2_txen),
              (Sig_gpio1_16, MPUPinSignal (Just 7) Sig_gpio1_16),
              (Sig_gpmc_a0, MPUPinSignal (Just 0) Sig_gpmc_a0),
              (Sig_gpmc_a16, MPUPinSignal (Just 4) Sig_gpmc_a16),
              (Sig_pr1_mii_mt1_clk, MPUPinSignal (Just 5) Sig_pr1_mii_mt1_clk),
              (Sig_rgmii2_tctl, MPUPinSignal (Just 2) Sig_rgmii2_tctl),
              (Sig_rmii2_txen, MPUPinSignal (Just 3) Sig_rmii2_txen)])),
       (MPU_GPMC_A1,
        MPUPin MPU_GPMC_A1 (Just "gpmc_a1")
          (Map.fromList
             [(Sig_ehrpwm0_synco, MPUPinSignal (Just 6) Sig_ehrpwm0_synco),
              (Sig_gmii2_rxdv, MPUPinSignal (Just 1) Sig_gmii2_rxdv),
              (Sig_gpio1_17, MPUPinSignal (Just 7) Sig_gpio1_17),
              (Sig_gpmc_a1, MPUPinSignal (Just 0) Sig_gpmc_a1),
              (Sig_gpmc_a17, MPUPinSignal (Just 4) Sig_gpmc_a17),
              (Sig_mmc2_dat0, MPUPinSignal (Just 3) Sig_mmc2_dat0),
              (Sig_pr1_mii1_txd3, MPUPinSignal (Just 5) Sig_pr1_mii1_txd3),
              (Sig_rgmii2_rctl, MPUPinSignal (Just 2) Sig_rgmii2_rctl)])),
       (MPU_GPMC_A2,
        MPUPin MPU_GPMC_A2 (Just "gpmc_a2")
          (Map.fromList
             [(Sig_ehrpwm1A, MPUPinSignal (Just 6) Sig_ehrpwm1A),
              (Sig_gmii2_txd3, MPUPinSignal (Just 1) Sig_gmii2_txd3),
              (Sig_gpio1_18, MPUPinSignal (Just 7) Sig_gpio1_18),
              (Sig_gpmc_a2, MPUPinSignal (Just 0) Sig_gpmc_a2),
              (Sig_gpmc_a18, MPUPinSignal (Just 4) Sig_gpmc_a18),
              (Sig_mmc2_dat1, MPUPinSignal (Just 3) Sig_mmc2_dat1),
              (Sig_pr1_mii1_txd2, MPUPinSignal (Just 5) Sig_pr1_mii1_txd2),
              (Sig_rgmii2_td3, MPUPinSignal (Just 2) Sig_rgmii2_td3)])),
       (MPU_GPMC_A3,
        MPUPin MPU_GPMC_A3 (Just "gpmc_a3")
          (Map.fromList
             [(Sig_ehrpwm1B, MPUPinSignal (Just 6) Sig_ehrpwm1B),
              (Sig_gmii2_txd2, MPUPinSignal (Just 1) Sig_gmii2_txd2),
              (Sig_gpio1_19, MPUPinSignal (Just 7) Sig_gpio1_19),
              (Sig_gpmc_a3, MPUPinSignal (Just 0) Sig_gpmc_a3),
              (Sig_gpmc_a19, MPUPinSignal (Just 4) Sig_gpmc_a19),
              (Sig_mmc2_dat2, MPUPinSignal (Just 3) Sig_mmc2_dat2),
              (Sig_pr1_mii1_txd1, MPUPinSignal (Just 5) Sig_pr1_mii1_txd1),
              (Sig_rgmii2_td2, MPUPinSignal (Just 2) Sig_rgmii2_td2)])),
       (MPU_GPMC_A4,
        MPUPin MPU_GPMC_A4 (Just "gpmc_a4")
          (Map.fromList
             [(Sig_eQEP1A_in, MPUPinSignal (Just 6) Sig_eQEP1A_in),
              (Sig_gmii2_txd1, MPUPinSignal (Just 1) Sig_gmii2_txd1),
              (Sig_gpio1_20, MPUPinSignal (Just 7) Sig_gpio1_20),
              (Sig_gpmc_a4, MPUPinSignal (Just 0) Sig_gpmc_a4),
              (Sig_gpmc_a20, MPUPinSignal (Just 4) Sig_gpmc_a20),
              (Sig_pr1_mii1_txd0, MPUPinSignal (Just 5) Sig_pr1_mii1_txd0),
              (Sig_rgmii2_td1, MPUPinSignal (Just 2) Sig_rgmii2_td1),
              (Sig_rmii2_txd1, MPUPinSignal (Just 3) Sig_rmii2_txd1)])),
       (MPU_GPMC_A5,
        MPUPin MPU_GPMC_A5 (Just "gpmc_a5")
          (Map.fromList
             [(Sig_eQEP1B_in, MPUPinSignal (Just 6) Sig_eQEP1B_in),
              (Sig_gmii2_txd0, MPUPinSignal (Just 1) Sig_gmii2_txd0),
              (Sig_gpio1_21, MPUPinSignal (Just 7) Sig_gpio1_21),
              (Sig_gpmc_a5, MPUPinSignal (Just 0) Sig_gpmc_a5),
              (Sig_gpmc_a21, MPUPinSignal (Just 4) Sig_gpmc_a21),
              (Sig_pr1_mii1_rxd3, MPUPinSignal (Just 5) Sig_pr1_mii1_rxd3),
              (Sig_rgmii2_td0, MPUPinSignal (Just 2) Sig_rgmii2_td0),
              (Sig_rmii2_txd0, MPUPinSignal (Just 3) Sig_rmii2_txd0)])),
       (MPU_GPMC_A6,
        MPUPin MPU_GPMC_A6 (Just "gpmc_a6")
          (Map.fromList
             [(Sig_eQEP1_index, MPUPinSignal (Just 6) Sig_eQEP1_index),
              (Sig_gmii2_txclk, MPUPinSignal (Just 1) Sig_gmii2_txclk),
              (Sig_gpio1_22, MPUPinSignal (Just 7) Sig_gpio1_22),
              (Sig_gpmc_a6, MPUPinSignal (Just 0) Sig_gpmc_a6),
              (Sig_gpmc_a22, MPUPinSignal (Just 4) Sig_gpmc_a22),
              (Sig_mmc2_dat4, MPUPinSignal (Just 3) Sig_mmc2_dat4),
              (Sig_pr1_mii1_rxd2, MPUPinSignal (Just 5) Sig_pr1_mii1_rxd2),
              (Sig_rgmii2_tclk, MPUPinSignal (Just 2) Sig_rgmii2_tclk)])),
       (MPU_GPMC_A7,
        MPUPin MPU_GPMC_A7 (Just "gpmc_a7")
          (Map.fromList
             [(Sig_eQEP1_strobe, MPUPinSignal (Just 6) Sig_eQEP1_strobe),
              (Sig_gmii2_rxclk, MPUPinSignal (Just 1) Sig_gmii2_rxclk),
              (Sig_gpio1_23, MPUPinSignal (Just 7) Sig_gpio1_23),
              (Sig_gpmc_a7, MPUPinSignal (Just 0) Sig_gpmc_a7),
              (Sig_gpmc_a23, MPUPinSignal (Just 4) Sig_gpmc_a23),
              (Sig_mmc2_dat5, MPUPinSignal (Just 3) Sig_mmc2_dat5),
              (Sig_pr1_mii1_rxd1, MPUPinSignal (Just 5) Sig_pr1_mii1_rxd1),
              (Sig_rgmii2_rclk, MPUPinSignal (Just 2) Sig_rgmii2_rclk)])),
       (MPU_GPMC_A8,
        MPUPin MPU_GPMC_A8 (Just "gpmc_a8")
          (Map.fromList
             [(Sig_gmii2_rxd3, MPUPinSignal (Just 1) Sig_gmii2_rxd3),
              (Sig_gpio1_24, MPUPinSignal (Just 7) Sig_gpio1_24),
              (Sig_gpmc_a8, MPUPinSignal (Just 0) Sig_gpmc_a8),
              (Sig_gpmc_a24, MPUPinSignal (Just 4) Sig_gpmc_a24),
              (Sig_mcasp0_aclkx, MPUPinSignal (Just 6) Sig_mcasp0_aclkx),
              (Sig_mmc2_dat6, MPUPinSignal (Just 3) Sig_mmc2_dat6),
              (Sig_pr1_mii1_rxd0, MPUPinSignal (Just 5) Sig_pr1_mii1_rxd0),
              (Sig_rgmii2_rd3, MPUPinSignal (Just 2) Sig_rgmii2_rd3)])),
       (MPU_GPMC_A9,
        MPUPin MPU_GPMC_A9 (Just "gpmc_a9")
          (Map.fromList
             [(Sig_gmii2_rxd2, MPUPinSignal (Just 1) Sig_gmii2_rxd2),
              (Sig_gpio1_25, MPUPinSignal (Just 7) Sig_gpio1_25),
              (Sig_gpmc_a9, MPUPinSignal (Just 0) Sig_gpmc_a9),
              (Sig_gpmc_a25, MPUPinSignal (Just 4) Sig_gpmc_a25),
              (Sig_mcasp0_fsx, MPUPinSignal (Just 6) Sig_mcasp0_fsx),
              (Sig_mmc2_dat7, MPUPinSignal (Just 3) Sig_mmc2_dat7),
              (Sig_pr1_mii_mr1_clk, MPUPinSignal (Just 5) Sig_pr1_mii_mr1_clk),
              (Sig_rgmii2_rd2, MPUPinSignal (Just 2) Sig_rgmii2_rd2)])),
       (MPU_GPMC_A10,
        MPUPin MPU_GPMC_A10 (Just "gpmc_a10")
          (Map.fromList
             [(Sig_gmii2_rxd1, MPUPinSignal (Just 1) Sig_gmii2_rxd1),
              (Sig_gpio1_26, MPUPinSignal (Just 7) Sig_gpio1_26),
              (Sig_gpmc_a10, MPUPinSignal (Just 0) Sig_gpmc_a10),
              (Sig_gpmc_a26, MPUPinSignal (Just 4) Sig_gpmc_a26),
              (Sig_mcasp0_axr0, MPUPinSignal (Just 6) Sig_mcasp0_axr0),
              (Sig_pr1_mii1_rxdv, MPUPinSignal (Just 5) Sig_pr1_mii1_rxdv),
              (Sig_rgmii2_rd1, MPUPinSignal (Just 2) Sig_rgmii2_rd1),
              (Sig_rmii2_rxd1, MPUPinSignal (Just 3) Sig_rmii2_rxd1)])),
       (MPU_GPMC_A11,
        MPUPin MPU_GPMC_A11 (Just "gpmc_a11")
          (Map.fromList
             [(Sig_gmii2_rxd0, MPUPinSignal (Just 1) Sig_gmii2_rxd0),
              (Sig_gpio1_27, MPUPinSignal (Just 7) Sig_gpio1_27),
              (Sig_gpmc_a11, MPUPinSignal (Just 0) Sig_gpmc_a11),
              (Sig_gpmc_a27, MPUPinSignal (Just 4) Sig_gpmc_a27),
              (Sig_mcasp0_axr1, MPUPinSignal (Just 6) Sig_mcasp0_axr1),
              (Sig_pr1_mii1_rxer, MPUPinSignal (Just 5) Sig_pr1_mii1_rxer),
              (Sig_rgmii2_rd0, MPUPinSignal (Just 2) Sig_rgmii2_rd0),
              (Sig_rmii2_rxd0, MPUPinSignal (Just 3) Sig_rmii2_rxd0)])),
       (MPU_GPMC_AD0,
        MPUPin MPU_GPMC_AD0 (Just "gpmc_ad0")
          (Map.fromList
             [(Sig_gpio1_0, MPUPinSignal (Just 7) Sig_gpio1_0),
              (Sig_gpmc_ad0, MPUPinSignal (Just 0) Sig_gpmc_ad0),
              (Sig_mmc1_dat0, MPUPinSignal (Just 1) Sig_mmc1_dat0)])),
       (MPU_GPMC_AD1,
        MPUPin MPU_GPMC_AD1 (Just "gpmc_ad1")
          (Map.fromList
             [(Sig_gpio1_1, MPUPinSignal (Just 7) Sig_gpio1_1),
              (Sig_gpmc_ad1, MPUPinSignal (Just 0) Sig_gpmc_ad1),
              (Sig_mmc1_dat1, MPUPinSignal (Just 1) Sig_mmc1_dat1)])),
       (MPU_GPMC_AD2,
        MPUPin MPU_GPMC_AD2 (Just "gpmc_ad2")
          (Map.fromList
             [(Sig_gpio1_2, MPUPinSignal (Just 7) Sig_gpio1_2),
              (Sig_gpmc_ad2, MPUPinSignal (Just 0) Sig_gpmc_ad2),
              (Sig_mmc1_dat2, MPUPinSignal (Just 1) Sig_mmc1_dat2)])),
       (MPU_GPMC_AD3,
        MPUPin MPU_GPMC_AD3 (Just "gpmc_ad3")
          (Map.fromList
             [(Sig_gpio1_3, MPUPinSignal (Just 7) Sig_gpio1_3),
              (Sig_gpmc_ad3, MPUPinSignal (Just 0) Sig_gpmc_ad3),
              (Sig_mmc1_dat3, MPUPinSignal (Just 1) Sig_mmc1_dat3)])),
       (MPU_GPMC_AD4,
        MPUPin MPU_GPMC_AD4 (Just "gpmc_ad4")
          (Map.fromList
             [(Sig_gpio1_4, MPUPinSignal (Just 7) Sig_gpio1_4),
              (Sig_gpmc_ad4, MPUPinSignal (Just 0) Sig_gpmc_ad4),
              (Sig_mmc1_dat4, MPUPinSignal (Just 1) Sig_mmc1_dat4)])),
       (MPU_GPMC_AD5,
        MPUPin MPU_GPMC_AD5 (Just "gpmc_ad5")
          (Map.fromList
             [(Sig_gpio1_5, MPUPinSignal (Just 7) Sig_gpio1_5),
              (Sig_gpmc_ad5, MPUPinSignal (Just 0) Sig_gpmc_ad5),
              (Sig_mmc1_dat5, MPUPinSignal (Just 1) Sig_mmc1_dat5)])),
       (MPU_GPMC_AD6,
        MPUPin MPU_GPMC_AD6 (Just "gpmc_ad6")
          (Map.fromList
             [(Sig_gpio1_6, MPUPinSignal (Just 7) Sig_gpio1_6),
              (Sig_gpmc_ad6, MPUPinSignal (Just 0) Sig_gpmc_ad6),
              (Sig_mmc1_dat6, MPUPinSignal (Just 1) Sig_mmc1_dat6)])),
       (MPU_GPMC_AD7,
        MPUPin MPU_GPMC_AD7 (Just "gpmc_ad7")
          (Map.fromList
             [(Sig_gpio1_7, MPUPinSignal (Just 7) Sig_gpio1_7),
              (Sig_gpmc_ad7, MPUPinSignal (Just 0) Sig_gpmc_ad7),
              (Sig_mmc1_dat7, MPUPinSignal (Just 1) Sig_mmc1_dat7)])),
       (MPU_GPMC_AD8,
        MPUPin MPU_GPMC_AD8 (Just "gpmc_ad8")
          (Map.fromList
             [(Sig_ehrpwm2A, MPUPinSignal (Just 4) Sig_ehrpwm2A),
              (Sig_gpio0_22, MPUPinSignal (Just 7) Sig_gpio0_22),
              (Sig_gpmc_ad8, MPUPinSignal (Just 0) Sig_gpmc_ad8),
              (Sig_lcd_data23, MPUPinSignal (Just 1) Sig_lcd_data23),
              (Sig_mmc1_dat0, MPUPinSignal (Just 2) Sig_mmc1_dat0),
              (Sig_mmc2_dat4, MPUPinSignal (Just 3) Sig_mmc2_dat4),
              (Sig_pr1_mii_mt0_clk,
               MPUPinSignal (Just 5) Sig_pr1_mii_mt0_clk)])),
       (MPU_GPMC_AD9,
        MPUPin MPU_GPMC_AD9 (Just "gpmc_ad9")
          (Map.fromList
             [(Sig_ehrpwm2B, MPUPinSignal (Just 4) Sig_ehrpwm2B),
              (Sig_gpio0_23, MPUPinSignal (Just 7) Sig_gpio0_23),
              (Sig_gpmc_ad9, MPUPinSignal (Just 0) Sig_gpmc_ad9),
              (Sig_lcd_data22, MPUPinSignal (Just 1) Sig_lcd_data22),
              (Sig_mmc1_dat1, MPUPinSignal (Just 2) Sig_mmc1_dat1),
              (Sig_mmc2_dat5, MPUPinSignal (Just 3) Sig_mmc2_dat5),
              (Sig_pr1_mii0_col, MPUPinSignal (Just 5) Sig_pr1_mii0_col)])),
       (MPU_GPMC_AD10,
        MPUPin MPU_GPMC_AD10 (Just "gpmc_ad10")
          (Map.fromList
             [(Sig_ehrpwm2_tripzone_input,
               MPUPinSignal (Just 4) Sig_ehrpwm2_tripzone_input),
              (Sig_gpio0_26, MPUPinSignal (Just 7) Sig_gpio0_26),
              (Sig_gpmc_ad10, MPUPinSignal (Just 0) Sig_gpmc_ad10),
              (Sig_lcd_data21, MPUPinSignal (Just 1) Sig_lcd_data21),
              (Sig_mmc1_dat2, MPUPinSignal (Just 2) Sig_mmc1_dat2),
              (Sig_mmc2_dat6, MPUPinSignal (Just 3) Sig_mmc2_dat6),
              (Sig_pr1_mii0_txen, MPUPinSignal (Just 5) Sig_pr1_mii0_txen)])),
       (MPU_GPMC_AD11,
        MPUPin MPU_GPMC_AD11 (Just "gpmc_ad11")
          (Map.fromList
             [(Sig_ehrpwm0_synco, MPUPinSignal (Just 4) Sig_ehrpwm0_synco),
              (Sig_gpio0_27, MPUPinSignal (Just 7) Sig_gpio0_27),
              (Sig_gpmc_ad11, MPUPinSignal (Just 0) Sig_gpmc_ad11),
              (Sig_lcd_data20, MPUPinSignal (Just 1) Sig_lcd_data20),
              (Sig_mmc1_dat3, MPUPinSignal (Just 2) Sig_mmc1_dat3),
              (Sig_mmc2_dat7, MPUPinSignal (Just 3) Sig_mmc2_dat7),
              (Sig_pr1_mii0_txd3, MPUPinSignal (Just 5) Sig_pr1_mii0_txd3)])),
       (MPU_GPMC_AD12,
        MPUPin MPU_GPMC_AD12 (Just "gpmc_ad12")
          (Map.fromList
             [(Sig_eQEP2A_in, MPUPinSignal (Just 4) Sig_eQEP2A_in),
              (Sig_gpio1_12, MPUPinSignal (Just 7) Sig_gpio1_12),
              (Sig_gpmc_ad12, MPUPinSignal (Just 0) Sig_gpmc_ad12),
              (Sig_lcd_data19, MPUPinSignal (Just 1) Sig_lcd_data19),
              (Sig_mmc1_dat4, MPUPinSignal (Just 2) Sig_mmc1_dat4),
              (Sig_mmc2_dat0, MPUPinSignal (Just 3) Sig_mmc2_dat0),
              (Sig_pr1_mii0_txd2, MPUPinSignal (Just 5) Sig_pr1_mii0_txd2),
              (Sig_pr1_pru0_pru_r30_14,
               MPUPinSignal (Just 6) Sig_pr1_pru0_pru_r30_14)])),
       (MPU_GPMC_AD13,
        MPUPin MPU_GPMC_AD13 (Just "gpmc_ad13")
          (Map.fromList
             [(Sig_eQEP2B_in, MPUPinSignal (Just 4) Sig_eQEP2B_in),
              (Sig_gpio1_13, MPUPinSignal (Just 7) Sig_gpio1_13),
              (Sig_gpmc_ad13, MPUPinSignal (Just 0) Sig_gpmc_ad13),
              (Sig_lcd_data18, MPUPinSignal (Just 1) Sig_lcd_data18),
              (Sig_mmc1_dat5, MPUPinSignal (Just 2) Sig_mmc1_dat5),
              (Sig_mmc2_dat1, MPUPinSignal (Just 3) Sig_mmc2_dat1),
              (Sig_pr1_mii0_txd1, MPUPinSignal (Just 5) Sig_pr1_mii0_txd1),
              (Sig_pr1_pru0_pru_r30_15,
               MPUPinSignal (Just 6) Sig_pr1_pru0_pru_r30_15)])),
       (MPU_GPMC_AD14,
        MPUPin MPU_GPMC_AD14 (Just "gpmc_ad14")
          (Map.fromList
             [(Sig_eQEP2_index, MPUPinSignal (Just 4) Sig_eQEP2_index),
              (Sig_gpio1_14, MPUPinSignal (Just 7) Sig_gpio1_14),
              (Sig_gpmc_ad14, MPUPinSignal (Just 0) Sig_gpmc_ad14),
              (Sig_lcd_data17, MPUPinSignal (Just 1) Sig_lcd_data17),
              (Sig_mmc1_dat6, MPUPinSignal (Just 2) Sig_mmc1_dat6),
              (Sig_mmc2_dat2, MPUPinSignal (Just 3) Sig_mmc2_dat2),
              (Sig_pr1_mii0_txd0, MPUPinSignal (Just 5) Sig_pr1_mii0_txd0),
              (Sig_pr1_pru0_pru_r31_14,
               MPUPinSignal (Just 6) Sig_pr1_pru0_pru_r31_14)])),
       (MPU_GPMC_AD15,
        MPUPin MPU_GPMC_AD15 (Just "gpmc_ad15")
          (Map.fromList
             [(Sig_eQEP2_strobe, MPUPinSignal (Just 4) Sig_eQEP2_strobe),
              (Sig_gpio1_15, MPUPinSignal (Just 7) Sig_gpio1_15),
              (Sig_gpmc_ad15, MPUPinSignal (Just 0) Sig_gpmc_ad15),
              (Sig_lcd_data16, MPUPinSignal (Just 1) Sig_lcd_data16),
              (Sig_mmc1_dat7, MPUPinSignal (Just 2) Sig_mmc1_dat7),
              (Sig_mmc2_dat3, MPUPinSignal (Just 3) Sig_mmc2_dat3),
              (Sig_pr1_ecap0_ecap_capin_apwm_o,
               MPUPinSignal (Just 5) Sig_pr1_ecap0_ecap_capin_apwm_o),
              (Sig_pr1_pru0_pru_r31_15,
               MPUPinSignal (Just 6) Sig_pr1_pru0_pru_r31_15)])),
       (MPU_GPMC_ADVn_ALE,
        MPUPin MPU_GPMC_ADVn_ALE (Just "gpmc_advn_ale")
          (Map.fromList
             [(Sig_gpio2_2, MPUPinSignal (Just 7) Sig_gpio2_2),
              (Sig_gpmc_advn_ale, MPUPinSignal (Just 0) Sig_gpmc_advn_ale),
              (Sig_timer4, MPUPinSignal (Just 2) Sig_timer4)])),
       (MPU_GPMC_BEn0_CLE,
        MPUPin MPU_GPMC_BEn0_CLE (Just "gpmc_ben0_cle")
          (Map.fromList
             [(Sig_gpio2_5, MPUPinSignal (Just 7) Sig_gpio2_5),
              (Sig_gpmc_be0n_cle, MPUPinSignal (Just 0) Sig_gpmc_be0n_cle),
              (Sig_timer5, MPUPinSignal (Just 2) Sig_timer5)])),
       (MPU_GPMC_BEn1,
        MPUPin MPU_GPMC_BEn1 (Just "gpmc_ben1")
          (Map.fromList
             [(Sig_gmii2_col, MPUPinSignal (Just 1) Sig_gmii2_col),
              (Sig_gpio1_28, MPUPinSignal (Just 7) Sig_gpio1_28),
              (Sig_gpmc_be1n, MPUPinSignal (Just 0) Sig_gpmc_be1n),
              (Sig_gpmc_csn6, MPUPinSignal (Just 2) Sig_gpmc_csn6),
              (Sig_gpmc_dir, MPUPinSignal (Just 4) Sig_gpmc_dir),
              (Sig_mcasp0_aclkr, MPUPinSignal (Just 6) Sig_mcasp0_aclkr),
              (Sig_mmc2_dat3, MPUPinSignal (Just 3) Sig_mmc2_dat3),
              (Sig_pr1_mii1_rxlink,
               MPUPinSignal (Just 5) Sig_pr1_mii1_rxlink)])),
       (MPU_GPMC_CLK,
        MPUPin MPU_GPMC_CLK (Just "gpmc_clk")
          (Map.fromList
             [(Sig_gpio2_1, MPUPinSignal (Just 7) Sig_gpio2_1),
              (Sig_gpmc_clk, MPUPinSignal (Just 0) Sig_gpmc_clk),
              (Sig_gpmc_wait1, MPUPinSignal (Just 2) Sig_gpmc_wait1),
              (Sig_lcd_memory_clk, MPUPinSignal (Just 1) Sig_lcd_memory_clk),
              (Sig_mcasp0_fsr, MPUPinSignal (Just 6) Sig_mcasp0_fsr),
              (Sig_mmc2_clk, MPUPinSignal (Just 3) Sig_mmc2_clk),
              (Sig_pr1_mdio_mdclk, MPUPinSignal (Just 5) Sig_pr1_mdio_mdclk),
              (Sig_pr1_mii1_crs, MPUPinSignal (Just 4) Sig_pr1_mii1_crs)])),
       (MPU_GPMC_CSn0,
        MPUPin MPU_GPMC_CSn0 (Just "gpmc_csn0")
          (Map.fromList
             [(Sig_gpio1_29, MPUPinSignal (Just 7) Sig_gpio1_29),
              (Sig_gpmc_csn0, MPUPinSignal (Just 0) Sig_gpmc_csn0)])),
       (MPU_GPMC_CSn1,
        MPUPin MPU_GPMC_CSn1 (Just "gpmc_csn1")
          (Map.fromList
             [(Sig_gpio1_30, MPUPinSignal (Just 7) Sig_gpio1_30),
              (Sig_gpmc_clk, MPUPinSignal (Just 1) Sig_gpmc_clk),
              (Sig_gpmc_csn1, MPUPinSignal (Just 0) Sig_gpmc_csn1),
              (Sig_mmc1_clk, MPUPinSignal (Just 2) Sig_mmc1_clk),
              (Sig_pr1_edio_data_in6,
               MPUPinSignal (Just 3) Sig_pr1_edio_data_in6),
              (Sig_pr1_edio_data_out6,
               MPUPinSignal (Just 4) Sig_pr1_edio_data_out6),
              (Sig_pr1_pru1_pru_r30_12,
               MPUPinSignal (Just 5) Sig_pr1_pru1_pru_r30_12),
              (Sig_pr1_pru1_pru_r31_12,
               MPUPinSignal (Just 6) Sig_pr1_pru1_pru_r31_12)])),
       (MPU_GPMC_CSn2,
        MPUPin MPU_GPMC_CSn2 (Just "gpmc_csn2")
          (Map.fromList
             [(Sig_gpio1_31, MPUPinSignal (Just 7) Sig_gpio1_31),
              (Sig_gpmc_be1n, MPUPinSignal (Just 1) Sig_gpmc_be1n),
              (Sig_gpmc_csn2, MPUPinSignal (Just 0) Sig_gpmc_csn2),
              (Sig_mmc1_cmd, MPUPinSignal (Just 2) Sig_mmc1_cmd),
              (Sig_pr1_edio_data_in7,
               MPUPinSignal (Just 3) Sig_pr1_edio_data_in7),
              (Sig_pr1_edio_data_out7,
               MPUPinSignal (Just 4) Sig_pr1_edio_data_out7),
              (Sig_pr1_pru1_pru_r30_13,
               MPUPinSignal (Just 5) Sig_pr1_pru1_pru_r30_13),
              (Sig_pr1_pru1_pru_r31_13,
               MPUPinSignal (Just 6) Sig_pr1_pru1_pru_r31_13)])),
       (MPU_GPMC_CSn3,
        MPUPin MPU_GPMC_CSn3 (Just "gpmc_csn3")
          (Map.fromList
             [(Sig_EMU4, MPUPinSignal (Just 6) Sig_EMU4),
              (Sig_gpio2_0, MPUPinSignal (Just 7) Sig_gpio2_0),
              (Sig_gpmc_csn3, MPUPinSignal (Just 0) Sig_gpmc_csn3),
              (Sig_mmc2_cmd, MPUPinSignal (Just 3) Sig_mmc2_cmd),
              (Sig_pr1_mdio_data, MPUPinSignal (Just 5) Sig_pr1_mdio_data),
              (Sig_pr1_mii0_crs, MPUPinSignal (Just 4) Sig_pr1_mii0_crs)])),
       (MPU_GPMC_OEn_REn,
        MPUPin MPU_GPMC_OEn_REn (Just "gpmc_oen_ren")
          (Map.fromList
             [(Sig_gpio2_3, MPUPinSignal (Just 7) Sig_gpio2_3),
              (Sig_gpmc_oen_ren, MPUPinSignal (Just 0) Sig_gpmc_oen_ren),
              (Sig_timer7, MPUPinSignal (Just 2) Sig_timer7)])),
       (MPU_GPMC_WAIT0,
        MPUPin MPU_GPMC_WAIT0 (Just "gpmc_wait0")
          (Map.fromList
             [(Sig_gmii2_crs, MPUPinSignal (Just 1) Sig_gmii2_crs),
              (Sig_gpio0_30, MPUPinSignal (Just 7) Sig_gpio0_30),
              (Sig_gpmc_csn4, MPUPinSignal (Just 2) Sig_gpmc_csn4),
              (Sig_gpmc_wait0, MPUPinSignal (Just 0) Sig_gpmc_wait0),
              (Sig_mmc1_sdcd, MPUPinSignal (Just 4) Sig_mmc1_sdcd),
              (Sig_pr1_mii1_col, MPUPinSignal (Just 5) Sig_pr1_mii1_col),
              (Sig_rmii2_crs_dv, MPUPinSignal (Just 3) Sig_rmii2_crs_dv),
              (Sig_uart4_rxd, MPUPinSignal (Just 6) Sig_uart4_rxd)])),
       (MPU_GPMC_WEn,
        MPUPin MPU_GPMC_WEn (Just "gpmc_wen")
          (Map.fromList
             [(Sig_gpio2_4, MPUPinSignal (Just 7) Sig_gpio2_4),
              (Sig_gpmc_wen, MPUPinSignal (Just 0) Sig_gpmc_wen),
              (Sig_timer6, MPUPinSignal (Just 2) Sig_timer6)])),
       (MPU_GPMC_WPn,
        MPUPin MPU_GPMC_WPn (Just "gpmc_wpn")
          (Map.fromList
             [(Sig_gmii2_rxerr, MPUPinSignal (Just 1) Sig_gmii2_rxerr),
              (Sig_gpio0_31, MPUPinSignal (Just 7) Sig_gpio0_31),
              (Sig_gpmc_csn5, MPUPinSignal (Just 2) Sig_gpmc_csn5),
              (Sig_gpmc_wpn, MPUPinSignal (Just 0) Sig_gpmc_wpn),
              (Sig_mmc2_sdcd, MPUPinSignal (Just 4) Sig_mmc2_sdcd),
              (Sig_pr1_mii1_txen, MPUPinSignal (Just 5) Sig_pr1_mii1_txen),
              (Sig_rmii2_rxerr, MPUPinSignal (Just 3) Sig_rmii2_rxerr),
              (Sig_uart4_txd, MPUPinSignal (Just 6) Sig_uart4_txd)])),
       (MPU_I2C0_SCL,
        MPUPin MPU_I2C0_SCL (Just "i2c0_scl")
          (Map.fromList
             [(Sig_I2C0_SCL, MPUPinSignal (Just 0) Sig_I2C0_SCL),
              (Sig_eCAP1_in_PWM1_out,
               MPUPinSignal (Just 3) Sig_eCAP1_in_PWM1_out),
              (Sig_gpio3_6, MPUPinSignal (Just 7) Sig_gpio3_6),
              (Sig_timer7, MPUPinSignal (Just 1) Sig_timer7),
              (Sig_uart2_rtsn, MPUPinSignal (Just 2) Sig_uart2_rtsn)])),
       (MPU_I2C0_SDA,
        MPUPin MPU_I2C0_SDA (Just "i2c0_sda")
          (Map.fromList
             [(Sig_I2C0_SDA, MPUPinSignal (Just 0) Sig_I2C0_SDA),
              (Sig_eCAP2_in_PWM2_out,
               MPUPinSignal (Just 3) Sig_eCAP2_in_PWM2_out),
              (Sig_gpio3_5, MPUPinSignal (Just 7) Sig_gpio3_5),
              (Sig_timer4, MPUPinSignal (Just 1) Sig_timer4),
              (Sig_uart2_ctsn, MPUPinSignal (Just 2) Sig_uart2_ctsn)])),
       (MPU_LCD_AC_BIAS_EN,
        MPUPin MPU_LCD_AC_BIAS_EN (Just "lcd_ac_bias_en")
          (Map.fromList
             [(Sig_gpio2_25, MPUPinSignal (Just 7) Sig_gpio2_25),
              (Sig_gpmc_a11, MPUPinSignal (Just 1) Sig_gpmc_a11),
              (Sig_lcd_ac_bias_en, MPUPinSignal (Just 0) Sig_lcd_ac_bias_en),
              (Sig_pr1_edio_data_in5,
               MPUPinSignal (Just 3) Sig_pr1_edio_data_in5),
              (Sig_pr1_edio_data_out5,
               MPUPinSignal (Just 4) Sig_pr1_edio_data_out5),
              (Sig_pr1_mii1_crs, MPUPinSignal (Just 2) Sig_pr1_mii1_crs),
              (Sig_pr1_pru1_pru_r30_11,
               MPUPinSignal (Just 5) Sig_pr1_pru1_pru_r30_11),
              (Sig_pr1_pru1_pru_r31_11,
               MPUPinSignal (Just 6) Sig_pr1_pru1_pru_r31_11)])),
       (MPU_LCD_DATA0,
        MPUPin MPU_LCD_DATA0 (Just "lcd_data0")
          (Map.fromList
             [(Sig_ehrpwm2A, MPUPinSignal (Just 3) Sig_ehrpwm2A),
              (Sig_gpio2_6, MPUPinSignal (Just 7) Sig_gpio2_6),
              (Sig_gpmc_a0, MPUPinSignal (Just 1) Sig_gpmc_a0),
              (Sig_lcd_data0, MPUPinSignal (Just 0) Sig_lcd_data0),
              (Sig_pr1_mii_mt0_clk, MPUPinSignal (Just 2) Sig_pr1_mii_mt0_clk),
              (Sig_pr1_pru1_pru_r30_0,
               MPUPinSignal (Just 5) Sig_pr1_pru1_pru_r30_0),
              (Sig_pr1_pru1_pru_r31_0,
               MPUPinSignal (Just 6) Sig_pr1_pru1_pru_r31_0)])),
       (MPU_LCD_DATA1,
        MPUPin MPU_LCD_DATA1 (Just "lcd_data1")
          (Map.fromList
             [(Sig_ehrpwm2B, MPUPinSignal (Just 3) Sig_ehrpwm2B),
              (Sig_gpio2_7, MPUPinSignal (Just 7) Sig_gpio2_7),
              (Sig_gpmc_a1, MPUPinSignal (Just 1) Sig_gpmc_a1),
              (Sig_lcd_data1, MPUPinSignal (Just 0) Sig_lcd_data1),
              (Sig_pr1_mii0_txen, MPUPinSignal (Just 2) Sig_pr1_mii0_txen),
              (Sig_pr1_pru1_pru_r30_1,
               MPUPinSignal (Just 5) Sig_pr1_pru1_pru_r30_1),
              (Sig_pr1_pru1_pru_r31_1,
               MPUPinSignal (Just 6) Sig_pr1_pru1_pru_r31_1)])),
       (MPU_LCD_DATA2,
        MPUPin MPU_LCD_DATA2 (Just "lcd_data2")
          (Map.fromList
             [(Sig_ehrpwm2_tripzone_input,
               MPUPinSignal (Just 3) Sig_ehrpwm2_tripzone_input),
              (Sig_gpio2_8, MPUPinSignal (Just 7) Sig_gpio2_8),
              (Sig_gpmc_a2, MPUPinSignal (Just 1) Sig_gpmc_a2),
              (Sig_lcd_data2, MPUPinSignal (Just 0) Sig_lcd_data2),
              (Sig_pr1_mii0_txd3, MPUPinSignal (Just 2) Sig_pr1_mii0_txd3),
              (Sig_pr1_pru1_pru_r30_2,
               MPUPinSignal (Just 5) Sig_pr1_pru1_pru_r30_2),
              (Sig_pr1_pru1_pru_r31_2,
               MPUPinSignal (Just 6) Sig_pr1_pru1_pru_r31_2)])),
       (MPU_LCD_DATA3,
        MPUPin MPU_LCD_DATA3 (Just "lcd_data3")
          (Map.fromList
             [(Sig_ehrpwm0_synco, MPUPinSignal (Just 3) Sig_ehrpwm0_synco),
              (Sig_gpio2_9, MPUPinSignal (Just 7) Sig_gpio2_9),
              (Sig_gpmc_a3, MPUPinSignal (Just 1) Sig_gpmc_a3),
              (Sig_lcd_data3, MPUPinSignal (Just 0) Sig_lcd_data3),
              (Sig_pr1_mii0_txd2, MPUPinSignal (Just 2) Sig_pr1_mii0_txd2),
              (Sig_pr1_pru1_pru_r30_3,
               MPUPinSignal (Just 5) Sig_pr1_pru1_pru_r30_3),
              (Sig_pr1_pru1_pru_r31_3,
               MPUPinSignal (Just 6) Sig_pr1_pru1_pru_r31_3)])),
       (MPU_LCD_DATA4,
        MPUPin MPU_LCD_DATA4 (Just "lcd_data4")
          (Map.fromList
             [(Sig_eQEP2A_in, MPUPinSignal (Just 3) Sig_eQEP2A_in),
              (Sig_gpio2_10, MPUPinSignal (Just 7) Sig_gpio2_10),
              (Sig_gpmc_a4, MPUPinSignal (Just 1) Sig_gpmc_a4),
              (Sig_lcd_data4, MPUPinSignal (Just 0) Sig_lcd_data4),
              (Sig_pr1_mii0_txd1, MPUPinSignal (Just 2) Sig_pr1_mii0_txd1),
              (Sig_pr1_pru1_pru_r30_4,
               MPUPinSignal (Just 5) Sig_pr1_pru1_pru_r30_4),
              (Sig_pr1_pru1_pru_r31_4,
               MPUPinSignal (Just 6) Sig_pr1_pru1_pru_r31_4)])),
       (MPU_LCD_DATA5,
        MPUPin MPU_LCD_DATA5 (Just "lcd_data5")
          (Map.fromList
             [(Sig_eQEP2B_in, MPUPinSignal (Just 3) Sig_eQEP2B_in),
              (Sig_gpio2_11, MPUPinSignal (Just 7) Sig_gpio2_11),
              (Sig_gpmc_a5, MPUPinSignal (Just 1) Sig_gpmc_a5),
              (Sig_lcd_data5, MPUPinSignal (Just 0) Sig_lcd_data5),
              (Sig_pr1_mii0_txd0, MPUPinSignal (Just 2) Sig_pr1_mii0_txd0),
              (Sig_pr1_pru1_pru_r30_5,
               MPUPinSignal (Just 5) Sig_pr1_pru1_pru_r30_5),
              (Sig_pr1_pru1_pru_r31_5,
               MPUPinSignal (Just 6) Sig_pr1_pru1_pru_r31_5)])),
       (MPU_LCD_DATA6,
        MPUPin MPU_LCD_DATA6 (Just "lcd_data6")
          (Map.fromList
             [(Sig_eQEP2_index, MPUPinSignal (Just 3) Sig_eQEP2_index),
              (Sig_gpio2_12, MPUPinSignal (Just 7) Sig_gpio2_12),
              (Sig_gpmc_a6, MPUPinSignal (Just 1) Sig_gpmc_a6),
              (Sig_lcd_data6, MPUPinSignal (Just 0) Sig_lcd_data6),
              (Sig_pr1_edio_data_in6,
               MPUPinSignal (Just 2) Sig_pr1_edio_data_in6),
              (Sig_pr1_edio_data_out6,
               MPUPinSignal (Just 4) Sig_pr1_edio_data_out6),
              (Sig_pr1_pru1_pru_r30_6,
               MPUPinSignal (Just 5) Sig_pr1_pru1_pru_r30_6),
              (Sig_pr1_pru1_pru_r31_6,
               MPUPinSignal (Just 6) Sig_pr1_pru1_pru_r31_6)])),
       (MPU_LCD_DATA7,
        MPUPin MPU_LCD_DATA7 (Just "lcd_data7")
          (Map.fromList
             [(Sig_eQEP2_strobe, MPUPinSignal (Just 3) Sig_eQEP2_strobe),
              (Sig_gpio2_13, MPUPinSignal (Just 7) Sig_gpio2_13),
              (Sig_gpmc_a7, MPUPinSignal (Just 1) Sig_gpmc_a7),
              (Sig_lcd_data7, MPUPinSignal (Just 0) Sig_lcd_data7),
              (Sig_pr1_edio_data_in7,
               MPUPinSignal (Just 2) Sig_pr1_edio_data_in7),
              (Sig_pr1_edio_data_out7,
               MPUPinSignal (Just 4) Sig_pr1_edio_data_out7),
              (Sig_pr1_pru1_pru_r30_7,
               MPUPinSignal (Just 5) Sig_pr1_pru1_pru_r30_7),
              (Sig_pr1_pru1_pru_r31_7,
               MPUPinSignal (Just 6) Sig_pr1_pru1_pru_r31_7)])),
       (MPU_LCD_DATA8,
        MPUPin MPU_LCD_DATA8 (Just "lcd_data8")
          (Map.fromList
             [(Sig_ehrpwm1_tripzone_input,
               MPUPinSignal (Just 2) Sig_ehrpwm1_tripzone_input),
              (Sig_gpio2_14, MPUPinSignal (Just 7) Sig_gpio2_14),
              (Sig_gpmc_a12, MPUPinSignal (Just 1) Sig_gpmc_a12),
              (Sig_lcd_data8, MPUPinSignal (Just 0) Sig_lcd_data8),
              (Sig_mcasp0_aclkx, MPUPinSignal (Just 3) Sig_mcasp0_aclkx),
              (Sig_pr1_mii0_rxd3, MPUPinSignal (Just 5) Sig_pr1_mii0_rxd3),
              (Sig_uart2_ctsn, MPUPinSignal (Just 6) Sig_uart2_ctsn),
              (Sig_uart5_txd, MPUPinSignal (Just 4) Sig_uart5_txd)])),
       (MPU_LCD_DATA9,
        MPUPin MPU_LCD_DATA9 (Just "lcd_data9")
          (Map.fromList
             [(Sig_ehrpwm0_synco, MPUPinSignal (Just 2) Sig_ehrpwm0_synco),
              (Sig_gpio2_15, MPUPinSignal (Just 7) Sig_gpio2_15),
              (Sig_gpmc_a13, MPUPinSignal (Just 1) Sig_gpmc_a13),
              (Sig_lcd_data9, MPUPinSignal (Just 0) Sig_lcd_data9),
              (Sig_mcasp0_fsx, MPUPinSignal (Just 3) Sig_mcasp0_fsx),
              (Sig_pr1_mii0_rxd2, MPUPinSignal (Just 5) Sig_pr1_mii0_rxd2),
              (Sig_uart2_rtsn, MPUPinSignal (Just 6) Sig_uart2_rtsn),
              (Sig_uart5_rxd, MPUPinSignal (Just 4) Sig_uart5_rxd)])),
       (MPU_LCD_DATA10,
        MPUPin MPU_LCD_DATA10 (Just "lcd_data10")
          (Map.fromList
             [(Sig_ehrpwm1A, MPUPinSignal (Just 2) Sig_ehrpwm1A),
              (Sig_gpio2_16, MPUPinSignal (Just 7) Sig_gpio2_16),
              (Sig_gpmc_a14, MPUPinSignal (Just 1) Sig_gpmc_a14),
              (Sig_lcd_data10, MPUPinSignal (Just 0) Sig_lcd_data10),
              (Sig_mcasp0_axr0, MPUPinSignal (Just 3) Sig_mcasp0_axr0),
              (Sig_pr1_mii0_rxd1, MPUPinSignal (Just 5) Sig_pr1_mii0_rxd1),
              (Sig_uart3_ctsn, MPUPinSignal (Just 6) Sig_uart3_ctsn)])),
       (MPU_LCD_DATA11,
        MPUPin MPU_LCD_DATA11 (Just "lcd_data11")
          (Map.fromList
             [(Sig_ehrpwm1B, MPUPinSignal (Just 2) Sig_ehrpwm1B),
              (Sig_gpio2_17, MPUPinSignal (Just 7) Sig_gpio2_17),
              (Sig_gpmc_a15, MPUPinSignal (Just 1) Sig_gpmc_a15),
              (Sig_lcd_data11, MPUPinSignal (Just 0) Sig_lcd_data11),
              (Sig_mcasp0_ahclkr, MPUPinSignal (Just 3) Sig_mcasp0_ahclkr),
              (Sig_mcasp0_axr2, MPUPinSignal (Just 4) Sig_mcasp0_axr2),
              (Sig_pr1_mii0_rxd0, MPUPinSignal (Just 5) Sig_pr1_mii0_rxd0),
              (Sig_uart3_rtsn, MPUPinSignal (Just 6) Sig_uart3_rtsn)])),
       (MPU_LCD_DATA12,
        MPUPin MPU_LCD_DATA12 (Just "lcd_data12")
          (Map.fromList
             [(Sig_eQEP1A_in, MPUPinSignal (Just 2) Sig_eQEP1A_in),
              (Sig_gpio0_8, MPUPinSignal (Just 7) Sig_gpio0_8),
              (Sig_gpmc_a16, MPUPinSignal (Just 1) Sig_gpmc_a16),
              (Sig_lcd_data12, MPUPinSignal (Just 0) Sig_lcd_data12),
              (Sig_mcasp0_aclkr, MPUPinSignal (Just 3) Sig_mcasp0_aclkr),
              (Sig_mcasp0_axr2, MPUPinSignal (Just 4) Sig_mcasp0_axr2),
              (Sig_pr1_mii0_rxlink, MPUPinSignal (Just 5) Sig_pr1_mii0_rxlink),
              (Sig_uart4_ctsn, MPUPinSignal (Just 6) Sig_uart4_ctsn)])),
       (MPU_LCD_DATA13,
        MPUPin MPU_LCD_DATA13 (Just "lcd_data13")
          (Map.fromList
             [(Sig_eQEP1B_in, MPUPinSignal (Just 2) Sig_eQEP1B_in),
              (Sig_gpio0_9, MPUPinSignal (Just 7) Sig_gpio0_9),
              (Sig_gpmc_a17, MPUPinSignal (Just 1) Sig_gpmc_a17),
              (Sig_lcd_data13, MPUPinSignal (Just 0) Sig_lcd_data13),
              (Sig_mcasp0_axr3, MPUPinSignal (Just 4) Sig_mcasp0_axr3),
              (Sig_mcasp0_fsr, MPUPinSignal (Just 3) Sig_mcasp0_fsr),
              (Sig_pr1_mii0_rxer, MPUPinSignal (Just 5) Sig_pr1_mii0_rxer),
              (Sig_uart4_rtsn, MPUPinSignal (Just 6) Sig_uart4_rtsn)])),
       (MPU_LCD_DATA14,
        MPUPin MPU_LCD_DATA14 (Just "lcd_data14")
          (Map.fromList
             [(Sig_eQEP1_index, MPUPinSignal (Just 2) Sig_eQEP1_index),
              (Sig_gpio0_10, MPUPinSignal (Just 7) Sig_gpio0_10),
              (Sig_gpmc_a18, MPUPinSignal (Just 1) Sig_gpmc_a18),
              (Sig_lcd_data14, MPUPinSignal (Just 0) Sig_lcd_data14),
              (Sig_mcasp0_axr1, MPUPinSignal (Just 3) Sig_mcasp0_axr1),
              (Sig_pr1_mii_mr0_clk, MPUPinSignal (Just 5) Sig_pr1_mii_mr0_clk),
              (Sig_uart5_ctsn, MPUPinSignal (Just 6) Sig_uart5_ctsn),
              (Sig_uart5_rxd, MPUPinSignal (Just 4) Sig_uart5_rxd)])),
       (MPU_LCD_DATA15,
        MPUPin MPU_LCD_DATA15 (Just "lcd_data15")
          (Map.fromList
             [(Sig_eQEP1_strobe, MPUPinSignal (Just 2) Sig_eQEP1_strobe),
              (Sig_gpio0_11, MPUPinSignal (Just 7) Sig_gpio0_11),
              (Sig_gpmc_a19, MPUPinSignal (Just 1) Sig_gpmc_a19),
              (Sig_lcd_data15, MPUPinSignal (Just 0) Sig_lcd_data15),
              (Sig_mcasp0_ahclkx, MPUPinSignal (Just 3) Sig_mcasp0_ahclkx),
              (Sig_mcasp0_axr3, MPUPinSignal (Just 4) Sig_mcasp0_axr3),
              (Sig_pr1_mii0_rxdv, MPUPinSignal (Just 5) Sig_pr1_mii0_rxdv),
              (Sig_uart5_rtsn, MPUPinSignal (Just 6) Sig_uart5_rtsn)])),
       (MPU_LCD_HSYNC,
        MPUPin MPU_LCD_HSYNC (Just "lcd_hsync")
          (Map.fromList
             [(Sig_gpio2_23, MPUPinSignal (Just 7) Sig_gpio2_23),
              (Sig_gpmc_a9, MPUPinSignal (Just 1) Sig_gpmc_a9),
              (Sig_lcd_hsync, MPUPinSignal (Just 0) Sig_lcd_hsync),
              (Sig_pr1_edio_data_in3,
               MPUPinSignal (Just 3) Sig_pr1_edio_data_in3),
              (Sig_pr1_edio_data_out3,
               MPUPinSignal (Just 4) Sig_pr1_edio_data_out3),
              (Sig_pr1_pru1_pru_r30_9,
               MPUPinSignal (Just 5) Sig_pr1_pru1_pru_r30_9),
              (Sig_pr1_pru1_pru_r31_9,
               MPUPinSignal (Just 6) Sig_pr1_pru1_pru_r31_9)])),
       (MPU_LCD_PCLK,
        MPUPin MPU_LCD_PCLK (Just "lcd_pclk")
          (Map.fromList
             [(Sig_gpio2_24, MPUPinSignal (Just 7) Sig_gpio2_24),
              (Sig_gpmc_a10, MPUPinSignal (Just 1) Sig_gpmc_a10),
              (Sig_lcd_pclk, MPUPinSignal (Just 0) Sig_lcd_pclk),
              (Sig_pr1_edio_data_in4,
               MPUPinSignal (Just 3) Sig_pr1_edio_data_in4),
              (Sig_pr1_edio_data_out4,
               MPUPinSignal (Just 4) Sig_pr1_edio_data_out4),
              (Sig_pr1_mii0_crs, MPUPinSignal (Just 2) Sig_pr1_mii0_crs),
              (Sig_pr1_pru1_pru_r30_10,
               MPUPinSignal (Just 5) Sig_pr1_pru1_pru_r30_10),
              (Sig_pr1_pru1_pru_r31_10,
               MPUPinSignal (Just 6) Sig_pr1_pru1_pru_r31_10)])),
       (MPU_LCD_VSYNC,
        MPUPin MPU_LCD_VSYNC (Just "lcd_vsync")
          (Map.fromList
             [(Sig_gpio2_22, MPUPinSignal (Just 7) Sig_gpio2_22),
              (Sig_gpmc_a8, MPUPinSignal (Just 1) Sig_gpmc_a8),
              (Sig_lcd_vsync, MPUPinSignal (Just 0) Sig_lcd_vsync),
              (Sig_pr1_edio_data_in2,
               MPUPinSignal (Just 3) Sig_pr1_edio_data_in2),
              (Sig_pr1_edio_data_out2,
               MPUPinSignal (Just 4) Sig_pr1_edio_data_out2),
              (Sig_pr1_pru1_pru_r30_8,
               MPUPinSignal (Just 5) Sig_pr1_pru1_pru_r30_8),
              (Sig_pr1_pru1_pru_r31_8,
               MPUPinSignal (Just 6) Sig_pr1_pru1_pru_r31_8)])),
       (MPU_MCASP0_ACLKR,
        MPUPin MPU_MCASP0_ACLKR (Just "mcasp0_aclkr")
          (Map.fromList
             [(Sig_eQEP0A_in, MPUPinSignal (Just 1) Sig_eQEP0A_in),
              (Sig_gpio3_18, MPUPinSignal (Just 7) Sig_gpio3_18),
              (Sig_mcasp0_aclkr, MPUPinSignal (Just 0) Sig_mcasp0_aclkr),
              (Sig_mcasp0_axr2, MPUPinSignal (Just 2) Sig_mcasp0_axr2),
              (Sig_mcasp1_aclkx, MPUPinSignal (Just 3) Sig_mcasp1_aclkx),
              (Sig_mmc0_sdwp, MPUPinSignal (Just 4) Sig_mmc0_sdwp),
              (Sig_pr1_pru0_pru_r30_4,
               MPUPinSignal (Just 5) Sig_pr1_pru0_pru_r30_4),
              (Sig_pr1_pru0_pru_r31_4,
               MPUPinSignal (Just 6) Sig_pr1_pru0_pru_r31_4)])),
       (MPU_MCASP0_ACLKX,
        MPUPin MPU_MCASP0_ACLKX (Just "mcasp0_aclkx")
          (Map.fromList
             [(Sig_ehrpwm0A, MPUPinSignal (Just 1) Sig_ehrpwm0A),
              (Sig_gpio3_14, MPUPinSignal (Just 7) Sig_gpio3_14),
              (Sig_mcasp0_aclkx, MPUPinSignal (Just 0) Sig_mcasp0_aclkx),
              (Sig_mmc0_sdcd, MPUPinSignal (Just 4) Sig_mmc0_sdcd),
              (Sig_pr1_pru0_pru_r30_0,
               MPUPinSignal (Just 5) Sig_pr1_pru0_pru_r30_0),
              (Sig_pr1_pru0_pru_r31_0,
               MPUPinSignal (Just 6) Sig_pr1_pru0_pru_r31_0),
              (Sig_spi1_sclk, MPUPinSignal (Just 3) Sig_spi1_sclk)])),
       (MPU_MCASP0_AHCLKR,
        MPUPin MPU_MCASP0_AHCLKR (Just "mcasp0_ahclkr")
          (Map.fromList
             [(Sig_eCAP2_in_PWM2_out,
               MPUPinSignal (Just 4) Sig_eCAP2_in_PWM2_out),
              (Sig_ehrpwm0_synci, MPUPinSignal (Just 1) Sig_ehrpwm0_synci),
              (Sig_gpio3_17, MPUPinSignal (Just 7) Sig_gpio3_17),
              (Sig_mcasp0_ahclkr, MPUPinSignal (Just 0) Sig_mcasp0_ahclkr),
              (Sig_mcasp0_axr2, MPUPinSignal (Just 2) Sig_mcasp0_axr2),
              (Sig_pr1_pru0_pru_r30_3,
               MPUPinSignal (Just 5) Sig_pr1_pru0_pru_r30_3),
              (Sig_pr1_pru0_pru_r31_3,
               MPUPinSignal (Just 6) Sig_pr1_pru0_pru_r31_3),
              (Sig_spi1_cs0, MPUPinSignal (Just 3) Sig_spi1_cs0)])),
       (MPU_MCASP0_AHCLKX,
        MPUPin MPU_MCASP0_AHCLKX (Just "mcasp0_ahclkx")
          (Map.fromList
             [(Sig_EMU4, MPUPinSignal (Just 4) Sig_EMU4),
              (Sig_eQEP0_strobe, MPUPinSignal (Just 1) Sig_eQEP0_strobe),
              (Sig_gpio3_21, MPUPinSignal (Just 7) Sig_gpio3_21),
              (Sig_mcasp0_ahclkx, MPUPinSignal (Just 0) Sig_mcasp0_ahclkx),
              (Sig_mcasp0_axr3, MPUPinSignal (Just 2) Sig_mcasp0_axr3),
              (Sig_mcasp1_axr1, MPUPinSignal (Just 3) Sig_mcasp1_axr1),
              (Sig_pr1_pru0_pru_r30_7,
               MPUPinSignal (Just 5) Sig_pr1_pru0_pru_r30_7),
              (Sig_pr1_pru0_pru_r31_7,
               MPUPinSignal (Just 6) Sig_pr1_pru0_pru_r31_7)])),
       (MPU_MCASP0_AXR0,
        MPUPin MPU_MCASP0_AXR0 (Just "mcasp0_axr0")
          (Map.fromList
             [(Sig_ehrpwm0_tripzone_input,
               MPUPinSignal (Just 1) Sig_ehrpwm0_tripzone_input),
              (Sig_gpio3_16, MPUPinSignal (Just 7) Sig_gpio3_16),
              (Sig_mcasp0_axr0, MPUPinSignal (Just 0) Sig_mcasp0_axr0),
              (Sig_mmc2_sdcd, MPUPinSignal (Just 4) Sig_mmc2_sdcd),
              (Sig_pr1_pru0_pru_r30_2,
               MPUPinSignal (Just 5) Sig_pr1_pru0_pru_r30_2),
              (Sig_pr1_pru0_pru_r31_2,
               MPUPinSignal (Just 6) Sig_pr1_pru0_pru_r31_2),
              (Sig_spi1_d1, MPUPinSignal (Just 3) Sig_spi1_d1)])),
       (MPU_MCASP0_AXR1,
        MPUPin MPU_MCASP0_AXR1 (Just "mcasp0_axr1")
          (Map.fromList
             [(Sig_EMU3, MPUPinSignal (Just 4) Sig_EMU3),
              (Sig_eQEP0_index, MPUPinSignal (Just 1) Sig_eQEP0_index),
              (Sig_gpio3_20, MPUPinSignal (Just 7) Sig_gpio3_20),
              (Sig_mcasp0_axr1, MPUPinSignal (Just 0) Sig_mcasp0_axr1),
              (Sig_mcasp1_axr0, MPUPinSignal (Just 3) Sig_mcasp1_axr0),
              (Sig_pr1_pru0_pru_r30_6,
               MPUPinSignal (Just 5) Sig_pr1_pru0_pru_r30_6),
              (Sig_pr1_pru0_pru_r31_6,
               MPUPinSignal (Just 6) Sig_pr1_pru0_pru_r31_6)])),
       (MPU_MCASP0_FSR,
        MPUPin MPU_MCASP0_FSR (Just "mcasp0_fsr")
          (Map.fromList
             [(Sig_EMU2, MPUPinSignal (Just 4) Sig_EMU2),
              (Sig_eQEP0B_in, MPUPinSignal (Just 1) Sig_eQEP0B_in),
              (Sig_gpio3_19, MPUPinSignal (Just 7) Sig_gpio3_19),
              (Sig_mcasp0_axr3, MPUPinSignal (Just 2) Sig_mcasp0_axr3),
              (Sig_mcasp0_fsr, MPUPinSignal (Just 0) Sig_mcasp0_fsr),
              (Sig_mcasp1_fsx, MPUPinSignal (Just 3) Sig_mcasp1_fsx),
              (Sig_pr1_pru0_pru_r30_5,
               MPUPinSignal (Just 5) Sig_pr1_pru0_pru_r30_5),
              (Sig_pr1_pru0_pru_r31_5,
               MPUPinSignal (Just 6) Sig_pr1_pru0_pru_r31_5)])),
       (MPU_MCASP0_FSX,
        MPUPin MPU_MCASP0_FSX (Just "mcasp0_fsx")
          (Map.fromList
             [(Sig_ehrpwm0B, MPUPinSignal (Just 1) Sig_ehrpwm0B),
              (Sig_gpio3_15, MPUPinSignal (Just 7) Sig_gpio3_15),
              (Sig_mcasp0_fsx, MPUPinSignal (Just 0) Sig_mcasp0_fsx),
              (Sig_mmc1_sdcd, MPUPinSignal (Just 4) Sig_mmc1_sdcd),
              (Sig_pr1_pru0_pru_r30_1,
               MPUPinSignal (Just 5) Sig_pr1_pru0_pru_r30_1),
              (Sig_pr1_pru0_pru_r31_1,
               MPUPinSignal (Just 6) Sig_pr1_pru0_pru_r31_1),
              (Sig_spi1_d0, MPUPinSignal (Just 3) Sig_spi1_d0)])),
       (MPU_MDC,
        MPUPin MPU_MDC (Just "mdio_clk")
          (Map.fromList
             [(Sig_gpio0_1, MPUPinSignal (Just 7) Sig_gpio0_1),
              (Sig_mdio_clk, MPUPinSignal (Just 0) Sig_mdio_clk),
              (Sig_mmc0_sdwp, MPUPinSignal (Just 4) Sig_mmc0_sdwp),
              (Sig_mmc1_clk, MPUPinSignal (Just 5) Sig_mmc1_clk),
              (Sig_mmc2_clk, MPUPinSignal (Just 6) Sig_mmc2_clk),
              (Sig_timer5, MPUPinSignal (Just 1) Sig_timer5),
              (Sig_uart3_rtsn, MPUPinSignal (Just 3) Sig_uart3_rtsn),
              (Sig_uart5_txd, MPUPinSignal (Just 2) Sig_uart5_txd)])),
       (MPU_MDIO,
        MPUPin MPU_MDIO (Just "mdio_data")
          (Map.fromList
             [(Sig_gpio0_0, MPUPinSignal (Just 7) Sig_gpio0_0),
              (Sig_mdio_data, MPUPinSignal (Just 0) Sig_mdio_data),
              (Sig_mmc0_sdcd, MPUPinSignal (Just 4) Sig_mmc0_sdcd),
              (Sig_mmc1_cmd, MPUPinSignal (Just 5) Sig_mmc1_cmd),
              (Sig_mmc2_cmd, MPUPinSignal (Just 6) Sig_mmc2_cmd),
              (Sig_timer6, MPUPinSignal (Just 1) Sig_timer6),
              (Sig_uart3_ctsn, MPUPinSignal (Just 3) Sig_uart3_ctsn),
              (Sig_uart5_rxd, MPUPinSignal (Just 2) Sig_uart5_rxd)])),
       (MPU_MII1_COL,
        MPUPin MPU_MII1_COL (Just "mii1_col")
          (Map.fromList
             [(Sig_gmii1_col, MPUPinSignal (Just 0) Sig_gmii1_col),
              (Sig_gpio3_0, MPUPinSignal (Just 7) Sig_gpio3_0),
              (Sig_mcasp0_axr2, MPUPinSignal (Just 6) Sig_mcasp0_axr2),
              (Sig_mcasp1_axr2, MPUPinSignal (Just 4) Sig_mcasp1_axr2),
              (Sig_mmc2_dat3, MPUPinSignal (Just 5) Sig_mmc2_dat3),
              (Sig_rmii2_refclk, MPUPinSignal (Just 1) Sig_rmii2_refclk),
              (Sig_spi1_sclk, MPUPinSignal (Just 2) Sig_spi1_sclk),
              (Sig_uart5_rxd, MPUPinSignal (Just 3) Sig_uart5_rxd)])),
       (MPU_MII1_CRS,
        MPUPin MPU_MII1_CRS (Just "mii1_crs")
          (Map.fromList
             [(Sig_I2C1_SDA, MPUPinSignal (Just 3) Sig_I2C1_SDA),
              (Sig_gmii1_crs, MPUPinSignal (Just 0) Sig_gmii1_crs),
              (Sig_gpio3_1, MPUPinSignal (Just 7) Sig_gpio3_1),
              (Sig_mcasp1_aclkx, MPUPinSignal (Just 4) Sig_mcasp1_aclkx),
              (Sig_rmii1_crs_dv, MPUPinSignal (Just 1) Sig_rmii1_crs_dv),
              (Sig_spi1_d0, MPUPinSignal (Just 2) Sig_spi1_d0),
              (Sig_uart2_rxd, MPUPinSignal (Just 6) Sig_uart2_rxd),
              (Sig_uart5_ctsn, MPUPinSignal (Just 5) Sig_uart5_ctsn)])),
       (MPU_MII1_RXD0,
        MPUPin MPU_MII1_RXD0 (Just "mii1_rxd0")
          (Map.fromList
             [(Sig_gmii1_rxd0, MPUPinSignal (Just 0) Sig_gmii1_rxd0),
              (Sig_gpio2_21, MPUPinSignal (Just 7) Sig_gpio2_21),
              (Sig_mcasp0_axr3, MPUPinSignal (Just 6) Sig_mcasp0_axr3),
              (Sig_mcasp1_aclkr, MPUPinSignal (Just 5) Sig_mcasp1_aclkr),
              (Sig_mcasp1_ahclkr, MPUPinSignal (Just 4) Sig_mcasp1_ahclkr),
              (Sig_mcasp1_ahclkx, MPUPinSignal (Just 3) Sig_mcasp1_ahclkx),
              (Sig_rgmii1_rd0, MPUPinSignal (Just 2) Sig_rgmii1_rd0),
              (Sig_rmii1_rxd0, MPUPinSignal (Just 1) Sig_rmii1_rxd0)])),
       (MPU_MII1_RXD1,
        MPUPin MPU_MII1_RXD1 (Just "mii1_rxd1")
          (Map.fromList
             [(Sig_eQEP0_strobe, MPUPinSignal (Just 5) Sig_eQEP0_strobe),
              (Sig_gmii1_rxd1, MPUPinSignal (Just 0) Sig_gmii1_rxd1),
              (Sig_gpio2_20, MPUPinSignal (Just 7) Sig_gpio2_20),
              (Sig_mcasp1_axr3, MPUPinSignal (Just 3) Sig_mcasp1_axr3),
              (Sig_mcasp1_fsr, MPUPinSignal (Just 4) Sig_mcasp1_fsr),
              (Sig_mmc2_clk, MPUPinSignal (Just 6) Sig_mmc2_clk),
              (Sig_rgmii1_rd1, MPUPinSignal (Just 2) Sig_rgmii1_rd1),
              (Sig_rmii1_rxd1, MPUPinSignal (Just 1) Sig_rmii1_rxd1)])),
       (MPU_MII1_RXD2,
        MPUPin MPU_MII1_RXD2 (Just "mii1_rxd2")
          (Map.fromList
             [(Sig_gmii1_rxd2, MPUPinSignal (Just 0) Sig_gmii1_rxd2),
              (Sig_gpio2_19, MPUPinSignal (Just 7) Sig_gpio2_19),
              (Sig_mcasp0_axr1, MPUPinSignal (Just 6) Sig_mcasp0_axr1),
              (Sig_mmc0_dat4, MPUPinSignal (Just 3) Sig_mmc0_dat4),
              (Sig_mmc1_dat3, MPUPinSignal (Just 4) Sig_mmc1_dat3),
              (Sig_rgmii1_rd2, MPUPinSignal (Just 2) Sig_rgmii1_rd2),
              (Sig_uart1_rin, MPUPinSignal (Just 5) Sig_uart1_rin),
              (Sig_uart3_txd, MPUPinSignal (Just 1) Sig_uart3_txd)])),
       (MPU_MII1_RXD3,
        MPUPin MPU_MII1_RXD3 (Just "mii1_rxd3")
          (Map.fromList
             [(Sig_gmii1_rxd3, MPUPinSignal (Just 0) Sig_gmii1_rxd3),
              (Sig_gpio2_18, MPUPinSignal (Just 7) Sig_gpio2_18),
              (Sig_mcasp0_axr0, MPUPinSignal (Just 6) Sig_mcasp0_axr0),
              (Sig_mmc0_dat5, MPUPinSignal (Just 3) Sig_mmc0_dat5),
              (Sig_mmc1_dat2, MPUPinSignal (Just 4) Sig_mmc1_dat2),
              (Sig_rgmii1_rd3, MPUPinSignal (Just 2) Sig_rgmii1_rd3),
              (Sig_uart1_dtrn, MPUPinSignal (Just 5) Sig_uart1_dtrn),
              (Sig_uart3_rxd, MPUPinSignal (Just 1) Sig_uart3_rxd)])),
       (MPU_MII1_RX_CLK,
        MPUPin MPU_MII1_RX_CLK (Just "mii1_rxclk")
          (Map.fromList
             [(Sig_gmii1_rxclk, MPUPinSignal (Just 0) Sig_gmii1_rxclk),
              (Sig_gpio3_10, MPUPinSignal (Just 7) Sig_gpio3_10),
              (Sig_mcasp0_fsx, MPUPinSignal (Just 6) Sig_mcasp0_fsx),
              (Sig_mmc0_dat6, MPUPinSignal (Just 3) Sig_mmc0_dat6),
              (Sig_mmc1_dat1, MPUPinSignal (Just 4) Sig_mmc1_dat1),
              (Sig_rgmii1_rclk, MPUPinSignal (Just 2) Sig_rgmii1_rclk),
              (Sig_uart1_dsrn, MPUPinSignal (Just 5) Sig_uart1_dsrn),
              (Sig_uart2_txd, MPUPinSignal (Just 1) Sig_uart2_txd)])),
       (MPU_MII1_RX_DV,
        MPUPin MPU_MII1_RX_DV (Just "mii1_rxdv")
          (Map.fromList
             [(Sig_gmii1_rxdv, MPUPinSignal (Just 0) Sig_gmii1_rxdv),
              (Sig_gpio3_4, MPUPinSignal (Just 7) Sig_gpio3_4),
              (Sig_lcd_memory_clk, MPUPinSignal (Just 1) Sig_lcd_memory_clk),
              (Sig_mcasp0_aclkr, MPUPinSignal (Just 6) Sig_mcasp0_aclkr),
              (Sig_mcasp1_aclkx, MPUPinSignal (Just 4) Sig_mcasp1_aclkx),
              (Sig_mmc2_dat0, MPUPinSignal (Just 5) Sig_mmc2_dat0),
              (Sig_rgmii1_rctl, MPUPinSignal (Just 2) Sig_rgmii1_rctl),
              (Sig_uart5_txd, MPUPinSignal (Just 3) Sig_uart5_txd)])),
       (MPU_MII1_RX_ER,
        MPUPin MPU_MII1_RX_ER (Just "mii1_rxerr")
          (Map.fromList
             [(Sig_I2C1_SCL, MPUPinSignal (Just 3) Sig_I2C1_SCL),
              (Sig_gmii1_rxerr, MPUPinSignal (Just 0) Sig_gmii1_rxerr),
              (Sig_gpio3_2, MPUPinSignal (Just 7) Sig_gpio3_2),
              (Sig_mcasp1_fsx, MPUPinSignal (Just 4) Sig_mcasp1_fsx),
              (Sig_rmii1_rxerr, MPUPinSignal (Just 1) Sig_rmii1_rxerr),
              (Sig_spi1_d1, MPUPinSignal (Just 2) Sig_spi1_d1),
              (Sig_uart2_txd, MPUPinSignal (Just 6) Sig_uart2_txd),
              (Sig_uart5_rtsn, MPUPinSignal (Just 5) Sig_uart5_rtsn)])),
       (MPU_MII1_TXD0,
        MPUPin MPU_MII1_TXD0 (Just "mii1_txd0")
          (Map.fromList
             [(Sig_eQEP0B_in, MPUPinSignal (Just 5) Sig_eQEP0B_in),
              (Sig_gmii1_txd0, MPUPinSignal (Just 0) Sig_gmii1_txd0),
              (Sig_gpio0_28, MPUPinSignal (Just 7) Sig_gpio0_28),
              (Sig_mcasp1_aclkr, MPUPinSignal (Just 4) Sig_mcasp1_aclkr),
              (Sig_mcasp1_axr2, MPUPinSignal (Just 3) Sig_mcasp1_axr2),
              (Sig_mmc1_clk, MPUPinSignal (Just 6) Sig_mmc1_clk),
              (Sig_rgmii1_td0, MPUPinSignal (Just 2) Sig_rgmii1_td0),
              (Sig_rmii1_txd0, MPUPinSignal (Just 1) Sig_rmii1_txd0)])),
       (MPU_MII1_TXD1,
        MPUPin MPU_MII1_TXD1 (Just "mii1_txd1")
          (Map.fromList
             [(Sig_eQEP0A_in, MPUPinSignal (Just 5) Sig_eQEP0A_in),
              (Sig_gmii1_txd1, MPUPinSignal (Just 0) Sig_gmii1_txd1),
              (Sig_gpio0_21, MPUPinSignal (Just 7) Sig_gpio0_21),
              (Sig_mcasp1_axr1, MPUPinSignal (Just 4) Sig_mcasp1_axr1),
              (Sig_mcasp1_fsr, MPUPinSignal (Just 3) Sig_mcasp1_fsr),
              (Sig_mmc1_cmd, MPUPinSignal (Just 6) Sig_mmc1_cmd),
              (Sig_rgmii1_td1, MPUPinSignal (Just 2) Sig_rgmii1_td1),
              (Sig_rmii1_txd1, MPUPinSignal (Just 1) Sig_rmii1_txd1)])),
       (MPU_MII1_TXD2,
        MPUPin MPU_MII1_TXD2 (Just "mii1_txd2")
          (Map.fromList
             [(Sig_dcan0_rx, MPUPinSignal (Just 1) Sig_dcan0_rx),
              (Sig_gmii1_txd2, MPUPinSignal (Just 0) Sig_gmii1_txd2),
              (Sig_gpio0_17, MPUPinSignal (Just 7) Sig_gpio0_17),
              (Sig_mcasp0_ahclkx, MPUPinSignal (Just 6) Sig_mcasp0_ahclkx),
              (Sig_mcasp1_axr0, MPUPinSignal (Just 4) Sig_mcasp1_axr0),
              (Sig_mmc2_dat2, MPUPinSignal (Just 5) Sig_mmc2_dat2),
              (Sig_rgmii1_td2, MPUPinSignal (Just 2) Sig_rgmii1_td2),
              (Sig_uart4_txd, MPUPinSignal (Just 3) Sig_uart4_txd)])),
       (MPU_MII1_TXD3,
        MPUPin MPU_MII1_TXD3 (Just "mii1_txd3")
          (Map.fromList
             [(Sig_dcan0_tx, MPUPinSignal (Just 1) Sig_dcan0_tx),
              (Sig_gmii1_txd3, MPUPinSignal (Just 0) Sig_gmii1_txd3),
              (Sig_gpio0_16, MPUPinSignal (Just 7) Sig_gpio0_16),
              (Sig_mcasp0_fsr, MPUPinSignal (Just 6) Sig_mcasp0_fsr),
              (Sig_mcasp1_fsx, MPUPinSignal (Just 4) Sig_mcasp1_fsx),
              (Sig_mmc2_dat1, MPUPinSignal (Just 5) Sig_mmc2_dat1),
              (Sig_rgmii1_td3, MPUPinSignal (Just 2) Sig_rgmii1_td3),
              (Sig_uart4_rxd, MPUPinSignal (Just 3) Sig_uart4_rxd)])),
       (MPU_MII1_TX_CLK,
        MPUPin MPU_MII1_TX_CLK (Just "mii1_txclk")
          (Map.fromList
             [(Sig_gmii1_txclk, MPUPinSignal (Just 0) Sig_gmii1_txclk),
              (Sig_gpio3_9, MPUPinSignal (Just 7) Sig_gpio3_9),
              (Sig_mcasp0_aclkx, MPUPinSignal (Just 6) Sig_mcasp0_aclkx),
              (Sig_mmc0_dat7, MPUPinSignal (Just 3) Sig_mmc0_dat7),
              (Sig_mmc1_dat0, MPUPinSignal (Just 4) Sig_mmc1_dat0),
              (Sig_rgmii1_tclk, MPUPinSignal (Just 2) Sig_rgmii1_tclk),
              (Sig_uart1_dcdn, MPUPinSignal (Just 5) Sig_uart1_dcdn),
              (Sig_uart2_rxd, MPUPinSignal (Just 1) Sig_uart2_rxd)])),
       (MPU_MII1_TX_EN,
        MPUPin MPU_MII1_TX_EN (Just "mii1_txen")
          (Map.fromList
             [(Sig_eQEP0_index, MPUPinSignal (Just 5) Sig_eQEP0_index),
              (Sig_gmii1_txen, MPUPinSignal (Just 0) Sig_gmii1_txen),
              (Sig_gpio3_3, MPUPinSignal (Just 7) Sig_gpio3_3),
              (Sig_mcasp1_axr0, MPUPinSignal (Just 4) Sig_mcasp1_axr0),
              (Sig_mmc2_cmd, MPUPinSignal (Just 6) Sig_mmc2_cmd),
              (Sig_rgmii1_tctl, MPUPinSignal (Just 2) Sig_rgmii1_tctl),
              (Sig_rmii1_txen, MPUPinSignal (Just 1) Sig_rmii1_txen),
              (Sig_timer4, MPUPinSignal (Just 3) Sig_timer4)])),
       (MPU_MMC0_CLK,
        MPUPin MPU_MMC0_CLK (Just "mmc0_clk")
          (Map.fromList
             [(Sig_dcan1_tx, MPUPinSignal (Just 4) Sig_dcan1_tx),
              (Sig_gpio2_30, MPUPinSignal (Just 7) Sig_gpio2_30),
              (Sig_gpmc_a24, MPUPinSignal (Just 1) Sig_gpmc_a24),
              (Sig_mmc0_clk, MPUPinSignal (Just 0) Sig_mmc0_clk),
              (Sig_pr1_pru0_pru_r30_12,
               MPUPinSignal (Just 5) Sig_pr1_pru0_pru_r30_12),
              (Sig_pr1_pru0_pru_r31_12,
               MPUPinSignal (Just 6) Sig_pr1_pru0_pru_r31_12),
              (Sig_uart2_rxd, MPUPinSignal (Just 3) Sig_uart2_rxd),
              (Sig_uart3_ctsn, MPUPinSignal (Just 2) Sig_uart3_ctsn)])),
       (MPU_MMC0_CMD,
        MPUPin MPU_MMC0_CMD (Just "mmc0_cmd")
          (Map.fromList
             [(Sig_dcan1_rx, MPUPinSignal (Just 4) Sig_dcan1_rx),
              (Sig_gpio2_31, MPUPinSignal (Just 7) Sig_gpio2_31),
              (Sig_gpmc_a25, MPUPinSignal (Just 1) Sig_gpmc_a25),
              (Sig_mmc0_cmd, MPUPinSignal (Just 0) Sig_mmc0_cmd),
              (Sig_pr1_pru0_pru_r30_13,
               MPUPinSignal (Just 5) Sig_pr1_pru0_pru_r30_13),
              (Sig_pr1_pru0_pru_r31_13,
               MPUPinSignal (Just 6) Sig_pr1_pru0_pru_r31_13),
              (Sig_uart2_txd, MPUPinSignal (Just 3) Sig_uart2_txd),
              (Sig_uart3_rtsn, MPUPinSignal (Just 2) Sig_uart3_rtsn)])),
       (MPU_MMC0_DAT0,
        MPUPin MPU_MMC0_DAT0 (Just "mmc0_dat0")
          (Map.fromList
             [(Sig_gpio2_29, MPUPinSignal (Just 7) Sig_gpio2_29),
              (Sig_gpmc_a23, MPUPinSignal (Just 1) Sig_gpmc_a23),
              (Sig_mmc0_dat0, MPUPinSignal (Just 0) Sig_mmc0_dat0),
              (Sig_pr1_pru0_pru_r30_11,
               MPUPinSignal (Just 5) Sig_pr1_pru0_pru_r30_11),
              (Sig_pr1_pru0_pru_r31_11,
               MPUPinSignal (Just 6) Sig_pr1_pru0_pru_r31_11),
              (Sig_uart1_rin, MPUPinSignal (Just 4) Sig_uart1_rin),
              (Sig_uart3_txd, MPUPinSignal (Just 3) Sig_uart3_txd),
              (Sig_uart5_rtsn, MPUPinSignal (Just 2) Sig_uart5_rtsn)])),
       (MPU_MMC0_DAT1,
        MPUPin MPU_MMC0_DAT1 (Just "mmc0_dat1")
          (Map.fromList
             [(Sig_gpio2_28, MPUPinSignal (Just 7) Sig_gpio2_28),
              (Sig_gpmc_a22, MPUPinSignal (Just 1) Sig_gpmc_a22),
              (Sig_mmc0_dat1, MPUPinSignal (Just 0) Sig_mmc0_dat1),
              (Sig_pr1_pru0_pru_r30_10,
               MPUPinSignal (Just 5) Sig_pr1_pru0_pru_r30_10),
              (Sig_pr1_pru0_pru_r31_10,
               MPUPinSignal (Just 6) Sig_pr1_pru0_pru_r31_10),
              (Sig_uart1_dtrn, MPUPinSignal (Just 4) Sig_uart1_dtrn),
              (Sig_uart3_rxd, MPUPinSignal (Just 3) Sig_uart3_rxd),
              (Sig_uart5_ctsn, MPUPinSignal (Just 2) Sig_uart5_ctsn)])),
       (MPU_MMC0_DAT2,
        MPUPin MPU_MMC0_DAT2 (Just "mmc0_dat2")
          (Map.fromList
             [(Sig_gpio2_27, MPUPinSignal (Just 7) Sig_gpio2_27),
              (Sig_gpmc_a21, MPUPinSignal (Just 1) Sig_gpmc_a21),
              (Sig_mmc0_dat2, MPUPinSignal (Just 0) Sig_mmc0_dat2),
              (Sig_pr1_pru0_pru_r30_9,
               MPUPinSignal (Just 5) Sig_pr1_pru0_pru_r30_9),
              (Sig_pr1_pru0_pru_r31_9,
               MPUPinSignal (Just 6) Sig_pr1_pru0_pru_r31_9),
              (Sig_timer6, MPUPinSignal (Just 3) Sig_timer6),
              (Sig_uart1_dsrn, MPUPinSignal (Just 4) Sig_uart1_dsrn),
              (Sig_uart4_rtsn, MPUPinSignal (Just 2) Sig_uart4_rtsn)])),
       (MPU_MMC0_DAT3,
        MPUPin MPU_MMC0_DAT3 (Just "mmc0_dat3")
          (Map.fromList
             [(Sig_gpio2_26, MPUPinSignal (Just 7) Sig_gpio2_26),
              (Sig_gpmc_a20, MPUPinSignal (Just 1) Sig_gpmc_a20),
              (Sig_mmc0_dat3, MPUPinSignal (Just 0) Sig_mmc0_dat3),
              (Sig_pr1_pru0_pru_r30_8,
               MPUPinSignal (Just 5) Sig_pr1_pru0_pru_r30_8),
              (Sig_pr1_pru0_pru_r31_8,
               MPUPinSignal (Just 6) Sig_pr1_pru0_pru_r31_8),
              (Sig_timer5, MPUPinSignal (Just 3) Sig_timer5),
              (Sig_uart1_dcdn, MPUPinSignal (Just 4) Sig_uart1_dcdn),
              (Sig_uart4_ctsn, MPUPinSignal (Just 2) Sig_uart4_ctsn)])),
       (MPU_PMIC_POWER_EN,
        MPUPin MPU_PMIC_POWER_EN Nothing
          (Map.fromList
             [(Sig_PMIC_POWER_EN, MPUPinSignal (Just 0) Sig_PMIC_POWER_EN)])),
       (MPU_PWRONRSTn,
        MPUPin MPU_PWRONRSTn Nothing
          (Map.fromList [(Sig_porz, MPUPinSignal (Just 0) Sig_porz)])),
       (MPU_RESERVED,
        MPUPin MPU_RESERVED Nothing
          (Map.fromList [(Sig_testout, MPUPinSignal (Just 0) Sig_testout)])),
       (MPU_RMII1_REF_CLK,
        MPUPin MPU_RMII1_REF_CLK (Just "rmii1_refclk")
          (Map.fromList
             [(Sig_gpio0_29, MPUPinSignal (Just 7) Sig_gpio0_29),
              (Sig_mcasp1_ahclkx, MPUPinSignal (Just 6) Sig_mcasp1_ahclkx),
              (Sig_mcasp1_axr3, MPUPinSignal (Just 4) Sig_mcasp1_axr3),
              (Sig_mmc0_pow, MPUPinSignal (Just 5) Sig_mmc0_pow),
              (Sig_rmii1_refclk, MPUPinSignal (Just 0) Sig_rmii1_refclk),
              (Sig_spi1_cs0, MPUPinSignal (Just 2) Sig_spi1_cs0),
              (Sig_uart5_txd, MPUPinSignal (Just 3) Sig_uart5_txd),
              (Sig_xdma_event_intr2,
               MPUPinSignal (Just 1) Sig_xdma_event_intr2)])),
       (MPU_RTC_KALDO_ENn,
        MPUPin MPU_RTC_KALDO_ENn Nothing
          (Map.fromList
             [(Sig_ENZ_KALDO_1P8V, MPUPinSignal (Just 0) Sig_ENZ_KALDO_1P8V)])),
       (MPU_RTC_PWRONRSTn,
        MPUPin MPU_RTC_PWRONRSTn Nothing
          (Map.fromList
             [(Sig_RTC_porz, MPUPinSignal (Just 0) Sig_RTC_porz)])),
       (MPU_RTC_XTALIN,
        MPUPin MPU_RTC_XTALIN Nothing
          (Map.fromList [(Sig_OSC1_IN, MPUPinSignal (Just 0) Sig_OSC1_IN)])),
       (MPU_RTC_XTALOUT,
        MPUPin MPU_RTC_XTALOUT Nothing
          (Map.fromList
             [(Sig_OSC1_OUT, MPUPinSignal (Just 0) Sig_OSC1_OUT)])),
       (MPU_SPI0_CS0,
        MPUPin MPU_SPI0_CS0 (Just "spi0_cs0")
          (Map.fromList
             [(Sig_I2C1_SCL, MPUPinSignal (Just 2) Sig_I2C1_SCL),
              (Sig_ehrpwm0_synci, MPUPinSignal (Just 3) Sig_ehrpwm0_synci),
              (Sig_gpio0_5, MPUPinSignal (Just 7) Sig_gpio0_5),
              (Sig_mmc2_sdwp, MPUPinSignal (Just 1) Sig_mmc2_sdwp),
              (Sig_pr1_edio_data_in1,
               MPUPinSignal (Just 5) Sig_pr1_edio_data_in1),
              (Sig_pr1_edio_data_out1,
               MPUPinSignal (Just 6) Sig_pr1_edio_data_out1),
              (Sig_pr1_uart0_txd, MPUPinSignal (Just 4) Sig_pr1_uart0_txd),
              (Sig_spi0_cs0, MPUPinSignal (Just 0) Sig_spi0_cs0)])),
       (MPU_SPI0_CS1,
        MPUPin MPU_SPI0_CS1 (Just "spi0_cs1")
          (Map.fromList
             [(Sig_EMU4, MPUPinSignal (Just 6) Sig_EMU4),
              (Sig_eCAP1_in_PWM1_out,
               MPUPinSignal (Just 2) Sig_eCAP1_in_PWM1_out),
              (Sig_gpio0_6, MPUPinSignal (Just 7) Sig_gpio0_6),
              (Sig_mmc0_pow, MPUPinSignal (Just 3) Sig_mmc0_pow),
              (Sig_mmc0_sdcd, MPUPinSignal (Just 5) Sig_mmc0_sdcd),
              (Sig_spi0_cs1, MPUPinSignal (Just 0) Sig_spi0_cs1),
              (Sig_uart3_rxd, MPUPinSignal (Just 1) Sig_uart3_rxd),
              (Sig_xdma_event_intr2,
               MPUPinSignal (Just 4) Sig_xdma_event_intr2)])),
       (MPU_SPI0_D0,
        MPUPin MPU_SPI0_D0 (Just "spi0_d0")
          (Map.fromList
             [(Sig_EMU3, MPUPinSignal (Just 6) Sig_EMU3),
              (Sig_I2C2_SCL, MPUPinSignal (Just 2) Sig_I2C2_SCL),
              (Sig_ehrpwm0B, MPUPinSignal (Just 3) Sig_ehrpwm0B),
              (Sig_gpio0_3, MPUPinSignal (Just 7) Sig_gpio0_3),
              (Sig_pr1_edio_latch_in,
               MPUPinSignal (Just 5) Sig_pr1_edio_latch_in),
              (Sig_pr1_uart0_rts_n, MPUPinSignal (Just 4) Sig_pr1_uart0_rts_n),
              (Sig_spi0_d0, MPUPinSignal (Just 0) Sig_spi0_d0),
              (Sig_uart2_txd, MPUPinSignal (Just 1) Sig_uart2_txd)])),
       (MPU_SPI0_D1,
        MPUPin MPU_SPI0_D1 (Just "spi0_d1")
          (Map.fromList
             [(Sig_I2C1_SDA, MPUPinSignal (Just 2) Sig_I2C1_SDA),
              (Sig_ehrpwm0_tripzone_input,
               MPUPinSignal (Just 3) Sig_ehrpwm0_tripzone_input),
              (Sig_gpio0_4, MPUPinSignal (Just 7) Sig_gpio0_4),
              (Sig_mmc1_sdwp, MPUPinSignal (Just 1) Sig_mmc1_sdwp),
              (Sig_pr1_edio_data_in0,
               MPUPinSignal (Just 5) Sig_pr1_edio_data_in0),
              (Sig_pr1_edio_data_out0,
               MPUPinSignal (Just 6) Sig_pr1_edio_data_out0),
              (Sig_pr1_uart0_rxd, MPUPinSignal (Just 4) Sig_pr1_uart0_rxd),
              (Sig_spi0_d1, MPUPinSignal (Just 0) Sig_spi0_d1)])),
       (MPU_SPI0_SCLK,
        MPUPin MPU_SPI0_SCLK (Just "spi0_sclk")
          (Map.fromList
             [(Sig_EMU2, MPUPinSignal (Just 6) Sig_EMU2),
              (Sig_I2C2_SDA, MPUPinSignal (Just 2) Sig_I2C2_SDA),
              (Sig_ehrpwm0A, MPUPinSignal (Just 3) Sig_ehrpwm0A),
              (Sig_gpio0_2, MPUPinSignal (Just 7) Sig_gpio0_2),
              (Sig_pr1_edio_sof, MPUPinSignal (Just 5) Sig_pr1_edio_sof),
              (Sig_pr1_uart0_cts_n, MPUPinSignal (Just 4) Sig_pr1_uart0_cts_n),
              (Sig_spi0_sclk, MPUPinSignal (Just 0) Sig_spi0_sclk),
              (Sig_uart2_rxd, MPUPinSignal (Just 1) Sig_uart2_rxd)])),
       (MPU_TCK,
        MPUPin MPU_TCK Nothing
          (Map.fromList [(Sig_TCK, MPUPinSignal (Just 0) Sig_TCK)])),
       (MPU_TDI,
        MPUPin MPU_TDI Nothing
          (Map.fromList [(Sig_TDI, MPUPinSignal (Just 0) Sig_TDI)])),
       (MPU_TDO,
        MPUPin MPU_TDO Nothing
          (Map.fromList [(Sig_TDO, MPUPinSignal (Just 0) Sig_TDO)])),
       (MPU_TMS,
        MPUPin MPU_TMS Nothing
          (Map.fromList [(Sig_TMS, MPUPinSignal (Just 0) Sig_TMS)])),
       (MPU_TRSTn,
        MPUPin MPU_TRSTn Nothing
          (Map.fromList [(Sig_nTRST, MPUPinSignal (Just 0) Sig_nTRST)])),
       (MPU_UART0_CTSn,
        MPUPin MPU_UART0_CTSn (Just "uart0_ctsn")
          (Map.fromList
             [(Sig_I2C1_SDA, MPUPinSignal (Just 3) Sig_I2C1_SDA),
              (Sig_dcan1_tx, MPUPinSignal (Just 2) Sig_dcan1_tx),
              (Sig_gpio1_8, MPUPinSignal (Just 7) Sig_gpio1_8),
              (Sig_pr1_edc_sync0_out,
               MPUPinSignal (Just 6) Sig_pr1_edc_sync0_out),
              (Sig_spi1_d0, MPUPinSignal (Just 4) Sig_spi1_d0),
              (Sig_timer7, MPUPinSignal (Just 5) Sig_timer7),
              (Sig_uart0_ctsn, MPUPinSignal (Just 0) Sig_uart0_ctsn),
              (Sig_uart4_rxd, MPUPinSignal (Just 1) Sig_uart4_rxd)])),
       (MPU_UART0_RTSn,
        MPUPin MPU_UART0_RTSn (Just "uart0_rtsn")
          (Map.fromList
             [(Sig_I2C1_SCL, MPUPinSignal (Just 3) Sig_I2C1_SCL),
              (Sig_dcan1_rx, MPUPinSignal (Just 2) Sig_dcan1_rx),
              (Sig_gpio1_9, MPUPinSignal (Just 7) Sig_gpio1_9),
              (Sig_pr1_edc_sync1_out,
               MPUPinSignal (Just 6) Sig_pr1_edc_sync1_out),
              (Sig_spi1_cs0, MPUPinSignal (Just 5) Sig_spi1_cs0),
              (Sig_spi1_d1, MPUPinSignal (Just 4) Sig_spi1_d1),
              (Sig_uart0_rtsn, MPUPinSignal (Just 0) Sig_uart0_rtsn),
              (Sig_uart4_txd, MPUPinSignal (Just 1) Sig_uart4_txd)])),
       (MPU_UART0_RXD,
        MPUPin MPU_UART0_RXD (Just "uart0_rxd")
          (Map.fromList
             [(Sig_I2C2_SDA, MPUPinSignal (Just 3) Sig_I2C2_SDA),
              (Sig_dcan0_tx, MPUPinSignal (Just 2) Sig_dcan0_tx),
              (Sig_eCAP2_in_PWM2_out,
               MPUPinSignal (Just 4) Sig_eCAP2_in_PWM2_out),
              (Sig_gpio1_10, MPUPinSignal (Just 7) Sig_gpio1_10),
              (Sig_pr1_pru1_pru_r30_14,
               MPUPinSignal (Just 5) Sig_pr1_pru1_pru_r30_14),
              (Sig_pr1_pru1_pru_r31_14,
               MPUPinSignal (Just 6) Sig_pr1_pru1_pru_r31_14),
              (Sig_spi1_cs0, MPUPinSignal (Just 1) Sig_spi1_cs0),
              (Sig_uart0_rxd, MPUPinSignal (Just 0) Sig_uart0_rxd)])),
       (MPU_UART0_TXD,
        MPUPin MPU_UART0_TXD (Just "uart0_txd")
          (Map.fromList
             [(Sig_I2C2_SCL, MPUPinSignal (Just 3) Sig_I2C2_SCL),
              (Sig_dcan0_rx, MPUPinSignal (Just 2) Sig_dcan0_rx),
              (Sig_eCAP1_in_PWM1_out,
               MPUPinSignal (Just 4) Sig_eCAP1_in_PWM1_out),
              (Sig_gpio1_11, MPUPinSignal (Just 7) Sig_gpio1_11),
              (Sig_pr1_pru1_pru_r30_15,
               MPUPinSignal (Just 5) Sig_pr1_pru1_pru_r30_15),
              (Sig_pr1_pru1_pru_r31_15,
               MPUPinSignal (Just 6) Sig_pr1_pru1_pru_r31_15),
              (Sig_spi1_cs1, MPUPinSignal (Just 1) Sig_spi1_cs1),
              (Sig_uart0_txd, MPUPinSignal (Just 0) Sig_uart0_txd)])),
       (MPU_UART1_CTSn,
        MPUPin MPU_UART1_CTSn (Just "uart1_ctsn")
          (Map.fromList
             [(Sig_I2C2_SDA, MPUPinSignal (Just 3) Sig_I2C2_SDA),
              (Sig_dcan0_tx, MPUPinSignal (Just 2) Sig_dcan0_tx),
              (Sig_gpio0_12, MPUPinSignal (Just 7) Sig_gpio0_12),
              (Sig_pr1_edc_latch0_in,
               MPUPinSignal (Just 6) Sig_pr1_edc_latch0_in),
              (Sig_pr1_uart0_cts_n, MPUPinSignal (Just 5) Sig_pr1_uart0_cts_n),
              (Sig_spi1_cs0, MPUPinSignal (Just 4) Sig_spi1_cs0),
              (Sig_timer6, MPUPinSignal (Just 1) Sig_timer6),
              (Sig_uart1_ctsn, MPUPinSignal (Just 0) Sig_uart1_ctsn)])),
       (MPU_UART1_RTSn,
        MPUPin MPU_UART1_RTSn (Just "uart1_rtsn")
          (Map.fromList
             [(Sig_I2C2_SCL, MPUPinSignal (Just 3) Sig_I2C2_SCL),
              (Sig_dcan0_rx, MPUPinSignal (Just 2) Sig_dcan0_rx),
              (Sig_gpio0_13, MPUPinSignal (Just 7) Sig_gpio0_13),
              (Sig_pr1_edc_latch1_in,
               MPUPinSignal (Just 6) Sig_pr1_edc_latch1_in),
              (Sig_pr1_uart0_rts_n, MPUPinSignal (Just 5) Sig_pr1_uart0_rts_n),
              (Sig_spi1_cs1, MPUPinSignal (Just 4) Sig_spi1_cs1),
              (Sig_timer5, MPUPinSignal (Just 1) Sig_timer5),
              (Sig_uart1_rtsn, MPUPinSignal (Just 0) Sig_uart1_rtsn)])),
       (MPU_UART1_RXD,
        MPUPin MPU_UART1_RXD (Just "uart1_rxd")
          (Map.fromList
             [(Sig_I2C1_SDA, MPUPinSignal (Just 3) Sig_I2C1_SDA),
              (Sig_dcan1_tx, MPUPinSignal (Just 2) Sig_dcan1_tx),
              (Sig_gpio0_14, MPUPinSignal (Just 7) Sig_gpio0_14),
              (Sig_mmc1_sdwp, MPUPinSignal (Just 1) Sig_mmc1_sdwp),
              (Sig_pr1_pru1_pru_r31_16,
               MPUPinSignal (Just 6) Sig_pr1_pru1_pru_r31_16),
              (Sig_pr1_uart0_rxd, MPUPinSignal (Just 5) Sig_pr1_uart0_rxd),
              (Sig_uart1_rxd, MPUPinSignal (Just 0) Sig_uart1_rxd)])),
       (MPU_UART1_TXD,
        MPUPin MPU_UART1_TXD (Just "uart1_txd")
          (Map.fromList
             [(Sig_I2C1_SCL, MPUPinSignal (Just 3) Sig_I2C1_SCL),
              (Sig_dcan1_rx, MPUPinSignal (Just 2) Sig_dcan1_rx),
              (Sig_gpio0_15, MPUPinSignal (Just 7) Sig_gpio0_15),
              (Sig_mmc2_sdwp, MPUPinSignal (Just 1) Sig_mmc2_sdwp),
              (Sig_pr1_pru0_pru_r31_16,
               MPUPinSignal (Just 6) Sig_pr1_pru0_pru_r31_16),
              (Sig_pr1_uart0_txd, MPUPinSignal (Just 5) Sig_pr1_uart0_txd),
              (Sig_uart1_txd, MPUPinSignal (Just 0) Sig_uart1_txd)])),
       (MPU_USB0_CE,
        MPUPin MPU_USB0_CE Nothing
          (Map.fromList [(Sig_USB0_CE, MPUPinSignal (Just 0) Sig_USB0_CE)])),
       (MPU_USB0_DM,
        MPUPin MPU_USB0_DM Nothing
          (Map.fromList [(Sig_USB0_DM, MPUPinSignal (Just 0) Sig_USB0_DM)])),
       (MPU_USB0_DP,
        MPUPin MPU_USB0_DP Nothing
          (Map.fromList [(Sig_USB0_DP, MPUPinSignal (Just 0) Sig_USB0_DP)])),
       (MPU_USB0_DRVVBUS,
        MPUPin MPU_USB0_DRVVBUS (Just "usb0_drvvbus")
          (Map.fromList
             [(Sig_USB0_DRVVBUS, MPUPinSignal (Just 0) Sig_USB0_DRVVBUS),
              (Sig_gpio0_18, MPUPinSignal (Just 7) Sig_gpio0_18)])),
       (MPU_USB0_ID,
        MPUPin MPU_USB0_ID Nothing
          (Map.fromList [(Sig_USB0_ID, MPUPinSignal (Just 0) Sig_USB0_ID)])),
       (MPU_USB0_VBUS,
        MPUPin MPU_USB0_VBUS Nothing
          (Map.fromList
             [(Sig_USB0_VBUS, MPUPinSignal (Just 0) Sig_USB0_VBUS)])),
       (MPU_USB1_CE,
        MPUPin MPU_USB1_CE Nothing
          (Map.fromList [(Sig_USB1_CE, MPUPinSignal (Just 0) Sig_USB1_CE)])),
       (MPU_USB1_DM,
        MPUPin MPU_USB1_DM Nothing
          (Map.fromList [(Sig_USB1_DM, MPUPinSignal (Just 0) Sig_USB1_DM)])),
       (MPU_USB1_DP,
        MPUPin MPU_USB1_DP Nothing
          (Map.fromList [(Sig_USB1_DP, MPUPinSignal (Just 0) Sig_USB1_DP)])),
       (MPU_USB1_DRVVBUS,
        MPUPin MPU_USB1_DRVVBUS (Just "usb1_drvvbus")
          (Map.fromList
             [(Sig_USB1_DRVVBUS, MPUPinSignal (Just 0) Sig_USB1_DRVVBUS),
              (Sig_gpio3_13, MPUPinSignal (Just 7) Sig_gpio3_13)])),
       (MPU_USB1_ID,
        MPUPin MPU_USB1_ID Nothing
          (Map.fromList [(Sig_USB1_ID, MPUPinSignal (Just 0) Sig_USB1_ID)])),
       (MPU_USB1_VBUS,
        MPUPin MPU_USB1_VBUS Nothing
          (Map.fromList
             [(Sig_USB1_VBUS, MPUPinSignal (Just 0) Sig_USB1_VBUS)])),
       (MPU_VDDA1P8V_USB0,
        MPUPin MPU_VDDA1P8V_USB0 Nothing
          (Map.fromList
             [(Sig_VDDA1P8V_USB0, MPUPinSignal Nothing Sig_VDDA1P8V_USB0)])),
       (MPU_VDDA1P8V_USB1,
        MPUPin MPU_VDDA1P8V_USB1 Nothing
          (Map.fromList
             [(Sig_VDDA1P8V_USB1, MPUPinSignal Nothing Sig_VDDA1P8V_USB1)])),
       (MPU_VDDA3P3V_USB0,
        MPUPin MPU_VDDA3P3V_USB0 Nothing
          (Map.fromList
             [(Sig_VDDA3P3V_USB0, MPUPinSignal Nothing Sig_VDDA3P3V_USB0)])),
       (MPU_VDDA3P3V_USB1,
        MPUPin MPU_VDDA3P3V_USB1 Nothing
          (Map.fromList
             [(Sig_VDDA3P3V_USB1, MPUPinSignal Nothing Sig_VDDA3P3V_USB1)])),
       (MPU_VDDA_ADC,
        MPUPin MPU_VDDA_ADC Nothing
          (Map.fromList
             [(Sig_VDDA_ADC, MPUPinSignal Nothing Sig_VDDA_ADC)])),
       (MPU_VDDS,
        MPUPin MPU_VDDS Nothing
          (Map.fromList [(Sig_VDDS, MPUPinSignal Nothing Sig_VDDS)])),
       (MPU_VDDSHV1,
        MPUPin MPU_VDDSHV1 Nothing
          (Map.fromList [(Sig_VDDSHV1, MPUPinSignal Nothing Sig_VDDSHV1)])),
       (MPU_VDDSHV2,
        MPUPin MPU_VDDSHV2 Nothing
          (Map.fromList [(Sig_VDDSHV2, MPUPinSignal Nothing Sig_VDDSHV2)])),
       (MPU_VDDSHV3,
        MPUPin MPU_VDDSHV3 Nothing
          (Map.fromList [(Sig_VDDSHV3, MPUPinSignal Nothing Sig_VDDSHV3)])),
       (MPU_VDDSHV4,
        MPUPin MPU_VDDSHV4 Nothing
          (Map.fromList [(Sig_VDDSHV4, MPUPinSignal Nothing Sig_VDDSHV4)])),
       (MPU_VDDSHV5,
        MPUPin MPU_VDDSHV5 Nothing
          (Map.fromList [(Sig_VDDSHV5, MPUPinSignal Nothing Sig_VDDSHV5)])),
       (MPU_VDDSHV6,
        MPUPin MPU_VDDSHV6 Nothing
          (Map.fromList [(Sig_VDDSHV6, MPUPinSignal Nothing Sig_VDDSHV6)])),
       (MPU_VDDS_DDR,
        MPUPin MPU_VDDS_DDR Nothing
          (Map.fromList
             [(Sig_VDDS_DDR, MPUPinSignal Nothing Sig_VDDS_DDR)])),
       (MPU_VDDS_OSC,
        MPUPin MPU_VDDS_OSC Nothing
          (Map.fromList
             [(Sig_VDDS_OSC, MPUPinSignal Nothing Sig_VDDS_OSC)])),
       (MPU_VDDS_PLL_CORE_LCD,
        MPUPin MPU_VDDS_PLL_CORE_LCD Nothing
          (Map.fromList
             [(Sig_VDDS_PLL_CORE_LCD,
               MPUPinSignal Nothing Sig_VDDS_PLL_CORE_LCD)])),
       (MPU_VDDS_PLL_DDR,
        MPUPin MPU_VDDS_PLL_DDR Nothing
          (Map.fromList
             [(Sig_VDDS_PLL_DDR, MPUPinSignal Nothing Sig_VDDS_PLL_DDR)])),
       (MPU_VDDS_PLL_MPU,
        MPUPin MPU_VDDS_PLL_MPU Nothing
          (Map.fromList
             [(Sig_VDDS_PLL_MPU, MPUPinSignal Nothing Sig_VDDS_PLL_MPU)])),
       (MPU_VDDS_RTC,
        MPUPin MPU_VDDS_RTC Nothing
          (Map.fromList
             [(Sig_VDDS_RTC, MPUPinSignal Nothing Sig_VDDS_RTC)])),
       (MPU_VDDS_SRAM_CORE_BG,
        MPUPin MPU_VDDS_SRAM_CORE_BG Nothing
          (Map.fromList
             [(Sig_VDDS_SRAM_CORE_BG,
               MPUPinSignal Nothing Sig_VDDS_SRAM_CORE_BG)])),
       (MPU_VDDS_SRAM_MPU_BB,
        MPUPin MPU_VDDS_SRAM_MPU_BB Nothing
          (Map.fromList
             [(Sig_VDDS_SRAM_MPU_BB,
               MPUPinSignal Nothing Sig_VDDS_SRAM_MPU_BB)])),
       (MPU_VDD_CORE,
        MPUPin MPU_VDD_CORE Nothing
          (Map.fromList
             [(Sig_VDD_CORE, MPUPinSignal Nothing Sig_VDD_CORE)])),
       (MPU_VDD_MPU,
        MPUPin MPU_VDD_MPU Nothing
          (Map.fromList [(Sig_VDD_MPU, MPUPinSignal Nothing Sig_VDD_MPU)])),
       (MPU_VDD_MPU_MON,
        MPUPin MPU_VDD_MPU_MON Nothing
          (Map.fromList
             [(Sig_VDD_MPU_MON, MPUPinSignal Nothing Sig_VDD_MPU_MON)])),
       (MPU_VPP,
        MPUPin MPU_VPP Nothing
          (Map.fromList [(Sig_VPP, MPUPinSignal Nothing Sig_VPP)])),
       (MPU_VREFN,
        MPUPin MPU_VREFN (Just "vrefn")
          (Map.fromList [(Sig_VREFN, MPUPinSignal (Just 0) Sig_VREFN)])),
       (MPU_VREFP,
        MPUPin MPU_VREFP (Just "vrefp")
          (Map.fromList [(Sig_VREFP, MPUPinSignal (Just 0) Sig_VREFP)])),
       (MPU_VSS,
        MPUPin MPU_VSS Nothing
          (Map.fromList [(Sig_VSS, MPUPinSignal Nothing Sig_VSS)])),
       (MPU_VSSA_ADC,
        MPUPin MPU_VSSA_ADC Nothing
          (Map.fromList
             [(Sig_VSSA_ADC, MPUPinSignal Nothing Sig_VSSA_ADC)])),
       (MPU_VSSA_USB,
        MPUPin MPU_VSSA_USB Nothing
          (Map.fromList
             [(Sig_VSSA_USB, MPUPinSignal Nothing Sig_VSSA_USB)])),
       (MPU_VSS_OSC,
        MPUPin MPU_VSS_OSC Nothing
          (Map.fromList [(Sig_VSS_OSC, MPUPinSignal Nothing Sig_VSS_OSC)])),
       (MPU_VSS_RTC,
        MPUPin MPU_VSS_RTC Nothing
          (Map.fromList [(Sig_VSS_RTC, MPUPinSignal Nothing Sig_VSS_RTC)])),
       (MPU_WARMRSTn,
        MPUPin MPU_WARMRSTn Nothing
          (Map.fromList
             [(Sig_nRESETIN_OUT, MPUPinSignal (Just 0) Sig_nRESETIN_OUT)])),
       (MPU_XDMA_EVENT_INTR0,
        MPUPin MPU_XDMA_EVENT_INTR0 (Just "xdma_event_intr0")
          (Map.fromList
             [(Sig_EMU2, MPUPinSignal (Just 6) Sig_EMU2),
              (Sig_clkout1, MPUPinSignal (Just 3) Sig_clkout1),
              (Sig_gpio0_19, MPUPinSignal (Just 7) Sig_gpio0_19),
              (Sig_pr1_pru1_pru_r31_16,
               MPUPinSignal (Just 5) Sig_pr1_pru1_pru_r31_16),
              (Sig_spi1_cs1, MPUPinSignal (Just 4) Sig_spi1_cs1),
              (Sig_timer4, MPUPinSignal (Just 2) Sig_timer4),
              (Sig_xdma_event_intr0,
               MPUPinSignal (Just 0) Sig_xdma_event_intr0)])),
       (MPU_XDMA_EVENT_INTR1,
        MPUPin MPU_XDMA_EVENT_INTR1 (Just "xdma_event_intr1")
          (Map.fromList
             [(Sig_EMU3, MPUPinSignal (Just 6) Sig_EMU3),
              (Sig_clkout2, MPUPinSignal (Just 3) Sig_clkout2),
              (Sig_gpio0_20, MPUPinSignal (Just 7) Sig_gpio0_20),
              (Sig_pr1_pru0_pru_r31_16,
               MPUPinSignal (Just 5) Sig_pr1_pru0_pru_r31_16),
              (Sig_tclkin, MPUPinSignal (Just 2) Sig_tclkin),
              (Sig_timer7, MPUPinSignal (Just 4) Sig_timer7),
              (Sig_xdma_event_intr1,
               MPUPinSignal (Just 0) Sig_xdma_event_intr1)])),
       (MPU_XTALIN,
        MPUPin MPU_XTALIN Nothing
          (Map.fromList [(Sig_OSC0_IN, MPUPinSignal (Just 0) Sig_OSC0_IN)])),
       (MPU_XTALOUT,
        MPUPin MPU_XTALOUT Nothing
          (Map.fromList
             [(Sig_OSC0_OUT, MPUPinSignal (Just 0) Sig_OSC0_OUT)]))]
 
signals :: Map.Map SignalId Signal
signals
  = Map.fromList
      [(Sig_AIN0, Signal Sig_AIN0 A Nothing Nothing),
       (Sig_AIN1, Signal Sig_AIN1 A Nothing Nothing),
       (Sig_AIN2, Signal Sig_AIN2 A Nothing Nothing),
       (Sig_AIN3, Signal Sig_AIN3 A Nothing Nothing),
       (Sig_AIN4, Signal Sig_AIN4 A Nothing Nothing),
       (Sig_AIN5, Signal Sig_AIN5 A Nothing Nothing),
       (Sig_AIN6, Signal Sig_AIN6 A Nothing Nothing),
       (Sig_AIN7, Signal Sig_AIN7 A Nothing Nothing),
       (Sig_CAP_VBB_MPU, Signal Sig_CAP_VBB_MPU A Nothing Nothing),
       (Sig_CAP_VDD_RTC, Signal Sig_CAP_VDD_RTC A Nothing Nothing),
       (Sig_CAP_VDD_SRAM_CORE,
        Signal Sig_CAP_VDD_SRAM_CORE A Nothing Nothing),
       (Sig_CAP_VDD_SRAM_MPU,
        Signal Sig_CAP_VDD_SRAM_MPU A Nothing Nothing),
       (Sig_EMU0, Signal Sig_EMU0 IO Nothing Nothing),
       (Sig_EMU1, Signal Sig_EMU1 IO Nothing Nothing),
       (Sig_EMU2, Signal Sig_EMU2 IO Nothing Nothing),
       (Sig_EMU3, Signal Sig_EMU3 IO Nothing Nothing),
       (Sig_EMU4, Signal Sig_EMU4 IO Nothing Nothing),
       (Sig_ENZ_KALDO_1P8V, Signal Sig_ENZ_KALDO_1P8V I Nothing Nothing),
       (Sig_EXT_WAKEUP, Signal Sig_EXT_WAKEUP I Nothing Nothing),
       (Sig_I2C0_SCL, Signal Sig_I2C0_SCL IOD Nothing Nothing),
       (Sig_I2C0_SDA, Signal Sig_I2C0_SDA IOD Nothing Nothing),
       (Sig_I2C1_SCL, Signal Sig_I2C1_SCL IOD Nothing Nothing),
       (Sig_I2C1_SDA, Signal Sig_I2C1_SDA IOD Nothing Nothing),
       (Sig_I2C2_SCL, Signal Sig_I2C2_SCL IOD Nothing Nothing),
       (Sig_I2C2_SDA, Signal Sig_I2C2_SDA IOD Nothing Nothing),
       (Sig_OSC0_IN, Signal Sig_OSC0_IN I Nothing Nothing),
       (Sig_OSC0_OUT, Signal Sig_OSC0_OUT O Nothing Nothing),
       (Sig_OSC1_IN, Signal Sig_OSC1_IN I Nothing Nothing),
       (Sig_OSC1_OUT, Signal Sig_OSC1_OUT O Nothing Nothing),
       (Sig_PMIC_POWER_EN, Signal Sig_PMIC_POWER_EN O Nothing Nothing),
       (Sig_RTC_porz, Signal Sig_RTC_porz I Nothing Nothing),
       (Sig_TCK, Signal Sig_TCK I Nothing Nothing),
       (Sig_TDI, Signal Sig_TDI I Nothing Nothing),
       (Sig_TDO, Signal Sig_TDO O Nothing Nothing),
       (Sig_TMS, Signal Sig_TMS I Nothing Nothing),
       (Sig_USB0_CE, Signal Sig_USB0_CE A Nothing Nothing),
       (Sig_USB0_DM, Signal Sig_USB0_DM A Nothing Nothing),
       (Sig_USB0_DP, Signal Sig_USB0_DP A Nothing Nothing),
       (Sig_USB0_DRVVBUS, Signal Sig_USB0_DRVVBUS O Nothing Nothing),
       (Sig_USB0_ID, Signal Sig_USB0_ID A Nothing Nothing),
       (Sig_USB0_VBUS, Signal Sig_USB0_VBUS A Nothing Nothing),
       (Sig_USB1_CE, Signal Sig_USB1_CE A Nothing Nothing),
       (Sig_USB1_DM, Signal Sig_USB1_DM A Nothing Nothing),
       (Sig_USB1_DP, Signal Sig_USB1_DP A Nothing Nothing),
       (Sig_USB1_DRVVBUS, Signal Sig_USB1_DRVVBUS O Nothing Nothing),
       (Sig_USB1_ID, Signal Sig_USB1_ID A Nothing Nothing),
       (Sig_USB1_VBUS, Signal Sig_USB1_VBUS A Nothing Nothing),
       (Sig_VDDA1P8V_USB0, Signal Sig_VDDA1P8V_USB0 PWR Nothing Nothing),
       (Sig_VDDA1P8V_USB1, Signal Sig_VDDA1P8V_USB1 PWR Nothing Nothing),
       (Sig_VDDA3P3V_USB0, Signal Sig_VDDA3P3V_USB0 PWR Nothing Nothing),
       (Sig_VDDA3P3V_USB1, Signal Sig_VDDA3P3V_USB1 PWR Nothing Nothing),
       (Sig_VDDA_ADC, Signal Sig_VDDA_ADC PWR Nothing Nothing),
       (Sig_VDDS, Signal Sig_VDDS PWR Nothing Nothing),
       (Sig_VDDSHV1, Signal Sig_VDDSHV1 PWR Nothing Nothing),
       (Sig_VDDSHV2, Signal Sig_VDDSHV2 PWR Nothing Nothing),
       (Sig_VDDSHV3, Signal Sig_VDDSHV3 PWR Nothing Nothing),
       (Sig_VDDSHV4, Signal Sig_VDDSHV4 PWR Nothing Nothing),
       (Sig_VDDSHV5, Signal Sig_VDDSHV5 PWR Nothing Nothing),
       (Sig_VDDSHV6, Signal Sig_VDDSHV6 PWR Nothing Nothing),
       (Sig_VDDS_DDR, Signal Sig_VDDS_DDR PWR Nothing Nothing),
       (Sig_VDDS_OSC, Signal Sig_VDDS_OSC PWR Nothing Nothing),
       (Sig_VDDS_PLL_CORE_LCD,
        Signal Sig_VDDS_PLL_CORE_LCD PWR Nothing Nothing),
       (Sig_VDDS_PLL_DDR, Signal Sig_VDDS_PLL_DDR PWR Nothing Nothing),
       (Sig_VDDS_PLL_MPU, Signal Sig_VDDS_PLL_MPU PWR Nothing Nothing),
       (Sig_VDDS_RTC, Signal Sig_VDDS_RTC PWR Nothing Nothing),
       (Sig_VDDS_SRAM_CORE_BG,
        Signal Sig_VDDS_SRAM_CORE_BG PWR Nothing Nothing),
       (Sig_VDDS_SRAM_MPU_BB,
        Signal Sig_VDDS_SRAM_MPU_BB PWR Nothing Nothing),
       (Sig_VDD_CORE, Signal Sig_VDD_CORE PWR Nothing Nothing),
       (Sig_VDD_MPU, Signal Sig_VDD_MPU PWR Nothing Nothing),
       (Sig_VDD_MPU_MON, Signal Sig_VDD_MPU_MON A Nothing Nothing),
       (Sig_VPP, Signal Sig_VPP PWR Nothing Nothing),
       (Sig_VREFN, Signal Sig_VREFN A Nothing Nothing),
       (Sig_VREFP, Signal Sig_VREFP A Nothing Nothing),
       (Sig_VSS, Signal Sig_VSS GND Nothing Nothing),
       (Sig_VSSA_ADC, Signal Sig_VSSA_ADC GND Nothing Nothing),
       (Sig_VSSA_USB, Signal Sig_VSSA_USB GND Nothing Nothing),
       (Sig_VSS_OSC, Signal Sig_VSS_OSC A Nothing Nothing),
       (Sig_VSS_RTC, Signal Sig_VSS_RTC A Nothing Nothing),
       (Sig_clkout1, Signal Sig_clkout1 O Nothing Nothing),
       (Sig_clkout2, Signal Sig_clkout2 O Nothing Nothing),
       (Sig_dcan0_rx, Signal Sig_dcan0_rx I Nothing Nothing),
       (Sig_dcan0_tx, Signal Sig_dcan0_tx O Nothing Nothing),
       (Sig_dcan1_rx, Signal Sig_dcan1_rx I Nothing Nothing),
       (Sig_dcan1_tx, Signal Sig_dcan1_tx O Nothing Nothing),
       (Sig_ddr_a0, Signal Sig_ddr_a0 O Nothing Nothing),
       (Sig_ddr_a1, Signal Sig_ddr_a1 O Nothing Nothing),
       (Sig_ddr_a2, Signal Sig_ddr_a2 O Nothing Nothing),
       (Sig_ddr_a3, Signal Sig_ddr_a3 O Nothing Nothing),
       (Sig_ddr_a4, Signal Sig_ddr_a4 O Nothing Nothing),
       (Sig_ddr_a5, Signal Sig_ddr_a5 O Nothing Nothing),
       (Sig_ddr_a6, Signal Sig_ddr_a6 O Nothing Nothing),
       (Sig_ddr_a7, Signal Sig_ddr_a7 O Nothing Nothing),
       (Sig_ddr_a8, Signal Sig_ddr_a8 O Nothing Nothing),
       (Sig_ddr_a9, Signal Sig_ddr_a9 O Nothing Nothing),
       (Sig_ddr_a10, Signal Sig_ddr_a10 O Nothing Nothing),
       (Sig_ddr_a11, Signal Sig_ddr_a11 O Nothing Nothing),
       (Sig_ddr_a12, Signal Sig_ddr_a12 O Nothing Nothing),
       (Sig_ddr_a13, Signal Sig_ddr_a13 O Nothing Nothing),
       (Sig_ddr_a14, Signal Sig_ddr_a14 O Nothing Nothing),
       (Sig_ddr_a15, Signal Sig_ddr_a15 O Nothing Nothing),
       (Sig_ddr_ba0, Signal Sig_ddr_ba0 O Nothing Nothing),
       (Sig_ddr_ba1, Signal Sig_ddr_ba1 O Nothing Nothing),
       (Sig_ddr_ba2, Signal Sig_ddr_ba2 O Nothing Nothing),
       (Sig_ddr_casn, Signal Sig_ddr_casn O Nothing Nothing),
       (Sig_ddr_ck, Signal Sig_ddr_ck O Nothing Nothing),
       (Sig_ddr_cke, Signal Sig_ddr_cke O Nothing Nothing),
       (Sig_ddr_csn0, Signal Sig_ddr_csn0 O Nothing Nothing),
       (Sig_ddr_d0, Signal Sig_ddr_d0 IO Nothing Nothing),
       (Sig_ddr_d1, Signal Sig_ddr_d1 IO Nothing Nothing),
       (Sig_ddr_d2, Signal Sig_ddr_d2 IO Nothing Nothing),
       (Sig_ddr_d3, Signal Sig_ddr_d3 IO Nothing Nothing),
       (Sig_ddr_d4, Signal Sig_ddr_d4 IO Nothing Nothing),
       (Sig_ddr_d5, Signal Sig_ddr_d5 IO Nothing Nothing),
       (Sig_ddr_d6, Signal Sig_ddr_d6 IO Nothing Nothing),
       (Sig_ddr_d7, Signal Sig_ddr_d7 IO Nothing Nothing),
       (Sig_ddr_d8, Signal Sig_ddr_d8 IO Nothing Nothing),
       (Sig_ddr_d9, Signal Sig_ddr_d9 IO Nothing Nothing),
       (Sig_ddr_d10, Signal Sig_ddr_d10 IO Nothing Nothing),
       (Sig_ddr_d11, Signal Sig_ddr_d11 IO Nothing Nothing),
       (Sig_ddr_d12, Signal Sig_ddr_d12 IO Nothing Nothing),
       (Sig_ddr_d13, Signal Sig_ddr_d13 IO Nothing Nothing),
       (Sig_ddr_d14, Signal Sig_ddr_d14 IO Nothing Nothing),
       (Sig_ddr_d15, Signal Sig_ddr_d15 IO Nothing Nothing),
       (Sig_ddr_dqm0, Signal Sig_ddr_dqm0 O Nothing Nothing),
       (Sig_ddr_dqm1, Signal Sig_ddr_dqm1 O Nothing Nothing),
       (Sig_ddr_dqs0, Signal Sig_ddr_dqs0 IO Nothing Nothing),
       (Sig_ddr_dqs1, Signal Sig_ddr_dqs1 IO Nothing Nothing),
       (Sig_ddr_dqsn0, Signal Sig_ddr_dqsn0 IO Nothing Nothing),
       (Sig_ddr_dqsn1, Signal Sig_ddr_dqsn1 IO Nothing Nothing),
       (Sig_ddr_nck, Signal Sig_ddr_nck O Nothing Nothing),
       (Sig_ddr_odt, Signal Sig_ddr_odt O Nothing Nothing),
       (Sig_ddr_rasn, Signal Sig_ddr_rasn O Nothing Nothing),
       (Sig_ddr_resetn, Signal Sig_ddr_resetn O Nothing Nothing),
       (Sig_ddr_vref, Signal Sig_ddr_vref A Nothing Nothing),
       (Sig_ddr_vtp, Signal Sig_ddr_vtp I Nothing Nothing),
       (Sig_ddr_wen, Signal Sig_ddr_wen O Nothing Nothing),
       (Sig_eCAP0_in_PWM0_out,
        Signal Sig_eCAP0_in_PWM0_out IO Nothing (Just "ecap.0")),
       (Sig_eCAP1_in_PWM1_out,
        Signal Sig_eCAP1_in_PWM1_out IO Nothing (Just "ecap.1")),
       (Sig_eCAP2_in_PWM2_out,
        Signal Sig_eCAP2_in_PWM2_out IO Nothing (Just "ecap.2")),
       (Sig_eQEP0A_in, Signal Sig_eQEP0A_in I Nothing Nothing),
       (Sig_eQEP0B_in, Signal Sig_eQEP0B_in I Nothing Nothing),
       (Sig_eQEP0_index, Signal Sig_eQEP0_index IO Nothing Nothing),
       (Sig_eQEP0_strobe, Signal Sig_eQEP0_strobe IO Nothing Nothing),
       (Sig_eQEP1A_in, Signal Sig_eQEP1A_in I Nothing Nothing),
       (Sig_eQEP1B_in, Signal Sig_eQEP1B_in I Nothing Nothing),
       (Sig_eQEP1_index, Signal Sig_eQEP1_index IO Nothing Nothing),
       (Sig_eQEP1_strobe, Signal Sig_eQEP1_strobe IO Nothing Nothing),
       (Sig_eQEP2A_in, Signal Sig_eQEP2A_in I Nothing Nothing),
       (Sig_eQEP2B_in, Signal Sig_eQEP2B_in I Nothing Nothing),
       (Sig_eQEP2_index, Signal Sig_eQEP2_index IO Nothing Nothing),
       (Sig_eQEP2_strobe, Signal Sig_eQEP2_strobe IO Nothing Nothing),
       (Sig_ehrpwm0A, Signal Sig_ehrpwm0A O Nothing (Just "ehrpwm.0:0")),
       (Sig_ehrpwm0B, Signal Sig_ehrpwm0B O Nothing (Just "ehrpwm.0:1")),
       (Sig_ehrpwm0_synci, Signal Sig_ehrpwm0_synci I Nothing Nothing),
       (Sig_ehrpwm0_synco, Signal Sig_ehrpwm0_synco O Nothing Nothing),
       (Sig_ehrpwm0_tripzone_input,
        Signal Sig_ehrpwm0_tripzone_input I Nothing Nothing),
       (Sig_ehrpwm1A, Signal Sig_ehrpwm1A O Nothing (Just "ehrpwm.1:0")),
       (Sig_ehrpwm1B, Signal Sig_ehrpwm1B O Nothing (Just "ehrpwm.1:1")),
       (Sig_ehrpwm1_tripzone_input,
        Signal Sig_ehrpwm1_tripzone_input I Nothing Nothing),
       (Sig_ehrpwm2A, Signal Sig_ehrpwm2A O Nothing (Just "ehrpwm.2:0")),
       (Sig_ehrpwm2B, Signal Sig_ehrpwm2B O Nothing (Just "ehrpwm.2:1")),
       (Sig_ehrpwm2_tripzone_input,
        Signal Sig_ehrpwm2_tripzone_input I Nothing Nothing),
       (Sig_gmii1_col, Signal Sig_gmii1_col I Nothing Nothing),
       (Sig_gmii1_crs, Signal Sig_gmii1_crs I Nothing Nothing),
       (Sig_gmii1_rxclk, Signal Sig_gmii1_rxclk I Nothing Nothing),
       (Sig_gmii1_rxd0, Signal Sig_gmii1_rxd0 I Nothing Nothing),
       (Sig_gmii1_rxd1, Signal Sig_gmii1_rxd1 I Nothing Nothing),
       (Sig_gmii1_rxd2, Signal Sig_gmii1_rxd2 I Nothing Nothing),
       (Sig_gmii1_rxd3, Signal Sig_gmii1_rxd3 I Nothing Nothing),
       (Sig_gmii1_rxdv, Signal Sig_gmii1_rxdv I Nothing Nothing),
       (Sig_gmii1_rxerr, Signal Sig_gmii1_rxerr I Nothing Nothing),
       (Sig_gmii1_txclk, Signal Sig_gmii1_txclk I Nothing Nothing),
       (Sig_gmii1_txd0, Signal Sig_gmii1_txd0 O Nothing Nothing),
       (Sig_gmii1_txd1, Signal Sig_gmii1_txd1 O Nothing Nothing),
       (Sig_gmii1_txd2, Signal Sig_gmii1_txd2 O Nothing Nothing),
       (Sig_gmii1_txd3, Signal Sig_gmii1_txd3 O Nothing Nothing),
       (Sig_gmii1_txen, Signal Sig_gmii1_txen O Nothing Nothing),
       (Sig_gmii2_col, Signal Sig_gmii2_col I Nothing Nothing),
       (Sig_gmii2_crs, Signal Sig_gmii2_crs I Nothing Nothing),
       (Sig_gmii2_rxclk, Signal Sig_gmii2_rxclk I Nothing Nothing),
       (Sig_gmii2_rxd0, Signal Sig_gmii2_rxd0 I Nothing Nothing),
       (Sig_gmii2_rxd1, Signal Sig_gmii2_rxd1 I Nothing Nothing),
       (Sig_gmii2_rxd2, Signal Sig_gmii2_rxd2 I Nothing Nothing),
       (Sig_gmii2_rxd3, Signal Sig_gmii2_rxd3 I Nothing Nothing),
       (Sig_gmii2_rxdv, Signal Sig_gmii2_rxdv I Nothing Nothing),
       (Sig_gmii2_rxerr, Signal Sig_gmii2_rxerr I Nothing Nothing),
       (Sig_gmii2_txclk, Signal Sig_gmii2_txclk I Nothing Nothing),
       (Sig_gmii2_txd0, Signal Sig_gmii2_txd0 O Nothing Nothing),
       (Sig_gmii2_txd1, Signal Sig_gmii2_txd1 O Nothing Nothing),
       (Sig_gmii2_txd2, Signal Sig_gmii2_txd2 O Nothing Nothing),
       (Sig_gmii2_txd3, Signal Sig_gmii2_txd3 O Nothing Nothing),
       (Sig_gmii2_txen, Signal Sig_gmii2_txen O Nothing Nothing),
       (Sig_gpio0_0, Signal Sig_gpio0_0 IO (Just 0) Nothing),
       (Sig_gpio0_1, Signal Sig_gpio0_1 IO (Just 1) Nothing),
       (Sig_gpio0_2, Signal Sig_gpio0_2 IO (Just 2) Nothing),
       (Sig_gpio0_3, Signal Sig_gpio0_3 IO (Just 3) Nothing),
       (Sig_gpio0_4, Signal Sig_gpio0_4 IO (Just 4) Nothing),
       (Sig_gpio0_5, Signal Sig_gpio0_5 IO (Just 5) Nothing),
       (Sig_gpio0_6, Signal Sig_gpio0_6 IO (Just 6) Nothing),
       (Sig_gpio0_7, Signal Sig_gpio0_7 IO (Just 7) Nothing),
       (Sig_gpio0_8, Signal Sig_gpio0_8 IO (Just 8) Nothing),
       (Sig_gpio0_9, Signal Sig_gpio0_9 IO (Just 9) Nothing),
       (Sig_gpio0_10, Signal Sig_gpio0_10 IO (Just 10) Nothing),
       (Sig_gpio0_11, Signal Sig_gpio0_11 IO (Just 11) Nothing),
       (Sig_gpio0_12, Signal Sig_gpio0_12 IO (Just 12) Nothing),
       (Sig_gpio0_13, Signal Sig_gpio0_13 IO (Just 13) Nothing),
       (Sig_gpio0_14, Signal Sig_gpio0_14 IO (Just 14) Nothing),
       (Sig_gpio0_15, Signal Sig_gpio0_15 IO (Just 15) Nothing),
       (Sig_gpio0_16, Signal Sig_gpio0_16 IO (Just 16) Nothing),
       (Sig_gpio0_17, Signal Sig_gpio0_17 IO (Just 17) Nothing),
       (Sig_gpio0_18, Signal Sig_gpio0_18 IO (Just 18) Nothing),
       (Sig_gpio0_19, Signal Sig_gpio0_19 IO (Just 19) Nothing),
       (Sig_gpio0_20, Signal Sig_gpio0_20 IO (Just 20) Nothing),
       (Sig_gpio0_21, Signal Sig_gpio0_21 IO (Just 21) Nothing),
       (Sig_gpio0_22, Signal Sig_gpio0_22 IO (Just 22) Nothing),
       (Sig_gpio0_23, Signal Sig_gpio0_23 IO (Just 23) Nothing),
       (Sig_gpio0_26, Signal Sig_gpio0_26 IO (Just 26) Nothing),
       (Sig_gpio0_27, Signal Sig_gpio0_27 IO (Just 27) Nothing),
       (Sig_gpio0_28, Signal Sig_gpio0_28 IO (Just 28) Nothing),
       (Sig_gpio0_29, Signal Sig_gpio0_29 IO (Just 29) Nothing),
       (Sig_gpio0_30, Signal Sig_gpio0_30 IO (Just 30) Nothing),
       (Sig_gpio0_31, Signal Sig_gpio0_31 IO (Just 31) Nothing),
       (Sig_gpio1_0, Signal Sig_gpio1_0 IO (Just 32) Nothing),
       (Sig_gpio1_1, Signal Sig_gpio1_1 IO (Just 33) Nothing),
       (Sig_gpio1_2, Signal Sig_gpio1_2 IO (Just 34) Nothing),
       (Sig_gpio1_3, Signal Sig_gpio1_3 IO (Just 35) Nothing),
       (Sig_gpio1_4, Signal Sig_gpio1_4 IO (Just 36) Nothing),
       (Sig_gpio1_5, Signal Sig_gpio1_5 IO (Just 37) Nothing),
       (Sig_gpio1_6, Signal Sig_gpio1_6 IO (Just 38) Nothing),
       (Sig_gpio1_7, Signal Sig_gpio1_7 IO (Just 39) Nothing),
       (Sig_gpio1_8, Signal Sig_gpio1_8 IO (Just 40) Nothing),
       (Sig_gpio1_9, Signal Sig_gpio1_9 IO (Just 41) Nothing),
       (Sig_gpio1_10, Signal Sig_gpio1_10 IO (Just 42) Nothing),
       (Sig_gpio1_11, Signal Sig_gpio1_11 IO (Just 43) Nothing),
       (Sig_gpio1_12, Signal Sig_gpio1_12 IO (Just 44) Nothing),
       (Sig_gpio1_13, Signal Sig_gpio1_13 IO (Just 45) Nothing),
       (Sig_gpio1_14, Signal Sig_gpio1_14 IO (Just 46) Nothing),
       (Sig_gpio1_15, Signal Sig_gpio1_15 IO (Just 47) Nothing),
       (Sig_gpio1_16, Signal Sig_gpio1_16 IO (Just 48) Nothing),
       (Sig_gpio1_17, Signal Sig_gpio1_17 IO (Just 49) Nothing),
       (Sig_gpio1_18, Signal Sig_gpio1_18 IO (Just 50) Nothing),
       (Sig_gpio1_19, Signal Sig_gpio1_19 IO (Just 51) Nothing),
       (Sig_gpio1_20, Signal Sig_gpio1_20 IO (Just 52) Nothing),
       (Sig_gpio1_21, Signal Sig_gpio1_21 IO (Just 53) Nothing),
       (Sig_gpio1_22, Signal Sig_gpio1_22 IO (Just 54) Nothing),
       (Sig_gpio1_23, Signal Sig_gpio1_23 IO (Just 55) Nothing),
       (Sig_gpio1_24, Signal Sig_gpio1_24 IO (Just 56) Nothing),
       (Sig_gpio1_25, Signal Sig_gpio1_25 IO (Just 57) Nothing),
       (Sig_gpio1_26, Signal Sig_gpio1_26 IO (Just 58) Nothing),
       (Sig_gpio1_27, Signal Sig_gpio1_27 IO (Just 59) Nothing),
       (Sig_gpio1_28, Signal Sig_gpio1_28 IO (Just 60) Nothing),
       (Sig_gpio1_29, Signal Sig_gpio1_29 IO (Just 61) Nothing),
       (Sig_gpio1_30, Signal Sig_gpio1_30 IO (Just 62) Nothing),
       (Sig_gpio1_31, Signal Sig_gpio1_31 IO (Just 63) Nothing),
       (Sig_gpio2_0, Signal Sig_gpio2_0 IO (Just 64) Nothing),
       (Sig_gpio2_1, Signal Sig_gpio2_1 IO (Just 65) Nothing),
       (Sig_gpio2_2, Signal Sig_gpio2_2 IO (Just 66) Nothing),
       (Sig_gpio2_3, Signal Sig_gpio2_3 IO (Just 67) Nothing),
       (Sig_gpio2_4, Signal Sig_gpio2_4 IO (Just 68) Nothing),
       (Sig_gpio2_5, Signal Sig_gpio2_5 IO (Just 69) Nothing),
       (Sig_gpio2_6, Signal Sig_gpio2_6 IO (Just 70) Nothing),
       (Sig_gpio2_7, Signal Sig_gpio2_7 IO (Just 71) Nothing),
       (Sig_gpio2_8, Signal Sig_gpio2_8 IO (Just 72) Nothing),
       (Sig_gpio2_9, Signal Sig_gpio2_9 IO (Just 73) Nothing),
       (Sig_gpio2_10, Signal Sig_gpio2_10 IO (Just 74) Nothing),
       (Sig_gpio2_11, Signal Sig_gpio2_11 IO (Just 75) Nothing),
       (Sig_gpio2_12, Signal Sig_gpio2_12 IO (Just 76) Nothing),
       (Sig_gpio2_13, Signal Sig_gpio2_13 IO (Just 77) Nothing),
       (Sig_gpio2_14, Signal Sig_gpio2_14 IO (Just 78) Nothing),
       (Sig_gpio2_15, Signal Sig_gpio2_15 IO (Just 79) Nothing),
       (Sig_gpio2_16, Signal Sig_gpio2_16 IO (Just 80) Nothing),
       (Sig_gpio2_17, Signal Sig_gpio2_17 IO (Just 81) Nothing),
       (Sig_gpio2_18, Signal Sig_gpio2_18 IO (Just 82) Nothing),
       (Sig_gpio2_19, Signal Sig_gpio2_19 IO (Just 83) Nothing),
       (Sig_gpio2_20, Signal Sig_gpio2_20 IO (Just 84) Nothing),
       (Sig_gpio2_21, Signal Sig_gpio2_21 IO (Just 85) Nothing),
       (Sig_gpio2_22, Signal Sig_gpio2_22 IO (Just 86) Nothing),
       (Sig_gpio2_23, Signal Sig_gpio2_23 IO (Just 87) Nothing),
       (Sig_gpio2_24, Signal Sig_gpio2_24 IO (Just 88) Nothing),
       (Sig_gpio2_25, Signal Sig_gpio2_25 IO (Just 89) Nothing),
       (Sig_gpio2_26, Signal Sig_gpio2_26 IO (Just 90) Nothing),
       (Sig_gpio2_27, Signal Sig_gpio2_27 IO (Just 91) Nothing),
       (Sig_gpio2_28, Signal Sig_gpio2_28 IO (Just 92) Nothing),
       (Sig_gpio2_29, Signal Sig_gpio2_29 IO (Just 93) Nothing),
       (Sig_gpio2_30, Signal Sig_gpio2_30 IO (Just 94) Nothing),
       (Sig_gpio2_31, Signal Sig_gpio2_31 IO (Just 95) Nothing),
       (Sig_gpio3_0, Signal Sig_gpio3_0 IO (Just 96) Nothing),
       (Sig_gpio3_1, Signal Sig_gpio3_1 IO (Just 97) Nothing),
       (Sig_gpio3_2, Signal Sig_gpio3_2 IO (Just 98) Nothing),
       (Sig_gpio3_3, Signal Sig_gpio3_3 IO (Just 99) Nothing),
       (Sig_gpio3_4, Signal Sig_gpio3_4 IO (Just 100) Nothing),
       (Sig_gpio3_5, Signal Sig_gpio3_5 IO (Just 101) Nothing),
       (Sig_gpio3_6, Signal Sig_gpio3_6 IO (Just 102) Nothing),
       (Sig_gpio3_7, Signal Sig_gpio3_7 IO (Just 103) Nothing),
       (Sig_gpio3_8, Signal Sig_gpio3_8 IO (Just 104) Nothing),
       (Sig_gpio3_9, Signal Sig_gpio3_9 IO (Just 105) Nothing),
       (Sig_gpio3_10, Signal Sig_gpio3_10 IO (Just 106) Nothing),
       (Sig_gpio3_13, Signal Sig_gpio3_13 IO (Just 109) Nothing),
       (Sig_gpio3_14, Signal Sig_gpio3_14 IO (Just 110) Nothing),
       (Sig_gpio3_15, Signal Sig_gpio3_15 IO (Just 111) Nothing),
       (Sig_gpio3_16, Signal Sig_gpio3_16 IO (Just 112) Nothing),
       (Sig_gpio3_17, Signal Sig_gpio3_17 IO (Just 113) Nothing),
       (Sig_gpio3_18, Signal Sig_gpio3_18 IO (Just 114) Nothing),
       (Sig_gpio3_19, Signal Sig_gpio3_19 IO (Just 115) Nothing),
       (Sig_gpio3_20, Signal Sig_gpio3_20 IO (Just 116) Nothing),
       (Sig_gpio3_21, Signal Sig_gpio3_21 IO (Just 117) Nothing),
       (Sig_gpmc_a0, Signal Sig_gpmc_a0 O Nothing Nothing),
       (Sig_gpmc_a1, Signal Sig_gpmc_a1 O Nothing Nothing),
       (Sig_gpmc_a2, Signal Sig_gpmc_a2 O Nothing Nothing),
       (Sig_gpmc_a3, Signal Sig_gpmc_a3 O Nothing Nothing),
       (Sig_gpmc_a4, Signal Sig_gpmc_a4 O Nothing Nothing),
       (Sig_gpmc_a5, Signal Sig_gpmc_a5 O Nothing Nothing),
       (Sig_gpmc_a6, Signal Sig_gpmc_a6 O Nothing Nothing),
       (Sig_gpmc_a7, Signal Sig_gpmc_a7 O Nothing Nothing),
       (Sig_gpmc_a8, Signal Sig_gpmc_a8 O Nothing Nothing),
       (Sig_gpmc_a9, Signal Sig_gpmc_a9 O Nothing Nothing),
       (Sig_gpmc_a10, Signal Sig_gpmc_a10 O Nothing Nothing),
       (Sig_gpmc_a11, Signal Sig_gpmc_a11 O Nothing Nothing),
       (Sig_gpmc_a12, Signal Sig_gpmc_a12 O Nothing Nothing),
       (Sig_gpmc_a13, Signal Sig_gpmc_a13 O Nothing Nothing),
       (Sig_gpmc_a14, Signal Sig_gpmc_a14 O Nothing Nothing),
       (Sig_gpmc_a15, Signal Sig_gpmc_a15 O Nothing Nothing),
       (Sig_gpmc_a16, Signal Sig_gpmc_a16 O Nothing Nothing),
       (Sig_gpmc_a17, Signal Sig_gpmc_a17 O Nothing Nothing),
       (Sig_gpmc_a18, Signal Sig_gpmc_a18 O Nothing Nothing),
       (Sig_gpmc_a19, Signal Sig_gpmc_a19 O Nothing Nothing),
       (Sig_gpmc_a20, Signal Sig_gpmc_a20 O Nothing Nothing),
       (Sig_gpmc_a21, Signal Sig_gpmc_a21 O Nothing Nothing),
       (Sig_gpmc_a22, Signal Sig_gpmc_a22 O Nothing Nothing),
       (Sig_gpmc_a23, Signal Sig_gpmc_a23 O Nothing Nothing),
       (Sig_gpmc_a24, Signal Sig_gpmc_a24 O Nothing Nothing),
       (Sig_gpmc_a25, Signal Sig_gpmc_a25 O Nothing Nothing),
       (Sig_gpmc_a26, Signal Sig_gpmc_a26 O Nothing Nothing),
       (Sig_gpmc_a27, Signal Sig_gpmc_a27 O Nothing Nothing),
       (Sig_gpmc_ad0, Signal Sig_gpmc_ad0 IO Nothing Nothing),
       (Sig_gpmc_ad1, Signal Sig_gpmc_ad1 IO Nothing Nothing),
       (Sig_gpmc_ad2, Signal Sig_gpmc_ad2 IO Nothing Nothing),
       (Sig_gpmc_ad3, Signal Sig_gpmc_ad3 IO Nothing Nothing),
       (Sig_gpmc_ad4, Signal Sig_gpmc_ad4 IO Nothing Nothing),
       (Sig_gpmc_ad5, Signal Sig_gpmc_ad5 IO Nothing Nothing),
       (Sig_gpmc_ad6, Signal Sig_gpmc_ad6 IO Nothing Nothing),
       (Sig_gpmc_ad7, Signal Sig_gpmc_ad7 IO Nothing Nothing),
       (Sig_gpmc_ad8, Signal Sig_gpmc_ad8 IO Nothing Nothing),
       (Sig_gpmc_ad9, Signal Sig_gpmc_ad9 IO Nothing Nothing),
       (Sig_gpmc_ad10, Signal Sig_gpmc_ad10 IO Nothing Nothing),
       (Sig_gpmc_ad11, Signal Sig_gpmc_ad11 IO Nothing Nothing),
       (Sig_gpmc_ad12, Signal Sig_gpmc_ad12 IO Nothing Nothing),
       (Sig_gpmc_ad13, Signal Sig_gpmc_ad13 IO Nothing Nothing),
       (Sig_gpmc_ad14, Signal Sig_gpmc_ad14 IO Nothing Nothing),
       (Sig_gpmc_ad15, Signal Sig_gpmc_ad15 IO Nothing Nothing),
       (Sig_gpmc_advn_ale, Signal Sig_gpmc_advn_ale O Nothing Nothing),
       (Sig_gpmc_be0n_cle, Signal Sig_gpmc_be0n_cle O Nothing Nothing),
       (Sig_gpmc_be1n, Signal Sig_gpmc_be1n O Nothing Nothing),
       (Sig_gpmc_clk, Signal Sig_gpmc_clk IO Nothing Nothing),
       (Sig_gpmc_csn0, Signal Sig_gpmc_csn0 O Nothing Nothing),
       (Sig_gpmc_csn1, Signal Sig_gpmc_csn1 O Nothing Nothing),
       (Sig_gpmc_csn2, Signal Sig_gpmc_csn2 O Nothing Nothing),
       (Sig_gpmc_csn3, Signal Sig_gpmc_csn3 O Nothing Nothing),
       (Sig_gpmc_csn4, Signal Sig_gpmc_csn4 O Nothing Nothing),
       (Sig_gpmc_csn5, Signal Sig_gpmc_csn5 O Nothing Nothing),
       (Sig_gpmc_csn6, Signal Sig_gpmc_csn6 O Nothing Nothing),
       (Sig_gpmc_dir, Signal Sig_gpmc_dir O Nothing Nothing),
       (Sig_gpmc_oen_ren, Signal Sig_gpmc_oen_ren O Nothing Nothing),
       (Sig_gpmc_wait0, Signal Sig_gpmc_wait0 I Nothing Nothing),
       (Sig_gpmc_wait1, Signal Sig_gpmc_wait1 I Nothing Nothing),
       (Sig_gpmc_wen, Signal Sig_gpmc_wen O Nothing Nothing),
       (Sig_gpmc_wpn, Signal Sig_gpmc_wpn O Nothing Nothing),
       (Sig_lcd_ac_bias_en, Signal Sig_lcd_ac_bias_en O Nothing Nothing),
       (Sig_lcd_data0, Signal Sig_lcd_data0 IO Nothing Nothing),
       (Sig_lcd_data1, Signal Sig_lcd_data1 IO Nothing Nothing),
       (Sig_lcd_data2, Signal Sig_lcd_data2 IO Nothing Nothing),
       (Sig_lcd_data3, Signal Sig_lcd_data3 IO Nothing Nothing),
       (Sig_lcd_data4, Signal Sig_lcd_data4 IO Nothing Nothing),
       (Sig_lcd_data5, Signal Sig_lcd_data5 IO Nothing Nothing),
       (Sig_lcd_data6, Signal Sig_lcd_data6 IO Nothing Nothing),
       (Sig_lcd_data7, Signal Sig_lcd_data7 IO Nothing Nothing),
       (Sig_lcd_data8, Signal Sig_lcd_data8 IO Nothing Nothing),
       (Sig_lcd_data9, Signal Sig_lcd_data9 IO Nothing Nothing),
       (Sig_lcd_data10, Signal Sig_lcd_data10 IO Nothing Nothing),
       (Sig_lcd_data11, Signal Sig_lcd_data11 IO Nothing Nothing),
       (Sig_lcd_data12, Signal Sig_lcd_data12 IO Nothing Nothing),
       (Sig_lcd_data13, Signal Sig_lcd_data13 IO Nothing Nothing),
       (Sig_lcd_data14, Signal Sig_lcd_data14 IO Nothing Nothing),
       (Sig_lcd_data15, Signal Sig_lcd_data15 IO Nothing Nothing),
       (Sig_lcd_data16, Signal Sig_lcd_data16 O Nothing Nothing),
       (Sig_lcd_data17, Signal Sig_lcd_data17 O Nothing Nothing),
       (Sig_lcd_data18, Signal Sig_lcd_data18 O Nothing Nothing),
       (Sig_lcd_data19, Signal Sig_lcd_data19 O Nothing Nothing),
       (Sig_lcd_data20, Signal Sig_lcd_data20 O Nothing Nothing),
       (Sig_lcd_data21, Signal Sig_lcd_data21 O Nothing Nothing),
       (Sig_lcd_data22, Signal Sig_lcd_data22 O Nothing Nothing),
       (Sig_lcd_data23, Signal Sig_lcd_data23 O Nothing Nothing),
       (Sig_lcd_hsync, Signal Sig_lcd_hsync O Nothing Nothing),
       (Sig_lcd_memory_clk, Signal Sig_lcd_memory_clk O Nothing Nothing),
       (Sig_lcd_pclk, Signal Sig_lcd_pclk O Nothing Nothing),
       (Sig_lcd_vsync, Signal Sig_lcd_vsync O Nothing Nothing),
       (Sig_mcasp0_aclkr, Signal Sig_mcasp0_aclkr IO Nothing Nothing),
       (Sig_mcasp0_aclkx, Signal Sig_mcasp0_aclkx IO Nothing Nothing),
       (Sig_mcasp0_ahclkr, Signal Sig_mcasp0_ahclkr IO Nothing Nothing),
       (Sig_mcasp0_ahclkx, Signal Sig_mcasp0_ahclkx IO Nothing Nothing),
       (Sig_mcasp0_axr0, Signal Sig_mcasp0_axr0 IO Nothing Nothing),
       (Sig_mcasp0_axr1, Signal Sig_mcasp0_axr1 IO Nothing Nothing),
       (Sig_mcasp0_axr2, Signal Sig_mcasp0_axr2 IO Nothing Nothing),
       (Sig_mcasp0_axr3, Signal Sig_mcasp0_axr3 IO Nothing Nothing),
       (Sig_mcasp0_fsr, Signal Sig_mcasp0_fsr IO Nothing Nothing),
       (Sig_mcasp0_fsx, Signal Sig_mcasp0_fsx IO Nothing Nothing),
       (Sig_mcasp1_aclkr, Signal Sig_mcasp1_aclkr IO Nothing Nothing),
       (Sig_mcasp1_aclkx, Signal Sig_mcasp1_aclkx IO Nothing Nothing),
       (Sig_mcasp1_ahclkr, Signal Sig_mcasp1_ahclkr IO Nothing Nothing),
       (Sig_mcasp1_ahclkx, Signal Sig_mcasp1_ahclkx IO Nothing Nothing),
       (Sig_mcasp1_axr0, Signal Sig_mcasp1_axr0 IO Nothing Nothing),
       (Sig_mcasp1_axr1, Signal Sig_mcasp1_axr1 IO Nothing Nothing),
       (Sig_mcasp1_axr2, Signal Sig_mcasp1_axr2 IO Nothing Nothing),
       (Sig_mcasp1_axr3, Signal Sig_mcasp1_axr3 IO Nothing Nothing),
       (Sig_mcasp1_fsr, Signal Sig_mcasp1_fsr IO Nothing Nothing),
       (Sig_mcasp1_fsx, Signal Sig_mcasp1_fsx IO Nothing Nothing),
       (Sig_mdio_clk, Signal Sig_mdio_clk O Nothing Nothing),
       (Sig_mdio_data, Signal Sig_mdio_data IO Nothing Nothing),
       (Sig_mmc0_clk, Signal Sig_mmc0_clk IO Nothing Nothing),
       (Sig_mmc0_cmd, Signal Sig_mmc0_cmd IO Nothing Nothing),
       (Sig_mmc0_dat0, Signal Sig_mmc0_dat0 IO Nothing Nothing),
       (Sig_mmc0_dat1, Signal Sig_mmc0_dat1 IO Nothing Nothing),
       (Sig_mmc0_dat2, Signal Sig_mmc0_dat2 IO Nothing Nothing),
       (Sig_mmc0_dat3, Signal Sig_mmc0_dat3 IO Nothing Nothing),
       (Sig_mmc0_dat4, Signal Sig_mmc0_dat4 IO Nothing Nothing),
       (Sig_mmc0_dat5, Signal Sig_mmc0_dat5 IO Nothing Nothing),
       (Sig_mmc0_dat6, Signal Sig_mmc0_dat6 IO Nothing Nothing),
       (Sig_mmc0_dat7, Signal Sig_mmc0_dat7 IO Nothing Nothing),
       (Sig_mmc0_pow, Signal Sig_mmc0_pow O Nothing Nothing),
       (Sig_mmc0_sdcd, Signal Sig_mmc0_sdcd I Nothing Nothing),
       (Sig_mmc0_sdwp, Signal Sig_mmc0_sdwp I Nothing Nothing),
       (Sig_mmc1_clk, Signal Sig_mmc1_clk IO Nothing Nothing),
       (Sig_mmc1_cmd, Signal Sig_mmc1_cmd IO Nothing Nothing),
       (Sig_mmc1_dat0, Signal Sig_mmc1_dat0 IO Nothing Nothing),
       (Sig_mmc1_dat1, Signal Sig_mmc1_dat1 IO Nothing Nothing),
       (Sig_mmc1_dat2, Signal Sig_mmc1_dat2 IO Nothing Nothing),
       (Sig_mmc1_dat3, Signal Sig_mmc1_dat3 IO Nothing Nothing),
       (Sig_mmc1_dat4, Signal Sig_mmc1_dat4 IO Nothing Nothing),
       (Sig_mmc1_dat5, Signal Sig_mmc1_dat5 IO Nothing Nothing),
       (Sig_mmc1_dat6, Signal Sig_mmc1_dat6 IO Nothing Nothing),
       (Sig_mmc1_dat7, Signal Sig_mmc1_dat7 IO Nothing Nothing),
       (Sig_mmc1_sdcd, Signal Sig_mmc1_sdcd I Nothing Nothing),
       (Sig_mmc1_sdwp, Signal Sig_mmc1_sdwp I Nothing Nothing),
       (Sig_mmc2_clk, Signal Sig_mmc2_clk IO Nothing Nothing),
       (Sig_mmc2_cmd, Signal Sig_mmc2_cmd IO Nothing Nothing),
       (Sig_mmc2_dat0, Signal Sig_mmc2_dat0 IO Nothing Nothing),
       (Sig_mmc2_dat1, Signal Sig_mmc2_dat1 IO Nothing Nothing),
       (Sig_mmc2_dat2, Signal Sig_mmc2_dat2 IO Nothing Nothing),
       (Sig_mmc2_dat3, Signal Sig_mmc2_dat3 IO Nothing Nothing),
       (Sig_mmc2_dat4, Signal Sig_mmc2_dat4 IO Nothing Nothing),
       (Sig_mmc2_dat5, Signal Sig_mmc2_dat5 IO Nothing Nothing),
       (Sig_mmc2_dat6, Signal Sig_mmc2_dat6 IO Nothing Nothing),
       (Sig_mmc2_dat7, Signal Sig_mmc2_dat7 IO Nothing Nothing),
       (Sig_mmc2_sdcd, Signal Sig_mmc2_sdcd I Nothing Nothing),
       (Sig_mmc2_sdwp, Signal Sig_mmc2_sdwp I Nothing Nothing),
       (Sig_nNMI, Signal Sig_nNMI I Nothing Nothing),
       (Sig_nRESETIN_OUT, Signal Sig_nRESETIN_OUT IOD Nothing Nothing),
       (Sig_nTRST, Signal Sig_nTRST I Nothing Nothing),
       (Sig_porz, Signal Sig_porz I Nothing Nothing),
       (Sig_pr1_ecap0_ecap_capin_apwm_o,
        Signal Sig_pr1_ecap0_ecap_capin_apwm_o IO Nothing Nothing),
       (Sig_pr1_edc_latch0_in,
        Signal Sig_pr1_edc_latch0_in I Nothing Nothing),
       (Sig_pr1_edc_latch1_in,
        Signal Sig_pr1_edc_latch1_in I Nothing Nothing),
       (Sig_pr1_edc_sync0_out,
        Signal Sig_pr1_edc_sync0_out O Nothing Nothing),
       (Sig_pr1_edc_sync1_out,
        Signal Sig_pr1_edc_sync1_out O Nothing Nothing),
       (Sig_pr1_edio_data_in0,
        Signal Sig_pr1_edio_data_in0 I Nothing Nothing),
       (Sig_pr1_edio_data_in1,
        Signal Sig_pr1_edio_data_in1 I Nothing Nothing),
       (Sig_pr1_edio_data_in2,
        Signal Sig_pr1_edio_data_in2 I Nothing Nothing),
       (Sig_pr1_edio_data_in3,
        Signal Sig_pr1_edio_data_in3 I Nothing Nothing),
       (Sig_pr1_edio_data_in4,
        Signal Sig_pr1_edio_data_in4 I Nothing Nothing),
       (Sig_pr1_edio_data_in5,
        Signal Sig_pr1_edio_data_in5 I Nothing Nothing),
       (Sig_pr1_edio_data_in6,
        Signal Sig_pr1_edio_data_in6 I Nothing Nothing),
       (Sig_pr1_edio_data_in7,
        Signal Sig_pr1_edio_data_in7 I Nothing Nothing),
       (Sig_pr1_edio_data_out0,
        Signal Sig_pr1_edio_data_out0 O Nothing Nothing),
       (Sig_pr1_edio_data_out1,
        Signal Sig_pr1_edio_data_out1 O Nothing Nothing),
       (Sig_pr1_edio_data_out2,
        Signal Sig_pr1_edio_data_out2 O Nothing Nothing),
       (Sig_pr1_edio_data_out3,
        Signal Sig_pr1_edio_data_out3 O Nothing Nothing),
       (Sig_pr1_edio_data_out4,
        Signal Sig_pr1_edio_data_out4 O Nothing Nothing),
       (Sig_pr1_edio_data_out5,
        Signal Sig_pr1_edio_data_out5 O Nothing Nothing),
       (Sig_pr1_edio_data_out6,
        Signal Sig_pr1_edio_data_out6 O Nothing Nothing),
       (Sig_pr1_edio_data_out7,
        Signal Sig_pr1_edio_data_out7 O Nothing Nothing),
       (Sig_pr1_edio_latch_in,
        Signal Sig_pr1_edio_latch_in I Nothing Nothing),
       (Sig_pr1_edio_sof, Signal Sig_pr1_edio_sof O Nothing Nothing),
       (Sig_pr1_mdio_data, Signal Sig_pr1_mdio_data IO Nothing Nothing),
       (Sig_pr1_mdio_mdclk, Signal Sig_pr1_mdio_mdclk O Nothing Nothing),
       (Sig_pr1_mii0_col, Signal Sig_pr1_mii0_col I Nothing Nothing),
       (Sig_pr1_mii0_crs, Signal Sig_pr1_mii0_crs I Nothing Nothing),
       (Sig_pr1_mii0_rxd0, Signal Sig_pr1_mii0_rxd0 I Nothing Nothing),
       (Sig_pr1_mii0_rxd1, Signal Sig_pr1_mii0_rxd1 I Nothing Nothing),
       (Sig_pr1_mii0_rxd2, Signal Sig_pr1_mii0_rxd2 I Nothing Nothing),
       (Sig_pr1_mii0_rxd3, Signal Sig_pr1_mii0_rxd3 I Nothing Nothing),
       (Sig_pr1_mii0_rxdv, Signal Sig_pr1_mii0_rxdv I Nothing Nothing),
       (Sig_pr1_mii0_rxer, Signal Sig_pr1_mii0_rxer I Nothing Nothing),
       (Sig_pr1_mii0_rxlink,
        Signal Sig_pr1_mii0_rxlink I Nothing Nothing),
       (Sig_pr1_mii0_txd0, Signal Sig_pr1_mii0_txd0 O Nothing Nothing),
       (Sig_pr1_mii0_txd1, Signal Sig_pr1_mii0_txd1 O Nothing Nothing),
       (Sig_pr1_mii0_txd2, Signal Sig_pr1_mii0_txd2 O Nothing Nothing),
       (Sig_pr1_mii0_txd3, Signal Sig_pr1_mii0_txd3 O Nothing Nothing),
       (Sig_pr1_mii0_txen, Signal Sig_pr1_mii0_txen O Nothing Nothing),
       (Sig_pr1_mii1_col, Signal Sig_pr1_mii1_col I Nothing Nothing),
       (Sig_pr1_mii1_crs, Signal Sig_pr1_mii1_crs I Nothing Nothing),
       (Sig_pr1_mii1_rxd0, Signal Sig_pr1_mii1_rxd0 I Nothing Nothing),
       (Sig_pr1_mii1_rxd1, Signal Sig_pr1_mii1_rxd1 I Nothing Nothing),
       (Sig_pr1_mii1_rxd2, Signal Sig_pr1_mii1_rxd2 I Nothing Nothing),
       (Sig_pr1_mii1_rxd3, Signal Sig_pr1_mii1_rxd3 I Nothing Nothing),
       (Sig_pr1_mii1_rxdv, Signal Sig_pr1_mii1_rxdv I Nothing Nothing),
       (Sig_pr1_mii1_rxer, Signal Sig_pr1_mii1_rxer I Nothing Nothing),
       (Sig_pr1_mii1_rxlink,
        Signal Sig_pr1_mii1_rxlink I Nothing Nothing),
       (Sig_pr1_mii1_txd0, Signal Sig_pr1_mii1_txd0 O Nothing Nothing),
       (Sig_pr1_mii1_txd1, Signal Sig_pr1_mii1_txd1 O Nothing Nothing),
       (Sig_pr1_mii1_txd2, Signal Sig_pr1_mii1_txd2 O Nothing Nothing),
       (Sig_pr1_mii1_txd3, Signal Sig_pr1_mii1_txd3 O Nothing Nothing),
       (Sig_pr1_mii1_txen, Signal Sig_pr1_mii1_txen O Nothing Nothing),
       (Sig_pr1_mii_mr0_clk,
        Signal Sig_pr1_mii_mr0_clk I Nothing Nothing),
       (Sig_pr1_mii_mr1_clk,
        Signal Sig_pr1_mii_mr1_clk I Nothing Nothing),
       (Sig_pr1_mii_mt0_clk,
        Signal Sig_pr1_mii_mt0_clk I Nothing Nothing),
       (Sig_pr1_mii_mt1_clk,
        Signal Sig_pr1_mii_mt1_clk I Nothing Nothing),
       (Sig_pr1_pru0_pru_r30_0,
        Signal Sig_pr1_pru0_pru_r30_0 O Nothing Nothing),
       (Sig_pr1_pru0_pru_r30_1,
        Signal Sig_pr1_pru0_pru_r30_1 O Nothing Nothing),
       (Sig_pr1_pru0_pru_r30_2,
        Signal Sig_pr1_pru0_pru_r30_2 O Nothing Nothing),
       (Sig_pr1_pru0_pru_r30_3,
        Signal Sig_pr1_pru0_pru_r30_3 O Nothing Nothing),
       (Sig_pr1_pru0_pru_r30_4,
        Signal Sig_pr1_pru0_pru_r30_4 O Nothing Nothing),
       (Sig_pr1_pru0_pru_r30_5,
        Signal Sig_pr1_pru0_pru_r30_5 O Nothing Nothing),
       (Sig_pr1_pru0_pru_r30_6,
        Signal Sig_pr1_pru0_pru_r30_6 O Nothing Nothing),
       (Sig_pr1_pru0_pru_r30_7,
        Signal Sig_pr1_pru0_pru_r30_7 O Nothing Nothing),
       (Sig_pr1_pru0_pru_r30_8,
        Signal Sig_pr1_pru0_pru_r30_8 O Nothing Nothing),
       (Sig_pr1_pru0_pru_r30_9,
        Signal Sig_pr1_pru0_pru_r30_9 O Nothing Nothing),
       (Sig_pr1_pru0_pru_r30_10,
        Signal Sig_pr1_pru0_pru_r30_10 O Nothing Nothing),
       (Sig_pr1_pru0_pru_r30_11,
        Signal Sig_pr1_pru0_pru_r30_11 O Nothing Nothing),
       (Sig_pr1_pru0_pru_r30_12,
        Signal Sig_pr1_pru0_pru_r30_12 O Nothing Nothing),
       (Sig_pr1_pru0_pru_r30_13,
        Signal Sig_pr1_pru0_pru_r30_13 O Nothing Nothing),
       (Sig_pr1_pru0_pru_r30_14,
        Signal Sig_pr1_pru0_pru_r30_14 O Nothing Nothing),
       (Sig_pr1_pru0_pru_r30_15,
        Signal Sig_pr1_pru0_pru_r30_15 O Nothing Nothing),
       (Sig_pr1_pru0_pru_r31_0,
        Signal Sig_pr1_pru0_pru_r31_0 I Nothing Nothing),
       (Sig_pr1_pru0_pru_r31_1,
        Signal Sig_pr1_pru0_pru_r31_1 I Nothing Nothing),
       (Sig_pr1_pru0_pru_r31_2,
        Signal Sig_pr1_pru0_pru_r31_2 I Nothing Nothing),
       (Sig_pr1_pru0_pru_r31_3,
        Signal Sig_pr1_pru0_pru_r31_3 I Nothing Nothing),
       (Sig_pr1_pru0_pru_r31_4,
        Signal Sig_pr1_pru0_pru_r31_4 I Nothing Nothing),
       (Sig_pr1_pru0_pru_r31_5,
        Signal Sig_pr1_pru0_pru_r31_5 I Nothing Nothing),
       (Sig_pr1_pru0_pru_r31_6,
        Signal Sig_pr1_pru0_pru_r31_6 I Nothing Nothing),
       (Sig_pr1_pru0_pru_r31_7,
        Signal Sig_pr1_pru0_pru_r31_7 I Nothing Nothing),
       (Sig_pr1_pru0_pru_r31_8,
        Signal Sig_pr1_pru0_pru_r31_8 I Nothing Nothing),
       (Sig_pr1_pru0_pru_r31_9,
        Signal Sig_pr1_pru0_pru_r31_9 I Nothing Nothing),
       (Sig_pr1_pru0_pru_r31_10,
        Signal Sig_pr1_pru0_pru_r31_10 I Nothing Nothing),
       (Sig_pr1_pru0_pru_r31_11,
        Signal Sig_pr1_pru0_pru_r31_11 I Nothing Nothing),
       (Sig_pr1_pru0_pru_r31_12,
        Signal Sig_pr1_pru0_pru_r31_12 I Nothing Nothing),
       (Sig_pr1_pru0_pru_r31_13,
        Signal Sig_pr1_pru0_pru_r31_13 I Nothing Nothing),
       (Sig_pr1_pru0_pru_r31_14,
        Signal Sig_pr1_pru0_pru_r31_14 I Nothing Nothing),
       (Sig_pr1_pru0_pru_r31_15,
        Signal Sig_pr1_pru0_pru_r31_15 I Nothing Nothing),
       (Sig_pr1_pru0_pru_r31_16,
        Signal Sig_pr1_pru0_pru_r31_16 I Nothing Nothing),
       (Sig_pr1_pru1_pru_r30_0,
        Signal Sig_pr1_pru1_pru_r30_0 O Nothing Nothing),
       (Sig_pr1_pru1_pru_r30_1,
        Signal Sig_pr1_pru1_pru_r30_1 O Nothing Nothing),
       (Sig_pr1_pru1_pru_r30_2,
        Signal Sig_pr1_pru1_pru_r30_2 O Nothing Nothing),
       (Sig_pr1_pru1_pru_r30_3,
        Signal Sig_pr1_pru1_pru_r30_3 O Nothing Nothing),
       (Sig_pr1_pru1_pru_r30_4,
        Signal Sig_pr1_pru1_pru_r30_4 O Nothing Nothing),
       (Sig_pr1_pru1_pru_r30_5,
        Signal Sig_pr1_pru1_pru_r30_5 O Nothing Nothing),
       (Sig_pr1_pru1_pru_r30_6,
        Signal Sig_pr1_pru1_pru_r30_6 O Nothing Nothing),
       (Sig_pr1_pru1_pru_r30_7,
        Signal Sig_pr1_pru1_pru_r30_7 O Nothing Nothing),
       (Sig_pr1_pru1_pru_r30_8,
        Signal Sig_pr1_pru1_pru_r30_8 O Nothing Nothing),
       (Sig_pr1_pru1_pru_r30_9,
        Signal Sig_pr1_pru1_pru_r30_9 O Nothing Nothing),
       (Sig_pr1_pru1_pru_r30_10,
        Signal Sig_pr1_pru1_pru_r30_10 O Nothing Nothing),
       (Sig_pr1_pru1_pru_r30_11,
        Signal Sig_pr1_pru1_pru_r30_11 O Nothing Nothing),
       (Sig_pr1_pru1_pru_r30_12,
        Signal Sig_pr1_pru1_pru_r30_12 O Nothing Nothing),
       (Sig_pr1_pru1_pru_r30_13,
        Signal Sig_pr1_pru1_pru_r30_13 O Nothing Nothing),
       (Sig_pr1_pru1_pru_r30_14,
        Signal Sig_pr1_pru1_pru_r30_14 O Nothing Nothing),
       (Sig_pr1_pru1_pru_r30_15,
        Signal Sig_pr1_pru1_pru_r30_15 O Nothing Nothing),
       (Sig_pr1_pru1_pru_r31_0,
        Signal Sig_pr1_pru1_pru_r31_0 I Nothing Nothing),
       (Sig_pr1_pru1_pru_r31_1,
        Signal Sig_pr1_pru1_pru_r31_1 I Nothing Nothing),
       (Sig_pr1_pru1_pru_r31_2,
        Signal Sig_pr1_pru1_pru_r31_2 I Nothing Nothing),
       (Sig_pr1_pru1_pru_r31_3,
        Signal Sig_pr1_pru1_pru_r31_3 I Nothing Nothing),
       (Sig_pr1_pru1_pru_r31_4,
        Signal Sig_pr1_pru1_pru_r31_4 I Nothing Nothing),
       (Sig_pr1_pru1_pru_r31_5,
        Signal Sig_pr1_pru1_pru_r31_5 I Nothing Nothing),
       (Sig_pr1_pru1_pru_r31_6,
        Signal Sig_pr1_pru1_pru_r31_6 I Nothing Nothing),
       (Sig_pr1_pru1_pru_r31_7,
        Signal Sig_pr1_pru1_pru_r31_7 I Nothing Nothing),
       (Sig_pr1_pru1_pru_r31_8,
        Signal Sig_pr1_pru1_pru_r31_8 I Nothing Nothing),
       (Sig_pr1_pru1_pru_r31_9,
        Signal Sig_pr1_pru1_pru_r31_9 I Nothing Nothing),
       (Sig_pr1_pru1_pru_r31_10,
        Signal Sig_pr1_pru1_pru_r31_10 I Nothing Nothing),
       (Sig_pr1_pru1_pru_r31_11,
        Signal Sig_pr1_pru1_pru_r31_11 I Nothing Nothing),
       (Sig_pr1_pru1_pru_r31_12,
        Signal Sig_pr1_pru1_pru_r31_12 I Nothing Nothing),
       (Sig_pr1_pru1_pru_r31_13,
        Signal Sig_pr1_pru1_pru_r31_13 I Nothing Nothing),
       (Sig_pr1_pru1_pru_r31_14,
        Signal Sig_pr1_pru1_pru_r31_14 I Nothing Nothing),
       (Sig_pr1_pru1_pru_r31_15,
        Signal Sig_pr1_pru1_pru_r31_15 I Nothing Nothing),
       (Sig_pr1_pru1_pru_r31_16,
        Signal Sig_pr1_pru1_pru_r31_16 I Nothing Nothing),
       (Sig_pr1_uart0_cts_n,
        Signal Sig_pr1_uart0_cts_n I Nothing Nothing),
       (Sig_pr1_uart0_rts_n,
        Signal Sig_pr1_uart0_rts_n O Nothing Nothing),
       (Sig_pr1_uart0_rxd, Signal Sig_pr1_uart0_rxd I Nothing Nothing),
       (Sig_pr1_uart0_txd, Signal Sig_pr1_uart0_txd O Nothing Nothing),
       (Sig_rgmii1_rclk, Signal Sig_rgmii1_rclk I Nothing Nothing),
       (Sig_rgmii1_rctl, Signal Sig_rgmii1_rctl I Nothing Nothing),
       (Sig_rgmii1_rd0, Signal Sig_rgmii1_rd0 I Nothing Nothing),
       (Sig_rgmii1_rd1, Signal Sig_rgmii1_rd1 I Nothing Nothing),
       (Sig_rgmii1_rd2, Signal Sig_rgmii1_rd2 I Nothing Nothing),
       (Sig_rgmii1_rd3, Signal Sig_rgmii1_rd3 I Nothing Nothing),
       (Sig_rgmii1_tclk, Signal Sig_rgmii1_tclk O Nothing Nothing),
       (Sig_rgmii1_tctl, Signal Sig_rgmii1_tctl O Nothing Nothing),
       (Sig_rgmii1_td0, Signal Sig_rgmii1_td0 O Nothing Nothing),
       (Sig_rgmii1_td1, Signal Sig_rgmii1_td1 O Nothing Nothing),
       (Sig_rgmii1_td2, Signal Sig_rgmii1_td2 O Nothing Nothing),
       (Sig_rgmii1_td3, Signal Sig_rgmii1_td3 O Nothing Nothing),
       (Sig_rgmii2_rclk, Signal Sig_rgmii2_rclk I Nothing Nothing),
       (Sig_rgmii2_rctl, Signal Sig_rgmii2_rctl I Nothing Nothing),
       (Sig_rgmii2_rd0, Signal Sig_rgmii2_rd0 I Nothing Nothing),
       (Sig_rgmii2_rd1, Signal Sig_rgmii2_rd1 I Nothing Nothing),
       (Sig_rgmii2_rd2, Signal Sig_rgmii2_rd2 I Nothing Nothing),
       (Sig_rgmii2_rd3, Signal Sig_rgmii2_rd3 I Nothing Nothing),
       (Sig_rgmii2_tclk, Signal Sig_rgmii2_tclk O Nothing Nothing),
       (Sig_rgmii2_tctl, Signal Sig_rgmii2_tctl O Nothing Nothing),
       (Sig_rgmii2_td0, Signal Sig_rgmii2_td0 O Nothing Nothing),
       (Sig_rgmii2_td1, Signal Sig_rgmii2_td1 O Nothing Nothing),
       (Sig_rgmii2_td2, Signal Sig_rgmii2_td2 O Nothing Nothing),
       (Sig_rgmii2_td3, Signal Sig_rgmii2_td3 O Nothing Nothing),
       (Sig_rmii1_crs_dv, Signal Sig_rmii1_crs_dv I Nothing Nothing),
       (Sig_rmii1_refclk, Signal Sig_rmii1_refclk IO Nothing Nothing),
       (Sig_rmii1_rxd0, Signal Sig_rmii1_rxd0 I Nothing Nothing),
       (Sig_rmii1_rxd1, Signal Sig_rmii1_rxd1 I Nothing Nothing),
       (Sig_rmii1_rxerr, Signal Sig_rmii1_rxerr I Nothing Nothing),
       (Sig_rmii1_txd0, Signal Sig_rmii1_txd0 O Nothing Nothing),
       (Sig_rmii1_txd1, Signal Sig_rmii1_txd1 O Nothing Nothing),
       (Sig_rmii1_txen, Signal Sig_rmii1_txen O Nothing Nothing),
       (Sig_rmii2_crs_dv, Signal Sig_rmii2_crs_dv I Nothing Nothing),
       (Sig_rmii2_refclk, Signal Sig_rmii2_refclk IO Nothing Nothing),
       (Sig_rmii2_rxd0, Signal Sig_rmii2_rxd0 I Nothing Nothing),
       (Sig_rmii2_rxd1, Signal Sig_rmii2_rxd1 I Nothing Nothing),
       (Sig_rmii2_rxerr, Signal Sig_rmii2_rxerr I Nothing Nothing),
       (Sig_rmii2_txd0, Signal Sig_rmii2_txd0 O Nothing Nothing),
       (Sig_rmii2_txd1, Signal Sig_rmii2_txd1 O Nothing Nothing),
       (Sig_rmii2_txen, Signal Sig_rmii2_txen O Nothing Nothing),
       (Sig_spi0_cs0, Signal Sig_spi0_cs0 IO Nothing Nothing),
       (Sig_spi0_cs1, Signal Sig_spi0_cs1 IO Nothing Nothing),
       (Sig_spi0_d0, Signal Sig_spi0_d0 IO Nothing Nothing),
       (Sig_spi0_d1, Signal Sig_spi0_d1 IO Nothing Nothing),
       (Sig_spi0_sclk, Signal Sig_spi0_sclk IO Nothing Nothing),
       (Sig_spi1_cs0, Signal Sig_spi1_cs0 IO Nothing Nothing),
       (Sig_spi1_cs1, Signal Sig_spi1_cs1 IO Nothing Nothing),
       (Sig_spi1_d0, Signal Sig_spi1_d0 IO Nothing Nothing),
       (Sig_spi1_d1, Signal Sig_spi1_d1 IO Nothing Nothing),
       (Sig_spi1_sclk, Signal Sig_spi1_sclk IO Nothing Nothing),
       (Sig_tclkin, Signal Sig_tclkin I Nothing Nothing),
       (Sig_testout, Signal Sig_testout O Nothing Nothing),
       (Sig_timer4, Signal Sig_timer4 IO Nothing Nothing),
       (Sig_timer5, Signal Sig_timer5 IO Nothing Nothing),
       (Sig_timer6, Signal Sig_timer6 IO Nothing Nothing),
       (Sig_timer7, Signal Sig_timer7 IO Nothing Nothing),
       (Sig_uart0_ctsn, Signal Sig_uart0_ctsn I Nothing Nothing),
       (Sig_uart0_rtsn, Signal Sig_uart0_rtsn O Nothing Nothing),
       (Sig_uart0_rxd, Signal Sig_uart0_rxd I Nothing Nothing),
       (Sig_uart0_txd, Signal Sig_uart0_txd O Nothing Nothing),
       (Sig_uart1_ctsn, Signal Sig_uart1_ctsn I Nothing Nothing),
       (Sig_uart1_dcdn, Signal Sig_uart1_dcdn I Nothing Nothing),
       (Sig_uart1_dsrn, Signal Sig_uart1_dsrn I Nothing Nothing),
       (Sig_uart1_dtrn, Signal Sig_uart1_dtrn O Nothing Nothing),
       (Sig_uart1_rin, Signal Sig_uart1_rin I Nothing Nothing),
       (Sig_uart1_rtsn, Signal Sig_uart1_rtsn O Nothing Nothing),
       (Sig_uart1_rxd, Signal Sig_uart1_rxd I Nothing Nothing),
       (Sig_uart1_txd, Signal Sig_uart1_txd O Nothing Nothing),
       (Sig_uart2_ctsn, Signal Sig_uart2_ctsn I Nothing Nothing),
       (Sig_uart2_rtsn, Signal Sig_uart2_rtsn O Nothing Nothing),
       (Sig_uart2_rxd, Signal Sig_uart2_rxd I Nothing Nothing),
       (Sig_uart2_txd, Signal Sig_uart2_txd O Nothing Nothing),
       (Sig_uart3_ctsn, Signal Sig_uart3_ctsn I Nothing Nothing),
       (Sig_uart3_rtsn, Signal Sig_uart3_rtsn O Nothing Nothing),
       (Sig_uart3_rxd, Signal Sig_uart3_rxd I Nothing Nothing),
       (Sig_uart3_txd, Signal Sig_uart3_txd O Nothing Nothing),
       (Sig_uart4_ctsn, Signal Sig_uart4_ctsn I Nothing Nothing),
       (Sig_uart4_rtsn, Signal Sig_uart4_rtsn O Nothing Nothing),
       (Sig_uart4_rxd, Signal Sig_uart4_rxd I Nothing Nothing),
       (Sig_uart4_txd, Signal Sig_uart4_txd O Nothing Nothing),
       (Sig_uart5_ctsn, Signal Sig_uart5_ctsn I Nothing Nothing),
       (Sig_uart5_rtsn, Signal Sig_uart5_rtsn O Nothing Nothing),
       (Sig_uart5_rxd, Signal Sig_uart5_rxd I Nothing Nothing),
       (Sig_uart5_txd, Signal Sig_uart5_txd O Nothing Nothing),
       (Sig_xdma_event_intr0,
        Signal Sig_xdma_event_intr0 I Nothing Nothing),
       (Sig_xdma_event_intr1,
        Signal Sig_xdma_event_intr1 I Nothing Nothing),
       (Sig_xdma_event_intr2,
        Signal Sig_xdma_event_intr2 I Nothing Nothing)]
