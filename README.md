# BeagleBone Pins

`beagle_bone_pins.json` contains in a machine readable format:

* the mappings from BeagleBone expansion header pins to the microprocessor pins
* the list of signals multiplexed to each microprocessor pin
* the mappings from the microprocessor pins to `/sys/kernel/debug/omap_mux`
  names on Linux
* for GPIO signals, the `/sys/class/gpio/gpioN` number

This hopefully eliminates the need for ad-hoc lists of subsets of that
information in various BeagleBone projects.

Corrections to any errors found in the file are appreciated.

[![CC0](http://i.creativecommons.org/p/zero/1.0/88x31.png)](http://creativecommons.org/publicdomain/zero/1.0/)

The references for the data:

* [BeagleBone System Reference Manual](http://beagleboard.org/static/beaglebone/latest/Docs/Hardware/BONE_SRM.pdf), P8/P9 Signal Pin Mux Options
* [AM335x ARM Cortex-A8 Microprocessors (MPUs)](http://www.ti.com/lit/gpn/am3359), Ball Characteristics
