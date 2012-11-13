# BeagleBone Pins

The repository contains in a machine readable format:

* the mappings from BeagleBone expansion header pins to the microprocessor pins
* the list of signals multiplexed to each microprocessor pin
* the mappings from the microprocessor pins to `/sys/kernel/debug/omap_mux`
  names on Linux
* for GPIO signals, the `/sys/class/gpio/gpioN` number
* for PWM signals, the `/sys/class/pwm` name

This hopefully eliminates the need for ad-hoc lists of subsets of that
information in various BeagleBone projects.

Corrections to any errors found in the file as well as additional data (see
TODO) are appreciated.

The `data` directory contains the original data. `sqlite_import` creates an
SQLite database out of it and `sqlite_export` dumps it back to the data files.

The `gen` directory contains files generated from the data. At the moment only
JSON output has been implemented.

`Generate*` contains the code to parse the CSV data files and generate output
files.

TODO:

* [The eeprom information](https://github.com/jadonk/bonescript/blob/master/node_modules/bonescript/bone.js)
* Generate modules/libraries in various programming languages from the data.

The references for the data:

* [BeagleBone System Reference Manual](http://beagleboard.org/static/beaglebone/latest/Docs/Hardware/BONE_SRM.pdf), P8/P9 Signal Pin Mux Options
* [AM335x ARM Cortex-A8 Microprocessors (MPUs)](http://www.ti.com/lit/gpn/am3359), Ball Characteristics
