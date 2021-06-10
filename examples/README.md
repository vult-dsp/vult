# Vult Examples

![Examples](/other/Images/synths.png?raw=true "Examples")

A set of Vult examples where each file is a standalone module that can be compiled to PureData or used to assemble more complex patches.

The modules follow these conventions:

* Pitch: 0.1 per octave, where 0.0 corresponds to C0
* Audio: -1.0 to 1.0
* Envelopes: 0.0 to 1.0
* Gates: 0.0 to 1.0

This convention lousy correspond to the typical Eurorack convention but divided by 10.0.

The examples are organized in the following folders:

* effects : Effects (e.g. Delay, Saturation)
* env : Envelopes (e.g. ADSR, LFO)
* filters : Filters (e.g. Ladder, State Variable)
* midi : MIDI utilities (e.g. MIDI to CV)
* osc : Oscillators (e.g. Saw, square)
* units : Full units (e.g. combination of the other modules)
* util : Utilities

## Build the Modules

To build the modules as PD externals you need to have installed [cmake](https://cmake.org) and the Vult compiler needs to be in your path as `vultc`.

```
$ cd examples
$ mkdir build
$ cd build
$ cmake ../
$ make
```
On Windows
```
$ cd examples
$ mkdir build
$ cd build
$ cmake ../ -G “NMake Makefiles”
$ nmake
```

After building, all the PD externals will be available in the `build` directory. To use them with PD you need to include this directory in the start path of PD.
