+++
date = "2017-07-29T14:08:43+03:00"
title = "New structure of the examples"
description = ""
tags = []
topics = []
layout = "post"
author = "Leonardo Laguna Ruiz"

+++

In the past, the examples folder of Vult contained just a few files that were developed in an early stage. Most of the 'real' examples were developed in a separate [repository](https://github.com/modlfo/vult-examples). In order to improve the testing of the Vult compiler the examples have been moved into the Vult repository.

The examples contain different types of:

- Oscillators
- Filters
- Effects
- Full modules

One characteristic of these examples is that all of them can be directly compiled into PureData externals.

The examples folder contains as well a build system based on CMake that generates the PureData externals.

You can check the examples [here](https://github.com/modlfo/vult/tree/master/examples).

