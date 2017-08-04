+++
date = "2016-07-13T20:10:57+02:00"
description = ""
tags = []
title = "Generating Pure Data and Teensy Audio templates"
topics = []
layout = "post"
author = "Leonardo Laguna Ruiz"
+++

As part of the Vult project I have developed a new standalone library: Pla (https://modlfo.github.io/pla/). Pla is a templating library that is used in Vult to generate code in a simpler way. Thanks to this library I have developed two templates to simplify using Vult:

<!--more-->

- [Pure Data externals](https://github.com/modlfo/pd-vult-example)
- [Teensy Audio Library objects](https://github.com/modlfo/pd-vult-example)

The template for Pure Data is more stable. It supports both, floating-point and fixed-point arithmetics. In addition, the number of inputs and outputs of the 'external' matches the inputs and outputs of the `process` function.

You can find an example of using it in this repository: https://github.com/modlfo/pd-vult-example

Exporting of objects for the Teensy Audio Library is only available in fixed-point and (currently) only supports synthesis objects (I have plans to make it as flexible as the PD template).

Here you can find an example: https://github.com/modlfo/teensy-vult-example

