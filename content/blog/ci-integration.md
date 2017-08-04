+++
date = "2016-10-18T22:56:13+03:00"
title = "Generating up-to-date binaries with CI"
description = ""
tags = []
topics = []
layout = "post"
author = "Leonardo Laguna Ruiz"
+++

I have been using Travis CI for running tests and checking that Vult build correctly. Travis CI can build Vult in OSX and Linux. However, due to the status of OCaml it was difficult to setup something similar in Windows with AppVeyor. Thankfully the OCaml community has published a set of script to configure different CI services:

<!--more-->


- https://github.com/ocaml/ocaml-ci-scripts

Using those scripts I could get the AppVeyor continuous integration working. Here you can take a look at the status of both services:

- https://ci.appveyor.com/project/modlfo/vult
- https://travis-ci.org/modlfo/vult

Once I got the things working. I started a long run of commits to get the CI services to deploy automatically every tag as a Github release. More than 30 commits later I got it working. You can see the releases here:

- https://github.com/modlfo/vult/releases

You can find there binaries for Windows, OSX and Linux.