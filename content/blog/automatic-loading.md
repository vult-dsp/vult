+++
date = "2016-12-01T15:49:04-06:00"
title = "Automatic loading of Vult files"
description = ""
tags = []
topics = []
layout = "post"
author = "Leonardo Laguna Ruiz"
+++

Recently I implemented simpler way to call the Vult compiler with multiple files. Previously Vult required that you passed every file in a correct order. For example:

```
$ vultc -ccode file1.vult file2.vult
```

In this case the `file2.vult` used functions from the `file1.vult`. As the number of files increased it also increased the complexity of the call to the compiler.

Compiling code now is easier. With the latest Vult compiler you just have to pass as argument your top file. In the previous example the call would be:

```
$ vultc -ccode file2.vult
```

Based on the dependencies Vult will look for the corresponding files matching the convention that every module name  (for example `Util`) has a corresponding file called `util.vult`. Vult will look for the matching files in all the directories of the input files. For example the call:

```
$ vultc /home/leonardo/dir1/file1.vult /home/leonardo/dir2/file2.vult
```

will look for dependencies in the directories:
```
/home/leonardo/dir1/
/home/leonardo/dir2/
```
One important thing to notice is that the file order does not matter anymore.

Vult will also look for files in the current working directory.

If you want to add more directories to the search you can use the flag `-i`. For example:

```
$ vultc file1.vult -i /home/leonardo/dir2
```

### Implementation

Solving this problem is in fact very easy. When a file is loaded first it's necessary to get all its dependencies. Once the dependencies are detected, we need to look for the matching files in the implicit and explicit search locations. The same process is repeated until every file has been loaded. At this point, we have a list of all the necessary files. Now we have to load every file in the correct order because Vult does not support forward declarations of functions. To determine the order we can use topological sorting of the dependency graph.

The actual implementation uses the [Strongly Connected Components](http://www.geeksforgeeks.org/strongly-connected-components/) algorithm. The reason of using this instead of simple [Topological sorting](https://en.wikipedia.org/wiki/Topological_sorting) is that the Strongly Connected Components algorithm will tell us if there are any cyclical dependencies, which is an error in Vult.

You can find the complete implementation in OCaml in the file [src/util/components.ml](https://github.com/modlfo/vult/blob/master/src/util/components.ml). The actual implementation of the Kosaraju's algorithm is very compact, less than 40 lines of code.
