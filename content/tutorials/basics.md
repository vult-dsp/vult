+++
date = "2016-12-01"
description = ""
tags = []
title = "Vult Language Basics"
topics = []
layout = "tutorial"
+++


Vult has syntax that may resemble languages like C/C++, Python or JavaScript. In order to declare a function you can use the keyword `fun` as follows:

<div class="vult_code" id="tut1-0">fun foo() return 0;
</div>

This functions takes no arguments and returns the integer value zero. If you have more than one statement you can use the curly braces `{}` to delimit the function body.

<div class="vult_code" id="tut1-1">fun foo(x) {
   val y = 1 + x;
   return y;
}
</div>

As you may have seen, you can declare variables with the keyword `val`.

Vult is a static language, this means that every variable has concrete type. Since Vult is focused on numeric computations there are two main types available: `real` and `int`. You can specify the type of the variables with colon `:` as follows:

<div class="vult_code" id="tut1-2">fun foo(x : int) : int {
   val y : int = 1 + x;
   return y;
}
</div>

This specifies that the function receives an argument `x` of type `int` and returns a value of type `int`.

The type annotations are not strictly necessary. Vult has type inference which means that it will try to automatically determine every type based on the use of the variables. We will cover more about the type inference in a different tutorial.

Function in Vult can return multiple values separated by commas. The values can be assigned to multiple variables in a similar way.

<div class="vult_code" id="tut1-3">fun foo() {
   return 1, 2;
}
//
fun bar(){
   val a, b = foo();
}
</div>

As mentioned before, Vult is static and strict. It is not possible to mix operations between reals and integers without making an explicit cast. The following operation is invalid:

<div class="vult_code" id="tut1-4">fun foo() {
   val x = 1;
   val y = 2.1;
   return x + y; // invalid operation
}
</div>

The problem here is that the values have different type. The number `1` is of type `int` and the number `2.1` is of type `real`. In order to perform operations, you need to explicitly cast the values. For example:

<div class="vult_code" id="tut1-5">fun foo() {
   val x = 1;
   val y = 2.1;
   val z = x + int(y);  // z = 3
   val w = real(x) + y; // w = 3.1
   return w;
}
</div>

By not making automatic conversion of types Vult gives you a more control on the kind of operation you want to perform.

In the following tutorials we will cover more advanced aspects of Vult.

<script type="text/javascript" src="../../javascripts/external/ace/ace.js"></script>
<script type="text/javascript" src="../../javascripts/main.js"></script>
