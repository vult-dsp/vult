![Vult](/other/Images/Vult-Small.png?raw=true "Vult")

Vult is a simple and powerful language to program high-performance algorithms that may run in small microprocessors or microcontrollers. Vult is specially useful when programming  Digital Signal Processing (DSP) algorithms like audio effects or synthesizers.

The Vult compiler is actually a transcompiler which takes Vult code and produces plain C code that can be compiled in any platform with a C/C++ compiler, for example: Arduino or Teensy boards. Javascript support is coming soon. Code written in Vult has freedom.

Some of the key feature of Vult are:
- Fixed-Point type: The same code can be generated using using float or fixed-point arithmetic. This makes it possible to run efficiently in small processors (or super-fast in big ones).
- Functions with context: Functions in Vult can have local memory and each call automatically creates a new environment. Contexts can be shared among functions.
- Powerful inlining: Vult automatically inlines simple functions but can also inline complex functions if you want.

These key features enable you to write clear and simple code without sacrificing performance.

**Note:** Vult is still in an alpha stage and is not recommended to use for serious projects.

Future features:
- A powerful type system: Type systems make your code safer and allow making analysis to check that the code is correct. Vult will have type inference and dependent types.
- More target languages: Vult will generate Javascript, C++ and Ocaml.
- More language commodities: See the planed language features section.

## Using Vult

### Vult Basics

Functions are declared with the keyword `fun`.

```
fun square(x) return x * x;
```
In this case the argument the function takes an argument `x` and returns the square of the value.

Intermediate results can be stored in variables with the keyword `val`.

```
val a = square(2);
val b = square(-1);
```

Vult has if-statements but also if-expressions.

```
val a = 0;
if(square(-1)> 0){
	a = 1;
}

val b = if square(-1) > 0 then -1 else 0;
```

The only restriction is that if-expressions must have a `else`.

Vult has while-loops...

```
fun fact(x){
	val i = x;
	val acc = 1;
	while(i>0){
		acc = acc * i;
		i = i-1;
	}
	return acc;
}
```

... but often is easier to write loops as recursive functions.
```
fun fact(x)
	if(x==0) return 1;
	else return x * fact(x-1);
```
So far, there's nothing special. Now lets make something more interesting.

### Active Functions (functions with context)

One of the key features of Vult are the "active functions" which are functions that can have a persistent context. The simplest way of understanding them is by making a few examples.

Lets say that we want to write in Vult a function that will be called in the main loop of an Arduino program.

```
void loop(){
	int x = analogRead(0);
	int result = processValue(x); // something like this
	analogWrite(result);
}
```

We want to replace the code of the function `processValue` with some Vult code that calculates, something more complex. We can start with something like this:

```
fun process(x) return 1/(x*x);
```

The first this that we can notice is that every time the function is called the operation `1/(x*x)` is performed. In this case the operation is not that expensive, but let's assume that it is and we want to calculate it only when is necessary. In order to do that, we need to remember which was the previous value we computed; if the value is the same we return the previously calculated result, if it's different we recalculate.

Now we are gonna declare two memory variables with the keyword `mem`.

```
fun process(x){
	mem pre_x;  // we will use it to keep the previous input
	mem result; // we will use it to keep the previous result

	if(x != pre_x)           // if the previous input is different from the current
		result = 1/(x*x);    // recompute

	pre_x = x;               // updates the previous input and result

	return result;
}
```

This function is longer than the original but only recalculates when necessary.

One important thing to have in mind is that if we call more than once the same function in a program each call has it's own context. For example:

```
val result_signal1 = process(signal1);
val result_signal2 = process(signal2);
```

Each call to `process` has independent `pre_x` and `pre_result` memory places. So the values that the variable `signal1` has do no affect the results produced by `signal2`.

We can simplify the above code by declaring the function `change` as follows:

```
// This function returns true when the input value changes
fun change(x){
	mem pre_x;               // remembers the previous value
	val result = pre_x != x; // checks if the value is different
	pre_x = x;               // updates the previous value
	return result;
}
```

Now we can simplify our `process` function as follows:

```
fun process(x){
	mem result; // we will use it to keep the previous result

	if(change(x))            // if the previous input is different from the current
		result = 1/(x*x);    // recompute

	return result;
}
```
In a similar way we can create a set of very handy functions. For example:
```
// This function returns true when the input value changes from zero to anything
fun edge(x){
	mem pre_x;
	// checks if the value is different and the previous was zero
	val result = pre_x != x && pre_x == 0;
	pre_x = x;
	return result;
}
```

```
// This function returns true every 'n' calls
fun every(n){
	mem count;
	val result = count == 0;
	count = (count+1)%n;
	return result;
}
```

The function every can be used as follows in order to reduce the number of times the result is recalculated.

```
fun process(x){
	mem result; // we will use it to keep the previous result

	if(every(4))           // the value is calculated every 4 calls
		result = 1/(x*x);  // recompute

	return result;
}
```

### Sharing and Reusing Contexts

As mentioned before every call to an active function creates it's own context. But what if we want to use the same context more than once?

Take a look at the function `count` defined below.

```
fun count(){
	mem counter;
	counter = counter + 1;
	return counter;
}
```

When this function is called increments it's internal counter and returns the value. But this function is useless because every time we call it, we will have a result of `1`.

```
val a = count(); // a = 1
val b = count(); // b = 1
val c = count(); // c = 1
val d = count(); // d = 1
val e = count(); // e = 1
val f = count(); // f = 1
```

In order to reuse contexts we need to give them a name using a colon `:`. For example:

```
val a = x:count(); // a = 1
val b = x:count(); // b = 2
val c = x:count(); // c = 3

val d = y:count(); // d = 1
val e = y:count(); // e = 2
val f = y:count(); // f = 3
```

In the above code we have two contexts `x` and `y` for the calls to the `count` function. Every time we call `x:count()` we access to the same internal `counter` memory. Now that our counter is useful one thing that we may want to do is reseting the counter. In order to access the the context of a function we need to create a new function that declares the same memory variables. For example the function `reset`.

```
fun reset(){
	mem counter;
	counter = 0;
}
```
Now we can reset the counter by using the same context.

```
val a = x:count(); // a = 1
val b = x:count(); // b = 2
val c = x:count(); // c = 3
        x:reset();
val d = x:count(); // d = 1
val e = x:count(); // e = 2
```

Note: currently you can share context among functions that declare the same `mem` and sub-contexts (that have the same named function calls). A future feature is to explicitly merge types.

### A Few Real Examples

Now we are gonna write some real code. We are gonna write code for a digital filter. We start by writing code for a generic biquad filter http://en.wikipedia.org/wiki/Digital_biquad_filter in direct for II.

```
fun biquad(x,b0,b1,b2,a1,a2){
    mem w1,w2;
    val w0 = x -a1*w1-a2*w2;
    val y0 = b0*w0 + b1*w1 + b2*w2;
    w2, w1 = w1, w0;
    return y0;
}
```

This function takes the input signal `x` and the filter coefficients. In the "DAFX: Digital Audio Effects" book we can find a table with all the coefficients for different filters. For example, for the low pass filter we can write:

```
fun lp6(x,fc) {
    mem b0,b1,a1;
    // we calculate the coefficients only when the 'fc' changes
    if(change(fc)){
        val k = tan(3.1415*fc/44100); // 44100 is the sample rate
    	val b0 = k/(k+1);
    	val b1 = k/(k+1);
    	val a1 = (k-1)/(k+1);
    }
    return biquad(x,b0,b1,0,a1,0);
}
```

This function takes the cut frequency value `fc` and uses the previously defined function `change` to trigger the recalculation of the biquad coefficients only when needed.

Let's say that we want to filter two signals (`signal1` and `signal2`) with our new filter and remove all frequencies above 1000 hertz.

```
val filtered1 = lp6(signal1,1000);
val filtered2 = lp6(signal2,1000);
```

### Planned Features

#### Function Specialization

#### Automatic Creation of Lookup Tables

#### VST Plugin Templates

#### VST with JIT Compilation

#### Type Inference

#### Type Merging

#### Dependent Types

## Installing

### Requirements

- Ocaml compiler >= 4.02
- [Optional] Node.js >= 10.33 (to run the web interpreter)

#### Ocaml Libraries

- ocaml-containers >= 0.6.1
- ppx_deriving >= 2.0
- ocamlgraph >= 1.8.6
- [Optional] js_of_ocaml >= 2.5 (to run the web interpreter)
- [Optional] oUnit >= 2.0 (to run the tests)

### Installing the tools
 The simplest way to instal the requirements is with Opam (https://opam.ocaml.org/)
```
$ opam switch 4.02.1
$ opam install containers
$ opam install ppx_deriving
```
Optionally to run the tests and build the web interpreter:
```
$ opam install js_of_ocaml
$ opam install ounit
```
### Compiling Vult
```
$ ocamlbuild -use-ocamlfind -pkg containers -pkg str vultc.native
```

### Compiling the web interpreter

First you need to install all the node.js dependencies:
```
$ cd node
$ npm install
$ cd ..
```
Then you can compile the Vult web interpreter:
```
$ ./scripts/build_node.sh
```
### Running the web interpreter
```
$ node ./node/app.js
```
Open the browser and go to http://localhost:3000

