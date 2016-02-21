#ifndef ._H
#define ._H
#include <stdint.h>
#include <math.h>

float process(float input);

void noteOn(int note, int velocity);

void noteOff(int note);

void controlChange(int control, int value);

void default_();



#endif // ._H
