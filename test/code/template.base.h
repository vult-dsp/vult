#ifndef TEMPLATE_H
#define TEMPLATE_H
#include <stdint.h>
#include <math.h>

float process(float input);

void noteOn(int note, int velocity);

void noteOff(int note);

void controlChange(int control, int value);

void default_();



#endif // TEMPLATE_H