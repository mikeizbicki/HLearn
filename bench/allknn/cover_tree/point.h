#include "stack.h"
#include<stdio.h>
#include<stdlib.h>

typedef float* point;

extern unsigned long numdist;

float complete_distance(point v1, point v2);
float distance(point v1, point v2, float upper_bound);
v_array<point > parse_points(FILE *input);
void print(point &p);
