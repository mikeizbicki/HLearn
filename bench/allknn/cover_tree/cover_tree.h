#include<string.h>
#include<math.h>
#include<list>
#include<stdlib.h>
#define NDEBUG
#include<assert.h>
#include "point.h"

struct node {
  point p;
  float max_dist;  // The maximum distance to any grandchild.
  float parent_dist; // The distance to the parent.
  node* children;
  unsigned short int num_children; // The number of children.
  short int scale; // Essentially, an upper bound on the distance to any child.
};

void print(int depth, node &top_node);

//construction
node new_leaf(const point &p);
node batch_create(v_array<point> points);
//node insert(point, node *top_node); // not yet implemented
//void remove(point, node *top_node); // not yet implemented
//query
void k_nearest_neighbor(const node &tope_node, const node &query, 
			v_array<v_array<point> > &results, int k);
void epsilon_nearest_neighbor(const node &tope_node, const node &query, 
			      v_array<v_array<point> > &results, float epsilon);
void unequal_nearest_neighbor(const node &tope_node, const node &query, 
			      v_array<v_array<point> > &results);
//information gathering
int height_dist(const node top_node,v_array<int> &heights);
void breadth_dist(const node top_node,v_array<int> &breadths);
void depth_dist(int top_scale, const node top_node,v_array<int> &depths);
