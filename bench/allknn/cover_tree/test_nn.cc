#include "cover_tree.h"
#include <limits.h>
#include <values.h>
#include <stdint.h>
#include <iostream>
#include <time.h>
#include <sys/time.h>
using namespace std;

float diff_timeval(timeval t1, timeval t2)
{
  return (float) (t1.tv_sec - t2.tv_sec) + (t1.tv_usec - t2.tv_usec) * 1e-6;
}

float diff_clock(clock_t t1, clock_t t2)
{
  return (float) (t1 - t2) / (float) CLOCKS_PER_SEC;
}

int compare(const void* p1, const void* p2)
{
  if (p1<p2) 
    return -1;
  else 
    return 1;
}

int main(int argc, char *argv[])
{
  timeval start;
  //  clock_t start_clock = clock();
  gettimeofday(&start, NULL);
  //v_array<point> v = make_same(10000);  
  if (argc <2 )
    {
      cout << "usage: test_nn <k> <dataset>" << endl;
      exit(1);
    }
  int k = atoi(argv[1]);
  FILE* fd = fopen(argv[2],"r");
  v_array<point> v = parse_points(fd);
  printf("point length = %f\n",v[0]);
  //printf("point length = %i\n",v[0].index);
  //printf("first point = \n");
  //print(v.elements[0]);

  cout << "fart" << endl;
  timeval parsed;
  clock_t parsed_clock = clock();
  gettimeofday(&parsed,NULL);
  //printf("parse in %f seconds\n",parsed-start);

  cout << "batch_create" << endl;
  node top = batch_create(v);
  cout << "done."  << endl;
  timeval created;
  //  clock_t created_clock = clock();
  gettimeofday(&created, NULL);
  //printf("created in %f seconds\n",diff(created,parsed));
  
  //print(0, top);
  /*  v_array<int> depths;
  depth_dist(top.scale, top, depths);
  
  printf("depth distribution = \n");
  for (int i = 0; i < depths.index; i++)
    if (depths[i] > 0)
      printf("%i\t",i);
  printf("\n");
  for (int i = 0; i < depths.index; i++)
    if (depths[i] > 0)
      printf("%i\t",depths[i]);
  printf("\n");

  v_array<int> heights;
  printf("max height = %i\n",height_dist(top, heights));
  
  printf("height distribution = \n");
  for (int i = 0; i < heights.index; i++)
    printf("%i\t",i);
  printf("\n");
  for (int i = 0; i < heights.index; i++)
    printf("%i\t",heights[i]);
  printf("\n");
  
  v_array<int> breadths;
  breadth_dist(top,breadths);
  
  printf("breadth distribution = \n");
  for (int i = 0; i < breadths.index; i++)
    if (breadths[i] > 0)
      printf("%i\t",i);
  printf("\n");
  for (int i = 0; i < breadths.index; i++)
    if (breadths[i] > 0)
      printf("%i\t",breadths[i]);
      printf("\n");*/
  
  cout << "v_array" << endl;
  v_array<v_array<point> > res;
  /*  for (int i = 0; i < v.index; i++) 
      k_nearest_neighbor(top,new_leaf(v[i]),res,k);      */
  cout << "starting knn" << endl;
  k_nearest_neighbor(top,top,res,k);
  
  /*  printf("Printing results\n");
  for (int i = 0; i< res.index; i++)
    {
      for (int j = 0; j<res[i].index; j++)
	print(res[i][j]);
      printf("\n");
    }
    printf("results printed\n");*/
  
  cout << "poop" << endl;

  timeval queried;
  clock_t queried_clock = clock();
  gettimeofday(&queried, NULL);
  //printf("queried in %f seconds\n",diff(queried,created));
  //printf("%i distance queries\n",get_count());
  //qsort(v.elements, v.index,sizeof(point), compare);
  int thresh=MAXINT;
  if (1e10 / v.index< v.index)
    thresh = (int) 1e10 / v.index;

  if (thresh < 10)
    thresh = 10;
  
  v_array<v_array<point> > brute_neighbors;
  for (int i=0; i < res.index  && i < thresh; i++) {
    point this_point = res[i][0];
    float upper_dist[k];
    point min_points[k];
    for (int j=0; j<k; j++)
      {
	upper_dist[j] = MAXFLOAT;
	min_points[j] = this_point;
      }
    for (int j=0; j < v.index; j++) {
      float dist = distance (this_point, v[j], upper_dist[0]);
      if (dist < upper_dist[0]) {
	int l=0;
	for (;l<k-1; l++)
	  {
	    if (dist < upper_dist[l+1])
	      {
		upper_dist[l] = upper_dist[l+1];
		min_points[l] = min_points[l+1];
	      }
	    else {
	      upper_dist[l] = dist;
	      min_points[l] = v[j];
	      break;
	    }
	  }
	if (l == k-1)
	  {
	    upper_dist[l] = dist;
	    min_points[l] = v[j];
	  }
      }
    }
    v_array<point> us;
    push(us,this_point);
    for (int j = 0; j<k; j++){
      push(us,min_points[j]);
    }
    push(brute_neighbors,us);
    }
  timeval bruted;
  clock_t bruted_clock = clock();
  gettimeofday(&bruted, NULL);
  float qp,bq;
  if (true) //diff_clock(queried_clock,parsed_clock) < 1)
    qp = diff_timeval(queried,parsed);
  else
    qp = diff_clock(queried_clock,parsed_clock);
  if (true) //diff_clock(bruted_clock,queried_clock) < 1)
    bq = diff_timeval(bruted,queried);
  else
    bq = diff_clock(bruted_clock,queried_clock);
  printf("%s\t",argv[2]);
  printf("%i\t",v.index);
  printf("%f\t",qp);
  if (v.index <= thresh)
    {
      printf("%f\t",bq);
      printf("%f\n",bq / qp);
    }
  else
    {
      float mul = v.index / thresh;
      printf("%f(*)\t",bq*mul);
      printf("%f(*)\n",bq / qp * mul);
    }
  for (int i=0; i < brute_neighbors.index; i++) {
    point this_point = brute_neighbors[i][0];
    for (int j = 1; j < brute_neighbors[i].index; j++)
      {
	int flag = 0;
	point this_neighbor = brute_neighbors[i][j];
	float brute_distance = distance (this_neighbor, this_point, MAXFLOAT);
	for (int l = 1; l < res[i].index; l++)
	  {
	    if (brute_distance == distance(res[i][l],this_point,MAXFLOAT))
	      {
		flag = 1;
		break;
	      }
	  }
	if (flag == 0)
	  {
	    printf(" distances unequal %f\n", brute_distance);
	    printf("point         = ");print(this_point);
	    printf("brute neighbor = "); print(this_neighbor);
	    printf("our_neighbors = \n");
	    for (int l = 1; l < res[i].index; l++)
	      {
		printf("%f = distance, point = ",distance(res[i][l],this_point,MAXFLOAT));
		print(res[i][l]);
	      }
	  }
      }
  }
}
