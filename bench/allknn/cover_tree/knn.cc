#include "cover_tree.h"

// Compute the k nearest neighbors

int main(int argc, char *argv[])
{
  printf("log %f\n",log(-1));

  int k = atoi(argv[1]);

  v_array<point> set_of_points = parse_points(fopen(argv[2],"r"));
  v_array<point> set_of_queries = parse_points(fopen(argv[3],"r"));

  numdist=0;
  node top = batch_create(set_of_points);
  printf("numdist in batch_create: %lu\n", numdist);

  v_array<v_array<point> > res;

  numdist=0;
  if (!strcmp(argv[4],"dual")) {
      printf("dual tree search\n");
      node top_query = batch_create(set_of_queries);
      k_nearest_neighbor(top,top_query,res,k);
  }
  else {
      printf("single tree search\n");
      for (int i = 0; i < set_of_queries.index; i++)
          k_nearest_neighbor(top,new_leaf(set_of_queries[i]),res,k);
  }
  printf("numdist in k_nearest_neighbor: %lu\n", numdist);

  /*
  printf("Printing results\n");
  for (int i = 0; i < res.index; i++)
    {
      for (int j = 0; j<res[i].index; j++)
        print(res[i][j]);
      printf("\n");
    }
  printf("results printed\n");
  */
}
