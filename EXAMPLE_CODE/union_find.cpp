// File    : union_find.cpp
// Author  : Sandeep Koranne
// Purpose : Concepts of Programming Languages course Lab1

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

const int MAX_N = 100; // maximum cardinality of the universe

int SET[ MAX_N ];

extern void InitializeUniverse( int N );
extern int Union( int A, int B );
extern int Find( int A );

void InitializeUniverse( int N )
{
  assert( N < MAX_N );
  for( int i=0; i < N; ++i ) SET[i] = i;
}

static void PrintUniverse( int N )
{
  assert( N < MAX_N );
  for( int i=0; i < N; ++i ) {
    printf("SET[%d] = %d\n", i, SET[i] );
  }
}

int Find( int A )
{
  int parent = A;
  if( SET[A] == A ) return A;
  parent = Find( SET[A] );
  SET[A] = parent;
  return parent;
}

int MarkRoot( int A, int root )
{
  // simple implementation
  SET[A] = root;
  return root;
}

int Union( int A, int B )
{
  int root_A = Find( A );
  int root_B = Find( B );
  if( root_A == root_B ) return root_A;
  if( root_A < root_B ) {
    MarkRoot( B, root_A );
    return root_A;
  }
  else {
    MarkRoot( A, root_B );
    return root_B;
  }
  assert( 0 );
  return 0;
}

static void CompressRoots( int N )
{
  for( int i=0; i < N; ++i ) 
    {
      int root = Find( i );
      if( SET[i] != root ) SET[i] = root;
    }
}

typedef struct 
{
  int source, destination, weight, in_tree;
} GraphEdge;

GraphEdge GRAPH[ MAX_N ]; // graphs of these many edges

static void ReadGraph( const char* filename, int* N, int* M)
{
  int number_nodes = 0, number_edges = 0;
  int numbers_read = 0;
  FILE *fp = NULL;
#ifdef WINDOWS
  fopen_s( &fp, filename, "rt" );
#else
  fp = fopen( filename, "rt" );
#endif
  if( !fp ) {
    printf("Unable to open file %s.\nExiting.", filename );
    exit( -1 );
  }
#ifdef WINDOWS
  numbers_read = fscanf_s( fp, "%d %d", &number_nodes, &number_edges );
#else
  numbers_read = fscanf( fp, "%d %d", &number_nodes, &number_edges );
#endif
  assert( numbers_read == 2 ); // syntax of the file
  *N = number_nodes;
  *M = number_edges;
  printf(" Reading Graph of %d nodes and %d edges.\n", *N, *M );
  for( int i=0; i < *M; ++i ) {
#ifdef WINDOWS
    numbers_read = fscanf_s( fp, "%d %d %d", &(GRAPH[i].source), &(GRAPH[i].destination), &(GRAPH[i].weight) );
#else
    numbers_read = fscanf( fp, "%d %d %d", &(GRAPH[i].source), &(GRAPH[i].destination), &(GRAPH[i].weight) );
#endif
    assert( numbers_read == 3 ); // syntax of the file is <source> <destination> <weight>
  }
  fclose( fp );
}

static void PrintGraph( int N, int M )
{
  for( int i=0; i < M; ++i )
    {
      assert( GRAPH[i].source <= N );
      assert( GRAPH[i].destination <= N );
      assert( GRAPH[i].weight > 0 ); // remove 0 wt edges
      printf("%4d %4d %4d\n", GRAPH[i].source, GRAPH[i].destination, GRAPH[i].weight );
    }
}

int EdgeSorter( const void* A, const void* B )
{
  GraphEdge *eA = (GraphEdge*) A;
  GraphEdge *eB = (GraphEdge*) B;
  if ( eA->weight < eB->weight )
    return -1;
  else if( eA->weight > eB->weight )
    return 1;
  else
    return 0;
}


static void SortGraphEdges( int N, int M )
{
  qsort( GRAPH, M, sizeof( GraphEdge ), EdgeSorter );
}

char LABEL[10] = {'0', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I' };

static void CalculateMST( int N, int M )
{
  int number_edges_added = 0;
  InitializeUniverse( N+1 ); // create universe of N, where N is number of nodes
  // Add each edge if source and destination are not in same set
  for( int i=0; i < M; ++i )
    {
      int A = GRAPH[i].source;
      int B = GRAPH[i].destination;
      int root_A = Find( A );
      int root_B = Find( B );
      //#define ROOT_DEBUG
#ifdef ROOT_DEBUG
      printf("Root of %d = %d\n", A, root_A );
      printf("Root of %d = %d\n", B, root_B );
#endif
      printf("Considering edge %c %c %d\n", LABEL[A], LABEL[B], GRAPH[i].weight );
      if( root_A == root_B )
	printf("Cannot add as forms cycle.\n");
      else {
	number_edges_added++;
	printf("Adding edge.\n");
	GRAPH[i].in_tree = 1;
	Union( root_A, root_B );
	//CompressRoots( N );
      }
    }
  printf("Added %d edges.\n", number_edges_added );
  assert( number_edges_added == ( N - 1 ) );
}

static void InitializeEmptyGraph()
{
  for( int i=0; i < MAX_N; ++i )
    GRAPH[i].source = GRAPH[i].destination = GRAPH[i].in_tree = GRAPH[i].weight = 0;
}

static void PrintMST( int N, int M )
{
  for( int i=0; i < M; ++i )
    {
      if( GRAPH[i].in_tree != 1 ) continue;
      printf("%c %c %d\n", LABEL[GRAPH[i].source], LABEL[GRAPH[i].destination], GRAPH[i].weight );
    }
}

int IntSorter( const void* A, const void* B )
{
  int iA = *((int*)A);
  int iB = *((int*)B);
  if( iA == iB ) return 0;
  if( iA < iB ) return -1;
  if( iA > iB ) return 1;
  return 0;
}

typedef struct _TreeNode 
{
  _TreeNode *left, *right, *parent;
  int value;
} TreeNode;

TreeNode* CreateNewNode( TreeNode* parent, int value )
{
  TreeNode* retval = (TreeNode*) malloc( sizeof( TreeNode ) );
  retval->left = retval->right = NULL;
  retval->parent = parent;
  retval->value = value;
  return retval;
}

static void PrintTreeNode( TreeNode* node )
{
  printf("[%d] L=%x R=%x P=%x\n", node->value, node->left, node->right, node->parent );
}

static void PrintTreeInfix( TreeNode* node, int depth )
{
  printf("Depth = %d\n", depth);
  if( node->left ) 
    PrintTreeInfix( node->left, depth+1 );
  PrintTreeNode( node );
  if( node->right )
    PrintTreeInfix( node->right, depth+1 );
}
  

TreeNode* ConvertSortedArrayToBinaryTree(int DATA[], TreeNode* parent, int start, int end) 
{
  if (start > end) return NULL;
  // same as (start+end)/2, avoids overflow.
  int mid = start + (end - start) / 2;
  TreeNode *node = CreateNewNode( parent, DATA[ mid ] );
  node->left = ConvertSortedArrayToBinaryTree( DATA, node, start, mid-1 );
  node->right = ConvertSortedArrayToBinaryTree( DATA, node, mid+1, end );
  return node;
}
 
TreeNode* CreateBST(int DATA[], int N) 
{
  return ConvertSortedArrayToBinaryTree( DATA, NULL, 0, N-1 );
}

static void ProcessBinaryTree( const char* filename )
{
  int N = 0;
  int KEY_N = 5;
  void* KEY = (void*) &KEY_N;
  void* retval = NULL;
  int * found_key = NULL;
  FILE *fp = NULL;
#ifdef WINDOWS
  fopen_s( &fp, filename, "rt" );
#else
  fp = fopen( filename, "rt" );
#endif
  if( !fp ) {
    printf("Unable to open file %s.\nExiting.", filename );
    exit( -1 );
  }
#ifdef WINDOWS
  fscanf_s( fp, "%d", &N );
#else
  fscanf( fp, "%d", &N );
#endif

  printf(" Reading dataset of %d numbers.\n", N );
  for( int i=0; i < N; ++i ) {
#ifdef WINDOWS
    fscanf_s( fp, "%d", &(SET[i] ) );
#else
    fscanf( fp, "%d", &(SET[i] ) );
#endif
  }
  // Now SET[N] is list of inputs
  qsort( SET, N, sizeof(int), IntSorter );
  for( int i=0; i < N; ++i ) printf("%d", SET[i] );
  retval = bsearch( KEY, SET, N, sizeof( int ), IntSorter );
  if( retval ) {
    found_key = (int*) retval;
    printf("Found %d at %d\n", KEY_N, *(found_key));
  }

  // Create a binary tree using recursive function
  TreeNode* tree_root = CreateBST( SET, N );
  PrintTreeNode( tree_root );
  PrintTreeInfix( tree_root, 0 );
}

int main( int argc, char* argv [] )
{
  int N=0, M=0;
  printf("Lab1 by Sandeep Koranne.\n");
  #ifdef BINARY_TREE_PROJECT
  // Binary Tree
  if( argc == 1 ) {
    printf("./lab1_bst <graph-file>\n");
    exit(1);
  }
  ProcessBinaryTree( argv[1] );
  return ( EXIT_SUCCESS );
  #endif

  #ifdef UNION_FIND_PROJECT
  printf( "Creating UNIVERSE with 10 elements");
  N = 10;
  InitializeUniverse( N );
  Union( 1, 2 );
  Union( 3, 4 );
  Union( 2, 3 );
  CompressRoots( N );
  PrintUniverse( N );
  return ( EXIT_SUCCESS );
  #endif
  
  #ifdef MST_PROJECT
  if( argc == 1 ) {
    printf("./lab1_mst <data-file>\n");
    exit(1);
  }
  InitializeEmptyGraph();
  if( argc > 1 )
    ReadGraph( argv[1], &N, &M );
  else
    ReadGraph( "g1.txt", &N, &M );
  //if( M > 0 ) PrintGraph( N, M );
  printf("Starting MST Algorithm\n");
  SortGraphEdges( N, M );
  PrintGraph( N, M );
  CalculateMST( N, M );
  PrintMST( N, M );
  return ( EXIT_SUCCESS );
  #endif

  return ( EXIT_SUCCESS );
}

