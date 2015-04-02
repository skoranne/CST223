#include <stdio.h>
#include <omp.h>

int main()
{
    int i;
    #pragma omp parallel for
    for(i=0; i < 8; i++) {
        printf("Thread %d says hello.\n", omp_get_thread_num() );
    }
    return 0;
}

// compile with gcc -fopenmp openmp.c
