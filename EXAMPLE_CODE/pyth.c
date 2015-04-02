/*******************************************************************************  
 *  File   : pyth.c                                                            *
 *  Author : Sandeep Koranne                                                   *
 *  Purpose: Print all Pythagorean Triples of the form a^2+b^2=c^2             *
 *           Extension, generalize to multiple variables and powers            *
 ******************************************************************************/
#include <stdio.h>
void PrintPythagoreanTriple( int N )
{
    int a,b,c;
    for( a=1; a<N-2; a++ ) {
        for( b=a+1; b<N-1; b++ ) {
            for( c=b+1; c<N; c++ ) {
                if( ( a*a + b*b) == ( c*c ) )
                    printf("%d^2 + %d^2 = %d^2\n", a, b, c );
            }
        }
    }
}
int main(int argc, char* argv[])
{
    int N=0;
    printf("Pythagorean Triple Search by Sandeep Koranne\n");
    if( argc > 1 ) N = atoi( argv[1] );
    PrintPythagoreanTriple( N );
    return 0;
}
