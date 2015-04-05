// Example : fibo.cpp
//
#include <stdio.h>
#include <stdlib.h>

unsigned long factorial( unsigned long N )
{
  unsigned long retval=1;
  for( unsigned long i=1; i <= N; ++i ) retval *= i;
  return retval;
}

unsigned long fibonacci(unsigned long N)
{
  unsigned long f0 = 0;
  unsigned long f1 = 1;
  unsigned long fn = 0;
  if( N == 0 ) return 0;
  if( N == 1 ) return 1;
  for( unsigned int i=1; i < N; ++i ) {
    fn = f1 + f0;
    f0 = f1;
    f1 = fn;
  }
  return fn;
}

int main()
{
  for( unsigned long i=0; i < 17; ++i ) {
    printf("%3d Fibonacci number = %5ld %3d! = %10ld\n", i, fibonacci(i), i, factorial( i ) );
  }
  return 0;
}
