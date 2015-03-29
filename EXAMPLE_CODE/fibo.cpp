// Example : fibo.cpp
//
#include <iostream>
unsigned long fibonacci(unsigned long N)
{
  unsigned long f0 = 0;
  unsigned long f1 = 1;
  unsigned long fn = 0;

  for( unsigned int i=0; i < N; ++i ) {
    fn = f1 + f0;
    f0 = f1;
    f1 = fn;
  }
  return fn;
}

int main()
{
  for( unsigned long i=0; i < 10; ++i ) {
    std::cout << "F " << i << " = " << fibonacci( i ) << std::endl;
  }
  return 0;
}
