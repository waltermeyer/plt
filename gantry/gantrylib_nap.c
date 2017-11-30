// Wrapper function for sleep() in C
#include<unistd.h>

int nap(int x) {

   return sleep(x); 

}

#ifdef BUILD_TEST
int main() {
    
    x = sleep(5);
    printf("%d", x);
    return 0;
}
#endif
