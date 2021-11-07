#include<cstdio>
#include<algorithm>
using namespace std;

const int maxn = 100000;
const int maxv = 1000;

int main()
{
    printf("%d\n", maxn);
    for (int i=0;i<maxn;i++)
    {
        int c = rand() % 3 + 1;
        printf("%d %d\n", c, rand()%maxv);
    }
}
