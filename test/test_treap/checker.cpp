#include<set>
#include<cstdio>
#include<algorithm>
using namespace std;

set<int> t;
int n;

int main()
{
    scanf("%d",&n);
    for (int i=0;i<n;i++)
    {
        int c,v;
        scanf("%d%d",&c,&v);
        if (c==1) t.insert(v); else
        if (c==2)
        {
            if (t.count(v)) t.erase(v);
        } else
        {
            auto res = t.lower_bound(v);
            if (res == t.end()) printf("None\n");
            else printf("%d\n", *res);
        }
    }
}
