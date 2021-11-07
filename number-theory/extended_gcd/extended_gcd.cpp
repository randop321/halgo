#include<cstdio>
#include<algorithm>
using namespace std;

long long extend_gcd(long long a, long long b, long long &x, long long &y)
{
	/*
		ax + by = gcd
		(kb+a')x + by = gcd
		b(kx+y) + a'x = gcd
	*/
	
	if (b==0) {x=1;y=0;return a;}
	
	long long gcd, _x, _y;
	gcd = extend_gcd(b, a%b, _x, _y);
	x = _y;
	y = _x-a/b*x;
	return gcd;
}


