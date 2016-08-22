/*
 * Filename: arrfun.c
 * 
 */

void multelem(double u[], double v[], double w[], int len_w)
// Multiply arguments element-wise; vectors must be same length
{
    int i;
	
    for (i = 0; i < len_w; i++)
    {
        w[i] = u[i] * v[i];
    }
}

double sumelem(double v[], int len_v)
// Sum of elements
{
    double ans = v[0];
    int i;
	
    for (i = 1; i < len_v; i++)
    {
	ans = ans + v[i];
    }
    
    return ans;
}
