#include <opencv/cv.h>
#include <stdio.h>

float emd_float(float *sig1, int len1, float *sig2, int len2, float *costmat)
{
    CvMat *s1=cvCreateMatHeader(len1,1,CV_32F),
          *s2=cvCreateMatHeader(len2,1,CV_32F),
          *c =cvCreateMatHeader(len1,len2,CV_32F);

    s1->data.fl=sig1;
    s2->data.fl=sig2;
    c->data.fl=costmat;

    float lb=1;

    float ret=cvCalcEMD2(s1,s2,CV_DIST_USER,NULL,c,NULL,NULL,NULL);

    cvReleaseMat(&s1);
    cvReleaseMat(&s2);
    cvReleaseMat(&c);
    return ret;
}

/*
int main()
{
    float s1[]={1,4,4};
    float s2[]={6,2,1};
    float c[]=
        { 0, 1, 4
        , 1, 0, 1
        , 4, 1, 0
        };

    float res=emd_float(s1,3,s2,3,c);
    printf("emd=%f\n",res);
    return 0;
}
*/
