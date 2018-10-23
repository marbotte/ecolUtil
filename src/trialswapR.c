#include <R.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>




void trialSwap(int *vecMat, int *nbrow, int *nbcol, int *nbtrial, int *result, int *verboseC)
{
//declaration des variable internes
//*********************************
    int i,j,k,l,trial,compt,ligne;
    int**m;
      srand(time(NULL));
//*********************************
//declaration des fonctions
//*********************************
int intrand(int n);

//remplissage de m
//*********************************


m=(int**) malloc(*nbcol*(*nbrow)*sizeof(int));
for(ligne=0;ligne<*nbrow;ligne++)
    {
        m[ligne]=(int*) malloc(*nbcol*sizeof(int));
	
    }

compt=0;
    for(j=0;j<*nbcol;j++)
    {
        for(i=0;i<*nbrow;i++)
        {
            m[i][j]=vecMat[compt];
            compt++;
        }
    }



//*********************************
//*********************************
//boucle sur le nombre de trial
//*********************************
//*********************************

	  for(trial=0;trial<*nbtrial;trial++)
	    {
	        if(*verboseC==1 && (trial%1000000)==0)
	        {
	            Rprintf("trial %d \n",trial);
	        }


    //tirage des numero de ligne et de colonne
    //***************************************
	      i=intrand(*nbrow);
	      while((j=intrand(*nbrow))==i);
	      k=intrand(*nbcol);
	      while((l=intrand(*nbcol))==k);
    //***************************************

    //si on a un checkerboard, on l'inverse
    //****************************************
	      if((m[i][k]*m[j][l]==1 && m[i][l]+m[j][k]==0)||(m[i][k]+m[j][l]==0 && m[i][l]*m[j][k]==1))
		{
		  m[i][k]=1-m[i][k];
		  m[i][l]=1-m[i][l];
		  m[j][k]=1-m[j][k];
		  m[j][l]=1-m[j][l];
		}
    //*****************************************

	    }

//*********************************
//*********************************
//fin de la boucle sur le nombre de trials
//*********************************
//*********************************
compt=0;
    for(j=0;j<*nbcol;j++)
    {
        for(i=0;i<*nbrow;i++)
        {
            result[compt]=m[i][j];
            compt++;
        }
    }

}
//fonction qui tire aléatoirement des nombres entiers
//*****************************************
int intrand(int n)
{

  double z;

  z = (double)rand() * n / (double)RAND_MAX;
  if(z>=n)
    z=n-1;
  if(z<0)
    z=0;
  return((int)floor(z));
}
//*****************************************
