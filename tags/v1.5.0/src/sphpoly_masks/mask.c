#include <stdio.h>
#include <math.h>
#include "export.h"
#include "mask.h"

/* global variables. Doh! */
int nr,nc[nrmax] ;
double caparea[nrmax],capweight[nrmax] ;
double r[3][50][nrmax],c[50][nrmax] ;

/* SUBROUTINE LOADMASK
--- loadmask()
--- Load an angular mask in polygon format
*/
void loadmask(char *maskfile)
{
   int region,cap,i ;

   double area,effarea ;
   /*char filename[55] ;*/
   FILE *fp ;

   area = effarea=0.0 ;
   
   /*
     E.S.S.
     strcpy(filename,"/home/esheldon/ccode/spectro_masks/sdss_mask.dat") ;
     fp=fopen(filename,"r") ; 
   */
 
   fp=fopen(maskfile,"r");
   
   fscanf(fp,"%d",&nr) ; 
   
   for(region=0;region<nr;region++)
     {
       fscanf(fp,"%d %d %lf %lf",&i,&nc[region],&capweight[region],&caparea[region]) ;
       if(i!=(region+1)) fprintf(stderr,"REGION DEATH MISMATCH: %d %d\n",region,i) ;
       
       for(cap=0;cap<nc[region];cap++)
	 {
	   for(i=0;i<3;i++)
	     fscanf(fp,"%lf",&r[i][cap][region]) ;
	   
	   fscanf(fp,"%lf",&c[cap][region]) ;
	 }
       
       area += caparea[region] ;
       effarea += caparea[region]*capweight[region] ;
       
     }
   
   
   area *= sqr(180./pi) ;
   effarea *= sqr(180./pi) ;
   
   
   fclose(fp) ;
   
   /* 
      fprintf(stderr,"Read %d regions from %s\n",nr,maskfile) ;
      fprintf(stderr,"Total area     = %f square degrees\n",area) ;
      fprintf(stderr,"Effective area = %f square degrees\n",effarea) ;
   */
   
}

void loadmask_info(char *maskfile, double *area, double *effarea)
{
   int region,cap,i ;
   FILE *fp ;

   *area = *effarea=0.0 ;
   
   /*
     E.S.S.
     strcpy(filename,"/home/esheldon/ccode/spectro_masks/sdss_mask.dat") ;
     fp=fopen(filename,"r") ; 
   */
 
   fp=fopen(maskfile,"r");
   
   fscanf(fp,"%d",&nr) ; 
   
   for(region=0;region<nr;region++)
     {
       fscanf(fp,"%d %d %lf %lf",&i,&nc[region],&capweight[region],&caparea[region]) ;
       if(i!=(region+1)) fprintf(stderr,"REGION DEATH MISMATCH: %d %d\n",region,i) ;
       
       for(cap=0;cap<nc[region];cap++)
	 {
	   for(i=0;i<3;i++)
	     fscanf(fp,"%lf",&r[i][cap][region]) ;
	   
	   fscanf(fp,"%lf",&c[cap][region]) ;
	 }
       
       *area += caparea[region] ;
       *effarea += caparea[region]*capweight[region] ;
       
     }
   
   
   *area *= sqr(180./pi) ;
   *effarea *= sqr(180./pi) ;
   
   
   fclose(fp) ;
   
   /* 
      fprintf(stderr,"Read %d regions from %s\n",nr,maskfile) ;
      fprintf(stderr,"Total area     = %f square degrees\n",*area) ;
      fprintf(stderr,"Effective area = %f square degrees\n",*effarea) ;
   */
   
}



/*---------------------------------------------------------*/
/* SUBROUTINE DOT1
--- dot1(*A,*B)
--- computes: 1 - dot product of 1D vectors A and B
*/
double dot1(double *A, double *B)
{
  double f ;

  f = 1-(A[0]*B[0] + A[1]*B[1] + A[2]*B[2]) ;
  return f ;
}
/*---------------------------------------------------------*/
/* SUBROUTINE COMPLETENESS
--- completeness(*x)
--- find the SDSS completeness for a given coordinate vector
   * x = array (x,y,z)
*/
double completeness(double *x)
{
  int i,region,cap,inside_region ;
  double length,vec[3],comp ;
  
  length = sqrt(sqr(x[0]) + sqr(x[1]) + sqr(x[2])) ;
  x[0] /= length ;
  x[1] /= length ;
  x[2] /= length ;
  
  for(region=0;region<nr;region++)
    {
      inside_region = 1 ;
      for(cap=0;cap<nc[region];cap++)
	{
	  for(i=0;i<3;i++)
	    vec[i] = r[i][cap][region] ;
	  if(dot1(x,vec)>c[cap][region]) inside_region = 0 ;
	}
      if(inside_region==1) 
	{
	  comp = capweight[region] ;
	  return comp ;
	}
    }
  return 0 ;
}

/*---------------------------------------------------------*/
/* SUBROUTINE COMPLETENESS_ID
--- completeness_id(*x, *completeness, *poly_id)
--- find the SDSS completeness for a given coordinate vector
    Return it and the polygon id
   * x = array (x,y,z)
*/

int completeness_id(double *x, 
		    double *comp, 
		    int *poly_id, 
		    double *poly_area)
{
  int i,region,cap,inside_region ;
  double length,vec[3];
  
  length = sqrt(sqr(x[0]) + sqr(x[1]) + sqr(x[2])) ;
  x[0] /= length ;
  x[1] /= length ;
  x[2] /= length ;
  
  *comp = 0.0;
  *poly_id = 0;
  *poly_area = 0.0;
  for(region=0;region<nr;region++)
    {
      inside_region = 1 ;
      for(cap=0;cap<nc[region];cap++)
	{
	  for(i=0;i<3;i++)
	    vec[i] = r[i][cap][region] ;
	  if(dot1(x,vec)>c[cap][region]) inside_region = 0 ;
	}
      if(inside_region==1) 
	{
	  *poly_id = region;
	  *comp = capweight[region];
	  *poly_area = caparea[*poly_id];
	  return(1);
	}
    }
  return(0);
}

/*---------------------------------------------------------*/
/* SUBROUTINE POLYGON
--- polygon(*x)
--- Returns the polygon number for a given coordinate vector
   * x = array (x,y,z)
*/
int polygon(double *x)
{
  int i,region,cap,inside_region ;
  double length,vec[3] ;
  
  length = sqrt(sqr(x[0]) + sqr(x[1]) + sqr(x[2])) ;
  x[0] /= length ;
  x[1] /= length ;
  x[2] /= length ;

  for(region=0;region<nr;region++)
    {
      inside_region = 1 ;
      for(cap=0;cap<nc[region];cap++)
	{
	  for(i=0;i<3;i++)
	    vec[i] = r[i][cap][region] ;
	  if(dot1(x,vec)>c[cap][region]) inside_region = 0 ;
	}
      if(inside_region==1) 
	{
	  return region ;
	}
    }
  return 0 ;
}
/*---------------------------------------------------------*/
/* SUBROUTINE OVERLAP
--- overlap(ipolygon)
--- Returns the overlap (sector) number for a given polygon number
   * ipolygon
*/
int overlap(char *polyfile, int ipolygon)
{
  int region,ioverlap,iboundary,ichunk ;
  double w ;
  /*char filename[60] ;*/
  FILE *fp ;

  /*
    E.S.S
    strcpy(filename,"/orbital/home/aberlind/Source/SDSS-Mask/polygon_details.dat") ;
  */

  fp=fopen(polyfile,"r") ;
  for(region=0;region<nr;region++)
    {
      fscanf(fp,"%d %d %d %lf",&ioverlap,&iboundary,&ichunk,&w) ; 

      if(region==ipolygon) break ;
    }
  fclose(fp) ;
  return ioverlap ;
}
/*---------------------------------------------------------*/
/* SUBROUTINE BOUNDARY
--- boundary(ipolygon)
--- Returns the boundary number for a given polygon number
   * ipolygon
*/

int boundary(char *polyfile, int ipolygon)
{
  int region,ioverlap,iboundary,ichunk ;
  double w ;
  /* char filename[60] ;*/
  FILE *fp ;
  
  /* 
     E.S.S.
     strcpy(filename,"/orbital/home/aberlind/Source/SDSS-Mask/polygon_details.dat") ;
  */
  fp=fopen(polyfile,"r") ;
  
  for(region=0;region<nr;region++)
    {
      fscanf(fp,"%d %d %d %lf",&ioverlap,&iboundary,&ichunk,&w) ; 

      if(region==ipolygon) break ;
    }
  fclose(fp) ;
  return iboundary ;
}
/*---------------------------------------------------------*/
/* SUBROUTINE CHUNK
--- chunk(ipolygon)
--- Returns the chunk number for a given polygon number
   * ipolygon
*/
int chunk(char *polyfile, int ipolygon)
{
  int region,ioverlap,iboundary,ichunk ;
  double w ;
  /*char filename[60] ;*/
  FILE *fp ;
  
  /*
    E.S.S.
    strcpy(filename,"/home/esheldon/ccode/spectro_masks/polygon_details.dat") ;
  */
  fp=fopen(polyfile,"r") ;
  
  for(region=0;region<nr;region++)
    {
      fscanf(fp,"%d %d %d %lf",&ioverlap,&iboundary,&ichunk,&w) ; 
      
      if(region==ipolygon) break ;
    }
  fclose(fp) ;
  return ichunk ;
}
