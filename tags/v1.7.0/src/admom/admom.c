#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "admom.h"

/*
 * This runs adaptive moments on a list of centers 
 */


void
admom(float *data, 
	  int nx, 
	  int ny, 
	  int nobj,
	  float *bkgd, 
	  float *sigsky, 
	  float *shiftmax, 
	  float *wguess, 
	  float *xcen, 
	  float *ycen, 
	  float *Ixx, 
	  float *Iyy, 
	  float *Ixy, 
	  float *momerr, 
	  float *rho4, 
	  int *whyflag)
{
	int i;

	for (i=0; i<nobj; i++) {
		admom1(data,
			   nx,
			   ny,
			   bkgd[i], 
			   sigsky[i], 
			   shiftmax[i], 
			   wguess[i], 
			   xcen[i], 
			   ycen[i], 
			   &Ixx[i], 
			   &Iyy[i], 
			   &Ixy[i], 
			   &momerr[i], 
			   &rho4[i], 
			   &whyflag[i]);
	}
}

/*
 * Calculate the adaptive moments for a single center on the input image.  This
 * does the loop of iterations, with each iteration calling calcmom
 
 *
 * Note: this routine assumes that the _centre_ of a pixel is (0.0, 0.0)
 * which is not the SDSS convention. Caveat Lector.
 */


void
admom1(float *data, 
       int nx, 
	   int ny, 
	   float bkgd, 
	   float sigsky, 
	   float shiftmax, 
	   float wguess, 
	   float xcenin, 
	   float ycenin, 
	   float *Ixx, 
	   float *Iyy, 
	   float *Ixy, 
	   float *momerr, 
	   float *rho4, 
	   int *whyflag)

{

	float d;				/* weighted size of object */
	float detw;				/* determinant of matrix containing
							   moments of weighting function */
	float detm;				/* determinant of m[12][12] matrix */
	float detn;				/* determinant of n[12][12] matrix */
	float e1, e2;			/* current and old values of */
	float e1old = 1.e6, e2old =1.e6;	/* shape parameters e1 and e2 */

	int imom;                            /* iteration number */
	int interpflag;			/* interpolate finer than a pixel? */
	int ix1,ix2,iy1,iy2;                 /* corners of the box being analyzed */
	float m11,m22,m12;			/* weighted Quad. moments of object */
	float m11old = 1.e6;			/* previous version of m11 (Qxx) */
	float n11, n22, n12;			/* matrix elements used to determine
									   next guess at weighting fcn */
	double sum2t;			       
	double sum;				/* sum of intensity*weight */
	double sums4,sums4p;			 /* higher order moment used for
										PSF correction */
	double sumx, sumy;			/* sum ((int)[xy])*intensity*weight */
	double sumxx, sumxy, sumyy;		/* sum {x^2,xy,y^2}*intensity*weight */
	float w1,w2,ww12;                    /* w11 etc. divided by determinant */ 
	float w11,w22,w12;                   /* moments of the weighting fcn */
	float xcen,ycen;

	double summ;
	double sum1,sum2;
	double s1,s2,s1p,s2p,sum1p,sum2p;
	float IxxErr, IyyErr, IxyErr;

	/* 
	 * default values 
	 */
	xcen = xcenin;
	ycen = ycenin;

	sums4 = -9999.;

	if(shiftmax < 2) {
		shiftmax = 2;
	} else if(shiftmax > 10) {
		shiftmax = 10;
	}

	/* beginning guesses for moments */
	/* w11 = w22 = 1.5; */
	w11 = w22 = wguess;
	w12 = 0;

	/********************************
	 * iteration loop 
	 ********************************/

	for(imom = 0; imom < AM_MAXIT; imom++) {
		detw = w11*w22 - w12*w12;

		/* should we interpolate? */
		if(w11 < AM_XINTERP || w22 < AM_XINTERP || detw < AM_XINTERP2) {
			interpflag = 1;
		} else {
			interpflag = 0;
		}
		/* Temporarily turn interpolation ON */
		/* interpflag = 1; */

		/* faint check */
		if (detw <= AM_DETTOL) {
			*whyflag |= AMFLAG_FAINT;
			*Ixx=0.; *Iyy=0.;*Ixy=0.;*momerr=0.;*rho4=0.;
			return;
		}

		/* calculate moments */
		if(calcmom(xcen, ycen,
					&ix1,&ix2,&iy1,&iy2,
					bkgd,interpflag,
					w11,w22,w12,detw,
					&w1,&w2,&ww12,
					&sumx,&sumy,&sumxx,&sumyy,&sumxy,
					&sum,&sum1,&sum2,
					data, nx, ny) < 0) {
			*whyflag |= AMFLAG_FAINT;
			return;
		}

		if(sum <= 0) {			/* too faint to process */
			*whyflag |= AMFLAG_FAINT;
			return;
		}

		/*
		 *  Find new centre
		 */

		/* Robert does not want us to recalculate center */
		/* Added back. E.S.S. 13-Jan-2005 */
		xcen = sumx/sum;
		ycen = sumy/sum;

		if(fabs(sumx/sum - (xcenin)) > shiftmax || 
				fabs(sumy/sum - (ycenin)) > shiftmax) {
			*whyflag |= AMFLAG_SHIFT;
			return;
		}
		/*
		 * OK, we have the centre. Proceed to find the second moments.
		 * This is only needed if we update the centre; if we use the
		 * obj1->{row,col}c we've already done the work
		 */

		/* convert sums to moments */
		sum1/=sum;
		sum2/=sum;
		m11 = sumxx/sum;
		m22 = sumyy/sum;
		m12 = sumxy/sum;

		if(m11 <= 0 || m22 <= 0) {
			*whyflag |= AMFLAG_FAINT;
			return;
		}

		/* calculate shape */
		d = m11 + m22;
		e1 = (m11 - m22)/d;
		e2 = 2.*m12/d;

		/*
		 * convergence criteria met?
		 */
		if(fabs(e1 - e1old) < AM_TOL1 && fabs(e2 - e2old) < AM_TOL1 &&
				fabs(m11/m11old - 1.) < AM_TOL2 ) {
			summ=sum;
			break;				
		}

		/*
		 * Didn't converge, calculate new values for weighting function
		 */
		e1old = e1;
		e2old = e2;
		m11old = m11;
		detm = m11*m22 - m12*m12;
		if(detm <= 0) {
			*whyflag |= AMFLAG_FAINT;
			return;
		}

		detm = 1./detm;
		detw = 1./detw;
		n11 = m22*detm - w22*detw;
		n22 = m11*detm - w11*detw;
		n12 = -m12*detm + w12*detw;
		detn = n11*n22 - n12*n12;
		if(detn <= 0) {
			*whyflag |= AMFLAG_FAINT;
			return;
		}

		/* 
		 * The covariance elements for weighting function
		 */
		detn = 1./detn;
		w11 = n22*detn;
		w22 = n11*detn;
		w12 = -n12*detn;

		if(w11 <= 0 || w22 <= 0) {
			*whyflag |= AMFLAG_FAINT;
			return;
		}
	}  /****** end iteration loop ******/

	/* do some checks */
	if(imom == AM_MAXIT) {
		*whyflag |= AMFLAG_MAXIT;
		return;
	}       
	if(sumxx + sumyy == 0.0) {
		*whyflag |= AMFLAG_FAINT;
		return;
	}

	/***************************
	 * calculate uncertainties
	 ***************************/

	sum1 = (sumxx - sumyy)/(sumxx + sumyy);
	sum2 = sumxy/(sumxx + sumyy);

	calcerr(xcen, ycen, 
			ix1, ix2, iy1, iy2, 
			bkgd, interpflag,
			w1, w2, ww12, 
			m11, m22, m12,
			sum1,sum2,
			&IxxErr, &IyyErr, &IxyErr, 
			&sums4, &s1, &s2, 
			data, nx, ny);

	/* faint check */
	if(sums4 == 0.0) {
		*whyflag |= AMFLAG_FAINT;
		return;
	}

	sum2t = sigsky/sum;
	if(interpflag) {
		sum2t *= 4;
	}
	s1=sqrt(s1)*sigsky/(sumxx+sumyy);
	s2=2.*sqrt(s2)*sigsky/(sumxx+sumyy);

	*Ixx = w11; 
	*Iyy = w22;
	*Ixy = w12;
	*momerr=4*sqrt(M_PI)*sigsky*pow(((*Ixx)*(*Iyy) - (*Ixy)*(*Ixy)),0.25)/(4*summ-sums4);
	*rho4=sums4/sum;
	if (interpflag) *momerr *= 16;

}


/* calculate adaptive moments for a single iteration */

int
calcmom(float xcen, float ycen,		/* centre of object */
		int *ix1, int *ix2, int *iy1, int *iy2, /* bounding box to consider */
		float bkgd,			/* data's background level */
		int interpflag,			/* interpolate within pixels? */
		float w11, float w22, float w12,	/* weights */
		float detw,
		float *w1,float *w2, float *ww12,
		double *sumx, double *sumy,	/* desired */
		double *sumxx, double *sumyy,	/* desired */
		double *sumxy, double *sum,	/*       sums */
		double *sum1, double *sum2,
		float *data, int nx, int ny)		/* the data */
{   
	int i,j;
	float xx=0.,xx2=0.,yy=0.,yy2=0.;
	float xl=0.,xh=0.,yl=0.,yh=0.;
	float expon=0.;
	float tmod=0.,ymod=0.;
	float xxx=0.,yyy=0.;
	float weight=0.;
	float t1=0.;
	float grad=0.;
	double sumt=0.,sumxt=0.,sumyt=0.,sumxxt=0.,sumyyt=0.,sumxyt=0.;

	/* Calculate the region over which we should measure the moments */

	/* 4-sigma */
	grad = 4.*sqrt( ((w11 > w22) ? w11 : w22));

	/* Don't go outside the image. */
	*ix1 = xcen - grad - 0.5;
	if (*ix1 < 0) *ix1 = 0;
	*iy1 = ycen - grad - 0.5;
	if (*iy1 < 0) *iy1 = 0;

	*ix2 = xcen + grad + 0.5;
	if (*ix2 >= nx) *ix2 = nx - 1;
	*iy2 = ycen + grad + 0.5;
	if (*iy2 >= ny) *iy2 = ny - 1;


	/* The input moments for this iteration */
	*w1 = w11/detw;
	*w2 = w22/detw;
	*ww12 = w12/detw;

	/* Loop over rows */
	for(i = *iy1;i <= *iy2;i++) {
		/* row = data->rows[i]; */
		yy = i-ycen;
		yy2 = yy*yy;
		yl = yy - 0.375;
		yh = yy + 0.375;

		/* Loop over columns */
		for(j = *ix1;j <= *ix2;j++) {
			xx = j-xcen;

			/* Should we interpolate across pixels? */
			if(interpflag) {

				xl = xx - 0.375;
				xh = xx + 0.375;
				expon = xl*xl*(*w2) + yl*yl*(*w1) - 2.*xl*yl*(*ww12);
				t1 = xh*xh*(*w2) + yh*yh*(*w1) - 2.*xh*yh*(*ww12);
				expon = (expon > t1) ? expon : t1;
				t1 = xl*xl*(*w2) + yh*yh*(*w1) - 2.*xl*yh*(*ww12);
				expon = (expon > t1) ? expon : t1;
				t1 = xh*xh*(*w2) + yl*yl*(*w1) - 2.*xh*yl*(*ww12);
				expon = (expon > t1) ? expon : t1;
				if(expon <= 9.0) {
					/* tmod = row[j] - bkgd; */
					tmod = *(&data[0] + i*nx + j) - bkgd;
					for(yyy = yl;yyy <= yh;yyy+=0.25) {
						yy2 = yyy*yyy;
						for(xxx = xl;xxx <= xh;xxx+=0.25) {
							xx2 = xxx*xxx;
							expon = xx2**w2 + yy2**w1 - 2.*xxx*yyy**ww12;
							weight = exp(-0.5*expon);
							ymod = tmod*weight;
							/* *sum1+=(xx2+yy2)*ymod;*/
							/* *sum2+=(xx2-yy2)*ymod;*/
							sumxt+=ymod*(xxx+xcen);
							sumyt+=ymod*(yyy+ycen);
							sumxxt+=xx2*ymod;
							sumyyt+=yy2*ymod;
							sumxyt+=xxx*yyy*ymod;
							sumt+=ymod;
						}
					}
				}

				/* Not interpolating */
			} else { 
				xx2 = xx*xx;
				expon = xx2*(*w2) + yy2*(*w1) - 2.*xx*yy*(*ww12);

				if(expon <= 9.0) {
					weight = exp(-0.5*expon);
					/* ymod = (row[j] - bkgd)*weight;*/
					ymod = *(&data[0] + i*nx + j) - bkgd;
					ymod = ymod*weight;

					/*	       *sum1+=(xx2+yy2)*ymod;*/
					/*	       *sum2+=(xx2-yy2)*ymod;*/
					sumxt+=ymod*j;
					sumyt+=ymod*i;
					sumxxt += xx2*ymod;
					sumyyt += yy2*ymod;
					sumxyt += xx*yy*ymod;
					sumt += ymod;

				}
			}
		} /* Columns */
	} /* Rows */

	*sum=sumt;*sumx=sumxt;*sumy=sumyt;*sumxx=sumxxt;*sumyy=sumyyt;*sumxy=sumxyt;

	/*   *sum1=sum1t; *sum2=sum2t; */
	*sum1 = *sum2 = 0;


	if(*sum <= 0) {
		return(-1);
	} else {
		return(0);
	}
}

/* calcerr */

void
calcerr(float xcen, float ycen,		/* centre of object */
		int ix1, int ix2, int iy1, int iy2, /* bounding box to consider */
		float bkgd,			/* data's background level */
		int interpflag,			/* interpolate within pixels? */
		float w1, float w2, float ww12,	/* weights */
		float sumxx, float sumyy, float sumxy, /* quadratic sums */
		double sum1, double sum2,
		float *errxx, float *erryy, float *errxy, /* errors in sums */
		double *sums4,			/* ?? */
		double *s1, double *s2,
		float *data, int nx, int ny)		/* the data */
{   
	int i,j;
	double sxx = 0, sxy = 0, syy = 0, s4 = 0; /* unalias err{xx,xy,yy}, sums4 */
	float xx,xx2,yy,yy2;
	float xl,xh,yl,yh;
	float tmp;
	float tmod,ymod;
	float xxx,yyy;
	float weight;
	float expon;
	double sum3,sum4;

	sum3=sum4=0;
	for(i = iy1;i <= iy2;i++) {
		yy = i-ycen;
		yy2 = yy*yy;
		if(interpflag) {
			yl = yy - 0.375;
			yh = yy + 0.375;
		}
		for(j = ix1;j <= ix2;j++) {
			xx = j-xcen;

			/* Should we interpolate? */
			if(interpflag) {
				xl = xx - 0.375;
				xh = xx + 0.375;

				expon = xl*xl*w2 + yl*yl*w1 - 2.*xl*yl*ww12;
				tmp = xh*xh*w2 + yh*yh*w1 - 2.*xh*yh*ww12;
				if(tmp > expon) {
					expon = tmp;
				}

				tmp = xl*xl*w2 + yh*yh*w1 - 2.*xl*yh*ww12;
				if(tmp > expon) {
					expon = tmp;
				}

				tmp = xh*xh*w2 + yl*yl*w1 - 2.*xh*yl*ww12;
				if(tmp > expon) {
					expon = tmp;
				}

				if(expon <= 9.0) {
					tmod = *(&data[0] + i*nx + j) - bkgd;
					for(yyy = yl;yyy <= yh;yyy+=0.25) {
						yy2 = yyy*yyy;
						for(xxx = xl; xxx <= xh; xxx += 0.25) {
							xx2 = xxx*xxx;
							expon = xx2*w2 + yy2*w1 - 2*xxx*yyy*ww12;
							weight = exp(-0.5*expon);
							ymod = tmod*weight;
							/*		     sum3+=pow(weight*(xx2+yy2 - sum1),2);*/
							/*		     sum4+=pow(weight*(xx2-yy2 - sum2),2);*/
							sum3+=pow(weight*(xx2-yy2 - sum1*(xx2+yy2)),2);
							sum4+=pow(weight*(xxx*yyy - sum2*(xx2+yy2)),2);
							sxx += pow(weight*(xx2 - sumxx),2);
							syy += pow(weight*(yy2 - sumyy),2);
							sxy += pow(weight*(xxx*yyy - sumxy),2);
							s4 += expon*expon*ymod;
						}
					}
				}

				/* Not interpolating */
			} else {
				xx2 = xx*xx;
				expon = xx2*w2 + yy2*w1 - 2.*xx*yy*ww12;
				if(expon <= 9.0) {
					weight = exp(-0.5*expon);
					ymod = *(&data[0] + i*nx + j) - bkgd;
					ymod = ymod*weight;

					/*	       sum3+=pow(weight*(xx2+yy2 - sum1),2);*/
					/*	       sum4+=pow(weight*(xx2-yy2 - sum2),2);*/
					sum3+=pow(weight*(xx2-yy2 - sum1*(xx2+yy2)),2);
					sum4+=pow(weight*(xx*yy - sum2*(xx2+yy2)),2);
					sxx += pow(weight*(xx2 - sumxx),2);
					syy += pow(weight*(yy2 - sumyy),2);
					sxy += pow(weight*(xx*yy - sumxy),2);
					s4 += expon*expon*ymod;
				}
			}	       
		}
	}
	/*
	 * Pack up desired results
	 */
	*errxx = sxx; *erryy = syy; *errxy = sxy; *sums4 = s4; *s1=sum3; *s2=sum4;
}


