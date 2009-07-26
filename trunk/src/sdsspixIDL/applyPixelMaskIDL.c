/* 
   Modified from Ryan Scranton's w(theta) code to just apply the mask to input
   lambda/eta points.  Also, added routines for edge checking; the user inputs
   points and maximum angles.  Then all the area inside a circle of that radius
   surrounding the object is checked against the mask. 
   Erin Sheldon 23-May-2003

   -Syntax: maskFlags = sdsspix_mask(lambda, eta, maskFile, 
   maxAngle=, status=, /verbose)

   Ryan's comments:
   This code is intended to calculate w(theta) from a dataset and a mask file.
   The dataset needs to be two columns (ostensibly RA and DEC).  The mask
   file should contain coordinates for the upper left and lower right corners
   of the rectangular areas that have been cut out of the survey area for one
   reason or another.  Thus, the mask file should have four columns:

   RAmin DECmax RAmax DECmin

   The first line of the mask file should contain the coordinates for the area 
   that the survey covers in the same fashion. The data file and mask file 
   need to be given in the command line; usage is 

   calc_wtheta datafile maskfile

*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_vector_int.h>
#include <gsl/gsl_matrix_long.h>
#include <gsl/gsl_vector_ulong.h>
#include <gsl/gsl_vector_char.h>
#include <gsl/gsl_integration.h>
#include <gsl/gsl_sort_vector_int.h>
#include <gsl/gsl_sort_vector_long.h>
#include <gsl/gsl_spline.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_math.h>

#include "sdsspix.c"
#include "idl_export.h"

#include "applyPixelMaskIDL.h"
#include "applyPixelMaskUtil.h"
#include "applyPixelMaskUtil.c"

#define FLAGS_MASKED       0x1
#define FLAGS_QUAD1_MASKED 0x2
#define FLAGS_QUAD2_MASKED 0x4
#define FLAGS_QUAD3_MASKED 0x8
#define FLAGS_QUAD4_MASKED 0x10

#define FLAGS_QUAD1_MASKED_MONTE 0x20  /* Quadrant masked, monte carlo */
#define FLAGS_QUAD2_MASKED_MONTE 0x40
#define FLAGS_QUAD3_MASKED_MONTE 0x80
#define FLAGS_QUAD4_MASKED_MONTE 0x100

/* Minimum size we want to resolve 
   in square degrees */
#define Amin 0.05
/* Chance of missing Amin sized region */
#define Pmax 0.02


typedef struct {
    double lammin, lammax, area, total_gal, total_rand;
    long n_gal;
} iter_struct;

typedef struct {
    double lammin, lammax, etamin, etamax;
    int stripe;
} stripe_struct;

typedef struct {
    int n_stripe;
    unsigned long n_gal;
    stripe_struct *stripe_bound;
    iter_struct *iter_bound;
    double lammin, lammax, etamin, etamax;
} bbox_struct;

unsigned long n_gal, n_masks;
unsigned long n_bbox;
bbox_struct *bbox;
superpixnum_struct *mask_struct;
gsl_vector_ulong *mask_superpixnum_array;
gsl_integration_workspace *w;
gsl_interp_accel *acc;
//gsl_rng *mt19937_rand;
double survey_area;
unsigned long n_superpix;
int superpix_resolution, pixel_resolution;

/* Structure that holds the input keywords.  See htmIntersectIDL.h */
KW_RESULT kw;

IDL_VPTR applyPixelMask(int argc, IDL_VPTR *argv, char *argk)
{
    extern unsigned long n_gal, n_masks;
    extern unsigned long n_bbox;
    extern bbox_struct *bbox;
    extern gsl_vector_ulong *mask_superpixnum_array;
    extern gsl_integration_workspace *w;
    extern gsl_interp_accel *acc;
    extern gsl_rng *mt19937_rand;
    extern int superpix_resolution;
    double bins_per_decade;
    double unit_double;
    double lammin, lammax, etamin, etamax;
    gsl_vector *fixed_theta_array;
    gsl_vector_ulong *mask_pixnum_array;
    gsl_vector_int *mask_resolution_array;
    gsl_vector_int *stripe_array;

    /* function prototypes */
    void CalculateActualArea();
    /* E.S.S. */
    int ApplyEdgeMask(double *lambdav, double *etav, float *maxAngle, 
            unsigned long n_gal, 
            IDL_LONG *maskflags);

    FILE *MaskFile;
    unsigned long n_fixed_thetabins;
    unsigned long i,c,j,k;
    unsigned long bbox_finder, n_masks_old;
    unsigned long pixnum, n_stripe;
    int resolution;

    /******************* My variables E.S.S. ******************/

    /* Input IDL_VARIABLES */
    IDL_VPTR lambdaVptr, etaVptr, maskFileVptr;

    /* Pointers to arrays */
    double *lambdav, *etav;
    char * maskFile;

    IDL_MEMINT numLam, numEta;

    /* Optional keyword */
    float  *maxAngle;
    IDL_MEMINT nMaxAngle;
    short check_edge;

    /* Keyword */
    short verbose;

    /* Local variables */
    int retval;
    short *is_unmaskedv;
    unsigned long n_unmasked;

    /* output variable */
    IDL_VPTR maskFlagsVptr;
    IDL_LONG *maskflags;

    /* For creating the output variable */
    IDL_ARRAY_DIM dim;
    IDL_MEMINT n_dim;

    char errMessage[100];

    /***************** Process keywords *****************/
    (void) IDL_KWProcessByOffset(argc, argv, argk, kw_pars, 
            (IDL_VPTR *) 0, 1, &kw);

    /* Check number of args */
    if (applyPixelMaskNParams(argc) < 3) 
    {
        applyPixelMaskErrOut("-Syntax: maskFlags = sdsspix_mask(lambda, eta, maskFile, maxAngle=, status=, /verbose)", maskFlagsVptr=IDL_Gettmp(), FAILURE);
        return(maskFlagsVptr);
    }


    /* Copy the input variable pointers */
    lambdaVptr = argv[0];
    etaVptr = argv[1];
    maskFileVptr = argv[2];


    /********************* Extract pointers to lambda/eta *********************/
    if (!getDblPtr(lambdaVptr, &lambdav, &numLam))
    { 
        applyPixelMaskErrOut("Lambda must be of type DOUBLE", 
                maskFlagsVptr=IDL_Gettmp(), FAILURE);
        return(maskFlagsVptr);
    }

    if (!getDblPtr(etaVptr, &etav, &numEta))
    { 
        applyPixelMaskErrOut("Eta must be of type DOUBLE", 
                maskFlagsVptr=IDL_Gettmp(), FAILURE);
        return(maskFlagsVptr);
    }
    /* Do they have the same length? */
    if (numLam != numEta)
    {
        applyPixelMaskErrOut("Lambda and Eta must be the same length",
                maskFlagsVptr=IDL_Gettmp(), FAILURE);
        return(maskFlagsVptr);
    }

    n_gal = numLam;

    /********************* Extract the file name *********************/
    if (maskFileVptr->type != IDL_TYP_STRING)
    {
        applyPixelMaskErrOut("Mask Filename must be a string",
                maskFlagsVptr=IDL_Gettmp(), FAILURE);
        return(maskFlagsVptr);
    }
    else 
        maskFile = maskFileVptr->value.str.s;

    /******************* Maximum angle for edge checking? *******************/
    retval = getMaxAngle(numLam, &maxAngle, &nMaxAngle);
    if (retval == 2)
    {
        applyPixelMaskErrOut("maxAngle must be same length as lambda/eta",
                maskFlagsVptr=IDL_Gettmp(), FAILURE);
        return(maskFlagsVptr);
    }
    else if (retval == 1)
    {
        applyPixelMaskErrOut("maxAngle must be type FLOAT",
                maskFlagsVptr=IDL_Gettmp(), FAILURE);
        return(maskFlagsVptr);
    }
    else if (nMaxAngle != 0)
        check_edge = 1;
    else
        check_edge = 0;

    /* Should we print informative messages? */
    verbose = 0;
    if (kw.verbose_there) verbose = kw.verbose;

    /* make sure verbose is boolean */
    verbose = (verbose >= 1) ? 1 : 0;

    /********************* Open the file *********************/
    if ( (MaskFile = fopen(maskFile,"r")) == NULL)
    {
        sprintf(errMessage,"Cannot open mask file %s",maskFile);
        applyPixelMaskErrOut(errMessage,
                maskFlagsVptr=IDL_Gettmp(), FAILURE);
        return(maskFlagsVptr);
    }

    /*-------------------------------------------------------------*/
    /* Beyond here its mostly Ryan's stuff until calling ApplyMask */
    /*-------------------------------------------------------------*/

    assign_parameters();

    //gsl_rng_env_setup();

    //gsl_rng_default_seed = time(NULL);

    //T = gsl_rng_default;
    //mt19937_rand = gsl_rng_alloc(T);
    w = gsl_integration_workspace_alloc(1000);
    acc = gsl_interp_accel_alloc();

    superpix_resolution = 4;

    /* 
       Setting up the number of pixels in each axis and the number of bins for
       theta 
       */

    n_fixed_thetabins = 50;
    bins_per_decade = 6.0;

    fixed_theta_array = gsl_vector_alloc(n_fixed_thetabins);


    for (i=0,unit_double=-3.0*bins_per_decade;i<n_fixed_thetabins;i++) {
        fixed_theta_array->data[i] = pow(10.0,unit_double/bins_per_decade);
        unit_double += 1.0;
    }

    /* 
       Now we read through the the mask file to find out how many masks 
       (subtracting one for the survey boundary on the first line of the mask file) 
       */

    /* 
     * Deal with the Mask File
     */

    n_masks=0;
    while ((c = getc(MaskFile)) != EOF) {
        if (c == '\n') n_masks++;
    }

    if (verbose) printf("There are %lu masks in %s\n",n_masks,maskFile);

    rewind(MaskFile);

    n_stripe = 0;
    bbox_finder = 1;
    n_masks_old = n_masks;
    while ((bbox_finder == 1) && (n_stripe < n_masks_old)) {
        fscanf(MaskFile,"%lu %i\n", &pixnum, &resolution);
        if (resolution < 0) {
            n_stripe++;
            n_masks--;
        } else {
            bbox_finder = 0;
        }
    }

    rewind(MaskFile);

    if (verbose) printf("Found %ld stripes in %s\n",n_stripe,maskFile);

    stripe_array = gsl_vector_int_alloc(n_stripe);

    for (i=0;i<n_stripe;i++)
        fscanf(MaskFile,"%i %i\n",&stripe_array->data[i],&resolution);

    gsl_sort_vector_int(stripe_array);

    n_bbox = 1;

    for (i=1;i<n_stripe;i++) {
        if (stripe_array->data[i] > stripe_array->data[i-1]+1) n_bbox++;
    }

    if (!(bbox=malloc(n_bbox*sizeof(bbox_struct)))) {
        printf("Couldn't allocate bbox_struct memory...\n");
        exit(1);
    }

    if (verbose) printf("Found %lu bounding regions...\n",n_bbox);

    for (i=0;i<n_bbox;i++) bbox[i].n_stripe = 1;

    j = 0;
    for (i=1;i<n_stripe;i++) {
        if (stripe_array->data[i] == stripe_array->data[i-1]+1) {
            bbox[j].n_stripe++;
        } else {
            j++;
        }
    }

    for (i=0;i<n_bbox;i++) {
        if (!(bbox[i].stripe_bound=
                    malloc(bbox[i].n_stripe*sizeof(stripe_struct)))) {
            printf("Couldn't allocate stripe_struct memory...\n");
            exit(1);
        }
    }

    j = k = 0;
    bbox[0].stripe_bound[k].stripe = stripe_array->data[0];
    for (i=1;i<n_stripe;i++) {
        if (stripe_array->data[i] == stripe_array->data[i-1]+1) {
            k++;
            bbox[j].stripe_bound[k].stripe = stripe_array->data[i];
        } else {
            j++;
            k = 0;
            bbox[j].stripe_bound[k].stripe = stripe_array->data[i];
        }
    }


    for (i=0;i<n_bbox;i++) {
        if (verbose) printf("BBOX %lu:\n\t",i+1);
        primary_bound(bbox[i].stripe_bound[0].stripe,
                &lammin,&lammax,&etamin,&etamax);
        bbox[i].stripe_bound[0].lammin = lammin; 
        bbox[i].stripe_bound[0].lammax = lammax; 
        bbox[i].stripe_bound[0].etamin = etamin; 
        bbox[i].stripe_bound[0].etamax = etamax; 
        bbox[i].lammin = lammin;
        bbox[i].lammax = lammax;
        bbox[i].etamin = etamin;
        bbox[i].etamax = etamax;
        for (j=0;j<bbox[i].n_stripe;j++) {
            if (verbose) printf("%i ",bbox[i].stripe_bound[j].stripe);
            primary_bound(bbox[i].stripe_bound[j].stripe,
                    &lammin,&lammax,&etamin,&etamax);
            bbox[i].stripe_bound[j].lammin = lammin; 
            bbox[i].stripe_bound[j].lammax = lammax; 
            bbox[i].stripe_bound[j].etamin = etamin; 
            bbox[i].stripe_bound[j].etamax = etamax; 
            if (lammax > bbox[i].lammax) bbox[i].lammax = lammax;
            if (lammin < bbox[i].lammin) bbox[i].lammin = lammin;
            if (etamax > bbox[i].etamax) bbox[i].etamax = etamax;
            if (etamin < bbox[i].etamin) bbox[i].etamin = etamin;
        }
        if (verbose) printf("\n");
        bbox[i].n_gal = 0;
    }

    mask_pixnum_array = gsl_vector_ulong_alloc(n_masks);
    mask_resolution_array = gsl_vector_int_alloc(n_masks);

    for (i=0;i<n_masks;i++) 
        fscanf(MaskFile,"%lu %i\n",&mask_pixnum_array->data[i],
                &mask_resolution_array->data[i]);

    fclose(MaskFile);

    n_superpix = find_n_superpix(superpix_resolution, mask_pixnum_array, 
            mask_resolution_array, n_masks);

    if (verbose) printf("%ld masks span %li superpixels...\n",
            n_masks,n_superpix);

    if (!(mask_struct=malloc(n_superpix*sizeof(superpixnum_struct)))) {
        printf("Couldn't allocate superpixnum_struct memory...\n");
        exit(1);
    }

    mask_superpixnum_array = gsl_vector_ulong_alloc(n_superpix);

    make_superpix_struct(superpix_resolution,mask_pixnum_array,
            mask_resolution_array,n_masks,mask_struct,n_superpix);

    for (i=0;i<n_superpix;i++) {
        mask_superpixnum_array->data[i] = mask_struct[i].superpixnum;
    }

    gsl_vector_ulong_free(mask_pixnum_array);
    gsl_vector_int_free(mask_resolution_array);

    /************************ Begin my stuff E.S.S. ************************/

    /* This is only calculated if the user sent a named variable through
       the area keyword */

    applyPixelMaskSetArea();

    /* Create space for our output variable */
    n_dim=1;
    dim[0] = n_gal;
    maskflags = 
        (IDL_LONG *) IDL_MakeTempArray(IDL_TYP_LONG, n_dim, dim, 
                IDL_ARR_INI_ZERO, &maskFlagsVptr);

    /* Should we look for edges? */
    if (check_edge==1)
    {
        if (verbose) printf("\nApplying the mask with edge checking\n");
        ApplyEdgeMask(lambdav, etav, maxAngle, n_gal, maskflags);
    }
    else 
        /* Don't look for edges, just check the mask */
    {
        if (verbose) printf("\nApplying the mask\n");
        if (!(is_unmaskedv = calloc(n_gal, sizeof(short))))
        {
            applyPixelMaskErrOut("Cannot allocate is_unmaskedv", 
                    maskFlagsVptr=IDL_Gettmp(), FAILURE);
            return(maskFlagsVptr);
        }

        ApplyMask(lambdav, etav, n_gal, is_unmaskedv, &n_unmasked);

        /* masked? */
        for(i=0;i<n_gal;i++)
        {
            if(! is_unmaskedv[i]) maskflags[i] |= FLAGS_MASKED;
        }

        free(is_unmaskedv);
    }

    /* Clean up the keyword info */
    IDL_KW_FREE;

    applyPixelMaskSetStatus(SUCCESS);

    return(maskFlagsVptr);
}

int ApplyEdgeMask(double *lambdav, double *etav, float *maxAngle, 
        unsigned long n_gal, 
        IDL_LONG *maskflags)
{

    unsigned long i, j, k, n, q;
    int bbox_num, stripe_num, is_unmasked;
    unsigned long pixnum, ilo, jlo, n_unmasked;

    /* global variables */
    extern bbox_struct *bbox;
    extern superpixnum_struct *mask_struct;
    extern int superpix_resolution;
    extern gsl_vector_ulong *mask_superpixnum_array;
    extern unsigned long n_superpix;
    extern double deg2Rad, rad2Deg;
    double psi, *circle_eta, *circle_lambda;
    short secquad[8], secflags[8], quadflags[4];
    short *is_unmaskedv;

    int ApplyMask(double *lambdav, double *etav, unsigned long n_gal, 
            short *is_unmaskedv, unsigned long *n_unmasked);


    int getCircleLamEta(double *lambdav, double *etav, float *maxAngle,
            unsigned long n_gal, float psi, short quadrant, 
            double *circle_lambda, double *circle_eta);
    int getRandLamEta(double lambda, double eta, float R, short quadrant, 
            double *rand_lambda, 
            double *rand_eta);

    double Pmiss, A, R;
    double tmp;
    long nrand, iter_count;
    double RAND_LAMBDA, RAND_ETA;

    /* quadrant for each point */
    secquad[0] = 0; secquad[1] = 0; secquad[2] = 0;
    secquad[3] = 1; secquad[4] = 1;
    secquad[5] = 2; secquad[6] = 2;
    secquad[7] = 3;

    /* flags for each section */

    secflags[0] = FLAGS_QUAD1_MASKED + FLAGS_QUAD4_MASKED; 
    secflags[1] = FLAGS_QUAD1_MASKED; 
    secflags[2] = FLAGS_QUAD1_MASKED + FLAGS_QUAD2_MASKED;

    secflags[3] = FLAGS_QUAD2_MASKED; 
    secflags[4] = FLAGS_QUAD2_MASKED + FLAGS_QUAD3_MASKED;

    secflags[5] = FLAGS_QUAD3_MASKED; 
    secflags[6] = FLAGS_QUAD3_MASKED + FLAGS_QUAD4_MASKED;

    secflags[7] = FLAGS_QUAD4_MASKED;

    /* montecarlo flags for each quadrant */

    quadflags[0] =  FLAGS_QUAD1_MASKED + FLAGS_QUAD1_MASKED_MONTE;
    quadflags[1] =  FLAGS_QUAD2_MASKED + FLAGS_QUAD2_MASKED_MONTE;
    quadflags[2] =  FLAGS_QUAD3_MASKED + FLAGS_QUAD3_MASKED_MONTE;
    quadflags[3] =  FLAGS_QUAD4_MASKED + FLAGS_QUAD4_MASKED_MONTE;

    /* allocate memory */

    circle_lambda = calloc(n_gal, sizeof(double));
    circle_eta = calloc(n_gal, sizeof(double));
    is_unmaskedv = calloc(n_gal, sizeof(short));

    /**************************************************************/
    /* First check if the point itself is masked */
    /**************************************************************/

    /*printf("\nApplying the mask\n");*/
    ApplyMask(lambdav, etav, n_gal, is_unmaskedv, &n_unmasked);

    /* masked? */
    for(i=0;i<n_gal;i++)
    {
        if(! is_unmaskedv[i]) maskflags[i] |= FLAGS_MASKED;
        is_unmaskedv[i] = 0;
    }

    /**************************************************************/
    /* Check points in a circle maxAngle[i] in size */
    /**************************************************************/

    /* generate circle positions for each object and test
       against the mask */

    /*printf("\nChecking circle points against mask\n");*/
    for(i=0;i<8;i++) 
    {

        /* The angle */
        psi = M_PI*i/4.0;
        /* Generate the point on the circle for each galaxy */
        getCircleLamEta(lambdav, etav, maxAngle,
                n_gal, psi, secquad[i], 
                circle_lambda, circle_eta);

        /* Check them against the mask */
        ApplyMask(circle_lambda, circle_eta, n_gal, is_unmaskedv, &n_unmasked);
        for(j=0;j<n_gal;j++)
        {
            if (!is_unmaskedv[j]) maskflags[j] |= secflags[i];
            is_unmaskedv[j] = 0;
        }

    }

    /* 
       Now monte-carlo within each quadrant and check against mask. This
       check is for holes and anything missed by simple circle check
       above
       */

    /*printf("\nDoing Monte Carlo Tests\n");*/
    for (i=0;i<n_gal;i++)
    {

        /* loop over the quadrants and generate 
           random points */

        /* how many random points per quadrant? */
        R = maxAngle[i];
        A = M_PI*R*R/4.0;
        Pmiss = 1. - Amin/A;

        if(Pmiss > 1.e-10)
        {
            tmp = log10(Pmax)/log10(Pmiss);
            if (tmp < 20) tmp = 20;
            if (tmp > 10000) tmp = 10000;
            nrand = lround(tmp);
            /*
               if(nrand < 20) nrand=20;
               if(nrand > 10000) nrand = 10000;
               */
        }
        else 
        {
            nrand = 20;
        }

        /*printf("obj: %d  nrand: %d\n", i, nrand);*/

        /* loop over quadrants */
        for(q=0;q<4;q++)
        {

            /* go until find a masked point or reach max
               number of random points. First see if this
               quadrant is already masked */

            if ( (maskflags[i] & quadflags[q]) == 0 ) 
                is_unmasked=1;
            else 
                is_unmasked=0;

            /* no need to repeat this is already masked */
            if (is_unmasked)
            {
                iter_count = 0;
                while((is_unmasked==1) && (iter_count < nrand))
                {
                    /* generate a random point in this quadrant */

                    getRandLamEta(lambdav[i], etav[i], R, q, 
                            &RAND_LAMBDA, &RAND_ETA);

                    /*printf("%lf %lf\n", RAND_LAMBDA, RAND_ETA);*/

                    /* 
                     * Find out in which bounding box the point is in 
                     */

                    bbox_num = -1;
                    for (k=0;k<n_bbox;k++) 
                    {
                        /* Is it in this big bounding box? */
                        if ((RAND_LAMBDA <= bbox[k].lammax) && 
                                (RAND_LAMBDA >= bbox[k].lammin) &&
                                (RAND_ETA <= bbox[k].etamax) && 
                                (RAND_ETA >= bbox[k].etamin)) 
                        {

                            bbox_num = k;
                            /* Find the stripe it is in */
                            stripe_num = -1;
                            for (j=0;j<bbox[bbox_num].n_stripe;j++) 
                            {
                                if ((RAND_ETA <= bbox[bbox_num].stripe_bound[j].etamax) && 
                                        (RAND_ETA >= bbox[bbox_num].stripe_bound[j].etamin)) 
                                {
                                    stripe_num = j;

                                    /* Is it within the bounds of this stripe? */
                                    if ((RAND_LAMBDA <= bbox[bbox_num].stripe_bound[stripe_num].lammax) && 
                                            (RAND_LAMBDA >= bbox[bbox_num].stripe_bound[stripe_num].lammin)) 
                                    {
                                        /* Now check the masks */

                                        /* Find the superpixel it is in */
                                        ang2pix(superpix_resolution,RAND_LAMBDA,RAND_ETA,&pixnum);

                                        /* Is this superpixel in the mask? Binary Search */
                                        lhunt(mask_superpixnum_array,pixnum,&jlo);

                                        /* If the superpixel is in the mask, check higher resolutions 
                                           because it may still not be masked */
                                        if (jlo < n_superpix) 
                                        {
                                            /* Double check */
                                            if (pixnum == mask_superpixnum_array->data[jlo]) 
                                            {
                                                /* this is the case where a whole superpixel is masked */
                                                /* -> Assumption that there are no submaskes was bad, remove this */
                                                /*if (mask_struct[jlo].n_pixel == 1) 
                                                  {*/
                                                /* THIS POINT IS MASKED */
                                                /*  is_unmasked = 0;
                                                    }*/ 
                                                /* check higher resolutions */
                                                /*else 
                                                  {*/
                                                /* loop over the resolutions */
                                                for (n=0;n<mask_struct[jlo].n_res;n++) 
                                                {
                                                    /* Find pixel at this resolution */
                                                    ang2pix(mask_struct[jlo].res_struct[n].resolution,
                                                            RAND_LAMBDA,RAND_ETA,&pixnum);
                                                    /* case where there is only one pixel at this res */
                                                    if (mask_struct[jlo].res_struct[n].n_pixel == 1) 
                                                    {
                                                        ilo = 0;
                                                    } 
                                                    /* case where more than one pixel at this res */
                                                    else 
                                                    {
                                                        /* Binary Search */
                                                        lhunt(mask_struct[jlo].res_struct[n].pixnum,pixnum,&ilo);
                                                    }
                                                    /* Check if this pixel is masked */
                                                    if (ilo < mask_struct[jlo].res_struct[n].n_pixel) 
                                                    {
                                                        /* Double check */
                                                        if (mask_struct[jlo].res_struct[n].pixnum->data[ilo] == pixnum) 
                                                        {
                                                            /* THIS POINT IS MASKED */
                                                            is_unmasked = 0;
                                                        } /* double check */
                                                    } /* check if pixel found in mask */
                                                } /* loop over higher res */
                                                /*}*/ /* checking higher resolutions */
                                            } /* double check */
                                        } /* Was pixnum found in superpixel array? */

                                    } /* is it in this stripes lambda bounds? */
                                    else
                                    {
                                        /* THIS POINT IS MASKED */
                                        is_unmasked = 0;
                                    }

                                    /* jump out of stripe loop cause we found it */
                                    break;
                                } /* Is it in stripe eta bounds? */
                            } /* Loop over stripes in big bbox */
                            /* not found in a stripe */
                            if (stripe_num == -1) 
                            {
                                /* THIS POINT IS MASKED */
                                is_unmasked = 0;
                            }
                            /* jump out of bbox loop because we found it */
                            break;
                        } /* Is it in this big bounding box? */

                    } /* Loop over bounding boxes */
                    /* not found in a big bbox */
                    if (bbox_num == -1) 
                    {
                        /* THIS POINT IS MASKED */
                        is_unmasked = 0;
                    }

                    /* increment the iteration count. Will go until
                       iter_count == nrand */
                    iter_count++;

                } /* while not masked and less than nrand */

                /* if masked, set the flag */
                if (!is_unmasked) maskflags[i] |= quadflags[q];
            } /* initial check if already masked */

        } /* loop over quadrants */

    } /* Loop over objects */

    /*printf("Done with monte carlo\n");*/

    /* Free the memory */

    free(circle_lambda); 
    free(circle_eta); 
    free(is_unmaskedv); 

    return(0);


} /* ApplyEdgeMask */


/* Angles are generated in a coordinate system where x is lambda
   and y is eta (I know, its a mess) */
int getCircleLamEta(double *lambdav, double *etav, float *maxAngle,
        unsigned long n_gal, float psi, short quadrant, 
        double *circle_lambda, double *circle_eta)

{

    extern double deg2Rad, rad2Deg;
    unsigned long i;

    double cospsi;
    double theta,phi,sintheta,costheta,sinphi,cosphi;
    double theta2,costheta2,sintheta2;
    double phi2;
    double Dphi,cosDphi;
    double r,sinr,cosr;

    cospsi = cos(psi);

    for (i=0;i<n_gal;i++) 
    {

        /* generate eta/lambda for circle positions */

        /* special cases */

        if (psi == 0.0)
        {
            circle_lambda[i] = lambdav[i] + maxAngle[i];
            circle_eta[i] = etav[i];
        }
        else if ( fabs(psi-M_PI) < 0.001)
        {
            circle_lambda[i] = lambdav[i] - maxAngle[i];
            circle_eta[i] = etav[i];
        }
        else

        {
            /* [0,180] */
            theta = (90.0 - lambdav[i])*deg2Rad;
            /* [0,360] */
            phi   = (etav[i] + 180.0)*deg2Rad;

            r = maxAngle[i]*deg2Rad;

            sintheta = sin(theta);
            costheta = cos(theta);
            sinphi = sin(phi);
            cosphi = cos(phi);

            sinr = sin(r);
            cosr = cos(r);

            costheta2 = costheta*cosr + sintheta*sinr*cospsi;
            if (costheta2 < -1.) costheta2 = -1.;
            if (costheta2 >  1.) costheta2 =  1.;
            theta2 = acos(costheta2);
            sintheta2 = sin(theta2);

            cosDphi = (cosr - costheta*costheta2)/(sintheta*sintheta2);
            /*if (isnan(cosDphi)) printf("Doh!!\n");*/

            if (cosDphi < -1.) cosDphi = -1.;
            if (cosDphi >  1.) cosDphi =  1.;
            Dphi = acos(cosDphi);


            switch(quadrant)
            {
                case 0: 
                    phi2 = phi + Dphi;
                    break;
                case 1: 
                    phi2 = phi + Dphi;
                    break;
                case 2: 
                    phi2 = phi - Dphi;
                    break;
                case 3: 
                    phi2 = phi - Dphi;
                    break;
                default: 
                    printf("Error: quadrant is undefined: %d\n",quadrant);
                    return(0);
            }

            circle_lambda[i] = 90.0 - rad2Deg*theta2 ;
            circle_eta[i] = rad2Deg*phi2 - 180.0;
        }
    }
    return(1);

} /* getCirclLamEta */

int getRandLamEta(double lambda, 
        double eta, 
        float R,             /* in degrees */
        short quadrant, 
        double *rand_lambda, 
        double *rand_eta)

{

    extern double deg2Rad, rad2Deg;

    double cospsi;
    double theta,phi,sintheta,costheta,sinphi,cosphi;
    double theta2,costheta2,sintheta2;
    double phi2;
    double Dphi,cosDphi;
    double sinr,cosr;
    float min_theta;

    double rand_r, rand_psi;

    /* generate uniformly in R^2 */
    rand_r = gsl_rng_uniform(mt19937_rand);
    rand_r = sqrt(rand_r)*R*deg2Rad;

    /* generate theta uniformly */
    min_theta = quadrant*M_PI/2.;
    rand_psi = M_PI/2.*gsl_rng_uniform(mt19937_rand) + min_theta;

    cospsi = cos(rand_psi);

    /* special cases */

    /* [0,180] */
    theta = (90.0 - lambda)*deg2Rad;
    /* [0,360] */
    phi   = (eta + 180.0)*deg2Rad;

    sintheta = sin(theta);
    costheta = cos(theta);
    sinphi = sin(phi);
    cosphi = cos(phi);

    sinr = sin(rand_r);
    cosr = cos(rand_r);

    costheta2 = costheta*cosr + sintheta*sinr*cospsi;
    if (costheta2 < -1.) costheta2 = -1.;
    if (costheta2 >  1.) costheta2 =  1.;
    theta2 = acos(costheta2);
    sintheta2 = sin(theta2);

    cosDphi = (cosr - costheta*costheta2)/(sintheta*sintheta2);

    if (cosDphi < -1.) cosDphi = -1.;
    if (cosDphi >  1.) cosDphi =  1.;
    Dphi = acos(cosDphi);

    switch(quadrant)
    {
        case 0: 
            phi2 = phi + Dphi;
            break;
        case 1: 
            phi2 = phi + Dphi;
            break;
        case 2: 
            phi2 = phi - Dphi;
            break;
        case 3: 
            phi2 = phi - Dphi;
            break;
        default: 
            printf("Error: quadrant is undefined: %d\n",quadrant);
            return(0);
    }

    *rand_lambda = 90.0 - rad2Deg*theta2;
    *rand_eta    = rad2Deg*phi2 - 180.0;

    return(1);
} /* getRandLamEta */


int ApplyMask(double *lambdav, double *etav, unsigned long n_gal, 
        short *is_unmaskedv, unsigned long *n_unmasked)
{

    double LAM,ETA;
    unsigned long i, j, k, n;
    int bbox_num, stripe_num, is_unmasked;
    unsigned long pixnum, ilo, jlo;

    /* global variables */
    extern bbox_struct *bbox;
    extern superpixnum_struct *mask_struct;
    extern int superpix_resolution;
    extern gsl_vector_ulong *mask_superpixnum_array;
    extern unsigned long n_superpix;

    *n_unmasked = 0;
    for (i=0;i<n_gal;i++)
    {

        LAM = lambdav[i];
        ETA = etav[i];

        /* 
         * Find out in which bounding box each
         * object is
         */
        is_unmasked = 1;
        bbox_num = -1;
        for (k=0;k<n_bbox;k++) 
        {
            /* Is it in this big bounding box? */
            if ((LAM <= bbox[k].lammax) && 
                    (LAM >= bbox[k].lammin) &&
                    (ETA <= bbox[k].etamax) && 
                    (ETA >= bbox[k].etamin)) 
            {

                bbox_num = k;
                /* Find the stripe it is in */
                /* find which stripe it is in */
                stripe_num = -1;
                for (j=0;j<bbox[bbox_num].n_stripe;j++) 
                {
                    if ((ETA <= bbox[bbox_num].stripe_bound[j].etamax) && 
                            (ETA >= bbox[bbox_num].stripe_bound[j].etamin)) 
                    {
                        stripe_num = j;

                        /* Is it within the bounds of this stripe? */
                        if ((LAM <= bbox[bbox_num].stripe_bound[stripe_num].lammax) && 
                                (LAM >= bbox[bbox_num].stripe_bound[stripe_num].lammin)) 
                        {
                            /* Now check the masks */

                            /* Find the superpixel it is in */
                            ang2pix(superpix_resolution,LAM,ETA,&pixnum);

                            /* Is this superpixel in the mask? Binary Search */
                            lhunt(mask_superpixnum_array,pixnum,&jlo);

                            /* If the superpixel is in the mask, check higher resolutions 
                               because it may still not be masked */
                            if (jlo < n_superpix) 
                            {
                                /* Double check */
                                if (pixnum == mask_superpixnum_array->data[jlo]) 
                                {
                                    /* this is the case where a whole superpixel is masked */
                                    /* -> Assumption that there are no submaskes was bad, remove this */
                                    /*if (mask_struct[jlo].n_pixel == 1) 
                                      {*/
                                    /* THIS POINT IS MASKED */
                                    /*  is_unmasked = 0;
                                        }*/ 
                                    /* check higher resolutions */
                                    /*else 
                                      {*/
                                    /* loop over the resolutions */
                                    for (n=0;n<mask_struct[jlo].n_res;n++) 
                                    {
                                        /* Find pixel at this resolution */
                                        ang2pix(mask_struct[jlo].res_struct[n].resolution,
                                                LAM,ETA,&pixnum);
                                        /* case where there is only one pixel at this res */
                                        if (mask_struct[jlo].res_struct[n].n_pixel == 1) 
                                        {
                                            ilo = 0;
                                        } 
                                        /* case where more than one pixel at this res */
                                        else 
                                        {
                                            /* Binary Search */
                                            lhunt(mask_struct[jlo].res_struct[n].pixnum,pixnum,&ilo);
                                        }
                                        /* Check if this pixel is masked */
                                        if (ilo < mask_struct[jlo].res_struct[n].n_pixel) 
                                        {
                                            /* Double check */
                                            if (mask_struct[jlo].res_struct[n].pixnum->data[ilo] == pixnum) 
                                            {
                                                /* THIS POINT IS MASKED */
                                                is_unmasked = 0;
                                            } /* double check */
                                        } /* check if pixel found in mask */
                                    } /* loop over higher res */
                                    /*}*/ /* checking higher resolutions */
                                } /* double check */
                            } /* Was pixnum found in superpixel array? */

                        } /* is it in this stripes lambda bounds? */
                        else
                        {
                            /* THIS POINT IS MASKED */
                            is_unmasked = 0;
                        }

                        /* jump out of stripe loop cause we found it */
                        break;
                    } /* Is it in stripe eta bounds? */
                } /* Loop over stripes in big bbox */
                /* not found in a stripe */
                if (stripe_num == -1) 
                {
                    /* THIS POINT IS MASKED */
                    is_unmasked = 0;
                }
                /* jump out of bbox loop because we found it */
                break;
            } /* Is it in this big bounding box? */

        } /* Loop over bounding boxes */
        /* not found in a big bbox */
        if (bbox_num == -1) 
        {
            /* THIS POINT IS MASKED */
            is_unmasked = 0;
        }
        /* Copy in data */
        is_unmaskedv[i] = is_unmasked;
        *n_unmasked += is_unmasked;

    } /* Loop over objects */

    return(1);

} /* ApplyMask */


void CalculateActualArea()
{
    int area_resolution, n_dropped, masked, n_partial, n_check;
    gsl_vector_ulong *pixnum_array;
    double lammin, lammax, area, unmasked_area, tarea;
    unsigned long i, j, k, n, m, jlo, ilo, n_pixel, x_min, x_max,y_min,y_max;
    unsigned long pixnum, nx, ny;

    area_resolution = 256;
    nx = nx0*area_resolution;
    ny = ny0*area_resolution;

    printf("Calculating unmasked area for each section...\n");

    for (n=0;n<n_bbox;n++) {

        tarea = 0.0;
        n_pixel = 0;

        for (i=0;i<bbox[n].n_stripe;i++) {
            lammin = bbox[n].stripe_bound[i].lammin;
            lammax = bbox[n].stripe_bound[i].lammax;

            area_index(area_resolution,lammin,lammax,
                    bbox[n].stripe_bound[i].etamin,
                    bbox[n].stripe_bound[i].etamax,
                    &x_min,&x_max,&y_min,&y_max);
            n_pixel += (x_max - x_min + 1)*(y_max - y_min + 1);
        }

        pixnum_array = gsl_vector_ulong_alloc(n_pixel);

        m = 0;
        for (k=0;k<bbox[n].n_stripe;k++) {

            lammin = bbox[n].stripe_bound[k].lammin;
            lammax = bbox[n].stripe_bound[k].lammax;

            area_index(area_resolution,lammin,lammax,
                    bbox[n].stripe_bound[k].etamin,
                    bbox[n].stripe_bound[k].etamax,
                    &x_min,&x_max,&y_min,&y_max);
            for (j=y_min;j<=y_max;j++) {
                for (i=x_min;i<=x_max;i++) {
                    pixnum_array->data[m] = nx*j + i;
                    m++;
                }
            }
        }

        gsl_sort_vector_ulong(pixnum_array);

        n_dropped = n_partial = n_check = 0;

        for (m=0;m<n_pixel;m++) {
            area = pix_area(area_resolution,pixnum_array->data[m]);
            unmasked_area = 1.0;

            superpix(area_resolution,pixnum_array->data[m],
                    superpix_resolution,&pixnum);

            masked = 0;
            lhunt(mask_superpixnum_array,pixnum,&jlo);

            if (jlo <= n_superpix - 1) {
                n_check++;
                if (mask_superpixnum_array->data[jlo] == pixnum) {
                    if (mask_struct[jlo].n_pixel == 1) {
                        unmasked_area = 0.0;
                        n_dropped++;
                    } else {
                        for (k=0;k<mask_struct[jlo].n_res;k++) {
                            if (mask_struct[jlo].res_struct[k].resolution == 
                                    area_resolution) {
                                if (mask_struct[jlo].res_struct[k].n_pixel == 1) {
                                    ilo = 0;
                                } else {
                                    lhunt(mask_struct[jlo].res_struct[k].pixnum,
                                            pixnum_array->data[m],&ilo);
                                }
                                if (ilo <= mask_struct[jlo].res_struct[k].n_pixel-1) {
                                    if (mask_struct[jlo].res_struct[k].pixnum->data[ilo] ==
                                            pixnum_array->data[m]) {
                                        unmasked_area = 0.0;
                                        n_dropped++;
                                    }
                                }
                            }
                            if (mask_struct[jlo].res_struct[k].resolution < 
                                    area_resolution) {
                                superpix(area_resolution,pixnum_array->data[m],
                                        mask_struct[jlo].res_struct[k].resolution,&pixnum);
                                if (mask_struct[jlo].res_struct[k].n_pixel == 1) {
                                    ilo = 0;
                                } else {
                                    lhunt(mask_struct[jlo].res_struct[k].pixnum,pixnum,&ilo);
                                }
                                if (ilo <= mask_struct[jlo].res_struct[k].n_pixel-1) {
                                    if (mask_struct[jlo].res_struct[k].pixnum->data[ilo] == 
                                            pixnum) {
                                        unmasked_area = 0.0;
                                        n_dropped++;
                                    }
                                }
                            }
                            if (mask_struct[jlo].res_struct[k].resolution > 
                                    area_resolution) {
                                for (j=0;j<mask_struct[jlo].res_struct[k].n_pixel;j++) {
                                    superpix(mask_struct[jlo].res_struct[k].resolution,
                                            mask_struct[jlo].res_struct[k].pixnum->data[j],
                                            area_resolution,&pixnum);
                                    if (pixnum_array->data[m] == pixnum) {
                                        unmasked_area -= 
                                            1.0*area_resolution*area_resolution/
                                            (mask_struct[jlo].res_struct[k].resolution*
                                             mask_struct[jlo].res_struct[k].resolution);
                                        masked = 1;
                                    }
                                }
                            }
                        }     
                    }
                }
            }
            if (masked == 1) n_partial++;
            tarea += unmasked_area*area;
        }

        gsl_vector_ulong_free(pixnum_array);
        printf("BBOX: %lu Area: %lf square degrees\n", n+1,tarea); 

        survey_area += tarea;

    }
    printf("Survey Area: %lf square degrees\n", survey_area);
} /* end CalculateActualArea */




/*===========================================================================
 * How many positional arguments were sent?
 *===========================================================================*/

int applyPixelMaskNParams(int argc)
{

    int nKeywords;

    nKeywords = 
        kw.area_there + kw.status_there + kw.verbose_there + kw.maxAngle_there;

    return 
        argc - nKeywords;

}

void applyPixelMaskSetArea()
{

    if (kw.area_there)
    {
        /* If its a temporary variable, there is no reason to calculate this
           slow function */
        if (! (kw.area->flags & IDL_V_TEMP))
        {
            CalculateActualArea();
            /* Frees existing memory and creates a scalar */
            IDL_StoreScalarZero(kw.area, IDL_TYP_DOUBLE);
            kw.area->value.d = survey_area;
        }
    }

}


/* /////////////////////////////////////////////////////////
// Print error message, set status, and set output var
/////////////////////////////////////////////////////////*/

void applyPixelMaskErrOut(char *message, IDL_VPTR outVar, int statusVal)
{

    IDL_VPTR errVal;
    errVal = IDL_Gettmp();
    errVal->type = IDL_TYP_INT;
    errVal->value.i = -1;

    IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_INFO, message);

    if (kw.status_there)
        applyPixelMaskSetStatus(statusVal);

    IDL_VarCopy(errVal, outVar);

    /* Clean up the keyword info */
    IDL_KW_FREE;

}

/* set the status if it is there */
    static void
applyPixelMaskSetStatus(int statusVal)
{

    if (kw.status_there) {
        /* This frees any existing memory and sets type to INT with value zero */
        IDL_StoreScalarZero(kw.status, IDL_TYP_INT);
        kw.status->value.i = statusVal;
    }

}


/* Get the data from the maxAngle keyword. */

int getMaxAngle(int numLam, float **maxAngle, IDL_MEMINT *numAngle)
{

    if (kw.maxAngle_there)
    {
        if (kw.maxAngle->type == IDL_TYP_UNDEF)
        {
            *numAngle = 0;
            return(0);
        }

        /* This is an error */
        if (kw.maxAngle->type != IDL_TYP_FLOAT)
            return(1);

        IDL_VarGetData(kw.maxAngle, numAngle, (char **) maxAngle, TRUE);

        /* Must be same size as lambda */
        if (*numAngle != numLam)
            return(2);

        return(0);
    }
    else
    {
        *numAngle = 0;
        return(0);
    }

}



#define ARRLEN(arr) (sizeof(arr)/sizeof(arr[0]))

int IDL_Load(void)
{

    /* This must be static. It is a struct. */
    /* The name in strings is the name by which it will be called from IDL and
       MUST BE CAPITALIZED 
       5th parameter will say if it accepts keywords and some other flags 
       For more info see page 325 of external dev. guide */
    static IDL_SYSFUN_DEF2 func_addr[] = {
        { (IDL_SYSRTN_GENERIC) applyPixelMask, "SDSSPIX_MASK", 0, 
            IDL_MAXPARAMS, IDL_SYSFUN_DEF_F_KEYWORDS, 0},
    };

    /* False means it is not a function */
    return IDL_SysRtnAdd(func_addr, IDL_TRUE, ARRLEN(func_addr));

}
