#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <getopt.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_vector_ulong.h>
#include <gsl/gsl_matrix_long.h>
#include <gsl/gsl_vector_char.h>
#include <gsl/gsl_sort_vector.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_spline.h>
#include <gsl/gsl_integration.h>
#include "sdsspix.c"
#include "getopt.c"

typedef struct {
  double lammin, lammax, etamin, etamax;
  int stripe;
} stripe_struct;

typedef struct {
  unsigned long n_pixel;
  int stripe_min, stripe_max, n_stripe;
  gsl_vector_ulong *pixnum;
  gsl_vector *unmasked_area;
  gsl_vector_int *iter;
} section_struct;

typedef struct {
  int n_stripe, n_section;
  unsigned long n_pixel;
  gsl_vector_ulong *pixnum, *superpixnum;
  gsl_vector_int *stripe;
  gsl_vector *unmasked_area;
  stripe_struct *stripe_bound;
  section_struct *sec;
  double lammin, lammax, etamin, etamax, pix_area;
} bbox_struct;

unsigned long n_gal, n_masks, n_bbox, n_pixel;
bbox_struct *bbox;
superpixnum_struct *mask_struct;
gsl_vector_ulong *mask_superpixnum_array, *unique_pixnum_array;
gsl_vector_int *unique_iter_array;
double survey_area, jack_length;
gsl_rng *mt19937_rand;
unsigned long bbox_iter, n_superpix;
int superpix_resolution, pixel_resolution, verbose, n_jack, jack_width;
int force_jack, check_area;

int main(int argc, char *argv[])
{
  extern unsigned long n_masks;
  extern unsigned long n_bbox;
  gsl_vector_ulong *mask_pixnum_array;
  gsl_vector_int *mask_resolution_array;
  gsl_vector_int *stripe_array;
  const gsl_rng_type *T;
  void InitializePixels();
  void MaskPixels();
  void InitializeSections();
  void AssignIterArea();
  gsl_permutation *pixel_index;
  double lammin, lammax, etamin, etamax;
  unsigned long i,j,k,n,u;
  unsigned long bbox_finder, n_masks_old;
  unsigned long pixnum, n_stripe;
  FILE *MaskFile, *OutputFile;
  char c;
  int resolution,start_argv,section_iter, option_index, n_arg;
  static struct option long_options[] = 
    {{"silent",no_argument,&verbose,0},
     {"n_jack",required_argument,0,'a'},
     {"n_stripe",required_argument,0,'b'},
     {"resolution",required_argument,0,'c'},
     {"force",no_argument,0,'d'},
     {"check",no_argument,0,'e'},
     {0,0,0,0}};

  assign_parameters();
  superpix_resolution = 4;
  gsl_rng_env_setup();

  gsl_rng_default_seed = time(NULL);

  T = gsl_rng_default;
  mt19937_rand = gsl_rng_alloc(T);

  start_argv = 1;
  verbose = 1;
  
  n_jack = -1;
  jack_width = 1;
  pixel_resolution = 256;
  force_jack = 0;
  check_area = 0;

  while ((c = getopt_long(argc,argv,"a:b:c:de",
			  long_options,&option_index)) != -1) {
    switch(c) {
    case 'a':
      n_jack = atoi(optarg);
      jack_width = -1;
      start_argv += 2;
      break;
    case 'b':
      jack_width = atoi(optarg);
      n_jack = -1;
      start_argv += 2;
      break;
    case 'c':
      pixel_resolution = atoi(optarg);
      start_argv += 2;
      break;
    case 'd':
      force_jack = 1;
      start_argv++;
      break;
    case 'e':
      verbose = 1;
      check_area = 1;
      start_argv++;
      break;
    }
  }

  if (verbose == 0) start_argv++;

  n_arg = argc - start_argv;

  if (n_arg < 2) {
    printf("Usage: jackknife_stripe [flags] InputMask OutputFile\n");
    printf("There are five flags:\n");
    printf("\n");
    printf("--n_jack XX\n");
    printf("  Tells the code to try generating the map with XX samples.\n");
    printf("  The width is determined automatically.  The number of \n");
    printf("  samples won't be exactly XX, but it should be close.  By\n");
    printf("  default this is calculated based upon the n_stripe \n");
    printf("  setting.\n");
    printf("\n");
    printf("--n_stripe XX\n");
    printf("  Sets the width of the samples to XX stripes.  The number\n");
    printf("  of samples is more or less inversely proportional to the\n");
    printf("  width of the sample; the narrower the sample, the more \n");
    printf("  samples you get.  In the limiting case, samples one stripe\n");
    printf("  width gives you something like 1200 samples.\n");
    printf("  Default is 1.\n");
    printf("\n");
    printf("--resolution XXX\n");
    printf("  Uses a map with resolution XXX to make the samples.  \n");
    printf("  Higher resolution means that the area match between the\n");
    printf("  samples is done with better precision, but it's also alot\n");
    printf("  more pixels to handle.  Don't go higher than 256 or lower\n");
    printf("  than 4.  Default is 256\n");
    printf("\n");
    printf("--force\n");
    printf("  Forces the code to use the specified number of samples.\n");
    printf("  The code will still determine the optimum width of the\n");
    printf("  samples, but the exact number will be determined by the\n");
    printf("  user.  This may result in slightly askew sample shapes,\n");
    printf("  the area for each sample should still be nearly equal.\n");
    printf("\n");
    printf("--check\n");
    printf("  Displays the area in each sample, along with pixel\n");
    printf("  information.\n");
    exit(1);
  }

  MaskFile = fopen(argv[start_argv],"r");

  if ((jack_width > 0) && (n_jack > 0)) {
    printf("Can only specify width of samples or number of samples...\n");
    printf("Exiting.\n");
    exit(1);
  }

  while ((c = getc(MaskFile)) != EOF) {
    if (c == '\n') n_masks++;
  }
  
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
  
  if (verbose) 
    printf("Found %ld stripes in %s\n",n_stripe,argv[start_argv]);

  stripe_array = gsl_vector_int_alloc(n_stripe);

  for (i=0;i<n_stripe;i++)
    fscanf(MaskFile,"%i %i\n",&stripe_array->data[i],&resolution);

  gsl_sort_vector_int(stripe_array);

  n_bbox = 1;

  for (i=1;i<n_stripe;i++) {
    if ((stripe_array->data[i] < 50) || (stripe_array->data[i-1] < 50)) {
      if (stripe_array->data[i] > stripe_array->data[i-1]+1) n_bbox++;
    }
  }

  if (!(bbox=malloc(n_bbox*sizeof(bbox_struct)))) {
    printf("Couldn't allocate bbox_struct memory...\n");
    exit(1);
  }

  if (verbose) 
    printf("Found %lu bounding regions...\n",n_bbox);

  for (i=0;i<n_bbox;i++) {
    bbox[i].n_stripe = 1;
    bbox[i].n_pixel = 0;
  }

  j = 0;
  for (i=1;i<n_stripe;i++) {
    if ((stripe_array->data[i] < 50) || (stripe_array->data[i-1] < 50)) {
      if (stripe_array->data[i] == stripe_array->data[i-1]+1) {
        bbox[j].n_stripe++;
      } else {
        j++;
      }
    } else {
      bbox[j].n_stripe++;
    }
  }

  for (i=0;i<n_bbox;i++) {
    if (!(bbox[i].stripe_bound=
	  malloc(bbox[i].n_stripe*sizeof(stripe_struct)))) {
      printf("Couldn't allocate bbox_struct memory...\n");
      exit(1);
    }
  }

  j = k = 0;
  bbox[0].stripe_bound[k].stripe = stripe_array->data[0];
  for (i=1;i<n_stripe;i++) {
    if ((stripe_array->data[i] < 50) || (stripe_array->data[i-1] < 50)) {
      if (stripe_array->data[i] == stripe_array->data[i-1]+1) {
        k++;
        bbox[j].stripe_bound[k].stripe = stripe_array->data[i];
      } else {
        j++;
        k = 0;
        bbox[j].stripe_bound[k].stripe = stripe_array->data[i];
      }
    } else {
      k++;
      bbox[j].stripe_bound[k].stripe = stripe_array->data[i];
    }
  }


  for (i=0;i<n_bbox;i++) {
    if (verbose) printf("BBOX %lu:\n\t",i);
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
  }

  InitializePixels();

  if (n_masks > 0) {

    mask_pixnum_array = gsl_vector_ulong_alloc(n_masks);
    mask_resolution_array = gsl_vector_int_alloc(n_masks);
    
    for (i=0;i<n_masks;i++) 
      fscanf(MaskFile,"%lu %i\n",&mask_pixnum_array->data[i],
             &mask_resolution_array->data[i]);

    fclose(MaskFile);

    n_superpix = find_n_superpix(superpix_resolution, mask_pixnum_array, 
                                 mask_resolution_array, n_masks);

    if (verbose) 
      printf("%ld masks span %li superpixels...\n",n_masks,n_superpix);

    if (!(mask_struct=malloc(n_superpix*sizeof(superpixnum_struct)))) {
      printf("Couldn't allocate superpixnum_struct memory...\n");
      exit(1);
    }

    mask_superpixnum_array = gsl_vector_ulong_alloc(n_superpix);
    
    make_superpix_struct(superpix_resolution,mask_pixnum_array,
                         mask_resolution_array,n_masks,mask_struct,n_superpix);

    for (i=0;i<n_superpix;i++) 
      mask_superpixnum_array->data[i] = mask_struct[i].superpixnum;

    gsl_vector_ulong_free(mask_pixnum_array);
    gsl_vector_int_free(mask_resolution_array);

    MaskPixels();
  }

  survey_area = 0.0;
  
  for (n=0;n<n_bbox;n++) {
    for (j=0;j<bbox[n].n_pixel;j++) {
      survey_area += bbox[n].pix_area*bbox[n].unmasked_area->data[j];
    }
  }

  if (verbose) printf("Survey Area: %1.3lf square degrees...\n",survey_area);

  if (jack_width > 0) {
    for (n=0;n<n_bbox;n++) {
      bbox[n].n_section = bbox[n].n_stripe/jack_width;

      if (bbox[n].n_section*jack_width != bbox[n].n_stripe) 
	bbox[n].n_section++;
      
      if (bbox[n].n_section == 0) bbox[n].n_section = 1;
    }
  } else {

    jack_length = sqrt(survey_area/n_jack);

    jack_width = jack_length/2.5;

    if (jack_width == 0) jack_width = 1;

    for (n=0;n<n_bbox;n++) {
      bbox[n].n_section = bbox[n].n_stripe/jack_width;

      if (bbox[n].n_section*jack_width != bbox[n].n_stripe) {
	if (1.0*bbox[n].n_stripe > 
	    1.0*bbox[n].n_section*jack_width + 0.5*jack_width) {
	  bbox[n].n_section++;
	}
      }
    
      if (bbox[n].n_section == 0) bbox[n].n_section = 1;
    }

  }

  for (n=0;n<n_bbox;n++) {
    if (!(bbox[n].sec=malloc(bbox[n].n_section*sizeof(section_struct)))) {
      printf("Couldn't allocate section_struct memory...\n");
      exit(1);
    }

    k = 0;
    bbox[n].sec[0].stripe_min = bbox[n].stripe_bound[0].stripe; 
    bbox[n].sec[0].stripe_max = bbox[n].stripe_bound[0].stripe; 
    
    section_iter = 1;

    for (j=1;j<bbox[n].n_stripe;j++) {
      if (section_iter == jack_width) {
	k++;
	bbox[n].sec[k].stripe_min = bbox[n].stripe_bound[j].stripe; 
	bbox[n].sec[k].stripe_max = bbox[n].stripe_bound[j].stripe; 
	section_iter = 1;
      } else {
	if (bbox[n].sec[k].stripe_min > bbox[n].stripe_bound[j].stripe)
	  bbox[n].sec[k].stripe_min = bbox[n].stripe_bound[j].stripe;
	if (bbox[n].sec[k].stripe_max < bbox[n].stripe_bound[j].stripe)
	  bbox[n].sec[k].stripe_max = bbox[n].stripe_bound[j].stripe;
	section_iter++;
	if (k == bbox[n].n_section - 1) section_iter = 0;
      }
    }
     
    if (verbose) printf("BBOX %lu: %i sections:\n",n,bbox[n].n_section);
    for (j=0;j<bbox[n].n_section;j++) 
      if (verbose) printf("\t%lu: %i - %i\n",j,bbox[n].sec[j].stripe_min,
			  bbox[n].sec[j].stripe_max);
  }
 
  InitializeSections();
	
  AssignIterArea();

  if (verbose) printf("Writing out jack-knife map to %s...\n",
		      argv[start_argv+1]);

  OutputFile = fopen(argv[start_argv+1],"w");

  n_pixel = 0;
  for (n=0;n<n_bbox;n++) 
    for (u=0;u<bbox[n].n_section;u++) n_pixel += bbox[n].sec[u].n_pixel;


  unique_pixnum_array = gsl_vector_ulong_alloc(n_pixel);
  unique_iter_array = gsl_vector_int_alloc(n_pixel);
  pixel_index = gsl_permutation_alloc(n_pixel);

  k = 0;

  for (n=0;n<n_bbox;n++) {
    for (u=0;u<bbox[n].n_section;u++) {
      for (j=0;j<bbox[n].sec[u].n_pixel;j++) {
	unique_pixnum_array->data[k] = bbox[n].sec[u].pixnum->data[j];
	unique_iter_array->data[k] = bbox[n].sec[u].iter->data[j];
	k++;
      }
    }
  }

  gsl_sort_vector_ulong_index(pixel_index,unique_pixnum_array);
    
  for (i=0;i<n_pixel;i++) {
    j = pixel_index->data[i];
    fprintf(OutputFile,"%ld %i %i\n",unique_pixnum_array->data[j],
	    pixel_resolution,unique_iter_array->data[j]);
  }

  fclose(OutputFile);
    
  if (verbose) printf("Done.\n");

  return 0;
}

void InitializePixels()
{
  double total_area;
  unsigned long x_min, x_max, y_min, y_max;
  unsigned long i,j,k,n,m,nx,ny,sum_pixel;

  for (n=0;n<n_bbox;n++) {

    nx = nx0*pixel_resolution;
    ny = ny0*pixel_resolution;

    bbox[n].n_pixel = 0;

    for (i=0;i<bbox[n].n_stripe;i++) {
      area_index_stripe(pixel_resolution,bbox[n].stripe_bound[i].stripe,
			&x_min,&x_max,&y_min,&y_max);
      bbox[n].n_pixel += (x_max - x_min + 1)*(y_max - y_min + 1);
    }

    if (verbose) 
      printf("Found %lu pixels in bounding box %li at %i resolution...\n",
	     bbox[n].n_pixel,n,pixel_resolution);
    
    sum_pixel += bbox[n].n_pixel;

    bbox[n].unmasked_area = gsl_vector_alloc(bbox[n].n_pixel);
    bbox[n].pixnum = gsl_vector_ulong_alloc(bbox[n].n_pixel);
    bbox[n].superpixnum = gsl_vector_ulong_alloc(bbox[n].n_pixel);
    bbox[n].stripe = gsl_vector_int_alloc(bbox[n].n_pixel);
      
    m = 0;
    
    for (k=0;k<bbox[n].n_stripe;k++) {
      area_index_stripe(pixel_resolution,bbox[n].stripe_bound[k].stripe,
			&x_min,&x_max,&y_min,&y_max);
      for (j=y_min;j<=y_max;j++) {
	for (i=x_min;i<=x_max;i++) {
	  bbox[n].pixnum->data[m] = nx*j + i;
	  m++;
	}
      }
    }
      
    if (verbose) printf("Sorting pixel ids...\n");
    gsl_sort_vector_ulong(bbox[n].pixnum);
    if (verbose) printf("Done.  Generating pixel positions...\n");
    
    total_area = 0.0;
    bbox[n].pix_area = pix_area(pixel_resolution,bbox[n].pixnum->data[0]);
    
    for (i=0;i<bbox[n].n_pixel;i++) {
      superpix(pixel_resolution,bbox[n].pixnum->data[i],
	       superpix_resolution,&bbox[n].superpixnum->data[i]);
      bbox[n].unmasked_area->data[i] = 1.0;
      bbox[n].stripe->data[i] = pix2stripe(pixel_resolution,
					   bbox[n].pixnum->data[i]);
    }
    if (verbose) printf("Done.\n");
  }

}

void MaskPixels()
{
  unsigned long pixnum, n_dropped, n_partial;
  unsigned long j,k,n,m,jlo,ilo,masked;

  if (verbose) printf("Masking pixels...\n");

  for (n=0;n<n_bbox;n++) {
    
    n_dropped = n_partial = 0;
    
    for (m=0;m<bbox[n].n_pixel;m++) {
      
      masked = 0;
      lhunt(mask_superpixnum_array,bbox[n].superpixnum->data[m],&jlo);
      
      if (jlo <= n_superpix - 1) {
	if (mask_superpixnum_array->data[jlo] == 
	    bbox[n].superpixnum->data[m]) {
	  for (k=0;k<mask_struct[jlo].n_res;k++) {
	    if (mask_struct[jlo].res_struct[k].resolution == 
		pixel_resolution) {
	      if (mask_struct[jlo].res_struct[k].n_pixel == 1) {
		ilo = 0;
	      } else {
		lhunt(mask_struct[jlo].res_struct[k].pixnum,
		      bbox[n].pixnum->data[m],&ilo);
	      }
	      if (ilo <= mask_struct[jlo].res_struct[k].n_pixel-1) {
		if (mask_struct[jlo].res_struct[k].pixnum->data[ilo] ==
		    bbox[n].pixnum->data[m]) {
		  bbox[n].unmasked_area->data[m] = 0.0;
		  n_dropped++;
		}
	      }
	    }
	    if (mask_struct[jlo].res_struct[k].resolution < pixel_resolution) {
	      superpix(pixel_resolution,bbox[n].pixnum->data[m],
		       mask_struct[jlo].res_struct[k].resolution,&pixnum);
	      if (mask_struct[jlo].res_struct[k].n_pixel == 1) {
		ilo = 0;
	      } else {
		lhunt(mask_struct[jlo].res_struct[k].pixnum,pixnum,&ilo);
	      }
	      if (ilo <= mask_struct[jlo].res_struct[k].n_pixel-1) {
		if (mask_struct[jlo].res_struct[k].pixnum->data[ilo] == 
		    pixnum) {
		  bbox[n].unmasked_area->data[m] = 0.0;
		  n_dropped++;
		}
	      }
	    }
	    if (mask_struct[jlo].res_struct[k].resolution > pixel_resolution) {
	      for (j=0;j<mask_struct[jlo].res_struct[k].n_pixel;j++) {
		superpix(mask_struct[jlo].res_struct[k].resolution,
			 mask_struct[jlo].res_struct[k].pixnum->data[j],
			 pixel_resolution,&pixnum);
		if (bbox[n].pixnum->data[m] == pixnum) {
		  bbox[n].unmasked_area->data[m] -= 
		    1.0*pixel_resolution*pixel_resolution/
		    (mask_struct[jlo].res_struct[k].resolution*
		     mask_struct[jlo].res_struct[k].resolution);
		  masked = 1;
		}
	      }
	    }   
	  }
	}
      }
      if (masked == 1) n_partial++;
    }
    if (verbose) 
      printf("BBOX %lu %i: %lu dropped and %lu pixels were partially masked.\n",
	     n,pixel_resolution,n_dropped,n_partial);
  }

}

void InitializeSections()
{
  unsigned long i, k, n, u;

  for (n=0;n<n_bbox;n++) {
    for (u=0;u<bbox[n].n_section;u++) {
      
      bbox[n].sec[u].n_pixel = 0;
      
      for (i=0;i<bbox[n].n_pixel;i++) {
	if ((bbox[n].stripe->data[i] >= bbox[n].sec[u].stripe_min) &&
	    (bbox[n].stripe->data[i] <= bbox[n].sec[u].stripe_max)) 
	  bbox[n].sec[u].n_pixel++;
      }

      printf("BBOX %lu,%lu: %lu pixels...\n",n,u,bbox[n].sec[u].n_pixel);

      bbox[n].sec[u].pixnum = gsl_vector_ulong_alloc(bbox[n].sec[u].n_pixel);
      bbox[n].sec[u].unmasked_area = gsl_vector_alloc(bbox[n].sec[u].n_pixel);
      bbox[n].sec[u].iter = gsl_vector_int_alloc(bbox[n].sec[u].n_pixel);

      k = 0;
      for (i=0;i<bbox[n].n_pixel;i++) {
	if ((bbox[n].stripe->data[i] >= bbox[n].sec[u].stripe_min) &&
	    (bbox[n].stripe->data[i] <= bbox[n].sec[u].stripe_max)) {
	  bbox[n].sec[u].pixnum->data[k] = bbox[n].pixnum->data[i];
	  bbox[n].sec[u].unmasked_area->data[k] = 
	    bbox[n].unmasked_area->data[i];
	  bbox[n].sec[u].iter->data[k] = -1;
	  k++;
	}
      }
    }
  }
}

void AssignIterArea()
{
  double mean_area, total_area, area_break, area_count;
  unsigned long pixel_count, pixel_iter;
  unsigned long i, j, n, u;
  unsigned long sum_pixel;
  gsl_vector *area_check;
  gsl_vector_ulong *iter_check;

  sum_pixel = 0;
  total_area = 0.0;

  for (n=0;n<n_bbox;n++) {
    for (u=0;u<bbox[n].n_section;u++) {
      sum_pixel += bbox[n].sec[u].n_pixel;
      for (i=0;i<bbox[n].sec[u].n_pixel;i++) 
	total_area += bbox[n].sec[u].unmasked_area->data[i];
    }
  }

  if (verbose) printf("Attempting to break %1.2lf every %1.2lf...\n",
		      total_area*bbox[0].pix_area,
		      2.5*2.5*jack_width*jack_width);

  if (force_jack == 0) {

    n_jack = 0;
    
    while (2.5*2.5*jack_width*jack_width*n_jack < survey_area) n_jack++;

    n_jack--;
  }

  iter_check = gsl_vector_ulong_calloc(n_jack);
  area_check = gsl_vector_calloc(n_jack);

  mean_area = total_area/sum_pixel;
  area_break = total_area/n_jack;
  pixel_count = pixel_iter = 0;
  area_count = 0.0;
  if (verbose)
    printf("Assigning %i jackknife regions, breaking %1.2lf every %1.2lf...\n",
         n_jack,survey_area,area_break*bbox[0].pix_area);

  if (verbose) printf("Buffer: %lf on %lu pixels\n",mean_area,sum_pixel);
  
  for (n=0;n<n_bbox;n++) {
    for (u=0;u<bbox[n].n_section;u++) {
      for (i=0;i<bbox[n].sec[u].n_pixel;i++) {
	if (area_count + 0.75*mean_area <= area_break*(pixel_iter+1)) {
	  area_count += bbox[n].sec[u].unmasked_area->data[i];
	} else {
	  area_count += bbox[n].sec[u].unmasked_area->data[i];
	  pixel_iter++;
	  if (pixel_iter == n_jack) pixel_iter = n_jack - 1;
	}
	bbox[n].sec[u].iter->data[i] = pixel_iter;
      }
    }
  }
  
  if (check_area == 1) {
    if (verbose) printf("Sample  Pixels  Unmasked Area  Masked Area\n");
    if (verbose) printf("------  ------  -------------  -----------\n");

    for (j=0;j<n_jack;j++) {
      for (n=0;n<n_bbox;n++) {
	for (u=0;u<bbox[n].n_section;u++) {
	  for (i=0;i<bbox[n].sec[u].n_pixel;i++) {
	    if (bbox[n].sec[u].iter->data[i] == j) {
	      iter_check->data[j] += 1;
	      area_check->data[j] += bbox[n].pix_area*
		bbox[n].sec[u].unmasked_area->data[i];
	    }
	  }
	}
      }
      if (verbose)
	printf("  %lu      %lu      %lf   %lf\n",j,iter_check->data[j], 
	       iter_check->data[j]*bbox[0].pix_area,area_check->data[j]);
    }
  }
  
}

  
  

