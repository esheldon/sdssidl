/*
 * Read an atlas image table produced by photo
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "dervish.h"
#include "phFits.h"
#include "phConsts.h"
#include "export.h"
/*
 * some symbols to prevent dervish.o from being loaded from libatlas.a
 * if we are also linking against real dervish
 */

/*int verbose = 0;*/

int call_atlas(int argc, void *argv[]);
int atls(int ac, char *av[]);
static void set_background(REGION *reg, int bkgd);
static void usage(void);

/* these will hold our image, which we have to copy again. doh! */
REGION *reg_u, *reg_g, *reg_r, *reg_i, *reg_z;

/* the desired atlas image */
ATLAS_IMAGE *ai;		

int call_atlas(int argc, void *argv[]) {

  char *send_av[6];
  char word[100];
  int i,j, ncol, nrow, retval;
  IDL_STRING *fname, *idstring;
  int *image_u, *image_g, *image_r, *image_i, *image_z;
  U16 arg_ncol, arg_nrow,*size,*row0,*col0,*drow,*dcol;
  U16 *docolor;
  float use_bias;
  int bad_pixval=-9999;

  image_u = (int *) argv[0];
  image_g = (int *) argv[1];
  image_r = (int *) argv[2];
  image_i = (int *) argv[3];
  image_z = (int *) argv[4];

  docolor = (U16 *) argv[5];

  size = (U16 *) argv[6];
  fname = (IDL_STRING *) argv[7];
  idstring = (IDL_STRING *) argv[8];
  row0=(U16 *) argv[9];
  col0=(U16 *) argv[10];
  drow=(U16 *) argv[11];
  dcol=(U16 *) argv[12];

  /* 
   * The atlas image is already offset by 1000, so put this in all images so 
   * so background will make sense 
   */

  use_bias = (float) SOFT_BIAS;

  /**************************************** 
   * create the arguments for Robert's code 
   ****************************************/

  strcpy(word,"n");
  send_av[0] = calloc( strlen(word) +1, sizeof(char) );
  strcpy(send_av[0], word);

  strcpy(word,"-c");
  send_av[1] = calloc( strlen(word) +1, sizeof(char) );
  strcpy(send_av[1], word);

  strcpy(word,"temp.fit");
  send_av[5] = calloc( strlen(word) +1, sizeof(char) );
  strcpy(send_av[5], word);
  
  send_av[2] = calloc(1+1, sizeof(char));
  strcpy(send_av[2],"1");

  send_av[3] = calloc( strlen(fname->s)+1,sizeof(char));
  strcpy(send_av[3], fname->s);

  send_av[4] = calloc( strlen(idstring->s)+1, sizeof(char));
  strcpy(send_av[4], idstring->s);
  
  /******************************************************************
   * call Lupton's original code to read ai, the atlas image
   *******************************************************************/

  retval = atls(6,send_av);

  if (retval==1) {
     /*shRegDel(reg);*/
     return(1);
  }   

  for (i=0; i<5; ++i) {
    /* position in image of llh corner */
    row0[i] = ai->master_mask->rmin + ai->drow[i];
    col0[i] = ai->master_mask->cmin + ai->dcol[i];

    /* offsets from the r-band */
    drow[i] = ai->drow[i];
    dcol[i] = ai->dcol[i];
  }

  /*******************************************
   * Set the region for each requested band
   *******************************************/

  arg_nrow = size[0];
  arg_ncol = size[1];
  
  nrow = -1000;
  ncol = -1000;

  if (docolor[0] == 1) {
    reg_u = 
      shRegNew("atlas image",
	       ai->master_mask->rmax - ai->master_mask->rmin + 1,
	       ai->master_mask->cmax - ai->master_mask->cmin + 1, TYPE_U16);
    set_background(reg_u, use_bias);
    phRegionSetFromAtlasImage(ai, 0, reg_u, row0[0], col0[0], 0.0);
    
    nrow = reg_u->nrow;
    ncol = reg_u->ncol;
  }

  if (docolor[1] == 1) {
    reg_g = 
      shRegNew("atlas image",
	       ai->master_mask->rmax - ai->master_mask->rmin + 1,
	       ai->master_mask->cmax - ai->master_mask->cmin + 1, TYPE_U16);
    set_background(reg_g, use_bias);
    phRegionSetFromAtlasImage(ai, 1, reg_g, row0[1], col0[1], 0.0);

    /* is this the first band? */
    if (nrow == -1000) {
      nrow = reg_g->nrow;
      ncol = reg_g->ncol;
    } else {
      if (reg_g->nrow < nrow) nrow=reg_g->nrow;
      if (reg_g->ncol < ncol) ncol=reg_g->ncol;
    }

  }

  if (docolor[2] == 1) {
    reg_r = 
      shRegNew("atlas image",
	       ai->master_mask->rmax - ai->master_mask->rmin + 1,
	       ai->master_mask->cmax - ai->master_mask->cmin + 1, TYPE_U16);
    set_background(reg_r, use_bias);
    phRegionSetFromAtlasImage(ai, 2, reg_r, row0[2], col0[2], 0.0);

    /* is this the first band? */
    if (nrow == -1000) {
      nrow = reg_r->nrow;
      ncol = reg_r->ncol;
    } else {
      if (reg_r->nrow < nrow) nrow=reg_r->nrow;
      if (reg_r->ncol < ncol) ncol=reg_r->ncol;
    }
  }

  if (docolor[3] == 1) {
    reg_i = 
      shRegNew("atlas image",
	       ai->master_mask->rmax - ai->master_mask->rmin + 1,
	       ai->master_mask->cmax - ai->master_mask->cmin + 1, TYPE_U16);
    set_background(reg_i, use_bias);
    phRegionSetFromAtlasImage(ai, 3, reg_i, row0[3], col0[3], 0.0);

    /* is this the first band? */
    if (nrow == -1000) {
      nrow = reg_i->nrow;
      ncol = reg_i->ncol;
    } else {
      if (reg_i->nrow < nrow) nrow=reg_i->nrow;
      if (reg_i->ncol < ncol) ncol=reg_i->ncol;
    }
  }
  
  if (docolor[4] == 1) {
    reg_z = 
      shRegNew("atlas image",
	       ai->master_mask->rmax - ai->master_mask->rmin + 1,
	       ai->master_mask->cmax - ai->master_mask->cmin + 1, TYPE_U16);
    set_background(reg_z, use_bias);   
    phRegionSetFromAtlasImage(ai, 4, reg_z, row0[4], col0[4], 0.0);
    
    /* is this the first band? */
    if (nrow == -1000) {
      nrow = reg_z->nrow;
      ncol = reg_z->ncol;
    } else {
      if (reg_z->nrow < nrow) nrow=reg_z->nrow;
      if (reg_z->ncol < ncol) ncol=reg_z->ncol;
    }
  }
   
  if (nrow > arg_nrow) {
    printf("Trimming rows\n");
    nrow = arg_nrow;
  }

  if (ncol > arg_ncol) {
    printf("Trimming cols\n");
    ncol = arg_ncol;
  }

  /******************************************************
   * Store reg->rows in input array for requested bands
   * Note, the images are signed, so we can subtract off
   * the sky again
   ******************************************************/
  
  if (docolor[0] == 1) {
    for (i=0; i< nrow; ++i)
      for (j=0; j<ncol; ++j) {

	/* if there are zero's, this is a problem with the atlas image */
	if (reg_u->rows[i][j] != 0)  
	  *(&image_u[0] + i*arg_ncol + j) = reg_u->rows[i][j];
	else
	  *(&image_u[0] + i*arg_ncol + j) = bad_pixval;
      }
    shRegDel(reg_u);
  }
  if (docolor[1] == 1) {
    for (i=0; i< nrow; ++i)
      for (j=0; j<ncol; ++j) {
	
	/* if there are zero's, this is a problem with the atlas image */
	if (reg_g->rows[i][j] != 0)  
	  *(&image_g[0] + i*arg_ncol + j) = reg_g->rows[i][j];
	else
	  *(&image_g[0] + i*arg_ncol + j) = bad_pixval;
      }
    shRegDel(reg_g);
  }
  if (docolor[2] == 1) {
    for (i=0; i< nrow; ++i)
      for (j=0; j<ncol; ++j) {

	/* if there are zero's, this is a problem with the atlas image */
	if (reg_r->rows[i][j] != 0) 
	  *(&image_r[0] + i*arg_ncol + j) = reg_r->rows[i][j];
	else 
	  *(&image_r[0] + i*arg_ncol + j) = bad_pixval;
	
      }
    shRegDel(reg_r);
  }
  if (docolor[3] == 1) {
    for (i=0; i< nrow; ++i)
      for (j=0; j<ncol; ++j) {
	
	/* if there are zero's, this is a problem with the atlas image */
	if (reg_i->rows[i][j] != 0)  
	  *(&image_i[0] + i*arg_ncol + j) = reg_i->rows[i][j];
	else
	  *(&image_i[0] + i*arg_ncol + j) = bad_pixval;
      }
    shRegDel(reg_i);
  }
  if (docolor[4] == 1) {
    for (i=0; i< nrow; ++i)
      for (j=0; j<ncol; ++j) {
	
	/* if there are zero's, this is a problem with the atlas image */
	if (reg_z->rows[i][j] != 0)  
	  *(&image_z[0] + i*arg_ncol + j) = reg_z->rows[i][j];
	else
	  *(&image_z[0] + i*arg_ncol + j) = bad_pixval;
      }
    shRegDel(reg_z);
  }
  
  /* Store size */
  size[0] = ncol;
  size[1] = nrow;

  phAtlasImageDel(ai,1);

  return(0);
}


int atls(int ac,char *av[])
     /*int ac;
       char *av[];*/
{
   
   int bkgd = SOFT_BIAS;		/* desired background level */
   int color = 0;			/* desired color */
   FITS *fits;				/* the table in question */
   char *infile, *outfile;		/* input and output filenames */
   /*   int row0, col0;	*/		/* origin of image in region */
   /*   REGION *reg;*/				/* region to write */
   int row;				/* desired row */

   while(ac > 1 && (av[1][0] == '-' || av[1][0] == '+')) {
      switch (av[1][1]) {
       case '?':
       case 'h':
	 usage();
	 exit(0);
	 break;
       case 'b':
	 if(ac == 2) {
	    shError("-b requires a number");
	 } else {
	    ac--; av++;
	    bkgd = atoi(av[1]);
	 }
	 break;
       case 'c':
	 if(ac == 2) {
	    shError("-c requires a number");
	 } else {
	    ac--; av++;
	    color = atoi(av[1]);
	 }
	 break;
       case 'i':
	 fprintf(stderr,"SDSS read_atlas_images. Id: %s\n", phPhotoVersion());
	 exit(0);
	 break;
       case 'v':
	 /*verbose++;*/
	 break;
       default:
	 shError("Unknown option %s\n",av[1]);
	 break;
      }
      ac--;
      av++;
   }
   if(ac <= 3) {
      shError("You must specify an input file, a row, and an output file\n");
      exit(1);
   }
   infile = av[1]; row = atoi(av[2]); outfile = av[3];
/*
 * dummy calls to pull .o files out of the real libdervish.a if we are
 * linking against it
 */
   (void)shTypeGetFromName("RHL");
/*
 * open file
 */
   if((fits = open_fits_table(infile, 1)) == NULL) {
      return(1);
   }
/*
 * read atlas image
 */
   if((ai = read_atlas_image(fits,row)) == NULL) {
      return(1);
   }
   if(ai->id < 0) {			/* no atlas image for this object */
      shError("Object %d has no atlas image", row);
      return(1);
   }
   shAssert(ai->master_mask != NULL);

   /*
   if(color < 0 || color >= ai->ncolor) {
      shError("Invalid color; please choose a number in 0..%d", ai->ncolor-1);
      return(1);
   }
   */


/*
 * convert it to regions
 */

   /* printf("Creating Regions\n"); */

   phFitsDel(fits);   
   return(0);
}

/*****************************************************************************/

static void
usage(void)
{
   char **line;

   static char *msg[] = {
      "Usage: read_atlas_image [options] input-file row output-file",
      "Your options are:",
      "       -?      This message",
      "       -b #    Set background level to #",
      "       -c #    Use colour # (0..ncolor-1; default 0)",
      "       -h      This message",
      "       -i      Print an ID string and exit",
      "       -v      Turn up verbosity (repeat flag for more chatter)",
      NULL,
   };

   for(line = msg;*line != NULL;line++) {
      fprintf(stderr,"%s\n",*line);
   }
}

/*****************************************************************************/

static void
set_background(REGION *reg,
	       int bkgd)
{
   U16 *row0;
   int i;

   row0 = reg->rows[0];
   for(i = 0; i < reg->ncol; i++) {
      row0[i] = bkgd;
   }
   for(i = 1; i < reg->nrow; i++) {
      memcpy(reg->rows[i], row0, reg->ncol*sizeof(U16));
   }
}
