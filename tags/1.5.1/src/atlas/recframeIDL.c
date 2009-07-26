/*===========================================================================
 *
 * recframeIDL.c
 * 
 *
 * Revision History:
 *   Created: 05-Feb-1999 Erin Sheldon UofChicago
 *   Converted to DLM 16-Nov-2004 Erin Sheldon
 *
 *
 *
 *
 *  Copyright (C) 2005  Erin Sheldon, NYU.  erin dot sheldon at gmail dot com
 *
 *    This program is free software; you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation; either version 2 of the License, or
 *    (at your option) any later version.
 *
 *    This program is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with this program; if not, write to the Free Software
 *    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

 *===========================================================================*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "dervish.h"
#include "phFits.h"
#include "phConsts.h"

#include "idl_export.h"
#include "read_atlasUtil.h"
#include "recframeIDL.h"

/* 
 * Global Variables 
 */

/* Structure that holds the input keywords.  See recframeIDL.h */
KW_RESULT kw;

/*===========================================================================
 *
 *  rdAtlas
 *
 *  The main procedure which links to IDL.
 *
 *===========================================================================*/

void recframe(int argc, IDL_VPTR *argv, char *argk) {

  /* the desired atlas image. */
  ATLAS_IMAGE *ai;		

  /* The fits file */
  FITS *fits;

  /* Regions for each bandpass */
  REGION *imu_reg, *img_reg, *imr_reg, *imi_reg, *imz_reg;

  /* The input file name and object id */
  char *atlasFileName;
  IDL_LONG *idlist;
  IDL_MEMINT nid;
  IDL_VPTR idlistVptr, idCvt;
  short istemp;

  int airow;

  /* Get the keywords */
  (void) IDL_KWProcessByOffset(argc, argv, argk, kw_pars, 
                               (IDL_VPTR *) 0, 1, &kw);

  /* Check arguments */
  if (nParams(argc) != 2)
  {
    IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_INFO,
            "Syntax: recframe, atlasFile, idlist, imu=, img=, imr=, imi=, imz=, row0=, col0=, drow=, dcol=, nrow=, ncol=, status=");
    setStatus(FAILURE);
    return;
  }

  /* Get the argument values */
  atlasFileName = getFileNameVal(argv[0]);
  idlistVptr = argv[1];

  if (idlistVptr->type != IDL_TYP_LONG) {
    istemp = 1;
    idCvt = IDL_CvtLng(1, &idlistVptr);
    IDL_VarGetData(idCvt, &nid, (char **) &idlist, IDL_TRUE);
  } else {
    istemp = 0;
    IDL_VarGetData(idlistVptr, &nid, (char **) &idlist, IDL_TRUE);
  }

  /* Open the fits file */
  if((fits = open_fits_table(atlasFileName, 1)) == NULL) {
    /* Memory is cleaned up in open_fits_table() */
    if (istemp) IDL_Deltmp(idCvt);
    setStatus(FAILURE);
    return;
  }

  if (kw.imu_there) 
  {
    imu_reg = 
        shRegNew("frame", FRAME_NROWS, FRAME_NCOLS, TYPE_U16);
    setRegionBackground(imu_reg, SOFT_BIAS); 
  }
  if (kw.img_there) 
  {
    img_reg = 
        shRegNew("frame", FRAME_NROWS, FRAME_NCOLS, TYPE_U16);
    setRegionBackground(img_reg, SOFT_BIAS); 
  }
  if (kw.imr_there) 
  {
    imr_reg = 
        shRegNew("frame", FRAME_NROWS, FRAME_NCOLS, TYPE_U16);
    setRegionBackground(imr_reg, SOFT_BIAS); 
  }
  if (kw.imi_there) 
  {
    imi_reg = 
        shRegNew("frame", FRAME_NROWS, FRAME_NCOLS, TYPE_U16);
    setRegionBackground(imi_reg, SOFT_BIAS); 
  }
  if (kw.imz_there) 
  {
    imz_reg = 
        shRegNew("frame", FRAME_NROWS, FRAME_NCOLS, TYPE_U16);
    setRegionBackground(imz_reg, SOFT_BIAS); 
  }

  for (airow=0;airow<nid;airow++) 
  {

    ai = readAtlasRow(fits, idlist[airow]);

    if (ai != NULL) 
    {
      /* Clean up the atlas image memory */

      /*
         row0 = ai->master_mask->rmin + ai->drow[2];
         col0 = ai->master_mask->cmin + ai->dcol[2];

         printf("row0 = %d   col0 = %d\n", row0, col0);
         */
      /*
         if (kw.imu_there)
         phRegionSetFromAtlasImage(ai, 0, imu_reg, 
         0, 0, 0.0);
         if (kw.img_there)
         phRegionSetFromAtlasImage(ai, 1, img_reg, 
         0, 0, 0.0);
         if (kw.imr_there)
         phRegionSetFromAtlasImage(ai, 2, imr_reg, 
         0, 0, 0.0);

         if (kw.imi_there)
         phRegionSetFromAtlasImage(ai, 3, imi_reg, 
         0, 0, 0.0);

         if (kw.imz_there)
         phRegionSetFromAtlasImage(ai, 4, imz_reg, 
         0, 0, 0.0);
         */

      /*
         if (kw.imu_there)
         phRegionSetFromAtlasImage(ai, 0, imu_reg, 
         ai->drow[0], ai->dcol[0], 0.0);
         if (kw.img_there)
         phRegionSetFromAtlasImage(ai, 1, img_reg, 
         ai->drow[1], ai->dcol[1], 0.0);
         if (kw.imr_there)
         phRegionSetFromAtlasImage(ai, 2, imr_reg, 
         ai->drow[2], ai->dcol[2], 0.0);

         if (kw.imi_there)
         phRegionSetFromAtlasImage(ai, 3, imi_reg, 
         ai->drow[3], ai->dcol[3], 0.0);

         if (kw.imz_there)
         phRegionSetFromAtlasImage(ai, 4, imz_reg, 
         ai->drow[4], ai->dcol[4], 0.0);
         */

        /*
      if (kw.imu_there)
        ai_regFrame_set(ai->master_mask, ai->pix[0], imu_reg, 0, 0);

      if (kw.img_there)
        ai_regFrame_set(ai->master_mask, ai->pix[1], img_reg, 0, 0);

      if (kw.imr_there)
        ai_regFrame_set(ai->master_mask, ai->pix[2], imr_reg, 0, 0);

      if (kw.imi_there)
        ai_regFrame_set(ai->master_mask, ai->pix[3], imi_reg, 0, 0);

      if (kw.imz_there)
        ai_regFrame_set(ai->master_mask, ai->pix[4], imz_reg, 0, 0);
        */
      if (kw.imu_there)
        ai_regFrame_set(ai->master_mask, ai->pix[0], imu_reg, 
                ai->drow[0], ai->dcol[0]);
      if (kw.img_there)
        ai_regFrame_set(ai->master_mask, ai->pix[1], img_reg, 
                ai->drow[1], ai->dcol[1]);
      if (kw.imr_there)
        ai_regFrame_set(ai->master_mask, ai->pix[2], imr_reg, 
                ai->drow[2], ai->dcol[2]);
      if (kw.imi_there)
        ai_regFrame_set(ai->master_mask, ai->pix[3], imi_reg, 
                ai->drow[3], ai->dcol[3]);
      if (kw.imz_there)
        ai_regFrame_set(ai->master_mask, ai->pix[4], imz_reg, 
                ai->drow[4], ai->dcol[4]);

      phAtlasImageDel(ai,1);
    }
  }

  /* clean up */
  phFitsDel(fits);  

  if (kw.imu_there)
  {
    /* Clean up the memory from reg */
    copyRegToImage(imu_reg, kw.imu);
    shRegDel(imu_reg);
  }
  if (kw.img_there)
  {
    /* Clean up the memory from reg */
    copyRegToImage(img_reg, kw.img);
    shRegDel(img_reg);
  }
  if (kw.imr_there)
  {
    /* Clean up the memory from reg */
    copyRegToImage(imr_reg, kw.imr);
    shRegDel(imr_reg);
  }
  if (kw.imi_there)
  {
    /* Clean up the memory from reg */
    copyRegToImage(imi_reg, kw.imi);
    shRegDel(imi_reg);
  }
  if (kw.imz_there)
  {
    /* Clean up the memory from reg */
    copyRegToImage(imz_reg, kw.imz);
    shRegDel(imz_reg);
  }

  /* Clean up the keyword info */
  IDL_KW_FREE;

  setStatus(SUCCESS);

  if (istemp) 
    IDL_Deltmp(idCvt);

  return;

}

static void 
ai_regFrame_set(const OBJMASK *om,	/* the OBJMASK specifying the pixels */
               const U16 *pix,		/* the values to use */
               REGION *reg,		/* the region wherein they dwell */
               int drow, int dcol)	/* how much to offset om's coords */
{
  int i;
  int npix;				/* number of pixels in OBJMASK */
  U16 **rows;				/* == reg->rows */
  const U16 *ptr;			/* pointer to pix */
  SPAN *sp;				/* == om->s[i] */
  int nrow, ncol;			/* == reg->n{row,col} */
  int x1, x2, y;			/* unpacked from sp */
  /*
   * Set the pixels in the region.
   *
   * We must be careful as the offset OBJMASK may lie outside the REGION
   */
  ptr = pix;
  rows = reg->rows;
  nrow = reg->nrow; ncol = reg->ncol;

  for(i = 0;i < om->nspan;i++) {
    sp = &om->s[i];
    y = sp->y; x1 = sp->x1; x2 = sp->x2;
    npix = x2 - x1 + 1;

    y += drow; x1 += dcol; x2 += dcol;
    if(y >= 0 && y < nrow) {
      int col0 = x1;			/* starting column in image */
      int ncopy = npix;		/* number of pixels to copy */
      int ptr0 = 0;			/* starting index in ptr[] */

      if(x1 < 0) {
        col0 = 0;
        ptr0 = -x1;

        ncopy -= ptr0;
      }
      if(x2 >= ncol) {
        ncopy -= (x2 - ncol + 1);
      }

      if(ncopy > 0) {
        int j;
        for(j = 0; j < ncopy; j++) {
          /* to prevent different objects in the same family from
             clobbering one another, add it in while subtracting
             the software bias. Note conversionto int allows negatives.
             rows already has soft bias so this is ok there */
          rows[y][col0 + j] += ( (int)ptr[ptr0 + j] ) - SOFT_BIAS;
        }
      }
    }
    ptr += npix;
  }
  shAssert(ptr == pix + om->npix);
}


/*===========================================================================
 *
 * Create the output image and copy from the region.
 *
 *===========================================================================*/

static void
copyRegToImage(REGION *reg, IDL_VPTR image)
{

  IDL_ARRAY_DIM dim;
  IMAGE_TYP *data;

  IDL_VPTR image_tmp;

  int i, j;

  dim[0] = FRAME_NCOLS;
  dim[1] = FRAME_NROWS;

  /* make a temporary array for copying */
  data = 
      (IMAGE_TYP *) IDL_MakeTempArray(IDL_IMAGE_TYP, N_DIM, dim, 
                                      IDL_ARR_INI_NOP, &image_tmp);

  for (i=0; i<FRAME_NROWS; ++i)
    for (j=0; j<FRAME_NCOLS; ++j) 
    {
      *(&data[0] + i*FRAME_NCOLS + j) = 
          (IDL_LONG) reg->rows[i][j] - SOFT_BIAS;
    }

  /* copy into image. This just copies pointers, its fast */
  /* Note, since image_tmp is a temporary variable, it is released */
  IDL_VarCopy(image_tmp, image);


}



/*===========================================================================
 *
 * nParams
 *
 * How many positional arguments were sent?
 *
 *===========================================================================*/

static
int nParams(int argc)
{

  int nKeywords;

  nKeywords =     
      kw.img_there + kw.imi_there + kw.imr_there + kw.imu_there + kw.imz_there +
      kw.status_there;

  return 
      argc - nKeywords;


}

/* set the status if it is there */
static void
setStatus(int statusVal)
{

  if (kw.status_there) {
    /* This frees any existing memory and sets type to INT with value zero */
    IDL_StoreScalarZero(kw.status, IDL_TYP_INT);
    kw.status->value.i = statusVal;
  }

}

/*===========================================================================
 *
 * This is the code for the IDL DLM interface 
 *
 *===========================================================================*/


#define ARRLEN(arr) (sizeof(arr)/sizeof(arr[0]))

int IDL_Load(void)
{

  /* This must be static. It is a struct. */
  /* The name in strings is the name by which it will be called from IDL and
     MUST BE CAPITALIZED 
     5th parameter will say if it accepts keywords and some other flags 
     For more info see page 325 of external dev. guide */
  static IDL_SYSFUN_DEF2 procedure_addr[] = {
    { (IDL_SYSRTN_GENERIC) recframe, 
      "RECFRAME", 0, IDL_MAXPARAMS, 
      IDL_SYSFUN_DEF_F_KEYWORDS, 0},
  };

  /* False means it is not a function */
  return IDL_SysRtnAdd(procedure_addr, IDL_FALSE, ARRLEN(procedure_addr));

}
