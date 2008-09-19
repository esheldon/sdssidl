/*===========================================================================
 *
 * read_atlasIDL.c
 * 
 * Read an atlas image table produced by photo Based on the stand-alone reader
 * written by Robert Lupton.  Made to interface to IDL though the DLM
 * mechanism.  The routine in IDL is called rdAtlas.  Also changed behavior of
 * shError to use the IDL_Message routine in dervish.c.  Grep for E.S.S. to
 * find the changes. 
 *   
 * IDL> rdAtlas
 * % RDATLAS: Syntax: rdAtlas, atlasFile, id, imu=, img=, imr=, imi=, imz=, 
 *            row0=, col0=, drow=, dcol=, nrow=, ncol=, status=
 *
 * Inputs:
 *   atlasFile:  The atlas file name.
 *   id. The object id in its field.  
 *
 * Outputs:
 *    imu=, img=, ....  The images are returned through these keywords.  
 *          This saves time since images are only copied if the keyword is 
 *          present.
 * 
 *   row0=, col0=: The bottom left position of the cutout in the original 
 *                  image.
 *   drow=, dcol=: Positional difference relative to the r-band.
 *   ncow=, ncol=: The size of the images in col and row.
 *
 * Revision History:
 *   Created: 05-Feb-1999 Erin Sheldon UofChicago
 *   Converted to DLM 16-Nov-2004 Erin Sheldon
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

#include "export.h"
#include "read_atlasUtil.h"
#include "read_atlasIDL.h"


/* 
 * Global Variables 
 */

/* Structure that holds the input keywords.  See read_atlasIDL.h */
KW_RESULT kw;

/*===========================================================================
 *
 *  rdAtlas
 *
 *  The main procedure which links to IDL.
 *
 *===========================================================================*/

void rdAtlas(int argc, IDL_VPTR *argv, char *argk) {

  /* the desired atlas image. */
  ATLAS_IMAGE *ai;		

  /* The fits file */
  FITS *fits;

  /* The input file name and object id */
  char *atlasFileName;
  int id;

  /* Get the keywords */
  (void) IDL_KWProcessByOffset(argc, argv, argk, kw_pars, 
			       (IDL_VPTR *) 0, 1, &kw);

  /* Check arguments */
  if (nParams(argc) != 2)
    {
      IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_INFO,
		  "Syntax: rdAtlas, atlasFile, id, imu=, img=, imr=, imi=, imz=, row0=, col0=, drow=, dcol=, nrow=, ncol=, status=");
      setStatus(FAILURE);
      return;
    }
  

  /* Get the argument values */
  atlasFileName = getFileNameVal(argv[0]);
  id            = getIdVal(argv[1]);

  /* Open the fits file */
  if((fits = open_fits_table(atlasFileName, 1)) == NULL) {
    /* Memory is cleaned up in open_fits_table() */
    setStatus(FAILURE);
    return;
  }

  /* call code to read ai, the atlas image */
  ai = readAtlasRow(fits, id);

  /* clean up */
  phFitsDel(fits);  

  if (ai == NULL) {
    setStatus(FAILURE);
    return;
  }

  
  /* Copy the images and statistics into our output keywords */
  copyAtlasImages(ai);
  copyStatsFromAtlas(ai);

  /* Clean up the atlas image memory */
  phAtlasImageDel(ai,1);

  /* Clean up the keyword info */
  IDL_KW_FREE;

  setStatus(SUCCESS);
  return;

}


/*===========================================================================
 * copyAtlasImages
 *
 * Copy the atlas images to the output variables
 *
 *===========================================================================*/

static void
copyAtlasImages(ATLAS_IMAGE *ai)
{

  if (kw.imu_there) 
    copyAtlasImageFromReg(ai, 0, kw.imu);
  
  if (kw.img_there) 
    copyAtlasImageFromReg(ai, 1, kw.img);
  
  if (kw.imr_there) 
    copyAtlasImageFromReg(ai, 2, kw.imr);
  
  if (kw.imi_there) 
    copyAtlasImageFromReg(ai, 3, kw.imi);
  
  if (kw.imz_there) 
    copyAtlasImageFromReg(ai, 4, kw.imz); 

}


/*===========================================================================
 *
 * copyStatsFromAtlas
 *
 * copy some statistics of our object from the atlas image into keywords
 *   row0,col0,drow,dcol
 *
 * The individual masks for each bandpass do not seem to have the information
 * we want, so we generate it from the master_mask and the dcol,drow.
 *
 *===========================================================================*/

static void copyStatsFromAtlas(ATLAS_IMAGE *ai)
{

  IDL_VPTR row0_tmp, col0_tmp, drow_tmp, dcol_tmp;
  short *row0_data, *col0_data, *drow_data, *dcol_data;
  IDL_ARRAY_DIM dim;

  int i;

  dim[0] = NBANDS;

  /* row0 */
  if (kw.row0_there) {
    row0_data = 
      (short *) IDL_MakeTempArray(IDL_TYP_INT, 1, dim, 
				IDL_ARR_INI_NOP, &row0_tmp);

    for (i=0; i<NBANDS; ++i) 
      row0_data[i] = ai->master_mask->rmin + ai->drow[i];

    /* copy into output. This just copied pointers, its fast */
    IDL_VarCopy(row0_tmp, kw.row0);

  }

  /* col0 */
  if (kw.col0_there) {
    col0_data = 
      (short *) IDL_MakeTempArray(IDL_TYP_INT, 1, dim, 
				IDL_ARR_INI_NOP, &col0_tmp);

    for (i=0; i<NBANDS; ++i)
      col0_data[i] = ai->master_mask->cmin + ai->dcol[i];

    /* copy into output. This just copied pointers, its fast */
    IDL_VarCopy(col0_tmp, kw.col0);

  }

  /* drow */
  if (kw.drow_there) {
    drow_data = 
      (short *) IDL_MakeTempArray(IDL_TYP_INT, 1, dim, 
				IDL_ARR_INI_NOP, &drow_tmp);

    for (i=0; i<NBANDS; ++i)
      drow_data[i] = ai->drow[i];

    /* copy into output. This just copied pointers, its fast */
    IDL_VarCopy(drow_tmp, kw.drow);

  }

  /* dcol */
  if (kw.dcol_there) {
    dcol_data = 
      (short *) IDL_MakeTempArray(IDL_TYP_INT, 1, dim, 
				IDL_ARR_INI_NOP, &dcol_tmp);

    for (i=0; i<NBANDS; ++i)
      dcol_data[i] = ai->dcol[i];

    /* copy into output. This just copied pointers, its fast */
    IDL_VarCopy(dcol_tmp, kw.dcol);

  }

  if (kw.nrow_there) {
    /* This frees any existing memory and sets type to INT with value zero */
    IDL_StoreScalarZero(kw.nrow, IDL_TYP_INT);
    kw.nrow->value.i = ai->master_mask->rmax - ai->master_mask->rmin + 1;
  }
  if (kw.ncol_there) {
    IDL_StoreScalarZero(kw.ncol, IDL_TYP_INT);
    kw.ncol->value.i = ai->master_mask->cmax - ai->master_mask->cmin + 1;
  }

}

/*===========================================================================
 *
 * nParams
 *
 * How many positional arguments were sent?
 *
 *===========================================================================*/

int nParams(int argc)
{

  int nKeywords;

  nKeywords =     
    kw.col0_there + kw.dcol_there + kw.drow_there +
    kw.img_there + kw.imi_there + kw.imr_there + kw.imu_there + kw.imz_there +
    kw.row0_there + 
    kw.ncol_there + kw.nrow_there +
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
    { (IDL_SYSRTN_GENERIC) rdAtlas, "RDATLAS", 0, IDL_MAXPARAMS, 
      IDL_SYSFUN_DEF_F_KEYWORDS, 0},
  };

  /* False means it is not a function */
  return IDL_SysRtnAdd(procedure_addr, IDL_FALSE, ARRLEN(procedure_addr));

}
