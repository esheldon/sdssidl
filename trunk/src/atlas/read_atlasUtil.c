/*
 * Utility functions for read_atlasIDL and recframeIDL
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
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "dervish.h"
#include "phFits.h"
#include "phConsts.h"

#include "read_atlasUtil.h"

/*===========================================================================
 *
 *  getAtlasRow
 *
 *  Read a row from the fits file with some error checking
 *
 *===========================================================================*/

ATLAS_IMAGE *
readAtlasRow(FITS *fits, int row)
{
   
  ATLAS_IMAGE *ai;
  /* read atlas image */
  if((ai = read_atlas_image(fits,row)) == NULL) {
    return(NULL);
  }
  
  /* no atlas image for this object */
  if((ai)->id < 0) {			
	  // shut up!
    //shError("Object %d has no atlas image", row); 
    /* Still need to clean up memory if this particular object not found */
     return(NULL);
  }
  shAssert((ai)->master_mask != NULL);
  
  return(ai);
}


/*===========================================================================
 *
 * getIdVal
 *
 * get the value from the input id variable
 *
 *===========================================================================*/

int getIdVal(IDL_VPTR id)
{

  IDL_LONG *idptr;
  IDL_MEMINT n;
  int row;
  IDL_VPTR idCvt;
  
  /* Extract data.  This way we don't demand scalar input, just simple 
     input.  We take the first input value */

  if (id->type != IDL_TYP_LONG) {
    idCvt = IDL_BasicTypeConversion(1, &id, IDL_TYP_LONG);
    IDL_VarGetData(idCvt, &n, (char **) &idptr, IDL_TRUE);

    row = idptr[0];
    IDL_Deltmp(idCvt);
  } else {
    IDL_VarGetData(id, &n, (char **) &idptr, IDL_TRUE);
    row = idptr[0];
  }

  return(row);
  
}

/*===========================================================================
 *
 * getIdList
 *
 * get the pointer to the data of the input idlistVptr
 *
 *===========================================================================*/

// A copy is made either way to simplify
IDL_VPTR getAsMEMINT(IDL_VPTR inputVptr, int* iscopy)
{
    IDL_VPTR idCvt;
    if (inputVptr->type != IDL_TYP_MEMINT) {
        idCvt = IDL_CvtMEMINT(1, &inputVptr);
        *iscopy=1;
        return(idCvt);
    } else {
        *iscopy=0;
        return(inputVptr);
    }

}

/*===========================================================================
 *
 * getFileNameVal
 *
 * get the actual string value from the input atlas file name.
 *
 *===========================================================================*/

char * getFileNameVal(IDL_VPTR fileName)
{
  char *name;

  IDL_ENSURE_SCALAR(fileName);
  if (fileName->type != IDL_TYP_STRING)
    {
      IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_LONGJMP,
		  "atlasFileName must be a string");
    }
  else
    name = fileName->value.str.s;

  return(name);
}


/*===========================================================================
 *
 * copyAtlasImageFromReg
 *
 * Copy individual region from atlas image to output variable
 *
 *===========================================================================*/

void 
copyAtlasImageFromReg(ATLAS_IMAGE *ai, int bandpass, IDL_VPTR image)
{

  REGION *reg;
  IDL_ARRAY_DIM dim;
  IMAGE_TYP *data;

  IDL_VPTR image_tmp;

  int row0, col0;

  int i, j;

  row0 = ai->master_mask->rmin + ai->drow[bandpass];
  col0 = ai->master_mask->cmin + ai->dcol[bandpass];

  /* get the region for this bandpass */
  reg = 
    shRegNew("atlas image",
	     ai->master_mask->rmax - ai->master_mask->rmin + 1,
	     ai->master_mask->cmax - ai->master_mask->cmin + 1, TYPE_U16);

  setRegionBackground(reg, SOFT_BIAS); 

  phRegionSetFromAtlasImage(ai, bandpass, reg, row0, col0, 0.0);

  dim[0] = reg->ncol;
  dim[1] = reg->nrow;

  /* make a temporary array for copying */
  data = 
    (IMAGE_TYP *) IDL_MakeTempArray(IDL_IMAGE_TYP, N_DIM, dim, 
				    IDL_ARR_INI_NOP, &image_tmp);

  for (i=0; i<reg->nrow; ++i)
    for (j=0; j<reg->ncol; ++j) 
      {
	/* if there are zero's, this is a problem with the atlas image */
	if (reg->rows[i][j] != 0) {
	  *(&data[0] + i*reg->ncol + j) = 
	    (IDL_LONG) reg->rows[i][j] - SOFT_BIAS;
	}
	else 
	  *(&data[0] + i*reg->ncol + j) = BAD_PIXVAL;
	
      }
  
  /* copy into image. This just copied pointers, its fast */
  /* Note, since image_tmp is a temporary variable, it is released */
  IDL_VarCopy(image_tmp, image);
  
  /* Clean up the memory from reg */
  shRegDel(reg);

}

/*===========================================================================
 *
 * setRegionBackground
 *
 * set all values in the region to the input value
 *
 *===========================================================================*/

void
setRegionBackground(REGION *reg,
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
