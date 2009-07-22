/*
  Header for read_atlasUtil.c

   Copyright (C) 2005  Erin Sheldon, NYU.  erin dot sheldon at gmail dot com
 
     This program is free software; you can redistribute it and/or modify
     it under the terms of the GNU General Public License as published by
     the Free Software Foundation; either version 2 of the License, or
     (at your option) any later version.
 
     This program is distributed in the hope that it will be useful,
     but WITHOUT ANY WARRANTY; without even the implied warranty of
     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
     GNU General Public License for more details.
 
     You should have received a copy of the GNU General Public License
     along with this program; if not, write to the Free Software
     Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 
  ---------------------------------------------------------------------------*/

#define NBANDS 5                        /* Number of bands */
#define IMAGE_TYP IDL_LONG              /* The type of our output images */
#define IDL_IMAGE_TYP IDL_TYP_LONG      /* The IDL type code */
#define BAD_PIXVAL -9999                /* value for bad pixels */
#define N_DIM 2                         /* images are 2 dimensions */
#define SUCCESS 0
#define FAILURE 1

#define FRAME_NCOLS 2048 
#define FRAME_NROWS 1489

#include "export.h"

/* Read a row from the atlas fits file */
ATLAS_IMAGE* readAtlasRow(FITS *fits, int row);

/* get the id */
int getIdVal(IDL_VPTR id);
IDL_VPTR getAsMEMINT(IDL_VPTR inputVptr, int* iscopy);


/* get the file name */
char * getFileNameVal(IDL_VPTR fileName);

/* copy from the REGION into the image */
void
copyAtlasImageFromReg(ATLAS_IMAGE *ai, int bandpass, IDL_VPTR image);

/* set the background */
void setRegionBackground(REGION *reg, int bkgd);
