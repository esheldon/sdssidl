/*
  Header for recframeIDL.c

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


static void
ai_regFrame_set(const OBJMASK *om,	/* the OBJMASK specifying the pixels */
		const U16 *pix,		/* the values to use */
		REGION *reg,		/* the region wherein they dwell */
		int drow, int dcol);    /* how much to offset om's coords */

/* Copy the atlas image(s) into the output keywords */
static void copyRegToImage(REGION *reg, IDL_VPTR image);

/* copy in the row0, col0, drow, dcol */
//static void copyStatsFromAtlas();

/* get number of params */
static int nParams(int argc);

/* store a value in the status variable */
static void setStatus(int statusVal);

/* Keyword structures */

typedef struct {
  IDL_KW_RESULT_FIRST_FIELD; /* Must be first entry in structure */


  IDL_VPTR img;
  int img_there;

  IDL_VPTR imi;
  int imi_there;

  IDL_VPTR imr;
  int imr_there;
    
  IDL_VPTR imu;
  int imu_there;

  IDL_VPTR imz;
  int imz_there;

  IDL_VPTR status;
  int status_there;

} KW_RESULT;

static IDL_KW_PAR kw_pars[] = {

  IDL_KW_FAST_SCAN, 

  /* These must be in alphabetical order */

  /* 
     IDL_KW_VIN: can be just an input, like var=3
     IDL_KW_OUT: can be output variable, var=var
     IDL_KW_ZERO: If not there, variable is set to 0

     If the VIN is set, then the _there variables will be
     0 when an undefined name variable is sent: no good for
     returning variables!
  */

  {"IMG", IDL_TYP_UNDEF, 1, IDL_KW_OUT | IDL_KW_ZERO, 
   IDL_KW_OFFSETOF(img_there), IDL_KW_OFFSETOF(img) },    

  {"IMI", IDL_TYP_UNDEF, 1, IDL_KW_OUT | IDL_KW_ZERO, 
   IDL_KW_OFFSETOF(imi_there), IDL_KW_OFFSETOF(imi) },    

  {"IMR", IDL_TYP_UNDEF, 1, IDL_KW_OUT | IDL_KW_ZERO, 
   IDL_KW_OFFSETOF(imr_there), IDL_KW_OFFSETOF(imr) },

  {"IMU", IDL_TYP_UNDEF, 1, IDL_KW_OUT | IDL_KW_ZERO, 
   IDL_KW_OFFSETOF(imu_there), IDL_KW_OFFSETOF(imu) },

  {"IMZ", IDL_TYP_UNDEF, 1, IDL_KW_OUT | IDL_KW_ZERO, 
   IDL_KW_OFFSETOF(imz_there), IDL_KW_OFFSETOF(imz) },


  {"STATUS", IDL_TYP_UNDEF, 1, IDL_KW_OUT | IDL_KW_ZERO, 
   IDL_KW_OFFSETOF(status_there), IDL_KW_OFFSETOF(status) },


  { NULL }
};
