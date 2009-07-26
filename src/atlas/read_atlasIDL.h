/*
  Header for read_atlasIDL.c

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

/* Copy the atlas image(s) into the output keywords */
static void copyAtlasImages(ATLAS_IMAGE *ai);

/* copy in the row0, col0, drow, dcol */
static void copyStatsFromAtlas(ATLAS_IMAGE *ai);

/* get number of params */
int nParams(int argc);

/* store a value in the status variable */
static void setStatus(int statusVal);

/* Keyword structures */

typedef struct {
  IDL_KW_RESULT_FIRST_FIELD; /* Must be first entry in structure */

  IDL_VPTR col0;
  int col0_there;

  IDL_VPTR dcol;
  int dcol_there;

  IDL_VPTR drow;
  int drow_there;

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

  IDL_VPTR ncol;
  int ncol_there;

  IDL_VPTR nrow;
  int nrow_there;

  IDL_VPTR row0;
  int row0_there;

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

  {"COL0", IDL_TYP_UNDEF, 1, IDL_KW_OUT | IDL_KW_ZERO, 
   IDL_KW_OFFSETOF(col0_there), IDL_KW_OFFSETOF(col0) },


  {"DCOL", IDL_TYP_UNDEF, 1, IDL_KW_OUT | IDL_KW_ZERO, 
   IDL_KW_OFFSETOF(dcol_there), IDL_KW_OFFSETOF(dcol) },
  {"DROW", IDL_TYP_UNDEF, 1, IDL_KW_OUT | IDL_KW_ZERO, 
   IDL_KW_OFFSETOF(drow_there), IDL_KW_OFFSETOF(drow) },

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

  {"NCOL", IDL_TYP_UNDEF, 1, IDL_KW_OUT | IDL_KW_ZERO, 
   IDL_KW_OFFSETOF(ncol_there), IDL_KW_OFFSETOF(ncol) },
  {"NROW", IDL_TYP_UNDEF, 1, IDL_KW_OUT | IDL_KW_ZERO, 
   IDL_KW_OFFSETOF(nrow_there), IDL_KW_OFFSETOF(nrow) },


  {"ROW0", IDL_TYP_UNDEF, 1, IDL_KW_OUT | IDL_KW_ZERO, 
   IDL_KW_OFFSETOF(row0_there), IDL_KW_OFFSETOF(row0) },


  {"STATUS", IDL_TYP_UNDEF, 1, IDL_KW_OUT | IDL_KW_ZERO, 
   IDL_KW_OFFSETOF(status_there), IDL_KW_OFFSETOF(status) },


  { NULL }
};
