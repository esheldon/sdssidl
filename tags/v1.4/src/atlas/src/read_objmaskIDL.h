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

/* The output structure definition */
static IDL_MEMINT fivedims[] = {1,5};
static IDL_STRUCT_TAG_DEF stags[] = {
    { "RUN", 0, (void *) IDL_TYP_LONG},
    { "RERUN", 0, (void *) IDL_TYP_LONG},
    { "CAMCOL", 0, (void *) IDL_TYP_LONG},
    { "FIELD", 0, (void *) IDL_TYP_LONG},
    { "ID", 0, (void *) IDL_TYP_LONG},
    { "PARENT", 0, (void *) IDL_TYP_LONG},
    { "ROW0", fivedims, (void *) IDL_TYP_LONG},
    { "COL0", fivedims, (void *) IDL_TYP_LONG},
    { "ROWMAX", fivedims, (void *) IDL_TYP_LONG},
    { "COLMAX", fivedims, (void *) IDL_TYP_LONG},
    { "DROW", fivedims, (void *) IDL_TYP_LONG},
    { "DCOL", fivedims, (void *) IDL_TYP_LONG},
    { 0 }
};


typedef struct {
    int id;
    int run;
    int rerun;
    int camcol;
    int field;
    int parent;
    int row0;
    int col0;
    int rowmax;
    int colmax;
    int drow;
    int dcol;
} offsetstruct;



/* Return an offset structure */
offsetstruct GetOffsets(void* sdef);

/* get number of params */
int nParams(int argc);

/* store a value in the status variable */
static void setStatus(int statusVal);

/* Keyword structures */

typedef struct {
  IDL_KW_RESULT_FIRST_FIELD; /* Must be first entry in structure */

  IDL_VPTR idlist;
  int idlist_there;

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

  {"IDLIST", 
   IDL_TYP_UNDEF, 
   1, 
   IDL_KW_VIN, 
   IDL_KW_OFFSETOF(idlist_there), 
   IDL_KW_OFFSETOF(idlist)},


  {"STATUS", IDL_TYP_UNDEF, 1, IDL_KW_OUT | IDL_KW_ZERO, 
   IDL_KW_OFFSETOF(status_there), IDL_KW_OFFSETOF(status) },


  { NULL }
};
