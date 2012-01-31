/*===========================================================================
 *
 * read_objmaskIDL.c
 * 
 * Read an objmask info from the atlas image for a set of objects. This is an
 * idl DLM interface to the stand-alone reader written by Robert Lupton.  The
 * routine in IDL is called rdobjmask but it is more convenient to use the
 * read_objmask() IDL wrapper which takes care of finding the atlas image.     
 *
 * IDL> struct = rdObjmask(atlasfile, idlist=, status=)
 * IDL> struct = read_objmask(run, camcol, field, rerun=, idlist=, status=)
 *
 * Inputs: 
 *   atlasFile:  The atlas file name.  
 * Optional Inputs:
 *   idlist: The list of ids.  Default is all.
 *
 * Output:
 *   A structure for each object with the following format:
 *      ID              LONG                 1
 *      ROW0            LONG      Array[5]
 *      COL0            LONG      Array[5]
 *      ROWMAX          LONG      Array[5]
 *      COLMAX          LONG      Array[5]
 *      DROW            LONG      Array[5]
 *      DCOL            LONG      Array[5]
 *
 *
 * Revision History: 
 *   Created: 2007-02-26 Erin Sheldon, NYU 
 *   Added rowmax, colmax, 2007-03-13, Erin Sheldon, NYU
 *
 *
 *  Copyright (C) 2007  Erin Sheldon, NYU.  erin dot sheldon at gmail dot com
 *
 *    This program is free software; you can redistribute it and/or modify it
 *    under the terms of the GNU General Public License as published by the
 *    Free Software Foundation; either version 2 of the License, or (at your
 *    option) any later version.
 *
 *    This program is distributed in the hope that it will be useful, but
 *    WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
 *    Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License along
 *    with this program; if not, write to the Free Software Foundation, Inc.,
 *    51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 *
 *===========================================================================*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "dervish.h"
#include "phFits.h"
#include "phConsts.h"

#include "idl_export.h"
#include "read_atlasUtil.h"
#include "read_objmaskIDL.h"


/* 
 * Global Variables 
 */

/* Structure that holds the input keywords.  See read_atlasIDL.h */
KW_RESULT kw;

/*===========================================================================
 *
 *  rdObjMask
 *
 *  The main procedure which links to IDL.
 *
 *===========================================================================*/

IDL_VPTR rdObjmask(int argc, IDL_VPTR *argv, char *argk) {

    /* the desired atlas image. */
    ATLAS_IMAGE *ai;

    /* The fits file */
    FITS *fits;

    char *atlasFileName;  // The input file name

    IDL_VPTR idlistVptr; // optional idlist input keyword
    IDL_MEMINT* idlist;
    IDL_MEMINT nid=0;
    int idiscopy=0;

    IDL_MEMINT nrows=0;  // actual number of rows in file
    IDL_MEMINT i=0, j=0;
  
    IDL_VPTR result;  // The result and temporary copies
    char* resptr;
    char* rowptr;
    char* elptr;
    IDL_LONG elval;
    int elsize;

    // Structure definition
    void* sdef;

    // offsets structure
    offsetstruct os;



    // First make sure IDL_LONG is 4-byte int
    elsize=sizeof(IDL_LONG);
    if (elsize != 4)
    {
        IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_LONGJMP, "sizeof(IDL_LONG) must be 4");
    }


    /* Get the keywords */
    (void) IDL_KWProcessByOffset(argc, argv, argk, kw_pars, 
            (IDL_VPTR *) 0, 1, &kw);

    /* Check arguments */
    if (nParams(argc) < 1)
    {
        IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_INFO,
                "Syntax: struct = rdObjmask(atlasFile, idlist=, status=)");
        setStatus(FAILURE);
        return(IDL_GettmpInt(-1));   
    }




    /* get file name and open fits file */
    atlasFileName = getFileNameVal(argv[0]);

    if((fits = open_fits_table(atlasFileName, 1)) == NULL) {
        /* Memory is cleaned up in open_fits_table() */
        setStatus(FAILURE);
        return(IDL_GettmpInt(-1));   
    }
    nrows = fits->naxis2;




    // See if idlist was sent, else generate a list of all
    if (kw.idlist_there)
    {
        /* make sure it is a MEMINT. Copy if needed */
        idlistVptr = getAsMEMINT(argv[1], &idiscopy);
        IDL_VarGetData(idlistVptr, &nid, (char **) &idlist, IDL_TRUE);
    } 
    else
    {
        idiscopy=1;
        nid=nrows;
        idlist = (IDL_MEMINT *) calloc(nrows, sizeof(IDL_MEMINT));
        for (i=1;i<=nrows;i++)
            idlist[i-1] = i;
    }



    /* structure definition and create output */ 
    sdef = IDL_MakeStruct(0, stags);
    resptr = IDL_MakeTempStructVector(sdef, nid, &result, IDL_TRUE); 
    os = GetOffsets(sdef);




    /* Copy in the info */
    for (i=0;i<nid;i++)
    {
        // Point to the appropriate row in output data
        rowptr = (char *) result->value.s.arr->data + i*( result->value.arr->elt_len );

        // Copy id.  Do it here since if the object has no atlas image
        // it wouldn't be copied
        elval = (IDL_LONG) idlist[i];
        elptr = rowptr + os.id;
        memcpy(elptr, &elval, elsize);


        // Read the atlas image
        ai = read_atlas_image(fits, (int) idlist[i]);


        // In what follows, copy -1 if the ai is invalid
        if (ai != NULL)
        {
            // Copy run
            elval = (IDL_LONG) ai->run;
            elptr = rowptr + os.run;
            memcpy(elptr, &elval, elsize);
            // Copy rerun
            elval = (IDL_LONG) ai->rerun;
            elptr = rowptr + os.rerun;
            memcpy(elptr, &elval, elsize);
            // Copy camcol 
            elval = (IDL_LONG) ai->camCol;
            elptr = rowptr + os.camcol;
            memcpy(elptr, &elval, elsize);
            // Copy field
            elval = (IDL_LONG) ai->field;
            elptr = rowptr + os.field;
            memcpy(elptr, &elval, elsize);
            // Copy id
            //elval = (IDL_LONG) ai->id;
            //elptr = rowptr + os.id;
            //memcpy(elptr, &elval, elsize);
            // Copy parent 
            elval = (IDL_LONG) ai->parent;
            elptr = rowptr + os.parent;
            memcpy(elptr, &elval, elsize);


            // copy row0 
            elptr = rowptr + os.row0;
            for (j=0;j<5;j++)
            {
                if (ai->id > 0)
                    elval = (IDL_LONG) (ai->master_mask->rmin + ai->drow[j]);
                else
                    elval=-1;
                memcpy(elptr, &elval, elsize);
                elptr+=elsize;
            }
            // copy col0 
            elptr = rowptr + os.col0;
            for (j=0;j<5;j++)
            {
                if (ai->id > 0)
                    elval = (IDL_LONG) (ai->master_mask->cmin + ai->dcol[j]);
                else
                    elval=-1;
                memcpy(elptr, &elval, elsize);
                elptr+=elsize;
            }
            // copy rowmax
            elptr = rowptr + os.rowmax;
            for (j=0;j<5;j++)
            {
                if (ai->id > 0)
                    elval = (IDL_LONG) (ai->master_mask->rmax + ai->drow[j]);
                else
                    elval=-1;
                memcpy(elptr, &elval, elsize);
                elptr+=elsize;
            }
            // copy colmax 
            elptr = rowptr + os.colmax;
            for (j=0;j<5;j++)
            {
                if (ai->id > 0)
                    elval = (IDL_LONG) (ai->master_mask->cmax + ai->dcol[j]);
                else
                    elval=-1;
                memcpy(elptr, &elval, elsize);
                elptr+=elsize;
            }


            // copy drow
            elptr = rowptr + os.drow;
            for (j=0;j<5;j++)
            {
                if (ai->id > 0)
                    elval = (IDL_LONG) (ai->drow[j]);
                else
                    elval = -1;
                memcpy(elptr, &elval, elsize);
                elptr+=elsize;
            }
            // copy dcol 
            elptr = rowptr + os.dcol;
            for (j=0;j<5;j++)
            {
                if (ai->id > 0)
                    elval = (IDL_LONG) (ai->dcol[j]);
                else
                    elval = -1;
                memcpy(elptr, &elval, elsize);
                elptr+=elsize;
            }

            phAtlasImageDel(ai,1);
        }
    }

    /* clean up */
    phFitsDel(fits);  

    if (kw.idlist_there & idiscopy)
        IDL_Deltmp(idlistVptr);
    else
        free(idlist);

    /* Clean up the keyword info */
    IDL_KW_FREE;

    setStatus(SUCCESS);
    //return(IDL_GettmpInt(-1));   
    return(result);

}

/* 
 * Given an input IDL structure definition, return a C struct with the offsets
 * for each tag
 */

offsetstruct GetOffsets(void* sdef)
{
    offsetstruct os;
    os.id = IDL_StructTagInfoByName(sdef, "ID", 0, NULL);
    os.run = IDL_StructTagInfoByName(sdef, "RUN", 0, NULL);
    os.rerun = IDL_StructTagInfoByName(sdef, "RERUN", 0, NULL);
    os.camcol = IDL_StructTagInfoByName(sdef, "CAMCOL", 0, NULL);
    os.field = IDL_StructTagInfoByName(sdef, "FIELD", 0, NULL);

    os.parent = IDL_StructTagInfoByName(sdef, "PARENT", 0, NULL);
    os.row0 = IDL_StructTagInfoByName(sdef, "ROW0", 0, NULL);
    os.col0 = IDL_StructTagInfoByName(sdef, "COL0", 0, NULL);
    os.rowmax = IDL_StructTagInfoByName(sdef, "ROWMAX", 0, NULL);
    os.colmax = IDL_StructTagInfoByName(sdef, "COLMAX", 0, NULL);
    os.drow = IDL_StructTagInfoByName(sdef, "DROW", 0, NULL);
    os.dcol = IDL_StructTagInfoByName(sdef, "DCOL", 0, NULL);

    return(os);
}


/*
 *
 * nParams
 *
 * How many positional arguments were sent?
 *
 */

int nParams(int argc)
{

    int nKeywords;

    nKeywords =     
        kw.idlist_there + kw.status_there;

    return 
        argc - nKeywords;
}

/* set the status if it is there */
static void setStatus(int statusVal)
{

    if (kw.status_there) {
        /* This frees any existing memory and sets type to INT with value zero */
        IDL_StoreScalarZero(kw.status, IDL_TYP_INT);
        kw.status->value.i = statusVal;
    }

}

/*
 *
 * This is the code for the IDL DLM interface 
 *
 */


#define ARRLEN(arr) (sizeof(arr)/sizeof(arr[0]))

int IDL_Load(void)
{

    /* This must be static. It is a struct. */
    /* The name in strings is the name by which it will be called from IDL and
       MUST BE CAPITALIZED 
       5th parameter will say if it accepts keywords and some other flags 
       For more info see page 325 of external dev. guide */
    static IDL_SYSFUN_DEF2 func_addr[] = {
        { (IDL_SYSRTN_GENERIC) rdObjmask, 
            "RDOBJMASK", 
            0, 
            IDL_MAXPARAMS, 
            IDL_SYSFUN_DEF_F_KEYWORDS, 
            0},
    };

    /* False means it is not a function */
    return IDL_SysRtnAdd(func_addr, IDL_TRUE, ARRLEN(func_addr));

}
