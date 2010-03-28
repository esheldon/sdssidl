//
// htmmatchc
// 
// Use the HTM code to find matches between ra/dec lists
// Written to be linked into IDL as a DLM.  This is a 
// utility program meant to be called by the IDL wrapper
// htm_match (umsdss_idl/pro/htm/htm_match.pro).
//
// usage from IDL:  htmmatchc, ra1, dec1, ra2, dec2, htmrev2, minid, maxid,$
//                             angle, m1, m2, distance12, depth=, status=
//
// The wrapper can create the inputs htmrev2,minid,maxid for the user.
//
// Uses Peter Kunsct HTM code.
// 
// Created 19-April-2006, Erin Sheldon, NYU

#include "SpatialInterface.h"
#include "SpatialDomain.h"
#include "VarStr.h"
//#include "fstream.h"
#include <time.h>
#include "idl_export.h"

#include <stdlib.h>
#include <vector>
#include <algorithm>

using namespace std;

#include "htmMatchIDL.h"
#include "htmUtilIDL.h"

#include "errno.h"

/* Structure that holds the input keywords.  See htmIndexIDL.h */
KW_RESULT kw;

void htmmatchc(int argc, IDL_VPTR *argv, char *argk)
{


    // These will point at the input data
    double *ra1, *dec1, *ra2, *dec2, *angleinput;
    IDL_LONG *htmrev2, minLeafId, maxLeafId;
    FILE *fptr=NULL;


    // Will be copies of inputs
    IDL_MEMINT n1, n2, nangle;
    IDL_MEMINT depth;
    IDL_MEMINT maxmatch;

    IDL_LONG64 ntotal=0;

    // Process the keywords
    int nparms = IDL_KWProcessByOffset(argc, argv, argk, kw_pars, 
            (IDL_VPTR *) 0, 1, &kw);


    ///////////////////////////////////////////////////////////
    // Check the arguments
    ///////////////////////////////////////////////////////////

    if (nparms != 11)
    {
        htmMatchErrOut("-Syntax: htmmatchc, ra1, dec1, ra2, dec2, htmrev2, minLeafId, maxLeafId, angle, match1, match2, d12, depth=, maxmatch=, status=\nRA/DEC must be double");
        return;
    }

    IDL_VPTR ra1Vptr  = argv[0];
    IDL_VPTR dec1Vptr = argv[1];
    IDL_VPTR ra2Vptr  = argv[2];
    IDL_VPTR dec2Vptr = argv[3];

    IDL_VPTR htmrev2Vptr = argv[4];
    IDL_VPTR minLeafIdVptr = argv[5];
    IDL_VPTR maxLeafIdVptr = argv[6];
    IDL_VPTR angleVptr = argv[7];

    IDL_VPTR match1Vptr = argv[8];
    IDL_VPTR match2Vptr = argv[9];
    IDL_VPTR dis12Vptr = argv[10];

    // Extract the inputs
    if (!htmMatchGetData(ra1Vptr, dec1Vptr, 
                ra2Vptr, dec2Vptr, 
                htmrev2Vptr, 
                minLeafIdVptr, maxLeafIdVptr,
                angleVptr, 

                &ra1, &dec1,
                &ra2, &dec2,
                &htmrev2, 
                &minLeafId, &maxLeafId,
                &n1, &n2, 
                &angleinput, &nangle,
                &depth, &maxmatch)) 
    {
        return;
    }

    // Defaults
    IDL_StoreScalarZero(match1Vptr, IDL_TYP_LONG);
    match1Vptr->value.l = -1;
    IDL_StoreScalarZero(match2Vptr, IDL_TYP_LONG);
    match2Vptr->value.l = -1;
    IDL_StoreScalarZero(dis12Vptr, IDL_TYP_DOUBLE);
    dis12Vptr->value.d = -1;

    if (kw.file_there)
    {
        char* file = IDL_VarGetString(kw.file);
        fptr = fopen(file, "w");
        if (fptr==NULL) 
        {
            char mess[100];
            sprintf(mess,"Cannot open file %s: %s", file, strerror(errno)); 
            htmMatchErrOut(mess);
            return;
        }
        // zero for starters
        fwrite(&ntotal, sizeof(IDL_LONG64), 1, fptr);

        // The size of the indices is IDL_LONG (3) for now
        IDL_LONG sz = 3;
        fwrite(&sz, sizeof(IDL_LONG), 1, fptr);

        // Size of distance is DOUBLE (5)
        sz = 5;
        fwrite(&sz, sizeof(IDL_LONG), 1, fptr);
    }

    try {

        int savedepth = 2;
        htmInterface htm(depth,savedepth);  // generate htm interface
        const SpatialIndex &index = htm.index();

        double angle=0, d=0;

        vector <int32> m1;
        vector <int32> m2;  
        vector <float64> d12;

        if (nangle == 1) {
            angle = angleinput[0];
            d = cos( angle );
        }


        time_t tleaves=0, tother=0;

        IDL_MEMINT nFound;

        for (IDL_LONG i1=0; i1<n1; i1++) {

            time_t t0 = clock();      

            // Declare the domain and the lists
            SpatialDomain domain;    // initialize empty domain
            ValVec<uint64> plist, flist;	// List results

            vector <uint32> idList;
            int32 idCount=0;

            // Find the triangles around this object

            if (nangle > 1) {
                angle = angleinput[i1];
                d = cos( angle );
            }

            domain.setRaDecD(ra1[i1],dec1[i1],d); //put in ra,dec,d E.S.S.

            domain.intersect(&index,plist,flist);	  // intersect with list


            nFound = flist.length() + plist.length();

            // Save the result in idlist. This is not a bottleneck
            idList.resize(nFound);

            // ----------- FULL NODES -------------
            for(size_t i = 0; i < flist.length(); i++)
            {  
                idList[idCount] = (uint32 )flist(i);
                idCount++;
            }
            // ----------- Partial Nodes ----------
            for(size_t i = 0; i < plist.length(); i++)
            {  
                idList[idCount] = (uint32 )plist(i);
                idCount++;
            }

            tleaves += clock()-t0;

            // Loop over the idList and check objects in those leaf ids

            t0 = clock();


            IDL_MEMINT nKeep = 0;
            for (IDL_MEMINT j=0; j<nFound; j++) {

                int32 leafId = idList[j];

                // Make sure leaf is in list for ra2,dec2
                if ( leafId >= minLeafId && leafId <= maxLeafId) {

                    int32 leafBin = idList[j] - minLeafId;

                    // Any found in this leaf?
                    if ( htmrev2[leafBin] != htmrev2[leafBin+1] ) {

                        // Now loop over the sources
                        int32 nLeafBin = htmrev2[leafBin+1] - htmrev2[leafBin];

                        for (int32 ileaf=0; ileaf<nLeafBin;ileaf++) {

                            IDL_LONG i2 = htmrev2[ htmrev2[leafBin] + ileaf ];

                            // Returns distance in radians
                            float64 dis = gcirc(ra1[i1], dec1[i1], ra2[i2], dec2[i2]);

                            // Turns out, this pushing is not a bottleneck!
                            // Time is negligible compared to the leaf finding
                            // and the gcirc.
                            if (dis <= angle) {

                                if (kw.file_there) 
                                {
                                    fwrite(&i1, sizeof(IDL_LONG), 1, fptr);
                                    fwrite(&i2, sizeof(IDL_LONG), 1, fptr);
                                    fwrite(&dis, sizeof(float64), 1, fptr);
                                }
                                else
                                {
                                    m1.push_back(i1);
                                    m2.push_back(i2);
                                    d12.push_back(dis);
                                }
                                nKeep += 1;
                            } // Within distance 

                            /* this is WRONG, we should sort and keep the closest */
                            if (nKeep >= maxmatch)
                                break;
                        } // loop over objects in leaf 

                    } // any in leaf?

                } // leaf id in list 2?
                if (nKeep >= maxmatch)
                    break;
            } // loop over leaves

            ntotal += nKeep;
            tother += clock()-t0;

        } /* loop over first list */

        IDL_ARRAY_DIM dim;
        IDL_VPTR m1tmp;
        IDL_VPTR m2tmp;
        IDL_VPTR d12tmp;

        // Case where there were no matches
        if (ntotal == 0)
        {
            htmMatchSetStatus(NO_MATCHES);
            if (kw.file_there)
            {
                fclose(fptr);
                return;
            }
        }
        else 
        {

            // We wrote to a file, no results returned
            if (kw.file_there)
            {
                // Write out the number of pairs found and close
                rewind(fptr);
                fwrite(&ntotal, sizeof(IDL_LONG64), 1, fptr);
                fclose(fptr);
                htmMatchSetStatus(SUCCESS);
            }
            else 
            {
                dim[0] = m1.size();
                IDL_LONG *match1 = 
                    (IDL_LONG *) IDL_MakeTempArray(IDL_TYP_LONG, 1, dim, 
                            IDL_ARR_INI_NOP, &m1tmp);
                IDL_LONG *match2 = 
                    (IDL_LONG *) IDL_MakeTempArray(IDL_TYP_LONG, 1, dim, 
                            IDL_ARR_INI_NOP, &m2tmp);
                double *dis12 = 
                    (double *) IDL_MakeTempArray(IDL_TYP_DOUBLE, 1, dim, 
                            IDL_ARR_INI_NOP, &d12tmp);

                for (size_t i=0;i<m1.size();i++) {
                    match1[i] = m1[i];
                    match2[i] = m2[i];
                    dis12[i] = d12[i];
                }

                IDL_VarCopy(m1tmp, match1Vptr);
                IDL_VarCopy(m2tmp, match2Vptr);
                IDL_VarCopy(d12tmp, dis12Vptr);

                htmMatchSetStatus(SUCCESS);
            }
        } // matches found

        return;


    } catch (SpatialException x) {
        char errMessage[100];
        sprintf(errMessage, "%s", x.what());
        htmMatchErrOut(errMessage);
        return;
    }




}

int 
htmMatchGetData(IDL_VPTR ra1Vptr, IDL_VPTR dec1Vptr, 
        IDL_VPTR ra2Vptr, IDL_VPTR dec2Vptr, 
        IDL_VPTR htmrev2Vptr, 
        IDL_VPTR minLeafIdVptr, IDL_VPTR maxLeafIdVptr,
        IDL_VPTR angleVptr, 

        double **ra1, double **dec1,
        double **ra2, double **dec2, 
        IDL_LONG **htmrev2,
        IDL_LONG *minLeafId, IDL_LONG *maxLeafId,
        IDL_MEMINT *n1, IDL_MEMINT *n2,
        double **angle, IDL_MEMINT *nangle,
        IDL_MEMINT *depth, IDL_MEMINT *maxmatch)
{

    IDL_MEMINT tmp_n1, tmp_n2, tmp_nrev;

    // Extract pointers to ra/dec data
    if (!getDblPtr(ra1Vptr, ra1, n1))
    { 
        htmMatchErrOut("RA1 must be of type double");
        return(0);
    }
    if (!getDblPtr(dec1Vptr, dec1, &tmp_n1))
    { 
        htmMatchErrOut("DEC1 must be of type double");
        return(0);
    }

    if (*n1 != tmp_n1)
    {
        htmMatchErrOut("RA1 must be same length as DEC1");
        return(0);
    }

    if (!getDblPtr(ra2Vptr, ra2, n2))
    { 
        htmMatchErrOut("RA2 must be of type double");
        return(0);
    }
    if (!getDblPtr(dec2Vptr, dec2, &tmp_n2))
    { 
        htmMatchErrOut("DEC2 must be of type double");
        return(0);
    }

    if (*n2 != tmp_n2)
    {
        htmMatchErrOut("RA2 must be same length as DEC2");
        return(0);
    }



    if (!getIDLLongPtr(htmrev2Vptr, htmrev2, &tmp_nrev) )
    {
        htmMatchErrOut("htmrev2 must be type IDL_LONG");
        return(0);
    }

    *minLeafId = getIntVal(minLeafIdVptr);
    *maxLeafId = getIntVal(maxLeafIdVptr);

    if (!getDblPtr(angleVptr, angle, nangle))
    { 
        htmMatchErrOut("Angle must be of type double");
        return(0);
    }

    if ( (*nangle != *n1) && (*nangle != 1) )
    {
        htmMatchErrOut("Angle must be a scalar or same length as RA1/DEC1");
        return(0);
    }


    if (kw.depth_there)
        *depth = IDL_MEMINTScalar(kw.depth);
    else
        *depth = DEFAULT_DEPTH;

    if (kw.maxmatch_there)
        *maxmatch = IDL_MEMINTScalar(kw.maxmatch);
    else
        *maxmatch = DEFAULT_MAXMATCH;

    return(1);

}




    double
gcirc(double ra1, double dec1, 
        double ra2, double dec2)

{


    double sindec1, cosdec1, sindec2, cosdec2, 
           radiff, cosradiff, cosdis; 

    static const double
        D2R=0.0174532925199433;

    sindec1 = sin(dec1*D2R);
    cosdec1 = cos(dec1*D2R);

    sindec2 = sin(dec2*D2R);
    cosdec2 = cos(dec2*D2R);

    radiff = (ra1-ra2)*D2R;
    cosradiff = cos(radiff);

    cosdis = sindec1*sindec2 + cosdec1*cosdec2*cosradiff;

    if (cosdis < -1.0) cosdis=-1.0;
    if (cosdis >  1.0) cosdis= 1.0;

    return( acos(cosdis) );

}








    void
htmMatchErrOut(const char *message)
{

    IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_INFO, message);

    if (kw.status_there)
        htmMatchSetStatus(FAILURE);

    /* Clean up the keyword info */
    IDL_KW_FREE;

}

/* set the status if it is there */
    static void
htmMatchSetStatus(int statusVal)
{

    if (kw.status_there) {
        /* This frees any existing memory and sets type to INT with value zero */
        IDL_StoreScalarZero(kw.status, IDL_TYP_INT);
        kw.status->value.i = statusVal;
    }

}

/*===========================================================================
 * htmMatchNParams
 * How many positional arguments were sent?
 *===========================================================================*/

int htmMatchNParams(int argc)
{

    int nKeywords;

    nKeywords = 
        kw.depth_there + kw.maxmatch_there + kw.status_there;

    return 
        argc - nKeywords;

}


//////////////////////////////////////////////////////////////////////////////
// This defines the IDL_Load function used for Dynamically Loadable Modules
// It includes a fix for the name mangling done by g++
//////////////////////////////////////////////////////////////////////////////

#define ARRLEN(arr) (sizeof(arr)/sizeof(arr[0]))

/*
 * Here's the code to fix the name mangling of g++
 */

//
// First is the name twist of the original function
//
int IDL_Load_(void);

//
// Next are the shells declared with "external C"
//
extern "C" {
    int IDL_Load(void);
}

//
// Last are the one-line functions to call the originals
//
int IDL_Load(void) {
    return(IDL_Load_());
}

int IDL_Load_(void)
{

    /* This must be static. It is a struct. */
    /* The name in strings is the name by which it will be called from IDL and
       MUST BE CAPITALIZED 
       5th parameter will say if it accepts keywords and some other flags 
       For more info see page 325 of external dev. guide */
    static IDL_SYSFUN_DEF2 func_addr[] = {
        { (IDL_SYSRTN_GENERIC) htmmatchc, 
            static_cast<char *>("HTMMATCHC"), 0, IDL_MAXPARAMS, 
            IDL_SYSFUN_DEF_F_KEYWORDS, 0},
    };

    /* The false means it is not a function */
    return IDL_SysRtnAdd(func_addr, IDL_FALSE, ARRLEN(func_addr));

}
