/*
 * File object class used by the IDL interface functions 
 *        ascii_read, binary_read, ascii_write
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
 *    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA#include "FileObj.hpp"
 *
 */

#include "FileObj.hpp"

//////////////////////////////////////////////////////////////////////////
//
// Initialization
//
//////////////////////////////////////////////////////////////////////////

// Constructor.  This is all generic with respect to reading and
// writing.  
FileObj::FileObj(
        int npars, 
        IDL_VPTR argv[], 
        char     argk[], 
        KW_RESULT* input_kw,
        int      iAction) 
{

    mFileVptr=NULL;
    mpFileName=NULL;
    mFptr=NULL;
    mResultVptr = NULL;
    mpResult=NULL;
    mStructdefVptr=NULL;
    mpTagInfo=NULL;
    mpCurrentRow=NULL;

    mDelimSent=0;

    // Just keep a pointer to it
    kw=input_kw;

    // Get the delimeter if sent.
    GetDelim();

    if (ACTION_READ_ASCII == iAction || ACTION_READ_BINARY == iAction)
    {

        mReadAction = iAction;
        if (!InitRead(npars, argv, argk))
           return; 

        // Make buffers for reading strings in binary mode
        if (ACTION_READ_BINARY == iAction)
        {
            MakeBuffers();
        }
    }
    else if (ACTION_WRITE_ASCII == iAction)
    {
        InitWrite(npars, argv, argk);
    }
    else
    {
        char mess[30];
        sprintf(mess, "Invalid action: %d", iAction);
        Message(mess);
        SetStatusKeyword(INPUT_ERROR);
        mStatus = FILEOBJ_INIT_ERROR;
    }
}


int FileObj::InitRead(
        int      npars, 
        IDL_VPTR argv[], 
        char     argk[])
{

    int status=0;
    // internal status
    mStatus = FILEOBJ_OK;

    mNumParams = npars;

    // Keyword status. No success until we read the file
    SetStatusKeyword(READ_FAILURE);

    // Should we just print a help statement?
    mHelp=0;
    if (kw->help_there)
    { 
        mHelp = kw->help;
        if (mHelp != 0) 
        {
            SetStatusKeyword(HELP_REQUEST);
            mStatus = FILEOBJ_PRINT_HELP;
            return(status);
        }
    }

    if (mNumParams != 3)
    {
        SetStatusKeyword(INPUT_ERROR);
        mStatus = FILEOBJ_SYNTAX_ERROR;
        return(status);
    }

    
    // Get the arguments
    mFileVptr = argv[0];
    mStructdefVptr = argv[1];
    mNumRowsInFile = IDL_MEMINTScalar(argv[2]);

    // Verbosity level
    mVerbose = 0;
    if (kw->verbose_there) mVerbose = kw->verbose;

    // Should we skip lines?
    mSkipLines=0;
    if (kw->skiplines_there) mSkipLines = kw->skiplines;

    // Get the structdef info
    if (!GetStructdefInfo()) return(status);

    // Get the input rows if there are any
    if (!GetInputRows()) return(status);





    // See which columns should be read
    //if (!GetInputColumns()) return(status);

    // Make sure they see what we wrote
    if (mVerbose) fflush(stdout);

    status=1;
    return(status);

}


int FileObj::InitWrite(
        int npars, 
        IDL_VPTR argv[], 
        char argk[])
{

    int status=0;

    // internal status
    mStatus = FILEOBJ_OK;


    mNumParams = npars;

    // Keyword status. No success until we read the file
    SetStatusKeyword(WRITE_FAILURE);

    // Should we just print a help statement?
    mHelp=0;
    if (kw->help_there)
    { 
        mHelp = kw->help;
        if (mHelp != 0) 
        {
            SetStatusKeyword(HELP_REQUEST);
            mStatus = FILEOBJ_PRINT_HELP;
            return(status);
        }
    }

    if (mNumParams != 2)
    {
        SetStatusKeyword(INPUT_ERROR);
        mStatus = FILEOBJ_SYNTAX_ERROR;
        return(status);
    }


    // Get the arguments
    mStructdefVptr = argv[0];
    mFileVptr = argv[1];

    // Should we print entire help statement when printing syntax?
    mHelp=0;
    if (kw->help_there) mHelp = kw->help;

    // verbosity level
    mVerbose = 0;
    if (kw->verbose_there) mVerbose = kw->verbose;

    // Get the structdef info
    if (!GetStructdefInfo()) return(status);

    if (mVerbose) fflush(stdout);

    status=1;
    return(status);

}



// I was careful to use nice classes everywhere
// so no explicit cleanup is needed 
FileObj::~FileObj() 
{
    //IDL_KW_FREE;
}



/////////////////////////////////////////////////////////////////////////////
//
// Get information about the input structure and store in such a way
// that is more easily accessible
//
/////////////////////////////////////////////////////////////////////////////

int FileObj::GetStructdefInfo()
{
    if (!mIdlst.Init(mStructdefVptr))
    {
        Message("The second argument must be a structure");
        SetStatusKeyword(INPUT_ERROR);
        mStatus = FILEOBJ_INIT_ERROR;
        return(0);
    }
    mpTagInfo = mIdlst.GetTagInfo();
    return(1);
}

int FileObj::MakeBuffers()
{

    mTagBuffers.resize(mpTagInfo->NumTags);

    for (IDL_MEMINT tag=0; tag<mpTagInfo->NumTags; tag++)
    {
        if (mpTagInfo->TagDesc[tag]->type == IDL_TYP_STRING)
        {
            // WARNING: This assumes that the elements in string arrays are all
            // the same size
            mTagBuffers[tag].resize(mpTagInfo->TagBytes[tag]+1);
        }
    }
    return(1);

}


/////////////////////////////////////////////////////////////////////////////
//
// Create the output structure
//
/////////////////////////////////////////////////////////////////////////////

void FileObj::CreateOutputStruct()
{

    if (!kw->columns_there) 
    {
        mpResult = 
            IDL_MakeTempStructVector(mStructdefVptr->value.s.sdef, 
                    (IDL_MEMINT) mNumRowsToGet, 
                    &mResultVptr, 
                    IDL_TRUE);
        // Getting all tags
        mGetColumn.resize(mpTagInfo->NumTags,1);
    } 
    else 
    {
        mIdlst.MatchIDL_VPTR2Tags(kw->columns, mColsToGet, mGetColumn);
        mpResult=mIdlst.MakeSubStruct(mColsToGet, mNumRowsToGet, &mResultVptr);

        // We could also do the following but we want to use col numbers
        //mpResult=mIdlst.MakeSubStruct(kw->columns, mNumRowsToGet, &mResultVptr);
    }

    // Call a method to get tag info
    mIdlst.MakeTagInfo(mResultVptr, mOutputTagInfo);
}


IDL_VPTR FileObj::OutputStruct()
{
    return(mResultVptr);
}









/////////////////////////////////////////////////////////////////////////////
//
// Open and close the file
//
/////////////////////////////////////////////////////////////////////////////

int FileObj::OpenFile(char* mode)
{

    int status=0;
    if (mFileVptr->type == IDL_TYP_STRING)
    {
        // Get the file name 
        mpFileName = IDL_VarGetString(mFileVptr);
        if (mVerbose) printf("Opening file = %s with mode \"%s\"\n", 
                mpFileName, mode);
        //    IDL_FileStat()  
        if (!(mFptr = fopen(mpFileName, mode)))
        {
            Message("Error opening file");
        } else {
            status=1;
        }
    }
    else
    {
        status = GetFilePtr(mode);
    }
    return(status);

}


int FileObj::GetFilePtr(char* mode)
{

    IDL_FILE_STAT stat_blk;

    IDL_ENSURE_SCALAR(mFileVptr);
    IDL_LONG unit = IDL_LongScalar(mFileVptr);

    IDL_FileStat(unit, &stat_blk);

    if (*mode == 'w' || *mode=='a')
    {
        if ( !(stat_blk.access & IDL_OPEN_W) &&
                !(stat_blk.access & IDL_OPEN_APND) &&
                !(stat_blk.access & IDL_OPEN_NEW) )
        {
            Message("File must be opened for writing");
            return(0);
        }
    }
    else if (*mode == 'r')
    {
        if ( !(stat_blk.access & IDL_OPEN_R) )
        {
            Message("File must be opened for reading");
            return(0);
        }
    }
    else
    {
        char mess[25];
        sprintf(mess,"Unknown file mode %s", mode);
        Message(mess);
        return(0);
    }

    if ( ! (stat_blk.flags & IDL_F_STDIO) ) 
        Message("File not opened with /stdio flag.  Reading may fail");

    if ( !(mFptr = stat_blk.fptr) )
    {
        Message("FILEPTR is invalid");
        return(0);
    }

    return(1);
}


void FileObj::CloseFile()
{
    // Only close if it wasn't already opened on input
    if (mFileVptr->type == IDL_TYP_STRING)
        fclose(mFptr);
}

int FileObj::SkipRows(IDL_MEMINT nskip)
{
    int status=0;
    if (ACTION_READ_ASCII == mReadAction)
    {
        status=SkipAsciiLines(nskip);
    } else
    {
        status=SkipBinaryRows(nskip);
    }
    return(status);
}

int FileObj::SkipAsciiLines(IDL_MEMINT nskip)
{
    if (nskip > 0)
    {
        IDL_MEMINT nlines = 0;
        char c;
        while (nlines < nskip) 
        {
            c = fgetc(mFptr);
            if (c == EOF) 
                return(0);
            if (c == '\n') 
                nlines++;
        }
    }
    return(1);
}

int FileObj::SkipBinaryRows(IDL_MEMINT nskip)
{
    if (nskip > 0)
    {
        if (fseek(mFptr, mpTagInfo->BytesPerRow*nskip, SEEK_CUR) != 0)
            return(0);
    }
    return(1);
}




// 
// Generic: started with binary
//

int FileObj::ReadFile()
{

    // Only this can result in no returned data at this point.
    if (!OpenFile("r")) 
    {
        SetStatusKeyword(READ_FAILURE);
        Message("No results returned");
        return(0);
    }

    // Skip any lines/rows if requested
    if (!SkipRows(mSkipLines))
    {
        SetStatusKeyword(READ_FAILURE);
        Message("No results returned");
        return(0);
    }

    // The output data structure
    CreateOutputStruct();

    // In some cases can just read the whole row at once
    if ( mOutputTagInfo.NumTags == mpTagInfo->NumTags 
            && ACTION_READ_BINARY == mReadAction && mpTagInfo->NStrings == 0)
        mReadWholeRowBinary=1;
    else
        mReadWholeRowBinary=0;


    if (ACTION_READ_ASCII == mReadAction)
    {
        // Should we read by delimiters or just whitespace?
        CheckDelimKeywords();
        GetScanFormats();
    }


    // Loop over the mRowsToGet
    IDL_MEMINT row2get, current_row=0, rows2skip;

    for (IDL_MEMINT irow2get=0; irow2get < mNumRowsToGet; irow2get++)
    {

        row2get = mRowsToGet[irow2get];

        // Are we skipping any rows? If not, then we just read
        if (current_row < row2get)
        {

            // Special case here for ascii reading with whitespace
            if (ACTION_READ_ASCII == mReadAction)
            {
                // If whitespace reading and not the first row, we must also 
                // grab the \n from the last line since it will be gobbled 
                // by scanf
                if (irow2get == 0)
                    rows2skip = row2get - current_row;
                else
                {
                    if (mReadAsWhitespace)
                        rows2skip = row2get - current_row + 1;
                    else
                        rows2skip = row2get - current_row;
                }
            } else {
                rows2skip = row2get-current_row;
            }

            if (!SkipRows(rows2skip))
            {
                SetStatusKeyword(READ_FAILURE);
                Message("Requested data beyond EOF. WARNING: Result will contain data read up to this error for debugging purposes.");
                break;
            }
            current_row = row2get;
        }

        // Point to the structure row in memory.
        mpCurrentRow = 
            (char *) mResultVptr->value.s.arr->data + irow2get*( mResultVptr->value.arr->elt_len );


        // Read the row
        if (!ReadRow())
        {
            SetStatusKeyword(READ_FAILURE);
            Message("Requested data beyond EOF. WARNING: Result will contain data read up to this error for debugging purposes.");
            break;
        }

        current_row++;

    } // Loop over mRowsToGet

    SetStatusKeyword(READ_SUCCESS);
    CloseFile();
    return(1);

}

int FileObj::ReadRow()
{
    /* Buffer for reading skipped columns and buffering string data
      in ascii*/
    IDL_MEMINT otag=0;
    int status=0;


    // In some cases can just read the whole row at once
    if (mReadWholeRowBinary)
    {
        int nRead = fread(
                mpCurrentRow, 1, mpTagInfo->BytesPerRow, mFptr);

        // check for read errors 
        if (nRead != mpTagInfo->BytesPerRow)
            return(0);
        else
            return(1);
    }

    for (IDL_MEMINT tag=0; tag < mpTagInfo->NumTags; tag++ )
    {

        if (ACTION_READ_ASCII == mReadAction)
        {
            // For ascii, we skip tags by reading them into mBuffer[256]
            char *tptr;
            if (mGetColumn[tag])
            {
                tptr = mpCurrentRow + mOutputTagInfo.TagOffsets[otag];
                otag++;
            }
            else
                tptr = mBuffer2;

            if (mpTagInfo->TagDesc[tag]->type != IDL_TYP_STRING)
                status = ReadAsciiNumbers(tptr, tag);
            else
                status = ReadAsciiStrings(tptr, tag);
            if (!status) 
                return(0);

        } // Ascii reading
        else
        {
            if (mGetColumn[tag])
            {
                char* tptr=mpCurrentRow + mOutputTagInfo.TagOffsets[otag];
                if (mpTagInfo->TagDesc[tag]->type != IDL_TYP_STRING)
                    status = ReadBinaryNumbers(tptr, tag);
                else
                    status = ReadBinaryStrings(tptr, tag);
                if (!status)
                    return(0);
                // Increment output tag index
                otag++;
            }
            else
            {
                if (!SkipBinaryTag(tag))
                    return(0);
            }

        } // Binary reading

    }
    return(1);

}

int FileObj::SkipBinaryTag(IDL_MEMINT tag)
{
    if (fseek(mFptr, 
                mpTagInfo->TagBytes[tag]*mpTagInfo->TagNelts[tag], 
                SEEK_CUR) != 0)
        return(0);
    else
        return(1);
}


int FileObj::ReadBinaryNumbers(char* tptr, IDL_MEMINT tag)
{
    int nRead = fread(tptr, 
            mpTagInfo->TagBytes[tag], 
            mpTagInfo->TagNelts[tag], 
            mFptr);

    // check for read errors 
    if (nRead != mpTagInfo->TagNelts[tag])
        return(0);
    else
        return(1);

}

int FileObj::ReadBinaryStrings(char* tptr, IDL_MEMINT tag)
{

    // Strings: Need to loop over and store each separately in a
    // buffer before copying to struct.  This will work for
    // scalars and arrays of strings.  Note we assume all strings
    // in array are same length as the example given in input
    // struct! 

    IDL_STRING *tStringPtr = (IDL_STRING *) tptr;
    for (IDL_MEMINT i=0; i<mpTagInfo->TagNelts[tag]; i++)
    {
        int nRead = fread( 
                (char *) &(mTagBuffers[tag])[0], 
                mpTagInfo->TagBytes[tag], 
                1, 
                mFptr);

        /* check for read errors */
        if (nRead != 1)
            return(0);

        IDL_StrStore(tStringPtr, (char *)mTagBuffers[tag].c_str());
        tStringPtr++;
    }

    return(1);

}


int FileObj::ReadAsciiNumbers(char* tptr, IDL_MEMINT tag)
{
    /* loop over elements */
    for (IDL_MEMINT i=0; i<mpTagInfo->TagNelts[tag]; i++)
    {
        if (!ScanVal(mpTagInfo->TagDesc[tag]->type, tptr))
        {
            SetStatusKeyword(READ_FAILURE);
            return(0);
        }
        tptr += mpTagInfo->TagBytes[tag];
    }
    return(1);
}

// This assumes reads from the current location in the file to
// the next occurrence of the delimiter or end of line or file
void FileObj::ReadDelimStringField(char* buffer, char delim)
{
    // Read until we get the delimiter or a newline or EOF
    // Don't read past the buffer length, accounting for null at the end
    IDL_MEMINT count=0;
    char c=fgetc(mFptr);
    while (c != delim && c != '\n' && c != EOF && count < FILEOBJ_MAXSTR)
    {
        *buffer = c;
        buffer++;
        // Number of characters copied
        count++;
        c=fgetc(mFptr);

    }
    *buffer = '\0';
}

void FileObj::ReadAsciiBytes(char* buffer, IDL_MEMINT input_nbytes)
{
    IDL_MEMINT nbytes=FILEOBJ_MAXSTR;

    // Don't read past the buffer length, accounting for null at the end
    if (input_nbytes < nbytes) 
        nbytes=input_nbytes;

    for (IDL_MEMINT j=0; j<nbytes; j++)
    {
        // Read the number of expected bytes, and pad with 
        // null character
        char c=fgetc(mFptr);
        if (c==EOF || c=='\n')
        {
            *buffer='\0';
            break;
        }
        *buffer = c;
        buffer++;
    }

    *buffer='\0';

}

int FileObj::ReadAsciiStrings(char* tptr, IDL_MEMINT tag)
{
    int nbytes = mpTagInfo->TagBytes[tag];
    IDL_STRING *tStringPtr = (IDL_STRING *) tptr;
    for (IDL_MEMINT i=0; i<mpTagInfo->TagNelts[tag]; i++)
    {
        // for not csv, we have not read the delimiter yet
        char c;
        if (mReadAsWhitespace)
        {
            c = fgetc(mFptr);
            if (c == EOF)
                return(0);
        }

        // For fixed delimiters we support variable length fields
        if (!mReadAsWhitespace)
        {
            ReadDelimStringField(mBuffer, mDelim);
        }
        else
        {
            ReadAsciiBytes(mBuffer, nbytes);
        }

        // Store the string in the IDL variable
        IDL_StrStore(tStringPtr, mBuffer);
        tStringPtr++;


    } // loop over number of strings we are reading
    return(1);

}





int FileObj::ScanVal(int   type, 
        char* buffer)
{

    int ret = fscanf(mFptr, mScanFormats[type].c_str(), buffer);
    if (ret != 1)
    {
        if (feof(mFptr))
            Message("ScanVal: EOF reached unexpectedly");
        else
        {
            char mess[50];
            sprintf(mess,"ScanVal: Read error: %d",ret);
            Message(mess);
        }
        return(0);
    }

    return(1);
}


void FileObj::GetScanFormats()
{

    int nf=16;
    mScanFormats.resize(nf);
    
    /* Need spaces before delimeters to allow for them in file 
       This works because scanf treats blanks as zero or more
       white space. Won't actually use the string one unless 
       the delimiter just white space */

    /*
    mScanFormats[0] = "NONE"; // Undef
    mScanFormats[1] = "%hhu "; // idl_byte
    mScanFormats[2] = "%hd "; // idl_int
    mScanFormats[3] = "%d "; // idl_long
    mScanFormats[4] = "%f "; // idl_float
    mScanFormats[5] = "%lf "; // idl_double
    mScanFormats[6] = "NONE"; // idl_complex
    mScanFormats[7] = "%s "; // idl_string
    mScanFormats[8] = "NONE"; // idl_struct
    mScanFormats[9] = "NONE"; // idl_double_complex
    mScanFormats[10] = "NONE"; // idl_ptr
    mScanFormats[11] = "NONE"; // idl_objref
    mScanFormats[12] = "%hu "; // idl_uint
    mScanFormats[13] = "%u "; // idl_ulong
    mScanFormats[14] = "%Ld "; // idl_long64
    mScanFormats[15] = "%Lu "; // idl_ulong64
    */
    mScanFormats[0] = "NONE"; // Undef
    mScanFormats[1] = "%hhu"; // idl_byte
    mScanFormats[2] = "%hd"; // idl_int
    mScanFormats[3] = "%d"; // idl_long
    mScanFormats[4] = "%f"; // idl_float
    mScanFormats[5] = "%lf"; // idl_double
    mScanFormats[6] = "NONE"; // idl_complex
    mScanFormats[7] = "%s"; // idl_string
    mScanFormats[8] = "NONE"; // idl_struct
    mScanFormats[9] = "NONE"; // idl_double_complex
    mScanFormats[10] = "NONE"; // idl_ptr
    mScanFormats[11] = "NONE"; // idl_objref
    mScanFormats[12] = "%hu"; // idl_uint
    mScanFormats[13] = "%u"; // idl_ulong
    mScanFormats[14] = "%Ld"; // idl_long64
    mScanFormats[15] = "%Lu"; // idl_ulong64
 
    if (!mReadAsWhitespace)
    {
        for (int i=0;i<nf;i++)
        {
            mScanFormats[i] += ' ';
            mScanFormats[i] += mDelim;
        }
    }

    return;


    if (mReadAsCsv) 
    {

        /* Need spaces before comma to allow for them in file 
           This works because scanf treats blanks as zero or more
           white space. Won't actually use the string one */
        mScanFormats[0] = "NONE"; // Undef
        mScanFormats[1] = "%hhu ,"; // idl_byte
        mScanFormats[2] = "%hd ,"; // idl_int
        mScanFormats[3] = "%d ,"; // idl_long
        mScanFormats[4] = "%f ,"; // idl_float
        mScanFormats[5] = "%lf ,"; // idl_double
        mScanFormats[6] = "NONE"; // idl_complex
        mScanFormats[7] = "%[^,],"; // idl_string
        mScanFormats[8] = "NONE"; // idl_struct
        mScanFormats[9] = "NONE"; // idl_double_complex
        mScanFormats[10] = "NONE"; // idl_ptr
        mScanFormats[11] = "NONE"; // idl_objref
        mScanFormats[12] = "%hu ,"; // idl_uint
        mScanFormats[13] = "%u ,"; // idl_ulong
        mScanFormats[14] = "%Ld ,"; // idl_long64
        mScanFormats[15] = "%Lu ,"; // idl_ulong64

    }
    else if (mReadAsTab)
    {
        /* Need spaces before comma to allow for them in file 
           This works because scanf treats blanks as zero or more
           white space. Won't actually use the string one */
        mScanFormats[0] = "NONE"; // Undef
        mScanFormats[1] = "%hhu \t"; // idl_byte
        mScanFormats[2] = "%hd \t"; // idl_int
        mScanFormats[3] = "%d \t"; // idl_long
        mScanFormats[4] = "%f \t"; // idl_float
        mScanFormats[5] = "%lf \t"; // idl_double
        mScanFormats[6] = "NONE"; // idl_complex
        mScanFormats[7] = "%[^\t]\t"; // idl_string
        mScanFormats[8] = "NONE"; // idl_struct
        mScanFormats[9] = "NONE"; // idl_double_complex
        mScanFormats[10] = "NONE"; // idl_ptr
        mScanFormats[11] = "NONE"; // idl_objref
        mScanFormats[12] = "%hu \t"; // idl_uint
        mScanFormats[13] = "%u \t"; // idl_ulong
        mScanFormats[14] = "%Ld \t"; // idl_long64
        mScanFormats[15] = "%Lu \t"; // idl_ulong64


    }
    else if (mDelimSent) 
    {
        mScanFormats[0] = "NONE"; // Undef
        mScanFormats[1] = "%hhu "; // idl_byte
        mScanFormats[2] = "%hd "; // idl_int
        mScanFormats[3] = "%d "; // idl_long
        mScanFormats[4] = "%f "; // idl_float
        mScanFormats[5] = "%lf "; // idl_double
        mScanFormats[6] = "NONE"; // idl_complex
        mScanFormats[7] = " "; // idl_string
        mScanFormats[8] = "NONE"; // idl_struct
        mScanFormats[9] = "NONE"; // idl_double_complex
        mScanFormats[10] = "NONE"; // idl_ptr
        mScanFormats[11] = "NONE"; // idl_objref
        mScanFormats[12] = "%hu "; // idl_uint
        mScanFormats[13] = "%u "; // idl_ulong
        mScanFormats[14] = "%Ld "; // idl_long64
        mScanFormats[15] = "%Lu "; // idl_ulong64

        for (int i=0;i<nf;i++)
        {
            mScanFormats[i] += mDelim;
        }
    }
    else
    {

        mScanFormats[0] = "NONE"; // Undef
        mScanFormats[1] = "%hhu"; // idl_byte
        mScanFormats[2] = "%hd"; // idl_int
        mScanFormats[3] = "%d"; // idl_long
        mScanFormats[4] = "%f"; // idl_float
        mScanFormats[5] = "%lf"; // idl_double
        mScanFormats[6] = "NONE"; // idl_comples
        mScanFormats[7] = "%s"; // idl_string
        mScanFormats[8] = "NONE"; // idl_struct
        mScanFormats[9] = "NONE"; // idl_double_complex
        mScanFormats[10] = "NONE"; // idl_ptr
        mScanFormats[11] = "NONE"; // idl_objref
        mScanFormats[12] = "%u"; // idl_uint
        mScanFormats[13] = "%hu"; // idl_ulong
        mScanFormats[14] = "%Ld"; // idl_long64
        mScanFormats[15] = "%Lu"; // idl_ulong64

    }

}




/////////////////////////////////////////////////////////////////////////////
//
// Write the input structure as Ascii
//
/////////////////////////////////////////////////////////////////////////////

int FileObj::WriteAsAscii()
{

    // Only this can result in no returned data at this point.
    string mode="w";
    if (kw->append_there)
        if (kw->append != 0) mode="a";

    if (!OpenFile((char *) mode.c_str())) 
    {
        SetStatusKeyword(READ_FAILURE);
        Message("No results returned");
        return(0);
    }

    // Should we place brackets around the arrays?
    if ( (kw->bracket_arrays_there) && (kw->bracket_arrays != 0) )
    {
        mBracketArrays = 1;
        mArrayDelim = ',';
    }
    else 
    {
        mBracketArrays = 0;
        mArrayDelim = mDelim;
    }


    IDL_MEMINT nrows = (IDL_MEMINT) mStructdefVptr->value.s.arr->n_elts;
    for (IDL_MEMINT row=0; row<nrows; row++)
    {
        // Process the fields for this row
        for (IDL_MEMINT tag=0; tag<mpTagInfo->NumTags;tag++)
        {

            /* Arrays */
            if ((mpTagInfo->TagDesc[tag]->flags & IDL_V_ARR) != 0) 
            {

                if (mBracketArrays) 
                    fprintf(mFptr,"{");

                for (IDL_MEMINT el=0;el<mpTagInfo->TagNelts[tag];el++)
                {
                    UCHAR* tptr = (mStructdefVptr->value.s.arr->data +
                            row*mStructdefVptr->value.arr->elt_len + 
                            mpTagInfo->TagOffsets[tag] + 
                            el*mpTagInfo->TagDesc[tag]->value.arr->elt_len
                            );
                    AsciiPrint(mpTagInfo->TagDesc[tag]->type, tptr);
                    if (el < mpTagInfo->TagNelts[tag]-1)
                        fputc(mArrayDelim,mFptr);

                }
                if (mBracketArrays) 
                    fprintf(mFptr,"}");

            }
            else 
            {
                UCHAR* tptr = (mStructdefVptr->value.s.arr->data +
                        row*mStructdefVptr->value.arr->elt_len + 
                        mpTagInfo->TagOffsets[tag]
                        );
                AsciiPrint(mpTagInfo->TagDesc[tag]->type, tptr);
            }

            if (tag < mpTagInfo->NumTags-1) 
                fputc(mDelim,mFptr);
            else
                fputc('\n',mFptr);


        } // loop over tags
    } // loop over rows


    SetStatusKeyword(WRITE_SUCCESS);
    CloseFile();
    return(1);
}




void FileObj::AsciiPrint(
        int idlType, 
        UCHAR* tptr)
{

    switch(idlType)
    {
        case IDL_TYP_FLOAT: 
            fprintf(mFptr, "%g", *(float *)tptr);
            break;
        case IDL_TYP_DOUBLE:
            fprintf(mFptr, "%15.8e", *(double *)tptr);
            break;
        case IDL_TYP_BYTE:
            fprintf(mFptr, "%d", *(short *)tptr);
            break;
        case IDL_TYP_INT:
            fprintf(mFptr, "%d", *(short *)tptr);
            break;
        case IDL_TYP_UINT:
            fprintf(mFptr, "%u", *(unsigned short *)tptr);
            break;
        // Note we err on the side of ld in case this platform
        // has long ints or something is not right
        case IDL_TYP_LONG:
            fprintf(mFptr, "%ld", *(IDL_LONG *)tptr);
            break;
        case IDL_TYP_ULONG:
            fprintf(mFptr, "%lu", *(IDL_ULONG *)tptr);
            break;
        case IDL_TYP_LONG64:
            fprintf(mFptr, "%lld", *(IDL_LONG64 *)tptr);
            break;
        case IDL_TYP_ULONG64:
            fprintf(mFptr, "%llu", *(IDL_ULONG64 *)tptr);
            break;
        case IDL_TYP_STRING:
            fprintf(mFptr, "%s", ( (IDL_STRING *)tptr )->s);
            break;
        default: 
            printf("Unsupported type %d found\n", idlType);
            fflush(stdout);
            break;
    }
}



void FileObj::StdoutPrint(
        int idlType, 
        UCHAR* tptr)
{

    switch(idlType)
    {
        case IDL_TYP_FLOAT: 
            printf("%g", *(float *)tptr);
            break;
        case IDL_TYP_DOUBLE:
            printf("%15.8e", *(double *)tptr);
            break;
        case IDL_TYP_BYTE:
            printf("%d", *(short *)tptr);
            break;
        case IDL_TYP_INT:
            printf("%d", *(short *)tptr);
            break;
        case IDL_TYP_UINT:
            printf("%u", *(unsigned short *)tptr);
            break;
        // Note we err on the side of ld in case this platform
        // has long ints or something is not right
        case IDL_TYP_LONG:
            printf("%ld", *(IDL_LONG *)tptr);
            break;
        case IDL_TYP_ULONG:
            printf("%lu", *(IDL_ULONG *)tptr);
            break;
        case IDL_TYP_LONG64:
            printf("%lld", *(IDL_LONG64 *)tptr);
            break;
        case IDL_TYP_ULONG64:
            printf("%llu", *(IDL_ULONG64 *)tptr);
            break;
        case IDL_TYP_STRING:
            printf("%s", ( (IDL_STRING *)tptr )->s);
            break;
        default: 
            printf("Unsupported type %d found\n", idlType);
            fflush(stdout);
            break;
    }
}

























/////////////////////////////////////////////////////////////////////////////
//
// Check if a row array has been input.  Select the unique elements within
// allowed range
//
/////////////////////////////////////////////////////////////////////////////

int FileObj::GetInputRows() 
{
    if (kw->rows_there)
    {

        IDL_ENSURE_SIMPLE(kw->rows);

        int rowsConverted = 0;
        if (kw->rows->type != IDL_TYP_MEMINT)
            rowsConverted = 1;

        // Will return same variable if it is already right type
        // We have to explicitly release this mem; but at least IDL
        // will warn us if we forget
        IDL_VPTR rowsV = IDL_CvtMEMINT(1, &kw->rows);

        IDL_MEMINT *rowsPtr;
        IDL_MEMINT nrows; // n_elements in the input row array; limited to IDL_MEMINT

        // The true will ensure simple, which we have already done 
        // WARNING: Need to only *return* as many rows as are allowed by the IDL_MEMINT
        // size for the current machine. 
        IDL_VarGetData(rowsV, &nrows, (char **) &rowsPtr, IDL_TRUE);

        if (nrows > 0)
        {

            for (IDL_MEMINT i=0; i< nrows; i++)
            {
                if (rowsPtr[i] >= 0 && rowsPtr[i] < mNumRowsInFile)
                    mRowsToGet.push_back(rowsPtr[i]);
            }

            if (rowsConverted) IDL_Deltmp(rowsV);


            if (mRowsToGet.size() > 0)
            {
                // Sort input rows
                sort(mRowsToGet.begin(), mRowsToGet.end());
                // This pushes the dups to the back, so we will have to 
                // count from the iterator
                vector<IDL_MEMINT>::iterator 
                    last_row = unique(mRowsToGet.begin(), mRowsToGet.end());

                // count unique rows
                mNumRowsToGet=0;
                vector<IDL_MEMINT>::iterator row_iter;
                for (row_iter=mRowsToGet.begin(); row_iter<last_row; row_iter++)
                    mNumRowsToGet++;

                // some bug with printf here, so used cout
                if (mVerbose)
                    cout << "Extracting " << mNumRowsToGet << "/" << mNumRowsInFile << " rows" << endl;
                return(1);
            }
            else
            {
                // No good rows were found.  We will exit with an error.
                Message("None of input rows is within allowed range");
                SetStatusKeyword(INPUT_ERROR);
                mStatus = FILEOBJ_INIT_ERROR;
                return(0);
            }

        }
        else
        {
            // No actual rows were sent
            if (rowsConverted) IDL_Deltmp(rowsV);
            // We continue on here
        }

    }

    // We get here if rows keyword not present or n_elements() is zero
    if (mVerbose) printf("Extracting all rows\n");
    mNumRowsToGet = mNumRowsInFile-mSkipLines;
    mRowsToGet.resize(mNumRowsInFile);
    for (IDL_MEMINT i=0;i<mNumRowsInFile;i++)
        mRowsToGet[i] = i;

    return(1);
}



/////////////////////////////////////////////////////////////////////////////
//  The size and names of IDL types from their code
/////////////////////////////////////////////////////////////////////////////

int FileObj::IDLTypeNbytes(int type)
{

    switch(type)
    {
        case IDL_TYP_UNDEF: return(0);
        case IDL_TYP_BYTE: return(1);
        case IDL_TYP_INT: return(2);
        case IDL_TYP_LONG: return(4);
        case IDL_TYP_FLOAT: return(4);
        case IDL_TYP_DOUBLE: return(8);
        case IDL_TYP_COMPLEX: return(4);
        case IDL_TYP_STRING: return(-1);
        case IDL_TYP_STRUCT: return(-1);
        case IDL_TYP_DCOMPLEX: return(8);
        case IDL_TYP_PTR: return(-1);
        case IDL_TYP_OBJREF: return(-1);
        case IDL_TYP_UINT: return(2);
        case IDL_TYP_ULONG: return(4);
        case IDL_TYP_LONG64: return(8);
        case IDL_TYP_ULONG64: return(8);
        default: printf("Unsupported type %d found\n",type); return(0);
    }
}


void FileObj::PrintIdlType(int type)
{

    switch(type)
    {
        case IDL_TYP_UNDEF: printf("UNDEF"); break;
        case IDL_TYP_BYTE: printf("BYTE"); break;
        case IDL_TYP_INT: printf("INT"); break;
        case IDL_TYP_LONG: printf("LONG"); break;
        case IDL_TYP_FLOAT: printf("FLOAT"); break;
        case IDL_TYP_DOUBLE: printf("DOUBLE"); break;
        case IDL_TYP_COMPLEX: printf("COMPLEX"); break;
        case IDL_TYP_STRING: printf("STRING"); break;
        case IDL_TYP_STRUCT: printf("STRUCT"); break;
        case IDL_TYP_DCOMPLEX: printf("DCOMPLEX"); break;
        case IDL_TYP_PTR: printf("PTR"); break;
        case IDL_TYP_OBJREF: printf("OBJREF"); break;
        case IDL_TYP_UINT: printf("UINT"); break;
        case IDL_TYP_ULONG: printf("ULONG"); break;
        case IDL_TYP_LONG64: printf("LONG64"); break;
        case IDL_TYP_ULONG64: printf("ULONG64"); break;
        default: printf("Unsupported type %d found\n",type); break;
    }
}


int FileObj::CsvKeywordSet()
{
    int csv_set=0;
    if (kw->csv_there)
    {
        if (kw->csv != 0) 
        {
            csv_set=1;
            mDelimSent=1;
        }
    }

    return(csv_set);
}

int FileObj::TabKeywordSet()
{
    int tab_set=0;
    if (kw->tab_there)
    {
        if (kw->tab != 0) 
        {
            tab_set=1;
            mDelimSent=1;
        }
    }

    return(tab_set);
}

int FileObj::WhitespaceKeywordSet()
{
    int whitespace_set=0;
    if (kw->whitespace_there)
    {
        if (kw->whitespace != 0) 
        {
            whitespace_set=1;
        }
    }

    return(whitespace_set);
}

void FileObj::CheckDelimKeywords()
{
    mReadAsCsv = CsvKeywordSet();
    mReadAsTab = TabKeywordSet();
    mReadAsWhitespace = WhitespaceKeywordSet();
}

void FileObj::GetDelim()
{

    // Default delimiter is a tab
    mDelim = '\t';

    // the csv keyword overrides any input delimeter string or the /tab
    // keyword
    if (CsvKeywordSet())
    {
        mDelim=',';
        mDelimSent=1;
    }
    if (TabKeywordSet())
    {
        // tab is already default, just note it was sent
        mDelimSent=1;
    }
    else
    {
        if (kw->delimiter_there)
        {
            // Note only the first character gets copied to mDelim
            const char* tdelim = IDL_STRING_STR(&kw->delimiter);
            mDelim=tdelim[0];
            mDelimSent=1;
        }
    }


}




/////////////////////////////////////////////////////////////////////////////
// Set the status keyword
/////////////////////////////////////////////////////////////////////////////

    void
FileObj::SetStatusKeyword(int statusVal)
{
    if (kw->status_there) {
        /* This frees any existing memory and sets type to INT with value zero */
        IDL_StoreScalarZero(kw->status, IDL_TYP_INT);
        kw->status->value.i = statusVal;
    }
}

/////////////////////////////////////////////////////////////////////////////
// Return the INTERNAL status (not the keyword value).  This is simpley
// 1 or 0
/////////////////////////////////////////////////////////////////////////////

int FileObj::Status()
{
    return(mStatus);
}

/////////////////////////////////////////////////////////////////////////////
// Send messages through the IDL message stack.
/////////////////////////////////////////////////////////////////////////////

void FileObj::Message(char *text)
{
    IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_INFO, text);
}


////////////////////////////////////////////////////////////////////////////
//
// Syntax statements
//
//
////////////////////////////////////////////////////////////////////////////

int FileObj::NumParams()
{
    return(mNumParams);
}

void FileObj::BinaryReadSyntax()
{

    if (mHelp != 0)
    {
        string docstring = "/*---------------------------------------------------------------------------\n  NAME:\n    binary_read\n  \n  CALLING SEQUENCE:\n    IDL> struct = binary_read(file/lun, structdef, numrows, \n                              rows=, columns=, skiplines=, \n                              verbose=, status=, /help)\n\n  PURPOSE:\n\n    Read unformatted binary data from a file into a structure.  The structdef\n    input provides the definition describing each row of the file. Particular\n    rows or columns may be extracted by number. For very large files, this is\n    a big advantage over the the IDL builtin procedure readu which can only\n    read contiguous chunks.  The return variable is a structure containing the\n    requested data.  Variable length columns are not currently supported.\n\n    The columns of the input file must be fixed length, and this includes\n    strings; this length must be represented in the input structure\n    definition.\n    \n    Either the file name or an IDL file unit may be sent.  When a file unit is\n    sent, it must be opened with the /stdio keyword and bufsize=0. Lines can\n    be skipped using the skiplines= keyword.\n    \n    In general, due to the complex inputs and the fact that most files will\n    have a header describing the data, this program will be used as a utility\n    program and an IDL wrapper will parse the header and format the structure\n    definition.\n\n    This program is written in C++ and is linked to IDL via the DLM mechanism.\n\n  INPUTS: \n     file/lun: Filename or file unit. For string file names, the user must \n               expand all ~ or other environment variables.  If the file\n        unit is entered, the file must be opened with the appropriate \n              keywords:\n                 openr, lun, file, /get_lun, /stdio, bufsize=0\n     structdef: A structure that describes the layout of the data in each row.\n                Variable length fields are not supported.\n     numrows: Number of rows in the file.\n\n  OPTIONAL INPUTS:\n     rows=: An array or scalar of unique rows to read\n     columns=: An array or scalar of unique column numbers to extract.\n     skiplines=: The number of lines, or rows, to skip.  \n     verbose=: 0 for standard quiet. 1 for Basic info. > 1 for debug mode.\n     /help: Print this message, full documentation.\n\n  OPTIONAL OUTPUTS:\n    status=: The status of the read. 0 for success, 1 for read failure, \n             2 for input errors such as bad file unit.\n\n  TODO:\n\n    Might write support for variable length columns, such as for strings.\n    This would need a binary_write.c to write them properly.  Would probably\n    require the user to specify which columns are variable and the first n\n    bytes of the field to describe the length. One byte supports strings of\n    length 255, two bytes would support 65,535 length strings, four\n    4,294,967,295\n\n  REVISION HISTORY:\n    Created 20-April-2006: Erin Sheldon, NYU\n    Converted to C++, 2006-July-17, E.S. NYU\n\n\n  Copyright (C) 2005  Erin Sheldon, NYU.  erin dot sheldon at gmail dot com\n\n    This program is free software; you can redistribute it and/or modify\n    it under the terms of the GNU General Public License as published by\n    the Free Software Foundation; either version 2 of the License, or\n    (at your option) any later version.\n\n    This program is distributed in the hope that it will be useful,\n    but WITHOUT ANY WARRANTY; without even the implied warranty of\n    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n    GNU General Public License for more details.\n\n    You should have received a copy of the GNU General Public License\n    along with this program; if not, write to the Free Software\n    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA\n\n\n  ---------------------------------------------------------------------------*/";
        cout << docstring << endl;
    }
    else
    {
        printf("\n");
        printf("   struct = binary_read(file/lun, structdef, numrows, \n");
        printf("                        rows=, columns=, skiplines=, status=, verbose=, /help)\n");
        printf("\n");
        printf("   Send /help for full documentation.\n");
    }

    fflush(stdout);

}

void FileObj::AsciiReadSyntax()
{

    if (mHelp != 0)
    {
        string docstring = "/*---------------------------------------------------------------------------\n  NAME:\n    ascii_read\n\n  CALLING SEQUENCE:\n    IDL> struct = ascii_read(file/lun, structdef, numrows, \n                             rows=, columns=, skiplines=, \n                             /csv, /tab, delimiter=, status=, verbose=, /help)\n  \n  PURPOSE: \n\n    Read ASCII data from file into a structure.  The fields in the file can be\n    separated by a distinct separator such as comma separated value (CSV), tab\n    separated, or a user specified separator.  By default the file is assumed\n    to be simply white space separated.  In this case strings fields must be\n    fixed length and this length is determined from the input structure\n    definition.  In other words, only if the separator is specified can string\n    fields be variable length.\n    \n    The structure definition input provides the definition describing each row\n    of the file.  Particular rows or columns may be extracted by number. For\n    very large files, this is a big advantage over the IDL readf procedure\n    which can only read contiguous chunks.  The return variable is a structure\n    containing the requested data.  A set of rows/lines at the top of the\n    file can also be skipped with the skiplines= keyword.\n\n    For CSV, tab, or user-defined field delimiters the columns may be variable\n    length and the user can input the string columns in structdef with any\n    size because the memory will be generated on the fly.  E.g. structdef =\n    {a:0L, b:'', c:0LL}. String columns are currently limited to 255\n    characters and will be truncated if longer.\n\n    Either the file name or an IDL file unit may be sent.  When a file unit is\n    sent, it must be opened with the /stdio keyword and bufsize=0. \n\n    In general, due to the complex inputs and the fact that most files will\n    have a header describing the data, this program will be used as a utility\n    program and an IDL wrapper will parse the header and format the structure\n    definition.\n\n    This program is written in C++ and is linked to IDL via the DLM mechanism.\n\n\n  INPUTS: \n    file/lun: Filename or file unit. For string file names, the user must \n               expand all ~ or other environment variables.  If the file\n         unit is entered, the file must be opened with the appropriate \n        keywords:\n                 openr, lun, file, /get_lun, /stdio, bufsize=0\n\n    structdef: A structure that describes the layout of the data in each row.\n                Variable length fields are not supported.\n    numrows: Number of rows in the file.\n\n  OPTIONAL INPUTS:\n    rows=: An array or scalar of unique rows to read\n    skiplines=: The number of lines to skip.  The newline character is \n        searched for to determine the lines, so be careful.  \n    columns=: An array or scalar of unique column numbers to extract.\n    /csv: The file is formatted as comma separated value.  The fields cannot \n        contain commas in this case.  Strings can be variable length in this\n        case.\n    /tab: The file is formatted as tab separated fields.  The fields cannot\n        contain the tab character. Strings can be variable length in this\n        case.\n    delimiter=:  A delimiter character.  This is a string input but only the\n        first character will be used if it is longer.  Strings can be \n        variable length in this case.\n    verbose=: 0 for standard quiet. 1 for basic info. > 1 for debug mode.\n    /help: print this message.\n\n  OPTIONAL OUTPUTS:\n    status=: The status of the read. 0 for success, 1 for read failure, \n             2 for input errors such as bad file unit.\n\n  REVISION HISTORY:\n    created 20-April-2006: Erin Sheldon, NYU\n    Converted to C++, 2006-July-17, E.S. NYU\n    Added /tab and delimiter= keywords. 2008-03-28, E.S. NYU\n\n\n  Copyright (C) 2005  Erin Sheldon, NYU.  erin dot sheldon at gmail dot com\n\n    This program is free software; you can redistribute it and/or modify\n    it under the terms of the GNU General Public License as published by\n    the Free Software Foundation; either version 2 of the License, or\n    (at your option) any later version.\n\n    This program is distributed in the hope that it will be useful,\n    but WITHOUT ANY WARRANTY; without even the implied warranty of\n    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n    GNU General Public License for more details.\n\n    You should have received a copy of the GNU General Public License\n    along with this program; if not, write to the Free Software\n    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA\n\n\n \n  ---------------------------------------------------------------------------*/";
        cout << docstring << endl;
    }
    else
    {
        printf("\n");
        printf("   struct = ascii_read(file/lun, structdef, numrows, \n");
        printf("                       rows=, columns=, skiplines=, /csv, status=, verbose=, /help)\n");
        printf("\n");
        printf("   Send /help for full documentation.\n");
    }

    fflush(stdout);

}

void FileObj::AsciiWriteSyntax()
{

    if (mHelp != 0)
    {
        string docstring = "/*---------------------------------------------------------------------------\n\n  NAME:\n    ascii_write\n  \n  CALLING SEQUENCE:\n    IDL> ascii_write, struct, filename/lun, /append, \n                    /csv, delimiter=, /bracket_arrays, status=, /help\n\n  PURPOSE:\n\n    Write an IDL structure to an ascii file.  This is about 12 times faster\n    than using the built in printf statement for small structure definitions.\n    For really big ones, getting the offsets is the bottleneck. For a tsObj\n    file its only about 5 times faster.\n    \n    The default field separator is a tab, but csv or any user-defined\n    delimeter character can be used.\n\n    This program is written in C++ and is linked to IDL via the DLM mechanism.\n\n  INPUTS: \n     struct: The structure array to write. \n     file/lun: Filename or file unit. For string file names, the user must \n               expand all ~ or other environment variables.  If the file\n        unit is entered, the file must be opened with the appropriate \n              keywords:\n                 openr, lun, file, /get_lun, /stdio, bufsize=0\n\n  OPTIONAL INPUTS:\n     /cvs: Use ',' as the field delimiter. The default delimeter is a tab\n        character.\n     delimiter: The field delimiter.  Only the first character of the\n        input string is used. The default is the tab character.\n     /append: Append the file.\n     /bracket_arrays: {} is placed around array data in each row, with \n        values comma delimited within.  Postgresql databases require\n        this format, with a whitespace delimiter for ordinary fields, for\n        the COPY command.\n     /help: Print this documentation.\n\n  OPTIONAL OUTPUTS:\n    status=: The status of the read. 0 for success, 1 for read failure, \n             2 for input errors such as bad file unit.\n\n\n  REVISION HISTORY:\n    Created December-2005: Erin Sheldon, NYU\n    Converted to C++, 2006-July-17, E.S. NYU\n    More general treatment of delimiters.  2008-03-28, E.S.\n\n\n  Copyright (C) 2005  Erin Sheldon, NYU.  erin dot sheldon at gmail dot com\n\n    This program is free software; you can redistribute it and/or modify\n    it under the terms of the GNU General Public License as published by\n    the Free Software Foundation; either version 2 of the License, or\n    (at your option) any later version.\n\n    This program is distributed in the hope that it will be useful,\n    but WITHOUT ANY WARRANTY; without even the implied warranty of\n    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n    GNU General Public License for more details.\n\n    You should have received a copy of the GNU General Public License\n    along with this program; if not, write to the Free Software\n    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA\n\n\n  ---------------------------------------------------------------------------*/";
        cout << docstring << endl;
    }
    else
    {
        printf("\n");
        printf("   ascii_write, struct, file/lun, \n");
        printf("              /csv, delimiter=, /bracket_arrays, status=, /help\n");
        printf("\n");
        printf("   Send /help for full documentation.\n");

    }

    fflush(stdout);

}
