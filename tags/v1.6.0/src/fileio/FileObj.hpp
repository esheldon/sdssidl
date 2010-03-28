/*
   Header for FileObj.cpp

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


#if !defined (_fileobj_hpp)
#define _fileobj_hpp


#include <iostream>
#include <cstdio>
#include <cctype> // for toupper
#include <algorithm> // for transform
#include <vector>
#include <string>

#include "idl_export.h"
#include "keywords.hpp"
#include "IDLStruct.hpp"

#define FO_DEBUG 1

// Constants

// Status values for the return keyword
#define READ_SUCCESS   0
#define WRITE_SUCCESS  0
#define READ_FAILURE   1
#define WRITE_FAILURE  1
#define INPUT_ERROR    2
#define HELP_REQUEST   3

// Internal status
#define FILEOBJ_OK            0
#define FILEOBJ_INIT_ERROR    1
#define FILEOBJ_SYNTAX_ERROR  2
#define FILEOBJ_PRINT_HELP    3

// Action types
#define ACTION_READ_BINARY    1
#define ACTION_READ_ASCII     2
#define ACTION_WRITE_BINARY   3 // Not yet supported
#define ACTION_WRITE_ASCII    4

#define FILEOBJ_BUFFLEN 256
// To allow for the null char at the end
#define FILEOBJ_MAXSTR 255


using namespace std;

class FileObj {

    public:
        FileObj(int npars, 
                IDL_VPTR argv[], 
                char argk[], 
                KW_RESULT* input_kw,
                int iAction);

        // I was careful to use nice classes everywhere
        // so no explicit cleanup is needed other than the IDL_KW_FREE call.
        ~FileObj();

        // Initialization.  The arg and keyword processing differs between 
        // read and write
        int InitRead(
                int npars, 
                IDL_VPTR argv[], 
                char argk[]);

        int InitWrite(
                int npars, 
                IDL_VPTR argv[], 
                char argk[]);

        // Set the output status value
        void SetStatusKeyword(int statusVal);

        // Return the internal status (not keyword status)
        int Status();

        // Process input keywords
        int GetStructdefInfo();
        int MakeBuffers();
        int GetInputRows();
        int GetInputColumns();
        int CsvKeywordSet();
        int TabKeywordSet();
        int WhitespaceKeywordSet();
        
        void CheckDelimKeywords();

        void GetDelim();

        // Creating the output structure when reading
        void CreateOutputStruct();
        IDL_STRUCT_TAG_DEF *GetSubStructdef();
        void FreeTagDefs(IDL_STRUCT_TAG_DEF *tagDefs);
        IDL_VPTR OutputStruct();


        // Reading from the file
        int OpenFile(char* mode);
        int GetFilePtr(char* mode);
        void CloseFile();


        int SkipRows(IDL_MEMINT num);
        int SkipAsciiLines(IDL_MEMINT num);
        int SkipBinaryRows(IDL_MEMINT num);


        int ReadFile();
        int ReadRow();


        int ReadAsciiNumbers(char* tptr, IDL_MEMINT tag);
        void ReadDelimStringField(char* buffer, char delim);
        void ReadAsciiBytes(char* buffer, IDL_MEMINT nbytes);
        int ReadAsciiStrings(char* tptr, IDL_MEMINT tag);

        int ReadBinaryNumbers(char* tptr, IDL_MEMINT tag);
        int ReadBinaryStrings(char* tptr, IDL_MEMINT tag);


        int SkipBinaryTag(IDL_MEMINT tag);


        int ScanVal(int   type, 
                char* buffer);

        void GetScanFormats();

        // Writing to the file
        int WriteAsAscii();
        void AsciiPrint(
                int idlType, 
                UCHAR* tptr);
        void StdoutPrint(
                int idlType, 
                UCHAR* tptr);

        // Utility functions
        int IDLTypeNbytes(int type);
        void PrintIdlType(int type);

        void Message(char* text);

        int NumParams();
        void BinaryReadSyntax();
        void AsciiReadSyntax();
        void AsciiWriteSyntax();

    protected:

        // IDL demands this is called kw so we cannot follow naming conventions.
        //KW_RESULT kw;

        KW_RESULT *kw;
        // Number of ordinary position parameters
        int mNumParams;

        // Just print the help message and quit?
        IDL_LONG mHelp;

        // This is internal status, not the keyword
        int mStatus; 
        int mVerbose;

        // Input file Vptr: may be string or lun
        IDL_VPTR mFileVptr;
        char*    mpFileName;
        FILE*    mFptr;

        // Read specific
        IDL_MEMINT mSkipLines;
        IDL_VPTR   mStructdefVptr;
        IDL_MEMINT mNumRowsInFile;

        vector<IDL_MEMINT> mRowsToGet;
        IDL_MEMINT         mNumRowsToGet;

        vector<IDL_MEMINT> mGetColumn;
        vector<IDL_MEMINT> mColsToGet;
        //IDL_MEMINT         mNumColsToGet;

        IDL_VPTR mResultVptr;  // The result of the read
        char*    mpResult;     // pointer to data section of result

        // For ascii reading
        vector<string> mScanFormats;
        int mReadAsCsv;
        int mReadAsTab;
        int mReadAsWhitespace;
        int mReadByDelim;


        // Delimeters
        char     mDelim;
        int      mDelimSent;
        char     mArrayDelim;

        int mBracketArrays;

        // IDLStruct class
        IDLStruct mIdlst;

        // This is the info to the main structure
        TAG_INFO_STRUCT* mpTagInfo;
        // This is the info for the output structure
        TAG_INFO_STRUCT mOutputTagInfo;

        vector<string> mTagBuffers;
        char mBuffer[FILEOBJ_BUFFLEN];
        char mBuffer2[FILEOBJ_BUFFLEN];
        char*         mpCurrentRow;

        int mReadAction;

        int mReadWholeRowBinary;

};


#endif  // _fileobj_hpp
