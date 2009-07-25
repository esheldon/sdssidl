#include <iostream>
// this is for the stuff in idl_export.h
#include <cstdio>
#include <vector>
#include <map>
#include <cctype> // for toupper
#include <algorithm> // for transform
#include "idl_export.h"

#define M_DEBUG 0

using namespace std;

typedef struct {
    vector<IDL_MEMINT> dims;
} TAG_DIM_VECTORS;

// 
// Safe because memory storage for dims is in the tagdims vectors
//
typedef struct {
    vector<IDL_STRUCT_TAG_DEF> tagdefs;
    vector<TAG_DIM_VECTORS> tagdims;
} IDL_STRUCT_TAG_DEF_SAFE;

typedef struct {

    IDL_MEMINT NumTags;

    vector<string> TagNames;
    map<string,IDL_MEMINT> TagMap;
    vector<IDL_MEMINT> TagOffsets;
    vector<IDL_VPTR> TagDesc;

    IDL_MEMINT BytesPerRow;

    vector<IDL_MEMINT> TagBytes;
    vector<IDL_MEMINT> TagNelts;

    int NStrings;

    //
    // These can be used to create a new structure
    // 

    IDL_STRUCT_TAG_DEF_SAFE stagdefs;

} TAG_INFO_STRUCT;

class IDLStruct {

    public:
        // The do nothing constructor
        IDLStruct();
        // Constructor where an idl variable is input and this data
        // is the initialization.
        IDLStruct(IDL_VPTR invar);
        // Take tag definitions instead of a IDL_VPTR
        IDLStruct(vector<IDL_STRUCT_TAG_DEF>& tagdefs);


        // Deconstructor
        ~IDLStruct();

        int Init(IDL_VPTR invar);

        //
        // Access to internal info.  BE CAREFUL with this!
        //
        TAG_INFO_STRUCT* GetTagInfo();        

        // 
        // Make a structure: A few different ways
        //

        char* MakeStruct(
                TAG_INFO_STRUCT& taginfo,
                IDL_MEMINT len, 
                IDL_VPTR* vptr);

        char* MakeStruct(
                vector<IDL_STRUCT_TAG_DEF>& tagdefs,
                IDL_MEMINT len, 
                IDL_VPTR* vptr);
        char* MakeStruct(
                IDL_StructDefPtr defptr,
                IDL_MEMINT len, 
                IDL_VPTR* vptr);


        // Make the an IDL_StructDefPtr from tagdefs
        IDL_StructDefPtr 
            MakeStructDef(vector<IDL_STRUCT_TAG_DEF>& tagdefs);

        //
        // A few different versions for making substructures
        //
        
        // This version takes an IDL_VPTR which should contain either
        // an array of tag names or an array of tag numbers
        char* MakeSubStruct(
                IDL_VPTR subtagvptr,
                IDL_MEMINT len,
                IDL_VPTR* vptr);

        // This one takes a vector of string tag names
        char* MakeSubStruct(
                vector<string>& tagnames,
                IDL_MEMINT len,
                IDL_VPTR* vptr);

        // Takes a list of tag numbers
        char* MakeSubStruct(
                vector<IDL_MEMINT>& tagnums, 
                IDL_MEMINT len, 
                IDL_VPTR* vptr);

        // Get copies of the structure info for easier access
        int MakeTagInfo(
                IDL_VPTR structvptr, 
                TAG_INFO_STRUCT& taginfo);



                


        // Create defs for a subset of the column definitions
        int MakeSubTagDefs(vector<IDL_MEMINT>& tagnums);
        int MakeTagDefs(
                IDL_VPTR structvptr, 
                TAG_INFO_STRUCT& taginfo,
                IDL_STRUCT_TAG_DEF_SAFE& stagdefs);
        int MakeTagDefs(
                IDL_VPTR structvptr, 
                vector<IDL_MEMINT>& tagnums,
                TAG_INFO_STRUCT& taginfo,
                IDL_STRUCT_TAG_DEF_SAFE& stagdefs);



        // Match an input IDL_VPTR of tag indicators, either strings or 
        // numerical to the tags of our structure and return the match ids.
        int MatchIDL_VPTR2Tags(
                IDL_VPTR tagvptr, 
                vector<IDL_MEMINT>& tagnums);

        // Same but also returns yes/no flags for each tag in struct
        int MatchIDL_VPTR2Tags(
                IDL_VPTR tagvptr, 
                vector<IDL_MEMINT>& tagnums,
                vector<IDL_MEMINT>& tagflags);



        // Take a set of input tag numbers and select the unique ones that
        // are in range.  Result is sorted.
        int MatchTagNums(
                vector<IDL_MEMINT>& tagnums, vector<IDL_MEMINT>& keeptagnums);

        // Match an input array of IDL_VPTR, of IDL_STRING type, to the 
        // tagnames of our structure
        int MatchTagNames(
                vector<string>& tagnames, 
                vector<IDL_MEMINT>& tagnums);


        // Convert the input IDL_VPTR of IDL_STRINGs to a vector<string> 
        // all uppercase
        int IDL_STRING2StringVecUpper(
                IDL_VPTR svptr, vector<string>& tagnames);


        // Convert the input numerical variable to a vector<IDL_MEMINT>
        int IDL_VAR2IDL_MEMINTVec(
                IDL_VPTR vptr, vector<IDL_MEMINT>& memintvec);

        // Sizes of each type, if defined
        int IDLTypeNbytes(int type);
        void PrintIdlType(int type);

    protected:
        // Means they can't be modified outside of the class but they
        // can be accessed

        // Just a pointer set to the input
        IDL_VPTR mStructVptr;
        TAG_INFO_STRUCT mTagInfo;

        IDL_MEMINT mNtags;

        // This will hold the actual memory for dims.
        vector<TAG_DIM_VECTORS> mSubTagDims;
        vector<IDL_STRUCT_TAG_DEF> mSubTagDefs;

        IDL_StructDefPtr mSubDefPtr;
};
