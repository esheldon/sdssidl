#include "IDLStruct.hpp"

IDLStruct::IDLStruct() {}

// Take a structure variable keep track of its properties
IDLStruct::IDLStruct(IDL_VPTR invar)
{
    Init(invar);
}

int IDLStruct::Init(IDL_VPTR invar)
{
    int status=0;
    // Need to first do a cleanup of all variables?
    if (invar->type != IDL_TYP_STRUCT)
    {
        IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_INFO,
                "The input IDL_VPTR must be a structure");
        return(status);
    }

    mStructVptr = invar;
    status = MakeTagInfo(invar, mTagInfo);
    return(status);
}

IDLStruct::~IDLStruct() {}


//
// Creating structures
//

// From a taginfo struct
char* IDLStruct::MakeStruct(
        TAG_INFO_STRUCT& taginfo,
        IDL_MEMINT len, 
        IDL_VPTR* vptr)
{
    IDL_StructDefPtr defptr = MakeStructDef(taginfo.stagdefs.tagdefs);
    char* result=MakeStruct(defptr, len, vptr);
    return(result);
}


// From tagdefs
char* IDLStruct::MakeStruct(
        vector<IDL_STRUCT_TAG_DEF>& tagdefs,
        IDL_MEMINT len, 
        IDL_VPTR* vptr)
{
    IDL_StructDefPtr defptr = MakeStructDef(tagdefs);
    char* result=MakeStruct(defptr, len, vptr);
    return(result);
}


// From an IDL_StructDefPtr
char* IDLStruct::MakeStruct(
        IDL_StructDefPtr defptr,
        IDL_MEMINT len, 
        IDL_VPTR* vptr)
{
    char* result;
    result = 
        IDL_MakeTempStructVector(
                defptr,
                len,
                vptr, 
                IDL_TRUE);

    return(result);
}

//
// Make an IDL_StructDefPtr from a tag defs structure
//

IDL_StructDefPtr IDLStruct::MakeStructDef(
        vector<IDL_STRUCT_TAG_DEF>& tagdefs)
{
    IDL_StructDefPtr defptr=NULL;
    if (M_DEBUG) cout<<"Making the substruct definition"<<endl;
    defptr = (IDL_StructDefPtr) IDL_MakeStruct(NULL, &tagdefs[0]);
    return(defptr);
}




//
// A few different versions for making substructures
//

// This one takes an IDL_VPTR and matches it up to the tags in the 
// structure
char* IDLStruct::MakeSubStruct(
        IDL_VPTR subtagvptr,
        IDL_MEMINT len,
        IDL_VPTR* vptr)
{
    char* result;

    vector<IDL_MEMINT> tagnums;

    // Match the input subtags vector to the tags of our structure
    if (!MatchIDL_VPTR2Tags(subtagvptr, tagnums))
        return(NULL);

    // Call the version that takes tagnums
    result = MakeSubStruct(tagnums, len, vptr);
    return(result);

}

// This one takes a vector of strings and matches it up to the tag names in the 
// structure
char* IDLStruct::MakeSubStruct(
        vector<string>& tagnames,
        IDL_MEMINT len,
        IDL_VPTR* vptr)
{
    char* result;

    // Match the requested tag names to the structure tags
    vector<IDL_MEMINT> keeptags;
    if (!MatchTagNames(tagnames, keeptags))
        return(NULL);

    // Call the version that takes tagnums
    result = MakeSubStruct(keeptags, len, vptr);
    return(result);

}



// This one takes a vector of numbers matches it up to the tag nums in the 
// structure
char* IDLStruct::MakeSubStruct(
        vector<IDL_MEMINT>& tagnums,
        IDL_MEMINT len,
        IDL_VPTR* vptr)
{
    char* result;

    // Match the requested tag names to the structure tags
    vector<IDL_MEMINT> keeptags;
    if (!MatchTagNums(tagnums, keeptags))
        return(NULL);

    // Info for the substructure
    IDL_STRUCT_TAG_DEF_SAFE ssubtagdefs;
    if (!MakeTagDefs(mStructVptr, tagnums, mTagInfo, ssubtagdefs))
        return(NULL);

    result = MakeStruct(ssubtagdefs.tagdefs, len, vptr);

    return(result);
}


// Private method that assumes tagnums matches and is unique already
int IDLStruct::MakeSubTagDefs(vector<IDL_MEMINT>& tagnums)
{
    int status=0;
    IDL_MEMINT ntags = (IDL_MEMINT) tagnums.size();

    mSubTagDims.resize(ntags);
    // Note last one is null
    mSubTagDefs.resize(ntags+1);

    if (M_DEBUG) cout<<endl<<"Making a substruct tag defs vector"<<endl;

    for (IDL_MEMINT i=0; i < ntags; i++)
    {

        IDL_MEMINT tagnum = tagnums[i];

        mSubTagDefs[i].name = 
            IDL_StructTagNameByIndex(mStructVptr->value.s.sdef, tagnum, 
                    IDL_MSG_INFO, NULL);

        if (M_DEBUG) cout<<"  Sub Tag: "<<mSubTagDefs[i].name<<endl;

        mSubTagDefs[i].type = 
            (void *) (( IDL_MEMINT ) mTagInfo.TagDesc[tagnum]->type);

        mSubTagDefs[i].flags = 0;

        // Store actual dims memory in a vector to make memory management
        // automatic
        if ((mTagInfo.TagDesc[tagnum]->flags & IDL_V_ARR) != 0)
        {
            IDL_MEMINT n_dim = mTagInfo.TagDesc[tagnum]->value.arr->n_dim;

            mSubTagDims[i].dims.resize(n_dim+1);
            mSubTagDims[i].dims[0] = n_dim;
            for (IDL_MEMINT j=1; j<n_dim+1; j++)
                mSubTagDims[i].dims[j] = 
                    mTagInfo.TagDesc[tagnum]->value.arr->dim[j-1];

            // Now just set this to point at our vector whose memory is
            // safe
            mSubTagDefs[i].dims = &mSubTagDims[i].dims[0];

        } 

        
    }

    status=1;
    return(status);
}


//
// Create tag definitions for the tags in taginfo
int IDLStruct::MakeTagDefs(
        IDL_VPTR structvptr, 
        TAG_INFO_STRUCT& taginfo,
        IDL_STRUCT_TAG_DEF_SAFE& stagdefs)
{
    vector<IDL_MEMINT> tagnums(taginfo.NumTags);
    for (IDL_MEMINT i=0; i< taginfo.NumTags; i++)
        tagnums[i] = i;
    return( MakeTagDefs(structvptr, tagnums, taginfo, stagdefs) );
}

// Explicit tagnums
int IDLStruct::MakeTagDefs(
        IDL_VPTR structvptr, 
        vector<IDL_MEMINT>& tagnums,
        TAG_INFO_STRUCT& taginfo,
        IDL_STRUCT_TAG_DEF_SAFE& stagdefs)
{
    int status=0;
    IDL_MEMINT ntags = (IDL_MEMINT) tagnums.size();

    // Note last one is null
    stagdefs.tagdefs.resize(ntags+1);
    stagdefs.tagdims.resize(ntags);

    if (M_DEBUG) cout<<endl<<"Making a substruct tag defs vector"<<endl;

    for (IDL_MEMINT i=0; i < ntags; i++)
    {

        IDL_MEMINT tagnum = tagnums[i];

        stagdefs.tagdefs[i].name = 
            IDL_StructTagNameByIndex(structvptr->value.s.sdef, tagnum, 
                    IDL_MSG_INFO, NULL);

        if (M_DEBUG) cout<<"  Sub Tag: "<<stagdefs.tagdefs[i].name<<endl;

        stagdefs.tagdefs[i].type = 
            (void *) (( IDL_MEMINT ) taginfo.TagDesc[tagnum]->type);

        stagdefs.tagdefs[i].flags = 0;

        // Store actual dims memory in a vector to make memory management
        // automatic
        if ((taginfo.TagDesc[tagnum]->flags & IDL_V_ARR) != 0)
        {
            IDL_MEMINT n_dim = taginfo.TagDesc[tagnum]->value.arr->n_dim;

            stagdefs.tagdims[i].dims.resize(n_dim+1);
            stagdefs.tagdims[i].dims[0] = n_dim;
            for (IDL_MEMINT j=1; j<n_dim+1; j++)
                stagdefs.tagdims[i].dims[j] = 
                    taginfo.TagDesc[tagnum]->value.arr->dim[j-1];

            // Now just set this to point at our vector whose memory is
            // safe
            stagdefs.tagdefs[i].dims = &stagdefs.tagdims[i].dims[0];

        } 

        
    }

    status=1;
    return(status);
}



//
// Access to internal info.  BE CAREFUL with this!
//
TAG_INFO_STRUCT* IDLStruct::GetTagInfo()
{
    return( &mTagInfo );
}

int IDLStruct::MakeTagInfo(
        IDL_VPTR structvptr, 
        TAG_INFO_STRUCT& taginfo)
{

    int status=0;
    if (structvptr->type != IDL_TYP_STRUCT)
    {
        IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_INFO,
                "The input variable must be a structure");
        return(status);
    }

    IDL_STRING *tptr;


    //------------------------------------------------------------------
    // Initialize the tag info structure
    // structvptr points to the input structdef
    //------------------------------------------------------------------


    taginfo.NumTags = IDL_StructNumTags(structvptr->value.s.sdef);
    mNtags = taginfo.NumTags;

    taginfo.TagNames.resize(taginfo.NumTags);
    taginfo.TagOffsets.resize(taginfo.NumTags);
    taginfo.TagDesc.resize(taginfo.NumTags);
    taginfo.TagBytes.resize(taginfo.NumTags);
    taginfo.TagNelts.resize(taginfo.NumTags);

    taginfo.BytesPerRow = 0;
    taginfo.NStrings=0;

    //---------------------------------------------------------------------------
    // Get the tag info
    //---------------------------------------------------------------------------



    for (IDL_MEMINT tag=0; tag<taginfo.NumTags; tag++)
    {
        // Get tag name
        taginfo.TagNames[tag] = IDL_StructTagNameByIndex(structvptr->value.s.sdef, tag, 
                IDL_MSG_INFO, NULL);
        taginfo.TagMap[taginfo.TagNames[tag]] = tag;

        // Get tag offsets and the tag description variable (a copy?)
        taginfo.TagOffsets[tag] = IDL_StructTagInfoByIndex(structvptr->value.s.sdef, 
                tag, 
                IDL_MSG_LONGJMP,
                &(taginfo.TagDesc[tag]) );
        if (M_DEBUG) 
        {
            cout <<"    Tag "<<tag<<" = "<<taginfo.TagNames[tag].c_str();
            cout <<"    TagMap[\"" 
                << taginfo.TagNames[tag] << "\"] = " <<
                taginfo.TagMap[ taginfo.TagNames[tag] ] << endl;
        }

        // Deal with arrays
        if ( (taginfo.TagDesc[tag]->flags & IDL_V_ARR) != 0)
        {
            // this is just for convenience
            taginfo.TagNelts[tag] = taginfo.TagDesc[tag]->value.arr->n_elts;
            if (M_DEBUG) cout<<" ARRAY["<<taginfo.TagNelts[tag]<<"] ";
        }
        else
        {
            taginfo.TagNelts[tag] = 1;
            if (M_DEBUG) cout<<" SCALAR ";
        }


        // Number of bytes for this variable type.
        if (taginfo.TagDesc[tag]->type == IDL_TYP_STRING) 
        {

            // WARNING: This assumes that the elements in string arrays are all
            // the same size
            tptr = (IDL_STRING *) (structvptr->value.s.arr->data + taginfo.TagOffsets[tag]);
            taginfo.TagBytes[tag] = tptr->slen;

            taginfo.NStrings++;

        }
        else 
            taginfo.TagBytes[tag] = IDLTypeNbytes(taginfo.TagDesc[tag]->type);

        // Bytes in each row
        taginfo.BytesPerRow += taginfo.TagBytes[tag]*taginfo.TagNelts[tag];

        if (M_DEBUG) 
        {
            PrintIdlType(taginfo.TagDesc[tag]->type);
            cout<<" "<<taginfo.TagBytes[tag]<<" bytes";
        }


        if (M_DEBUG) cout<<endl;


    }

    // Add the tag defs for use creating a new struct or substruct
    MakeTagDefs(structvptr, taginfo, taginfo.stagdefs);
    status=1;
    return(status);

}


int IDLStruct::MatchIDL_VPTR2Tags(
        IDL_VPTR tagvptr, 
        vector<IDL_MEMINT>& tagnums,
        vector<IDL_MEMINT>& tagflags)
{
    if (!MatchIDL_VPTR2Tags(tagvptr, tagnums))
        return(0);
    tagflags.resize(mTagInfo.NumTags, 0);
    for (IDL_MEMINT i=0; i<(IDL_MEMINT)tagnums.size(); i++)
        tagflags[ tagnums[i] ] = 1;
    return(1);
}

// Match an input IDL_VPTR of tag indicators, either strings or numerical
// to the tags of our structure and return the match ids.
int IDLStruct::MatchIDL_VPTR2Tags(IDL_VPTR tagvptr, vector<IDL_MEMINT>& tagnums)
{
    int status=0;

    // Checking here, but better to check outside of this in case of memory
    // issues
    IDL_ENSURE_SIMPLE(tagvptr);

    if (tagvptr->type == IDL_TYP_STRING)
    {
        vector<string> tagnamevec;
        // Try to convert to upper case strings
        if (IDL_STRING2StringVecUpper(tagvptr, tagnamevec) != 1)
        {
            return(status);
        }

        return( MatchTagNames(tagnamevec, tagnums) );
    } else {

        vector<IDL_MEMINT> tmptagnums;
        if (IDL_VAR2IDL_MEMINTVec(tagvptr, tmptagnums) != 1)
        {
            return(status);
        }

        return( MatchTagNums(tmptagnums, tagnums) );

    }

}


// Take a set of input tag numbers and select the unique ones that
// are in range.  Result is sorted.
int IDLStruct::MatchTagNums(
        vector<IDL_MEMINT>& tagnums, vector<IDL_MEMINT>& keeptagnums)
{

    int status=0;
    // Need to do more here.
    if (tagnums.size() == 0) 
    {
        IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_INFO, 
                "MatchTagNums: Size of requested tag nums is zero");
        return(status);
    }

    // Flag keep or not keep for each tag
    vector<IDL_MEMINT> keep;
    keep.resize(mNtags,0);
    for (IDL_MEMINT i=0; i<(IDL_MEMINT)tagnums.size(); i++)
    {
        IDL_MEMINT tagnum=tagnums[i];
        if (tagnum >= 0 && tagnum < mNtags) 
        {
            keep[tagnum] = 1;
        }
    }

    // The actual tags to keep.
    for (IDL_MEMINT i=0; i<mNtags; i++)
    {
        if (keep[i])
            keeptagnums.push_back(i);
    }

    if (keeptagnums.size() == 0)
    {
        IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_INFO, 
                "No sub tags matched");
        return(status);
    }

    status=1;
    return(status);


}




// Match an input array of IDL_VPTR, of IDL_STRING type, to the tagnames of 
// our structure
int IDLStruct::MatchTagNames(
        vector<string>& tagnames, vector<IDL_MEMINT>& tagnums)
{
    int status=0;

    tagnums.clear();

    if (tagnames.size() == 0) 
    {
        IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_INFO, 
                "MatchTagNames: Size of requested tag names is zero");
        return(status);
    }

    vector<IDL_MEMINT> tmp_tagnums;
    for (IDL_MEMINT i=0; i<(IDL_MEMINT) tagnames.size(); i++)
    {
        string name=tagnames[i];

        // Make upper case
        std::transform(name.begin(), name.end(), 
                name.begin(), (int(*)(int)) toupper);

        if (mTagInfo.TagMap.find(name) != mTagInfo.TagMap.end())
        {
            IDL_MEMINT tagnum = mTagInfo.TagMap[name];
            tmp_tagnums.push_back(tagnum);
        }
    }

    if (tmp_tagnums.size() == 0)
    {
        IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_INFO, 
                "MatchTagNames: No sub tags matched");
        return(status);
    }

    // Now extract unique tags in sorted order
    MatchTagNums(tmp_tagnums, tagnums);

    status=1;
    return(status);
}

// Convert the input IDL_VPTR of IDL_STRINGs to a vector<string> all uppercase
int IDLStruct::IDL_STRING2StringVecUpper(
        IDL_VPTR svptr, vector<string>& tagnames)
{
    int status=0;
    IDL_MEMINT num=0;
    IDL_STRING* ptr;


    // Checking here, but better to check outside of this in case of memory
    // issues
    IDL_ENSURE_SIMPLE(svptr);

    if (svptr->type != IDL_TYP_STRING)
    {
        IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_INFO, 
                "IDL_STRING2StringVecUpper: Input must be of type IDL_STRING");
        return(status);
    }

    IDL_VarGetData(svptr, &num, (char **) &ptr, IDL_TRUE);

    tagnames.resize(num);
    for (IDL_MEMINT i=0; i<num; i++)
    {
        string name=ptr[i].s;
        // Make upper case
        std::transform(name.begin(), name.end(), 
                name.begin(), (int(*)(int)) toupper);
        tagnames[i] = name;
    }
    status=1;
    return(status);
}

// Convert the input numerical variable to a vector<IDL_MEMINT>
int IDLStruct::IDL_VAR2IDL_MEMINTVec(
        IDL_VPTR vptr, vector<IDL_MEMINT>& memintvec)
{
    int status=0;

    // Checking here, but better to check outside of this in case of memory
    // issues
    IDL_ENSURE_SIMPLE(vptr);

    int converted = 0;
    if (vptr->type != IDL_TYP_MEMINT)
        converted = 1;

    // Will return same variable if it is already type IDL_MEMINT
    // We have to explicitly release this mem; but at least IDL
    // will warn us if we forget
    IDL_VPTR vptr_use = IDL_CvtMEMINT(1, &vptr);


    // The true will ensure simple, which we have already done 
    IDL_MEMINT num;
    IDL_MEMINT* ptr;
    IDL_VarGetData(vptr_use, &num, (char **) &ptr, IDL_TRUE);

    memintvec.clear();

    if (num > 0)
    {
        memintvec.resize(num);
        for (IDL_MEMINT i=0; i<num; i++)
        {
            memintvec[i] = ptr[i];
        }

    }

    if (converted)
        IDL_Deltmp(vptr_use);

    status=1;
    return(status);

}

/////////////////////////////////////////////////////////////////////////////
//  The size and names of IDL types from their code
/////////////////////////////////////////////////////////////////////////////

int IDLStruct::IDLTypeNbytes(int type)
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

void IDLStruct::PrintIdlType(int type)
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

/*
int IDLStruct::CheckSimple(IDL_VPTR vptr)
{
    if ( (vptr->flags & IDL_V_FILE) || (vptr->flags & IDL_V_STRUCT) )
        return(0);
    else
        return(1);

}
*/
