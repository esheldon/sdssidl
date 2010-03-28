#include "IDLStruct.hpp"


static IDL_VPTR testidlstruct(int argc, IDL_VPTR argv[])
{
    IDL_VPTR var;
    IDL_VPTR tagvptr=NULL;

    IDL_VPTR output;
    char* result;

    var = argv[0];

    tagvptr = argv[1];
    IDL_ENSURE_SIMPLE(tagvptr);

    IDLStruct st;
    if (!st.Init(var)) 
        return(IDL_GettmpInt(-1));   

    IDL_MEMINT len=5;

    result = st.MakeSubStruct(tagvptr, len, &output);

    if (result==NULL)
        return(IDL_GettmpInt(-1));   
    else
        return(output);
}

static IDL_VPTR testidlstruct2(int argc, IDL_VPTR argv[])
{
    IDL_VPTR var;

    IDL_VPTR output;
    char* result;

    var = argv[0];

    IDLStruct st;
    if (!st.Init(var)) 
        return(IDL_GettmpInt(-1));   

    IDL_MEMINT len=5;

    vector<IDL_MEMINT> subtagnums;
    subtagnums.push_back(1);
    subtagnums.push_back(2);

    result = st.MakeSubStruct(subtagnums, len, &output);

    if (result==NULL)
        return(IDL_GettmpInt(-1));   
    else
        return(output);

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
  static IDL_SYSFUN_DEF2 func1_addr[] = {
    { (IDL_SYSRTN_GENERIC) testidlstruct, 
      "TESTIDLSTRUCT", 
      0, 
      IDL_MAXPARAMS, 
      0, 
      0},
  };
  static IDL_SYSFUN_DEF2 func2_addr[] = {
    { (IDL_SYSRTN_GENERIC) testidlstruct2, 
      "TESTIDLSTRUCT2", 
      0, 
      IDL_MAXPARAMS, 
      0, 
      0},
  };



  /* The false means it is not a function */
  return 
      (IDL_SysRtnAdd(func1_addr, IDL_TRUE, ARRLEN(func1_addr)) &&
       IDL_SysRtnAdd(func2_addr, IDL_TRUE, ARRLEN(func2_addr)));

}

