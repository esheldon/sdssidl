### Sub Directories ###
  * [DLM](SrcDLM.md)
  * [IDLStruct](SrcIDLStruct.md) -- A C++ class that wrapps an IDL structure and gives an interface to the elements and tags.
  * [atlas](SrcAtlas.md) -- Wrappers for Robert Lupton's atlas reader code.
  * [fileio](SrcFileio.md) -- C++ extensions for efficient reading and writing of binary and ascii files.
  * [gauleg](SrcGauleg.md) -- C extension for calculating gauss-legendre integration weights and abscissa.
  * [htmIndexIDL](SrcHtmIndexIDL.md) -- C++ extension for performing hierarchical trianglar mesh indicex calculation, searches, and matches. These are wrappers around the basic library of Peter Kunszt.
  * [pgsql](SrcPgsql.md) -- C extension for interfacing with a postgres database. Returns the results in an IDL structure.
  * [sdsspixIDL](SrcSdsspixIDL.md) -- C extension wrapping Ryan Scranton's sdsspix routines.
  * [sphpoly\_masks](SrcSphpoly_masks.md) -- C extension wrapping Andreas Berlind's spherical polygon search code.
  * [total\_int](SrcTotal_int.md) -- C extension to total an array as integers. Like built in total(), but works on integers without converting to float. IDL versions greater than 6 also support total(x, /int)
### Files ###
  * [DESCRIPTION](http://code.google.com/p/sdssidl/source/browse/trunk/src/DESCRIPTION)
  * [Makefile](http://code.google.com/p/sdssidl/source/browse/trunk/src/Makefile)
  * [configure](http://code.google.com/p/sdssidl/source/browse/trunk/src/configure)