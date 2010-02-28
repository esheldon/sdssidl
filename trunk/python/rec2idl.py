"""

Convert the header for a .rec file to something IDL can understand and print
to stdout. The result will be a string representing and IDL structure. The
variable in the struct will all be strings which must be evaled to get
idl variables.

"""

import sys
import os

def read_header(filename):
    """
    Name:
        read_header()

    Read the header from the simple self-describing file format 'rec' with an
    ascii header.  See the write() function for information about reading this
    file format, and read() for reading.

    See the esutil/sfile.py module for more info.


    The file format:
      First line:
          SIZE = --------------number

    where if possible the number should be formatted as %20d.  This is
    large enough to hold a 64-bit number.  This exact formatting is
    required so SIZE can be updated *in place* when appending rows to a
    file holding structured arrays.  Note the file can always be read as
    long as the first line reads SIZE = some_number but appending requires
    the exact format.


    Last two lines of the header region must be:
            END
            blank line
    case does not matter.

      
    In between the SIZE and END lines is the header data.  This is a
    string that must eval() to a dictionary.  It must contain the
    following entry:

          _DTYPE = array data type description in list of tuples or
            string form (case does not matter, can also be called _dtype).
              
            for a structured array.
                [('field1', 'f8'), ('f2','2i4')]
            for a simple array:
                '<f4'

    As noted above, only files holding structured arrays (recarray, array
    with fields) can be appended.  If the data is a simple array or matrix
    no appending is supported.

    There should also be a _VERSION tag.

          '_VERSION': '1.0'

    If '_VERSION' is not present, it is assumed that the version is 1.0,
    but you should always set this.  If you use this module to write data,
    it will always be set.


    If the file holds a simple array, and the dtype field is a simple
    string, then the following keyword, if present, will be used to
    reshape the array:

          '_SHAPE'

    If the total elements in the _shape field matches the size then it
    will be used to reshape the array before returning or when using
    memory maps.

    If the data are ascii then delimiter must be given in the keyword

          _DELIM  
          
    This can be for example ',', ' ', or a tab character.  Again, case does
    not matter.  

    The rest of the keywords can by any variable can be used as long as it
    can be eval()d.

    An example header:
        SIZE =                   10
        {'_VERSION': '1.0',
         '_DELIM': ',',
         '_DTYPE': [('x', 'f4'),
                    ('y', 'f4'),
                    ('ra', 'f8'),
                    ('dec', 'f8'),
                    ('exposurename', 'S20'),
                    ('ccd', 'i1'),
                    ('size_flags', 'i4'),
                    ('magi', 'f4'),
                    ('sigma0', 'f4'),
                    ('star_flag', 'i4'),
                    ('shear_flags', 'i4'),
                    ('shapelet_sigma', 'f4'),
                    ('shear1', 'f4'),
                    ('shear2', 'f4'),
                    ('shear_cov00', 'f4'),
                    ('shear_cov01', 'f4'),
                    ('shear_cov11', 'f4')],
         'listvar': [1, 2, 3],
         'subd': {'subd1': 'subfield', 'sublist': [8.5, 6.6]},
         'svar': 'hello',
         'test1': 35}
        END

        -- data begins --

    """

    fobj = open(filename)

    # read first line, which should be 
    # SIZE = .....
    # or
    # NROWS = ...

    line = fobj.readline().strip()

    lsplit = line.split('=')
    if len(lsplit) != 2:
        raise ValueError("First line of header must be SIZE = %20d")

    fname=lsplit[0].strip()
    if fname.upper() != 'SIZE' and fname.upper() != 'NROWS':
        raise ValueError("First line of header must be SIZE = %20d")

    size = eval(lsplit[1])

    # the rest is a python dict. Read through the header until we hit "END"

    lines = []
    line=fobj.readline().strip()
    while line.upper() != 'END':
        lines.append(line)
        line=fobj.readline().strip()

    fobj.close()

    hdrstring = ' '.join(lines)
    hdr = eval(hdrstring)

    hdr['_SIZE'] = size

    # this will leave open the possibility of changing the header or other
    # details later
    if '_version' in hdr or '_VERSION' in hdr:
        pass
    else:
        hdr['_VERSION'] = '1.0'

    return hdr


def hdrdict2idlstruct(hdr_dict):
    """

    Convert the header dict into a string representing an idl structure All
    fields are represented as strings, so it is up to the user to attempt to
    convert them to IDL variables using eval() or similar. Not all will
    convert correctly, such as tuples or lists of non-equal member types.

    """

    struct_items = []
    for key in hdr_dict:
        val=hdr_dict[key]
        if isinstance(val,str):
            item = '%s:"\'%s\'"' % (key,val)
        else:
            val=str(val)
            val=val.replace("'", '"')

            item = "%s:'%s'" % (key,val)

        struct_items.append(item)

    structdef = '{' + ', '.join(struct_items)+'}'

    return structdef

if len(sys.argv) < 2:
    sys.stdout.write("usage: python rec2idl.py recfile")
    sys.exit(45)


fname=sys.argv[1]
fname=os.path.expandvars(fname)
fname=os.path.expanduser(fname)

hdr=read_header(fname)

stdef=hdrdict2idlstruct(hdr)

sys.stdout.write(stdef+"\n")

