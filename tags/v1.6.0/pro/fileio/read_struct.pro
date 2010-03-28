;+
; NAME:
;   READ_STRUCT
;
; PURPOSE:
;   Quickly read a numeric ASCII data file(s) into an IDL structure.  
;   The file(s) can be white space or csv. NOTE: write_idlstruct and 
;   read_idlstruct is a better method as the files are self-describing.
;
; CALLING SEQUENCE:
;      read_struct, file(s), struct_def, outstruct,
;           nlines=, skiplines=, endskip=, numlines=, /silent
;
; INPUTS:
;   files/lun - Name of ASCII data file(s).  Can be an array of file names.
;       May also be a single logical unit of an already-opened file, but then 
;       nlines must be sent.
;   struct_def: a struct which will be replicated and returned
;       with the data. The order of tags should equal the order
;       of the columns in the data file. This can include arrays as
;       long as they match up with the columns. Every entry in a string
;       column must be the same length and match the structure definition.
;
; OPTIONAL INPUTS:
;   nlines - Total number of lines in file. Saves time since the number of 
;       lines does not have to be determined.  If a logical unit is sent
;       then this is the number of lines after the current line.
;   skiplines - Integer specifying number of lines to skip at the top of file 
;       before reading.   Default is to start at the first line.
;   endskip - Number of lines to skip at the end of the file.
;   numlines - Integer scalar specifying number of lines in the file to read.
;       Default is to read the entire file.  The above keywords take effect
;       first, so that the numlines <= nlines - skiplines - endskip
;
; KEYWORD PARAMETERS:
;   /compress: The file is gzip compressed.
;   /silent: if set, nothing is printed to STDOUT
;
; OUTPUTS:
;   outstruct: the output structure. It is an array of structures, each
;       element is identical to the input struct_def. the number
;       of array elements is the number of lined of data in the file. 
;
;
; RESTRICTIONS:
;   Cannot be used to read strings unless the string column is fixed width and
;   this width is properly matched to the structure definition.
;
; EXAMPLES:
;   ;; Read a 5 column file into a structure.
;          0      0.0749063     85.3804855     15.8031492
;          1      0.3044448     16.1201153     70.0963593
;          2      0.8440148     36.8116074     93.5149231
;          .......
;
;   IDL> struct_def = {a:0L, b:0.0, c:fltarr(2)}
;   IDL> read_struct, 'test.dat', struct_def, outstruct
;   Reading 20 lines from file test.dat
;   IDL> help, outstruct, /str
;   ** Structure <8277fd4>, 3 tags, length=16, data length=16, refs=1:
;      A               LONG                 0
;      B               FLOAT         0.0749063
;      C               FLOAT     Array[2]
;
; REVISION HISTORY:
;   Created: Erin Scott Sheldon 07-Mar-2001 Interface based on RDFLOAT.PRO
;                                   
;   2007-09-27: Can now read multiple files.  More self-sufficient method
;       for determining number of lines.
;

function _read_struct_nlines, filename, compress=compress

    if !version.release ge '5.6' then begin
        nlines = file_lines(filename, compress=compress)
    endif else begin
        f = expand_path(filename)
        if keyword_set(compress) then begin
            spawn,'gzip -dc '+f+' | wc -l', result, /sh    
        endif else begin
            spawn,'wc -l '+f,result,/sh
        endelse
        nlines = long(result[0])
    endelse

    return, nlines

end

; Read multiple files.  This is a recursive call.
function _read_struct_multi, files, struct_def, $
                 skiplines=skiplines, $
                 endskip=endskip, $
                 numlines=numlines, $
                 nlines=nlines, $
                 compress=compress, $
                 silent=silent

    nf = n_elements(files)
    ptrlist = ptrarr(nf)
    for i=0L, nf-1 do begin
        read_struct, files[i], struct_def, tmpst, $
                 skiplines=skiplines, $
                 endskip=endskip, $
                 numlines=numlines, $
                 nlines=nlines, $
                 compress=compress, $
                 silent=silent

        ptrlist[i] = ptr_new(tmpst, /no_copy)
    endfor

    struct = combine_ptrlist(ptrlist)
    return, struct

end

pro read_struct, filename, struct_def, outstruct,$
                 skiplines=_skiplines, $
                 endskip=_endskip, $
                 numlines=_numlines, $
                 nlines=_nlines, $
                 compress=compress, $
                 silent=silent

    if N_params() lt 2 then begin
        on_error, 2
        print,'Syntax - read_struct, file, struct_def, outstruct [, '
        print,'                      skiplines=, endskip=, '
        print,'                      nlines=, numlines =, /compress, /silent]'
        message,'Halting'
    endif

    ; We don't want these returned
    if n_elements(_nlines) ne 0 then nlines=_nlines
    if n_elements(_numlines) ne 0 then numlines=_numlines
    if n_elements(_skiplines) ne 0 then skiplines=_skiplines
    if n_elements(_endskip) ne 0 then endskip=_endskip

    ;; open file and skip lines
    tname = size(filename,/tname)
    if tname eq 'INT' or tname eq 'LONG' then begin 
        if n_elements(nlines) eq 0 then begin
            message,'You must send nlines if you input the logical unit'
        endif
        lun = filename
    endif else if tname eq 'STRING' then begin 

        ; Reading multiple files?
        if n_elements(filename) gt 1 then begin
            outstruct = _read_struct_multi(filename, struct_def, $
                 skiplines=skiplines, $
                 endskip=endskip, $
                 numlines=numlines, $
                 nlines=nlines, $
                 compress=compress, $
                 silent=silent)
             return
        endif


        if n_elements(nlines) eq 0 then begin
            nlines = _read_struct_nlines(filename, compress=compress)
        endif
        openr, lun, filename, /get_lun, error=error, compress=compress
        if error ne 0 then message,'Error opening file: ',filename
    endif else begin 
        message,'Datatype of filename/lun '+tname+' is incorrect'
    endelse 

    
    ;; Should we skip lines at the top or bottom?
    if n_elements(skiplines) eq 0 then skiplines = 0
    if n_elements(endskip) eq 0 then endskip = 0
    nlines = nlines - skiplines - endskip

    ;; How many lines to actually read
    if n_elements(numlines) ne 0 then nlines = numlines < nlines
    
    if nlines le 0 then message,'Number of lines to read must be >= 0'

    ; Skip some lines?
    temp = ' '
    if skiplines GT 0 then begin
        for i=0,skiplines-1 do readf, lun, temp
    endif
  
    if not keyword_set(silent) then begin
        print,'Reading ',ntostr(nlines),' lines from file ',filename
    endif
  
    ;; replicate the structure and read it in
    outstruct = replicate(struct_def[0], nlines)
    readf, lun, outstruct

    ;; close the file
    if tname eq 'STRING' then free_lun, lun
  
    return
end
