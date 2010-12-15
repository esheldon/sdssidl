pro which,proc_name,files=files,show=show,help=help, dlm=dlm, silent=silent
;+
; Name:
;   which
;
; Purpose:
;   Find the location of the named IDL program file.  This is
;   similar to the unix 'which' command.  
;
; Procedure:
;   The !path variable is searched for any occurrences of a file 
;   named {name}.pro 
;
;   If no files are found, search for intrinsic procedures/functions
;	with the input name and explain if found.
;
; Calling Sequence:
;    which, [ name, files=, /dlm, /show, /help, /silent ]
;
; OPTIONAL INPUT:
;	proc_name - Character string giving the name of the IDL procedure or 
;		function.  Do not give an extension.   If omitted, 
;		the program will prompt for PROC_NAME.
;	/dlm: Search !DLM_PATH for dynamically loaded modules instead
;			of !PATH for .pro files
;   /show: if set, will display the first file found
;   /help: print syntax
;   /silent: Don't print the locations of the files.
;
; OPTIONAL OUTPUT KEYWORDS:
;	files: An array containing the filenames
;
; EXAMPLE:
;	Find out where the procedure CURVEFIT lives.
;
;	    IDL> which, 'curvefit'
;       Using: /home/products/Linux/idl/v7_0/lib/curvefit.pro
;
; REVISION HISTORY:
;	29-MAY-94  Modified from getpro.pro by E. Deutsch
;	14-JUL-95  Fixed for IDL 4.0
;   03-DEC-2000 Added files and show keywords. Erin Sheldon
;   21-JUN-2004 Use FILE_WHICH procedure for IDL > 5.3  for significant
;       speed increase. Fixed intrinsic procedure searching. E.S.
;		Look also for dynamically loaded modules if /dlm is set
;	27-May-2009: Due to bug in idl 7.0 file_which, moved over to using 
;		straight file_search.  This now makes the dividing line 5.5 between
;		the two methods.  Erin Sheldon, BNL
;   2010-12-15: Simplify: Just check every directory in !path for the filename.
;       Dropped VMS support.  ESS, BNL
;-

    if keyword_set(help) then begin
        print,'-syntax: which, proc_name, files=files, /dlm, /show, /help, /silent'
        return
    endif 

    common which_block, funcnames, pronames

    ; prompt for procedure name?
    if (n_params() eq 0) then begin 	     
        proc_name = ' ' 
        read,'Enter name of procedure to look for: ',proc_name     
    endif else begin
        zparcheck, 'which', proc_name, 1, 7, 0, 'Procedure name'
    endelse

    ; Don't want file extensions
    fdecomp, proc_name, disk, dir, name      
    name = strtrim( name, 2 )  
    pname = "'"+name+"'"



    ; Get current IDL path of directories
    if keyword_set(dlm) then begin 
        path_string = !dlm_path
        ext = '.dlm'
    endif else begin 
        path_string = !path
        ext = '.pro'
    endelse                 
    sep = ':'

    paths = strsplit(path_string, sep, /extract)

    delvarx, files
    found=0
    for i=0L, n_elements(paths)-1 do begin
        dir = paths[i]

        filename = filepath(root=dir, proc_name+ext)
        if file_test(filename) then begin
            found = found+1
            add_arrval, filename, files

            if not keyword_set(silent) then begin 
                if (found eq 1) then print,'Using: '+filename
                if (found gt 1) then print,'Also found: '+filename
                ;print,'    '+filename
            endif 
        endif
    endfor
  
    if (found ge 1) then begin
        if keyword_set(show) then spawn,'more '+files[0]
        return
    endif 


    ; the search failed. check for intrinsic idl procedures
    if n_elements(funcnames) eq 0 then begin 
        proNames  = routine_info(/system)
        funcNames = routine_info(/system,/func)
    endif 

    uname = strupcase(name)
    functest = where (funcnames eq uname, fcount)
    protest  = where (pronames  eq uname, pcount)
  
    if (fcount eq 0) and (pcount eq 0) then begin

        ;; not found
        if not keyword_set(silent) then begin 
            print, pname+' is not in the !PATH directory and is not inrinsic'
        endif 

    endif else begin 
        files = 'INTRINSIC'

        ;; Its either a built in function or procedure
        if not keyword_set(silent) then begin 

            if fcount ne 0 then begin 
                print, pname + ' is an intrinsic IDL Function.'
            endif else begin 
                print, pname + ' is an intrinsic IDL procedure.'
            endelse 
        endif 

    endelse 

    return

end 
  
