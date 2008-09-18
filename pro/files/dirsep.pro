
;+
; NAME:
;   dirsep
;
; PURPOSE:
;   Split the input file name(s) into directory and basename.
;
; CALLING SEQUENCE:
;   dirsep, files, dirs, basenames
;
; INPUTS:
;   files: A file or files.
;
; OUTPUTS:
;   dirs: The directory names.
;   basenames: The base names.
;
; EXAMPLE:
;   IDL> files = ['/some/path1/stuff.txt', '/other/path2/things.fits']
;   IDL> dirsep, files, dirs, basenames
;   IDL> print,dirs
;   /some/path1/ /other/path2/
;   IDL> print,basenames
;   stuff.txt things.fits
;
; MODIFICATION HISTORY:
;   Author: Erin Sheldon, NYU
;
;-
;  This program is part of sdssidl.
;  Copyright (C) 2005  Erin Sheldon, NYU.  erin.sheldon at gmail.com
;
;    This program is free software; you can redistribute it and/or modify
;    it under the terms of the GNU General Public License version 2 as 
;    published by the Free Software Foundation.
;
pro dirsep, files, dirs, basenames

    ;; separate the file name from the directory in UNIX
  
    if n_elements(files) eq 0 then begin 
        print,'-Syntax: dirsep, files, dirs, basenames'
        return
    endif 

    ; Can use new functions for 6.0 or later
    if float(!version.release) gt 6 then begin
        dirs = file_dirname(files)
        basenames  = file_basename(files)
        return
    endif
 
    nfile = n_elements(files)
    if nfile eq 1 then begin 
        basenames = '' 
        dirs = ''
    endif else begin 
        basenames = strarr(nfile)
        dirs = strarr(nfile)
    endelse 

    psep = path_sep()
    for i=0l, nfile-1 do begin 
        tmp = strsplit(files[i], psep)
        ntmp = n_elements(tmp)
        if ntmp eq 1 then begin 
            dirs[i] = ''
            basenames[i] = files[i]
        endif else begin 
            dirs[i] = strmid(files[i], 0, tmp[ntmp-1]-1)
            basenames[i] = strmid(files[i], tmp[ntmp-1])
        endelse 
    endfor 

    return

end 
