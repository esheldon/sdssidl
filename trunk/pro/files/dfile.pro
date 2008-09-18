;+
; NAME:
;   dfile
;
; PURPOSE:
;   Create a generic filename based upon the directory structure.  Combined with ddir defines a generic naming scheme for directories and files.
;
; CALLING SEQUENCE:
;   file = dfile(pathlist, root=, prefix=, postfix=, extension=)
;
; INPUTS:
;
; OPTIONAL INPUTS:
;   pathlist: An array of path elements above the root.
;   root: The root directory.  Default is the empty string ''
;   prefix: A prefix added to the front of the file name.  Default ''
;   postfix: A postfix added to the end of the file name.  Default ''
;   extension: A file extension. Default ''
;
; KEYWORDS:
;   /nodir: Don't add the directory to the file name.
;
; OUTPUTS:
;   file: A generic file name.  Directory elements are connected with the path 
;       separator for the current operating system.  The file name contains the
;       same elements separted by dashes '-'.  On UNIX this looks like:
;           root/level1/..../levelN/prefix-level1-...-levelN-postfix.ext 
;
; OPTIONAL OUTPUTS:
;   dir: Set to a named variable to return the directory for this file.
;
; NOTES:
;   If there are no inputs the empty string is returned.
;
; EXAMPLE:
;   file = dfile(root='~/project', ['output','sample10','split16'], 
;                prefix='proj', postfix='02', ext='.dat')
;   print,file
;   ~/project/output/sample01/split16/proj-output-sample10-split16-02.dat
;
; MODIFICATION HISTORY:
;   Created: 2007-04-22.  Moved to main archive. Erin Sheldon, NYU
;
;-
;
;
;
;  Copyright (C) 2005  Erin Sheldon, NYU.  erin dot sheldon at gmail dot com
;
;    This program is free software; you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation; either version 2 of the License, or
;    (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with this program; if not, write to the Free Software
;    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
;
;



; remove leading or trailing strings
function _rmleadtrail, strin, pattern
    str = strin 

    len = strlen(str)
    first = strpos(str, pattern)
    if first eq 0 then begin
        str=strmid(str,1) 
    endif

    len = strlen(str)
    last = strpos(str, pattern, /reverse_search)
    if last eq len-1 then begin
        str = strmid(str, 0, len-1)
    endif

    return, str
end
function dfile, pathlist, root=root, prefix=prefix_in, postfix=postfix_in, extension=ext_in, nodir=nodir, $
    dir=dir

    if n_elements(prefix_in)  eq 0 then prefix=''  else prefix=_rmleadtrail(prefix_in, '-')
    if n_elements(postfix_in) eq 0 then postfix='' else postfix=_rmleadtrail(postfix_in, '-')
    if n_elements(ext_in)     eq 0 then ext=''     else ext=_rmleadtrail(ext_in, '.')

    dir = ddir(pathlist, root=root, filebase=file)

    ; build up file elements
    file_elements = file

    if prefix ne '' then file_elements = [prefix, file_elements]
    if postfix ne '' then file_elements = [file_elements, postfix]
    w=where(file_elements ne '', nw)
    if nw ne 0 then begin
        file_elements=file_elements[w]
        file = strjoin(file_elements, '-')
        if ext ne '' then file = file + '.' + ext

        if not keyword_set(nodir) then begin
            file = filepath(root=dir, file)
        endif
    endif else begin
        file = ''
    endelse

    return, file
end

