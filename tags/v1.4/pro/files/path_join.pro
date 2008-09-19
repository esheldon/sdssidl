;+
; NAME:
;   path_join
;
; PURPOSE:
;   Join path elements using the default path separator, such as directory and filenames.
;
; CALLING SEQUENCE:
;   f = path_join(path1, path2, subdirectory=) 
;
; INPUTS:
;   path1, path2:  Path elements.  For example, path1 could be a directory 
;       and path2 a filename.  The output will be the same length as path2.
;       Both are optional, but if not sent then subdir must be sent.
; OPTIONAL INPUTS:
;   subdirectory: A scalar or array of strings.  These are combined with
;       the path separator and combined with path1 if it was sent.
;
; OUTPUTS:
;   New path with all paths joined.
;
; EXAMPLES:
;   print,path_join('/tmp', 'tmp.txt')
;   /tmp/tmp.txt
;   print,path_join('~/data', subdir=['images','raw'], 'image.fits')
;   ~/data/images/raw/image.fits
;   print,path_join(subdir=['local','dir'])
;   local/dir
;
; MODIFICATION HISTORY:
;   Moved to main archive: 2007-04-22, Erin Sheldon, NYU
;   2008-03-22: Fixed bug when path1 is empty string. E.S.
;
;-
;
;  Copyright (C) 2005  Erin Sheldon, NYU.  erin dot sheldon at gmail dot com
;
;    This program is free software; you can redistribute it and/or modify
;    it under the terms of version 2 of the GNU General Public License as 
;    published by the Free Software Foundation.
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

function _path_join_add, oldpath, newpath

    ; The path separator for this platform
    psep = path_sep()

    parray = [oldpath, newpath]
    w=where(parray ne '', nw)

    if nw eq 0 then begin
        path=''
    endif else begin
        parray = parray[w]
        path = strjoin(parray, psep, /single) 
    endelse
    return, path
end

function path_join, path1, path2, subdirectory=subdir

    n1=n_elements(path1) & n2=n_elements(path2) & nsub=n_elements(subdir)
    if n1 eq 0 and n2 eq 0 and nsub eq 0 then begin
        print,'-Syntax: f = path_join(path1 [, path2, subdirectory=])'
        on_error, 2
        message,'Halting'
    endif

    path=''
    if n1 ne 0 then begin
        path = _path_join_add(path, path1)
    endif

    if n_elements(subdir) ne 0 then begin
        path = _path_join_add(path, subdir)
    endif

    if n2 ne 0 then begin
        path = _path_join_add(path, path2)
    endif

    return, path

end
