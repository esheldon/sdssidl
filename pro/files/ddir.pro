;+
; NAME:
;   ddir
;
; PURPOSE:
;   Generate a directory name based upon input elements.  Combined with dfile defines a generic naming scheme for directories and files.
;
; CALLING SEQUENCE:
;   dir = ddir(pathlist, root=, file=) 
;
; OPTIONAL INPUTS:
;   pathlist: An array of path elements above the root.
;   root: The root directory.  Default is the empty string.
;
; OUTPUTS:
;   directory build up as the level elements combined with the root dir.  These
;   are connected using the operating system's path separator.  On UNIX:
;       root/level1/level2/....
;
; OPTIONAL OUTPUTS:
;   filebase: The basic file element associated with this directory structure.
;           level1-level2-level3
;       This is used by dfile to build a file name. Note root is not includec. 
;
; NOTES:
;   If there are no inputs the empty string is returned.
;
; EXAMPLE:
;   dir = ddir(root='~/project', ['output','sample01','split16'], filebase=filebase)
;   print,dir
;   ~/project/output/sample01/split16
;   print,filebase
;   'output-sample01-split16
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

function ddir, pathlist_in, root=root, filebase=filebase

    if n_elements(root) eq 0 then root=''

    filebase=''
    ndir = n_elements(pathlist_in)
    if ndir eq 0 then begin
        return, root
    endif

    w=where(pathlist_in ne '', ndir)
    if ndir eq 0 then begin
        return, root
    endif

    pathlist = pathlist_in[w]

    ; get path separator for this platform
    dir = strjoin(pathlist, path_sep())

    ; now create file by replacing the directory separator with
    ; a dash, if requested
    if arg_present(filebase) then filebase = strjoin( strsplit(dir, path_sep(),/extract), '-')

    ; Add root
    if root ne '' then dir = filepath(root=root, dir)
    return, dir
end


