;+
; NAME:
;  read
;
; PURPOSE:
;  Generic SDSS file reader. Reads asTrans, tsObj, tsField, fpObjc, psField...
;  Atlas files have a special reader atlas_read
;  psField files are normally read the same way as tsObj, etc for extension 6 
;  (the default).  For the other extensions holding the psf reconstruction,
;  use psfield_read
;
; CATEGORY:
;  SDSS routine.
;
; CALLING SEQUENCE:
;   st = sf->read(type, run [, camcol, fields, frange=, rerun=, 
;                 filter=, bandpass=, 
;                 taglist=, wstring=, ex_struct=, 
;                 /pointers, verbose=)
;
;   if type is 'astrans' the extra keywords node= and inc= can also
;   be used to return those values
;
; INPUTS:
;   type: The file type.
;         Atlas files have a special reader sf->atlas_read, or for the
;         procedural interface read_atlas.
;   run, camcol: SDSS id info.
;
; OPTIONAL INPUTS:
;   fields: Field number(s) or a glob '*'.
;       can also be entered as a keyword fields=
;
; Keywords:
;   frange: A range of fields to read; can be used instead of the
;       fields argument.
;   rerun: Rerun number.  If not sent, latest is used.
;   bandpass:  For files that require a bandpass.
;   filter: synonym for bandpass
;
;   taglist: List of tags to read. Default is all.
;   wstring: A string that can be sent to the where function to select
;     objects.  Should refer the structure as "lnew"
;   ex_struct: A structure that will be added to the output structure
;     definition.
;
;   /pointers:  Return an array of pointers rather than an array of structures.
;      This saves a factor of two in memory since no copy must be made.
;   verbose=: Verbosity level.  0 for silent, 1 for some info, 2 for lots.
;             default 1
;   /silent: Same as verbose=0.  Verbose keyword takes precedence.
;
; OUTPUTS:
;   An array structure or array of pointers if /pointers is sent.
;
; EXAMPLES:
;   run=756
;   camcol=3
;   fields=[35,88]
;   st = sdss_read('tsobj', run, camcol, fields)
;
; MODIFICATION HISTORY:
;   Conglomeration of various read tools.  Erin Sheldon, NYU
;
;-
;
;  Copyright (C) 2006  Erin Sheldon, NYU.  erin dot sheldon at gmail dot com
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

function sdss_read, filetype, run, camcol, fnums, fields=fields, frange=frange, rerun=rerun, filter=filter, bandpass=bandpass, all=all, extension=extension, indir=indir, dir=dir, taglist=taglist, ex_struct=ex_struct, wstring=wstring, nomodrow=nomodrow, node=node, inc=inc, pointers=pointers, silent=silent, verbose=verbose, status=status

    s = obj_new('sdss_files')
    data = s->read(filetype, run, camcol, fnums, fields=fields, frange=frange, rerun=rerun, filter=filter, bandpass=bandpass, indir=indir, dir=dir, taglist=taglist, ex_struct=ex_struct, wstring=wstring, nomodrow=nomodrow, node=node, inc=inc, pointers=pointers, silent=silent, verbose=verbose, status=status)

    obj_destroy, s
    return, data

end
