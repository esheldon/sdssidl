;+
; NAME:
;  sdss_read()
;
; PURPOSE:
;  Generic SDSS file reader. Reads asTrans, tsObj, tsField, fpObjc, psField...
;  Atlas files have a special reader read_atlas.  psField files are normally
;  read the same way as tsObj, etc for extension 6 (the default).  For the
;  other extensions you can send an extension and read a single field only. 
;
; CATEGORY:
;  SDSS routine.
;
; CALLING SEQUENCE:
;   st = sdss_read(type, run, camcol, rerun=, bandpass=, fields=, /all,
;                  taglist=, wstring=, ex_struct=, extension=, dir=, 
;                  /pointers, verbose=, status=)
;
;   if type is 'astrans' the extra keywords node= and inc= exist.
;
; INPUTS:
;   type: The file type. Currently supported types are:
;            astrans, tsobj, tsfield, fpobjc, psfield  
;         Note: for psField, Multiple files can only be read for 
;               extension=6 (the default)
;
;         Atlas files have a special reader sf->atlas read, or for the
;         procedural interface read_atlas.
;   run, camcol: SDSS id info.
;
; OPTIONAL INPUTS:
;   rerun: Rerun number.  If not sent, latest is used.
;   bandpass:  For files that require a bandpass.
;   fields: Field number(s).  If not sent, the first is read.
;   /all: read all fields.
;   taglist: List of tags to read. Default is all.
;   wstring: A string that can be sent to the where function to select
;     objects.  Should refer the structure as "lnew"
;   ex_struct: A structure that will be added to the output structure
;     definition.
;   extension: FITS extension to read.
;   indir: Directory from which to read.  Overrides default directory.
;   dir: Directory used for the read.  
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
; OPTIONAL OUTPUTS:
;   status: 0 for success, 1 for failure
;
; EXAMPLES:
;   run=756
;   camcol=3
;   fields=[35,88]
;   st = sdss_read('tsobj', run, camcol, fields=fields)
;
;   kl=sdss_read('psfield', run, camcol, field=field, extension=3)
;   psf=sdss_psfrec(kl, row, col)
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

function sdss_read, filetype, run, camcol, rerun=rerun, bandpass=bandpass, fields=fields, all=all, extension=extension, indir=indir, dir=dir, taglist=taglist, ex_struct=ex_struct, wstring=wstring, nomodrow=nomodrow, node=node, inc=inc, pointers=pointers, silent=silent, verbose=verbose, status=status

    sdssidl_setup
    return, !sdss->read(filetype, run, camcol, rerun=rerun, bandpass=bandpass, fields=fields, all=all, extension=extension, indir=indir, dir=dir, taglist=taglist, ex_struct=ex_struct, wstring=wstring, nomodrow=nomodrow, node=node, inc=inc, pointers=pointers, silent=silent, verbose=verbose, status=status)

end
