;+
; NAME:
;  read_psfield()
;
; PURPOSE:
;  This is a special psfield reader used for psf reconstruction.  This reads
;  extensions 1-6 from the file and places them in a pointer array.  The
;  result can be sent to sdss_psfrec.
;
; CATEGORY:
;  SDSS routine.
;
; CALLING SEQUENCE:
;  psp = read_psfield(run, camcol, field, rerun=, status=)
;
; INPUTS:
;  run, camcol, field: sdss id info.
;
; OPTIONAL INPUTS:
;  rerun: Optional rerun number.
;
; OUTPUTS:
;  pointer array with extensions 1-6
;
; OPTIONAL OUTPUTS:
;  status: 1 for success 0 for failure.
;   
; MODIFICATION HISTORY:
;  Some time in 2002.  Dave Johnston, Erin Sheldon NYU.
;
;-
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


; special psfield reader.  one can use the read method to extract
; the 6th extension for a series of fields.  this allows the user
; to extract the psf reconstruction info, which contains variable
; length arrays, for a single field.   the result will be a pointer
; array containing the first six extensions.  the first 5 are the
; psf reconstruction info, the 6th is the statistics.

function read_psfield, run, camcol, field, rerun=rerun, status=status

    sdssidl_setup
    return, !sdss->psfield_read(run, camcol, field, rerun=rerun, status=status)

end 

