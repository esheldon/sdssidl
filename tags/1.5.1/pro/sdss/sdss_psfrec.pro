
;+
; NAME:
;  sdss_psfrec()
;
; PURPOSE:
;  reconstruct the psf at position (row,col) in an sdss
;  field from the KL-decompositions. These can be read
;  from the psField files using sdss_read()
;
; CATEGORY:
;  SDSS specific routine.
;
; CALLING SEQUENCE:
;  psf=sdss_psfrec(struct, row, col, counts=)
;
; INPUTS:
;  struct: A PSF KL-decomposition structure read from
;       a psField file.
;  row,col: row and column in the field.
;
; OPTIONAL INPUTS:
;  counts: The total counts in the image. Default is 
;       whatever comes from the reconstruction, which
;       is usually close to unity.
;
; OUTPUTS:
;  An image of the PSF.
;
; EXAMPLE:
;  kl=sdss_read('psfield',run, camcol, field=field, extension=3)
;  psf=sdss_psfrec(kl, row, col, counts=1000)
;
; MODIFICATION HISTORY:
;  Added to archive, 2007-03-26, written by Dave Johnston
;  in the depths of time. 
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

function sdss_psfrec, pstruct, row, col, counts=counts

    on_error, 2
    sdssidl_setup
    return, !sdss->psfrec(pstruct, row, col, counts=counts)

end
