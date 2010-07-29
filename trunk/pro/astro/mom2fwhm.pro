function mom2fwhm, mom, pixscale=pixscale

;+
; Name:
;    mom2fwhm
;
; Calling Sequence:
;   fwhm = mom2fwhm(mom, pixscale=0.4)
;
; PURPOSE:
;   Take the "mom" ixx+iyy adaptive moments and convert this 
;   to "seeing" value as a FWHM. This assumes Gaussianity
;
; Inputs:
;   mom: ixx+iyy in pixels^2
;
; Keywords:
;   pixscale: The pixel scale in arcsec, default 0.4
;
; MODIFICATION HISTORY:
;    Creation:  ??-??-?? Dave Johnston, UofChicago
;
;-
;
;
;
;  Copyright (C) 2006  Dave Johnston
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


    if n_elements(mom) eq 0 then begin
        on_error, 2
        message,'-syntax: fwhm=mom2fwhm(mom, pixscale=0.4)'
    endif

    if n_elements(pixscale) eq 0 then pixscale=0.4

    fwhm=2.35*pixscale*sqrt ( mom/2.0 > 0.0)

    return, fwhm
end

