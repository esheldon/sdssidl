function fwhm2mom, fwhm, pixscale=pixscale

;+
; NAME:
;   fwhm2mom 
;
; PURPOSE:
;   Take a FWHM in arcsec and convert to ixx+iyy in pixels^2
;   assuming gaussianity.
;
; CALLING SEQUENCE:
;   mom = fwhm2mom(fwhm, pixscale=0.4)
;
; Inputs:
;   fwhm: The full width half max in arcseconds.
;
; Keywords:
;   pixscale: The pixel scale in arcsec.  Default 0.4
;
; Output:
;   "mom", the ixx+iyy in pixels^2
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


    if n_elements(fwhm) eq 0 then begin
        on_error, 2
        message,'-syntax: mom=fwhm2mom(fwhm, pixscale=0.4)'
    endif

    if n_elements(pixscale) eq 0 then pixscale=0.4

    fac = 2*sqrt(2*alog(2))
    mom = 2*(fwhm/fac/pixscale)^2
    
    return, mom
end

