pro mom2seeing,mom,seeing

;+
; NAME:
;    MOM2SEEING
;
; PURPOSE:
;    Take the adaptive moments of the psf and convert this a "seeing" value 
;    or FWHM assumes the psf is Gaussian
;
; CALLING SEQUENCE:
;    mom2seeing, mom, seeing
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


if n_params() eq 0 then begin
   print,'-syntax  mom2seeing,mom,seeing'
   return
endif

seeing=2.35*0.4*sqrt ( mom/2.0 > 0.0)

return
end

