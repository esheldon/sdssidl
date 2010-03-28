;+
; NAME:
;  nmgy2mag
;
; PURPOSE:
;  Convert flux in nanomaggies to magnitudes.
;
; CATEGORY:
;  SDSS specific routine
;
; CALLING SEQUENCE:
;   mag=nmgy2mag(nmgy, ivar=ivar, err=err)
;
; INPUTS:
;  flux in namomaggies
;
; OPTIONAL INPUTS:
;   ivar: inverse variance in flux.  If this is sent
;		and err= is present, the mag errors are also 
;		calculated.
;
; OUTPUTS:
;   mag: mags in luptitudes
;
; OPTIONAL OUTPUTS:
;   err: Error on magnitudes.
;
;
; MODIFICATION HISTORY:
;   2010-02-05: Erin Sheldon, BNL
;
;-
;
;
;
;  Copyright (C) 2010 Erin Sheldon
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



function nmgy2mag, nmgy, ivar=ivar, err=err
		
	if n_elements(nmgy) eq 0 then begin
		on_error,2
		message,'usage: mag=nmgy2mag(nmgy, ivar=, err=)'
	endif

	; preserve structure of input
	mag=nmgy
	mag[*] = -9999.0

	mag[*] = 22.5-2.5*alog10(nmgy > 0.001)

	if n_elements(ivar) ne 0 and arg_present(err) then begin
		err = ivar
		err[*] = 9999.0

		w=where(ivar gt 0, nw)
		if nw gt 0 then begin
			err[w] = sqrt(1/ivar[w])
			a = 2.5/alog(10)
			err[w] = a*err[w]/(nmgy[w] > 0.001)
		endif
	endif

	return, mag
end
