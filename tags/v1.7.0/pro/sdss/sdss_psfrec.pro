
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
;  ; read r-band kl psf decomposition in extension 3
;   sf = obj_new('sdss_files')
;  kl=sf->psfield_read(run, camcol, field=field, extension=3)
;  ; reconstruct the image
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

function sdss_psfrec, pstruct, row, col, counts=counts, trim=trim, coeffs=coeffs, ecoeffs=ecoeff

	np = $
		(n_elements(pstruct) gt 0) $
		+ (n_elements(row) gt 0) $
		+ (n_elements(col) gt 0)

	if np lt 3 then begin 
		on_error, 2
		print,'-Syntax: psf_image = ' +$
			'sdss_psfrec(pstruct, row, col, /trim, counts=)'
		message,'Halting'
	endif 

	rcs=.001

	nrow_b=(pstruct.nrow_b)[0]
	ncol_b=(pstruct.ncol_b)[0]
	;assumes they are the same for each eigen
	;so only use the 0 one
	rnrow=(pstruct.rnrow)[0]
	rncol=(pstruct.rncol)[0]

	nb=nrow_b*ncol_b
	coeffs=fltarr(nb)
	ecoeff=fltarr(3)
	cmat=pstruct.c


	for i=0l, nb-1 do coeffs[i]=(row*rcs)^(i mod nrow_b) * (col*rcs)^(i/nrow_b)

	for j=0,2 do begin
		for i=0l, nb-1 do begin
			ecoeff[j]=ecoeff[j]+cmat(i/nrow_b,i mod nrow_b,j)*coeffs[i]
		endfor	
	endfor
	p=(pstruct.rrows)[*,0]*ecoeff[0]+$
		(pstruct.rrows)[*,1]*ecoeff[1]+$
		(pstruct.rrows)[*,2]*ecoeff[2]

	if n_elements(counts) ne 0 then begin 
		p = p/total(p)*counts
	endif 

	p = reform(p,rncol,rnrow)

	if keyword_set(trim) then begin
		; trim to the non-zero region
		p = p[10:40, 10:40]
	endif
	return,p

end
