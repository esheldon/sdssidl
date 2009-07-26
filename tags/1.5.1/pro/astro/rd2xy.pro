pro rd2xy, a, d, astr, x, y
;+
; NAME:
;	RD2XY
; PURPOSE:
;	Compute an X and Y position given the RA and DEC and an astrometry
;	structure from a FITS header.  A tangent (gnomonic) projection is
;	computed directly; other projections are computed using WCSXY2SPH.  
;	AD2XY is meant to be used internal to other procedures.   For 
;	interactive purposes, use ADXY.
;
; CALLING SEQUENCE:
;	AD2XY, a ,d, astr, x, y   
;
; INPUTS:
;	A -     R.A. in DEGREES, scalar or vector
;	D -     Dec. in DEGREES, scalar or vector
;	ASTR - astrometry structure, output from EXTAST procedure containing:
;   	 .CD   -  2 x 2 array containing the astrometry parameters CD1_1 CD1_2
;		in DEGREES/PIXEL                                   CD2_1 CD2_2
;	 .CDELT - 2 element vector giving increment at reference point in
;		DEGREES/PIXEL
;	 .CRPIX - 2 element vector giving X and Y coordinates of reference pixel
;		(def = NAXIS/2)
;	 .CRVAL - 2 element vector giving R.A. and DEC of reference pixel 
;		in DEGREES
;	 .CTYPE - 2 element vector giving projection types 
;
; OUTPUTS:
;	X     - row position in pixels, scalar or vector
;	Y     - column position in pixels, scalar or vector
;
; REVISION HISTORY:
;	Converted to IDL by B. Boothman, SASC Tech, 4/21/86
;	Use astrometry structure,  W. Landsman      Jan. 1994	
;       Do computation correctly in degrees  W. Landsman       Dec. 1994
;	Only pass 2 CRVAL values to WCSSPH2XY   W. Landsman      June 1995
;	Don't subscript CTYPE      W. Landsman       August 1995	
;	Changed to RD2XY forces tangent projection  David Johnston 1998
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

 On_error,2

 if N_params() lT 4 then begin
	print,'Syntax -- RD2XY, a, d, astr, x, y'
	return
 endif

 radeg = 180.0D/!DPI                 ;Double precision !RADEG
; ctype = astr.ctype

 ;if  (strmid(ctype(0),5,3) EQ 'TAN') or (ctype(0) EQ '') then begin   

         crval = astr.crval/ radeg
	 radif = a/RADEG - crval(0)
         dec = d / radeg
	 h = sin(dec)*sin(crval(1)) + cos(dec)*cos(crval(1))*cos(radif)

	 xsi = cos(dec)*sin(radif)/h
	 eta = (sin(dec)*cos(crval(1)) -  cos(dec)*sin(crval(1))*cos(radif))/h
 
	 xsi = xsi*RADEG
	 eta = eta*RADEG

 ;endif else wcssph2xy, a, d, xsi, eta, CTYPE = ctype, PROJP1 = astr.projp1, $
;	PROJP2 = astr.projp2, LONGPOLE = astr.longpole, CRVAL = astr.crval(0:1)

 xsi = xsi/astr.cdelt(0)
 eta = eta/astr.cdelt(1)

 crpix = astr.crpix
 cdinv = invert(astr.cd)
 x = ( cdinv(0,0)*xsi + cdinv(0,1)*eta + crpix(0) - 1 )
 y = ( cdinv(1,0)*xsi + cdinv(1,1)*eta + crpix(1) - 1 )

 return
 end


