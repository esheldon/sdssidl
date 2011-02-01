;+
; NAME:
;	mom2gauss
;
; PURPOSE:
;	Take input Ixx,Ixy,Iyy and return an image with pixel values corresponding
;	to those values.
;
; CALLING SEQUENCE:
;	gauss=mom2gauss(Ixx, Ixy, Iyy, imsize, counts=, cen=, status=)
;
; INPUTS:
;	Ixx, Ixy, Iyy: Moments of the distribution. These could be the moments
;		returned by adaptive moments for example. Should be in pixels^2.
;	imsize: Size of the image in the x and y directions [sx, sy]
;
; OPTIONAL INPUTS:
;	counts:  Default is 1.0
;	cen: Default is (sx-1)/2 and (sy-1)/2
;
; OUTPUTS:
;	An image of size [sx,sy].
;
; MODIFICATION HISTORY:
;	Created 2010-02-13, Erin Sheldon, BNL
;
;-
function mom2gauss, ixx, ixy, iyy, imsize, $
               counts=counts, cen=cen, status=status

	status = 1
	if n_params() LT 4 then begin 
		print,'-Syntax gauss = mom2gauss(ixx, ixy, iyy, imsize, counts=, cen=)'
		print,'produce a gaussian image from the input ixx,ixy,iyy'
		on_error, 2
		message,'halting'
	endif

	det = ixx*iyy - ixy^2
	if det eq 0.0 then begin
		message,'Error:  determinant is zero'
	endif

	sx=long(imsize[0])
	sy=long(imsize[1])

    if n_elements(counts) eq 0 then counts=1.
	if n_elements(cen) eq 0 then begin
		cx = (sx-1.)/2.
		cy = (sy-1.)/2.
        cen = [cx,cy]
	endif else begin
		cx=cen[0]
		cy=cen[1]
	endelse


	; Compute the exponent of the gaussian at each point in the image.  
	; Use clever indexing technique

	index=lindgen(sx*sy)

	x=index MOD sx
	y=index/sx

	rr = (x-cx)^2*Iyy -2*(x-cx)*(y-cy)*Ixy + (y-cy)^2*Ixx

	rr = 0.5*rr/det


	; Compute the gaussian
	gauss=fltarr(sx,sy)
	w=where(rr lt 10.8, nw)      ; watch floating point underflow
                                 ; this will allow ~.00002 as smallest number
                                 ; in disk

	if nw gt 0 then gauss[index[w]]=exp(-rr[w])
 
	;; set the counts
	norm=total(gauss)
	if norm gt 0 then begin
		gauss = counts/total(gauss)*gauss
	endif
	return, gauss
end





