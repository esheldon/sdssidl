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
;		returned by adaptive moments for example.
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
	if n_params() LT 2 then begin 
		print,'-Syntax gauss = mom2gauss(ixx, ixy, iyy, nx,ny, counts=, cen=)'
		print,'produce a gaussian image from the input ixx,ixy,iyy'
		return, -1
	endif

	det = ixx*iyy - ixy^2
	if det eq 0 then begin
		print,'Error:  determinant is zero.  Returning nothing'
		return, -1
	endif

	sx=float(imsize[0])
	sy=float(imsize[1])

    if n_elements(counts) eq 0 then counts=1.
	if n_elements(cen) eq 0 then begin
		cx = (sx-1.)/2.
		cy = (sy-1.)/2.
	endif else begin
		cx=cen[0]
		cy=cen[1]
	endelse


	;; Compute the exponent of the gaussian at each point in
	;; the image.  Use clever indexing technique

	index=lindgen(sx*sy)

	x=index MOD sx
	y=index/sx

	rr = (x-cx)^2*Iyy -2*(x-cx)*(y-cy)*Ixy + (y-cy)^2*Ixx

	rr = 0.5*rr/det

    ;aratio=aratio > .1
    ;ct=cos(theta*!Pi/180.)
    ;st=sin(theta*!Pi/180.)
    ;xp=(x-cx)*ct + (y-cy)*st
    ;yp=((cx-x)*st + (y-cy)*ct)/aratio


	;; The exponent
	;rr=(1.0/2.0/sigma^2)*(xp^2+yp^2)

	;; Compute the gaussian

	gauss=fltarr(sx,sy)
	w=where(rr lt 10.8, nw)      ; watch floating point underflow
                                 ; this will allow ~.00002 as smallest number
                                 ; in disk

  IF nw GT 0 THEN gauss[index[w]]=exp(-rr[w])
 
  ;; set the counts
  gauss = counts/total(gauss)*gauss
  return, gauss
END





