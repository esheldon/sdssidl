pro make_color_flag,cat,str,flag,psf=psf,petro=petro

;+
; NAME:
;	make_color_flag
;PURPOSE:
;	takes the str from search color_space
;	and sets the flag depending on whether the object
;	lies in that region of color space (and size-mag space)
;	flag values are anded together
;	they range from 0 to 15  (four different bits may be set
;	depending on whether that object is in each region)
;	objects in all four regions have flag=15
;
;CALLING SEQUENCE:
;	-syntax make_color_flag,cat,str,flag,
;	 psfcounts=psfcounts,petrocounts=petrocounts
;
;INPUTS:
;	cat : the sdss photo output structure
;	str : the structure output by search_color_space 
;	
;OUTPUTS:
;	flag : the flag arrays, each obejct has a nember 0 to 15	
;
;KEYWORDS:
;	psf : if set will use psfcounts rather than the
;		    default fibercounts
;	petro : if set will use petrocounts rather than the
;		    default fibercounts
;
;EXTERNAL CALLS:
;	none
;	
;METHOD:
;	just extract the value in each region of str.map
;	and sum them
;	see search_color_space for details
;	
;EXAMPLE
;
;NOTES
;
;HISTORY:  written by David Johnston -University of Chicago
;	Physics Department 1999
;-

if n_params() eq 0 then begin	
	print,'-syntax make_color_flag,cat,str,flag,psfcounts=psfcounts'
	return
endif

done=0					;get the magnitudes 
if keyword_set(psf) then begin
	u=cat.psfcounts(0)	
	g=cat.psfcounts(1)
	r=cat.psfcounts(2)
	i=cat.psfcounts(3)
	z=cat.psfcounts(4)
	done=1
endif 

if keyword_set(petro) then begin
	u=cat.petrocounts(0)	
	g=cat.petrocounts(1)
	r=cat.petrocounts(2)
	i=cat.petrocounts(3)
	z=cat.petrocounts(4)
	done=1
endif 

if done eq 0 then begin
	u=cat.fibercounts(0)
	g=cat.fibercounts(1)
	r=cat.fibercounts(2)
	i=cat.fibercounts(3)
	z=cat.fibercounts(4)
endif
					;now go through all
flag=intarr(n_elements(u))		;the regions and add
x=str.vx(0)*(str.x0(0)+str.x1(0)*(u-g)) ;up all the individual
y=str.vy(0)*(str.y0(0)+str.y1(0)*(g-r)) ;flags
in=str.map(round(x),round(y))
flag=flag+in

x=str.vx(0)*(str.x0(1)+str.x1(1)*(g-r))
y=str.vy(0)*(str.y0(1)+str.y1(1)*(r-i))
in=str.map(round(x),round(y))
flag=flag+in

x=str.vx(0)*(str.x0(2)+str.x1(2)*(r-i))
y=str.vy(0)*(str.y0(2)+str.y1(2)*(i-z))
in=str.map(round(x),round(y))
flag=flag+in
 
x=str.vx(0)*(str.x0(3)+str.x1(3)*r)
y=str.vy(0)*(str.y0(3)+str.y1(3)*cat.petrorad(2))
in=str.map(round(x),round(y))
flag=flag+in

return
end




