pro search_color_space,cat,str,qso=qso,$
all=all,psf=psf,petro=petro,qfile=qfile,hist=hist,$
radrange=radrange,_extra=ex

;+
; NAME:
;	search_color_space
;PURPOSE:
;	For taking an sdss catalog and selecting objects of desired	
;	colors,magnitude and size. Does this by plotting four plots.
;	u-g versus g-r
;	g-r versus r-i
;	r-i versus i-z
;	r versus petrorad(in r color) 
;	Then it prompts you to draw a closed curve on all four plots.
;	Then it makes a structure with this information stored in it.	
;	The structure and a photo catalog can be handed to make_color_flag
;	which will then flag each objects with a number 0 to 15
;	ie. it will set each of four bits depending on whether the object
;	was in each one of the four regions selected. So an object with
;	flag =15 was inside of all closed curves drawn.
;	This structure can be saved as a binary table and used later for doing
;	NON-interactive selections. For example one could define the stellar
;	locus once and save it to use again another day.
;	
;
;CALLING SEQUENCE:
;	-syntax search_color_space,cat,str,qso=qso,
;	        all=all,psf=psf,petro=petro,qsofile=qsofile,
;	        hist=hist,radrange=radrange
;
;INPUTS:
;	cat : the sdss photo-output structure
;	
;OUTPUTS:
;	str : the structure containing your selected regions
;	      and the transformation parameters to map
;	      the data onto this array. This is used by 
;	      make_color_flag.				
;
;KEYWORDS:
;	all : if set will use all objects not just stars
;	psf : if set will use psfcounts rather than fibercounts
;	petro : if set will use petrocounts rather than fobercounts
;	qso : if set will over plot known quasars to aid selection
;		of quasars	
;	qfile : fits binary file of a certain format
;		will try default file if this is not given
;		quasars from (G. Richards and D. Schneider)
;	hist : if set it will use 2D histograms instead of scatter plots
;		 (see  "ploth" procedure)
;	radrange : the range to use on petrorad default=[0,6]
;	_extra=ex : extra keywords handed to ploth or plot
;
;EXTERNAL CALLS:
;	extract_stars : called if all is NOT set
;	ploth : if hist is set
;	mrdfits : if overplotting quasars
;	exist
;	
;METHOD:
;	uses IDL function defroi (Define Region Of Interest)
;	to make an array having four regions
;	the first set to 2^0=1 the second set to 2^1=2 the third 2^2=4
;	and the fourth 2^3=8. All other are set to 0.
;	The parameters for the linear transformation
;	from data to device coordinated are all saved with this array in 
;	the structure str. So later one can map a star or galaxy onto this 
;	array and read its pixlel. It will map onto four pixles 
;	since it has four pairs of data points and these can be summed
;	to give it a flag 0 to 15 indicating uniquely whether it is in 
;	each of the regions of interest. Most often people will
;	use this with "select_colors" wrapper which just outputs the 
;	index of objects flagging to 15 (ie. they are in all regions) 
;	but this program is slightly more general.	
;	
;EXAMPLE
;	see select_colors
;NOTES
;DO NOT CHANGE THE WINDOW SIZE
;it must remain [640,640] for defroi to work
;
;HISTORY:  written by David Johnston -University of Chicago
;	Physics Department 1999
;-

	
if n_params() eq 0 then begin
	print,'-syntax  search_color_space,cat,str,qso=qso,'
	print,'all=all,psf=psf,petro=petro,qfile=qfile,'
	print,'hist=hist,radrange=radrange'
	return
endif

if keyword_set(hist) then hist=1 else hist=0

window,0,xsize=640,ysize=640
;window must be this size
;do not change

!p.multi=[0,2,2]
arr=fltarr(4)

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

ug=u-g
gr=g-r
ri=r-i
iz=i-z

str={vx:arr,x0:arr,x1:arr,vy:arr,y0:arr,y1:arr,map:intarr(640,640)}
  
if not keyword_set(all) then begin
	extract_stars,cat,2,indices=s,/silent
endif else s=lindgen(n_elements(u))
	

if keyword_set(qso) then begin
			;we want to overplot the known quasars
			;presumably to select similarly colored things
	
	if n_elements(qfile) eq 0 then begin
		qfile='/sdss3/data3/masterQSOlist-matched.fits'
			;the default file
	endif
	if exist(qfile) then begin
		qsolist=mrdfits(qsofile,1,hdr)
		uqso=qsolist.sdssmagu
		gqso=qsolist.sdssmagg
		rqso=qsolist.sdssmagr
		iqso=qsolist.sdssmagi
		zqso=qsolist.sdssmagz
	endif else begin
		print,'quasar file  ',qfile
		print,'does not exist'
		qso=0   	;unset the keyword 
	endelse
endif

				;plot u-g   g-r
if hist then begin
	ploth,ug(s),gr(s),psym=3,xrange=[-1,4],yrange=[-1,3],$
	xtitle='u-g',ytitle='g-r',_extra=ex
endif else begin
	plot,ug(s),gr(s),psym=3,xrange=[-1,4],yrange=[-1,3],$
	xtitle='u-g',ytitle='g-r',_extra=ex
endelse			

str.vx(0)=!d.x_size     ;fill in these tranformation parameters
str.vy(0)=!d.y_size     ;basically these parameters convert
str.x0(0)=!x.s(0)       ;coordinates data -> device
str.x1(0)=!x.s(1)	;convert_coord is not good enough
str.y0(0)=!y.s(0)	;since this structure may be saved
str.y1(0)=!y.s(1)	;and used next week whereas convert_coord
			;only uses temporary information
			;that is lost when the next plot is made


if keyword_set(qso) then oplot,uqso-gqso,gqso-rqso,psym=1

if hist then begin
	ploth,gr(s), ri(s),psym=3,xrange=[-1,4],yrange=[-1,3],$
	xtitle='g-r',ytitle='r-i',_extra=ex
endif else begin
	plot,gr(s), ri(s),psym=3,xrange=[-1,4],yrange=[-1,3],$
	xtitle='g-r',ytitle='r-i',_extra=ex
endelse

str.vx(1)=!d.x_size
str.vy(1)=!d.y_size
str.x0(1)=!x.s(0)
str.x1(1)=!x.s(1)
str.y0(1)=!y.s(0)
str.y1(1)=!y.s(1)
if keyword_set(qso) then oplot,gqso-rqso,rqso-iqso,psym=1 

if hist then begin
	ploth,ri(s),iz(s),psym=3,xrange=[-1,4],yrange=[-1,3],$
	xtitle='r-i',ytitle='i-z',_extra=ex
endif else begin
	plot,ri(s),iz(s),psym=3,xrange=[-1,4],yrange=[-1,3],$
	xtitle='r-i',ytitle='i-z',_extra=ex
endelse

str.vx(2)=!d.x_size
str.vy(2)=!d.y_size  
str.x0(2)=!x.s(0)
str.x1(2)=!x.s(1)
str.y0(2)=!y.s(0)
str.y1(2)=!y.s(1)
if keyword_set(qso) then oplot,rqso-iqso,iqso-zqso,psym=1

if n_elements(radrange) eq 0 then radrange=[0,6]
if hist then begin
	ploth,r(s),cat(s).petrorad(2),psym=3,xrange=[10,25],$
	xtitle='fibercounts(2)',ytitle='petrorad(2)',yrange=radrange $
	,_extra=ex
endif else begin	
	plot,r(s),cat(s).petrorad(2),psym=3,xrange=[10,25],$
	xtitle='fibercounts(2)',ytitle='petrorad(2)',yrange=radrange $
	,_extra=ex
endelse

str.vx(3)=!d.x_size
str.vy(3)=!d.y_size
str.x0(3)=!x.s(0)
str.x1(3)=!x.s(1)
str.y0(3)=!y.s(0)
str.y1(3)=!y.s(1)

a1=defroi(640,640)		;use the IDL function defroi
a2=defroi(640,640)		;Define Region Of Interest
a3=defroi(640,640)		;to do just that,	
a4=defroi(640,640)		;will prompt you to draw closed 
b=intarr(640,640)		;curves in each of the four quadrants

b(a1)=b(a1)+1			;set bit 0
b(a2)=b(a2)+2			;set next bit
b(a3)=b(a3)+4			;set next bit
b(a4)=b(a4)+8			;set next bit

str.map=b			;the output structure
				;now has the map
				;and the transfomation
				;parameters

			;defroi requires a window
			;no bigger than 640,640
			;don't ask me why

if (!d.x_size ne 640) or (!d.y_size ne 640) then begin
	print,'YOU CHANGED THE WINDOW SIZE'
	print,'SEARCH_COLOR_SPACE REQUIRES THAT THE'
	print,'WINDOW STAY 640,640'
 	str=-1
	return
endif 
	
return
end




