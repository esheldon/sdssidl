;+
; NAME:
;	admom
;
; PURPOSE:
;	Run adaptive moments on the input image.
;
; CATEGORY:
;	Image processing
;
; CALLING SEQUENCE:
;	admom, $
;		image, x, y, sky, skysig, wguess, $
;		ixx, iyy, ixy, momerr, rho4, whyflag, $
;		shiftmax=5.0, /recompile
;
; INPUTS:
;	** All inputs must be floating point ** or an error is raised.
;
;	image: The input image.  Must be two dimensional.
;	x,y: Locations in the image to measure adaptive moments.  Must be
;		floating point.
;	sky: Sky value, Either scalar or same len as x,y
;	skysig: Sky sigma value, Either scalar or same len as x,y
;	wguess: Guess at ixx,iyy moment.  Either scalar or same len as x,y
;
; OPTIONAL INPUTS:
;	shiftmax: Max allowed shift in centroid, Either scalar or same len as x,y
;		default is 5
;
; KEYWORD PARAMETERS:
;	/recompile:  Recompile the C code.
;
; OUTPUTS:
;	ixx,iyy,ixy: Moments of the object.
;	momerr:  Error on the ellipticity.
;	rho4: 4th order moment.
;	whyflag: Flags.  Should be zero for success.
;
;
; MODIFICATION HISTORY:
;	Added to sdssidl.  2010-02-17, Erin Sheldon, BNL
;
;-
pro _admom_compile, dll_path, name, extra_cflags, recompile=recompile

	name = 'admom'
	;extra_cflags = '-O3 -mtune=native'
	extra_cflags = '-O3'
	sdssidl_dir=getenv('SDSSIDL_DIR')
	if sdssidl_dir eq '' then message,'You must have SDSSIDL_DIR set'

	srcdir=filepath(root=sdssidl_dir,sub='src','admom')

	if keyword_set(recompile) then begin
		reuse_existing=0
	endif else begin
		reuse_existing=1
	endelse

	make_dll, name, name, $
		reuse_existing=reuse_existing, $
		input_directory=srcdir, $
		output_directory=srcdir, $
		compile_directory=srcdir, $
		dll_path=dll_path, /show_all_output, $
		extra_cflags=extra_cflags
end

pro admom_test_with_compile, recompile=recompile

	ixx=2.0
	ixy=0.5
	iyy=1.0
	im=mom2gauss(2.0,0.0,2.0,[20,20],counts=10000.)
	;im[*] += 5.5*randomn(seed,n_elements(im))
	x=10.0
	y=10.0
	sky=0.0
	skysig=5.5
	shiftmax=10.0
	wguess=2.0

	admom,$
		im,x,y,sky,skysig,wguess,$
		tixx,tiyy,tixy,tmomerr,trho4,twhyflag,$
		recompile=recompile

	print,'Comparing output to input: '
	print,'ixx: ',tixx,' (',ixx,')', f='(a,f0.3,a,f0.3,a)'
	print,'iyy: ',tiyy,' (',iyy,')', f='(a,f0.3,a,f0.3,a)'
	print,'ixy: ',tixy,' (',ixy,')', f='(a,f0.3,a,f0.3,a)'
end


function _admom_checkpar, par, nobj, parname

	npar = n_elements(par)
	if npar eq nobj then begin 
		return, par
	endif else if nsky eq 1 then begin 
		return, replicate(par, nobj)
	endif else begin 
		message,$
			parname+' must be either length 1 or same length as object list'
	endelse 

end

pro _admom_testfloat, par, parname
	if size(par,/type) ne 4 then begin
		message,parname+' must be type float'
	endif
end

pro admom, $
		image, x, y, sky, skysig, wguess, $
		ixx, iyy, ixy, momerr, rho4, whyflag, $
		shiftmax=shiftmax, $
		recompile=recompile

	tm0=systime(1)
	if n_params() lt 12 then begin
		on_error, 2
		print,'usage: admom, image, x, y, sky, skysig, wguess, '
		print,'              ixx, iyy, ixy, momerr, rho4, whyflag, '
		print,'              shiftmax=, /recompile'
		message,'halting'
	endif

	; default to 5 pixels allowed shift
	if n_elements(shiftmax) eq 0 then shiftmax=5.0

	; compile if not yet compiled.  Get dll path and entry name.
	_admom_compile, dll_path, name, extra_cflags, recompile=recompile


	if keyword_set(recompile) then begin
		unload=1
		ignore_existing_glue=1
	endif

	; make sure these are floating point
	_admom_testfloat, image, 'image'
	_admom_testfloat, x, 'x'
	_admom_testfloat, y, 'y'
	_admom_testfloat, sky, 'sky'
	_admom_testfloat, skysig, 'skysig'
	_admom_testfloat, shiftmax, 'shiftmax'
	_admom_testfloat, wguess, 'wguess'

	; make sure it is a 2D image
	sz=size(image)
	if sz[0] ne 2 then message,'Input image must be 2D'

	; get dimensions
	nx=sz[1]
	ny=sz[2]

	; make sure x,y are same length
	nobj = n_elements(x)
	if n_elements(y) ne nobj then begin
		message,'x and y must be same length'
	endif

	; these should either be scaler or same lenght
	; as x,y
	sky_input = _admom_checkpar(sky, nobj, 'sky')
	skysig_input = _admom_checkpar(skysig, nobj, 'skysig')
	shiftmax_input = _admom_checkpar(shiftmax, nobj, 'skysig')
	wguess_input = _admom_checkpar(wguess, nobj, 'wguess')



	; the outputs
	if nobj eq 1 then begin
		ixx = 0.0
	endif else begin
		ixx=fltarr(nobj)
	endelse
	iyy=ixx
	ixy=ixx
	momerr=ixx
	rho4=ixx
	whyflag=long(ixx)
	
	print,systime(1)-tm0

	tm0=systime(1)
	; ok, call admom
    res = call_external( $
        dll_path, name, $
		image,$
		nx, $
		ny, $
		nobj, $
		sky_input, $
		skysig_input, $
		shiftmax_input, $
		wguess_input, $
		x, $
		y, $
		ixx, $
		iyy, $
		ixy, $
		momerr, $
		rho4, $
		whyflag, $
        unload=unload,$
        ignore_existing_glue=ignore_existing_glue, $
        /auto_glue, $
        extra_cflags=extra_cflags,$
        /show_all_output, $
		value=[0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0])

	print,systime(1)-tm0
	; if failure, copy in the defval
	w=where(whyflag ne 0,nw)
	if nw ne 0 then begin
		defval = -9999.0
		ixx[w] = defval
		iyy[w] = defval
		ixy[w] = defval
		momerr[w] = -defval
		rho4[w] = defval
	endif

end
