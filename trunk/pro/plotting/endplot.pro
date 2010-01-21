;+
; NAME:
;	ENDPLOT	
;
; PURPOSE:
;	Closes and saves the current plotfile and returns to 'X'. See also BEGPLOT.
;
;
; CALLING SEQUENCE:
;   endplot, /trim_bbox, /landfix, /setup, /help
;
; KEYWORD PARAMETERS:
;   /landfix: 
;      fix the landscape so its not flipped (which is the only way IDL writes
;      landscape mode)
;   /trim_bbox: 
;      trim the bounding box on encapsulated ps files. Requires that the
;      external program "epstool" is installed.  This must be the later mature
;      version that accepts --bbox, etc as options.
;             http://www.cs.wisc.edu/~ghost/gsview/epstool.htm
;   /setup: 
;      run setupplot after closing device.  If !pslayout.runsetup is true then
;      this will be run automatically.
;   /help: 
;      print this doc
;
; COMMON BLOCKS:
;   PLOTFILE_COM:  See the documentation for BEGPLOT.
;
; EXAMPLE:
;       begplot, 'test.ps'
;       plot, .....
;       endplot
;
; MODIFICATION HISTORY: 
;  Originally based on ENDPLOT by Michael J. Carter.  Almost complete rewrite
;       Erin Sheldon, March 2001.
;
;-

pro endplot, setup=setup, landfix=landfix, $
		trim_bbox=trim_bbox, help=help, $
		png=png, dpi=dpi, $
		noprint=noprint    ;ignored

   
	COMMON plotfile_com, plotfile, save_device, spool
   
	if keyword_set(help) then begin
		doc_library, 'endplot'
		return
	ENDIF

	if n_elements(plotfile) eq 0 then plotfile = 'null'
	if n_elements(save_device) eq 0 then save_device = !d.name

	print, 'BEGPLOT: Closing current plot file ', plotfile, f='(a,a)'
	if !D.name ne 'X' then device, /close
	set_plot, save_device

	pslayout
	if keyword_set(setup) then begin 
		setupplot
	endif else begin 
		defsysv, '!pslayout', exist=exist
		if exist then begin 
			if !pslayout.runsetup then setupplot
		endif 
	endelse 

	if keyword_set(landfix) then begin 
		print,'BEGPLOT: Fixing landscape mode', f='(a,a)'
		pslandfix,plotfile
	endif 

	if keyword_set(trim_bbox) then begin 

		print,'BEGPLOT: Trimming the bounding box',f='(a)'
		tf = tmpfile(tmpdir='/tmp', prefix='endplot-tmpfile-',suffix='.ps')

		pfile = expand_path(plotfile)
		spawn,['epstool','--copy','--bbox','--quiet',pfile,tf],res,/noshell
		spawn,['mv','-f',tf,pfile],res,trim_err,/noshell

	endif 

	if keyword_set(png) then begin
		pfile = expand_path(plotfile)
		newfile=repstr(pfile,'.eps','.png')
		newfile=repstr(newfile,'.ps','.png')
		if n_elements(dpi) eq 0 then dpi=75
		dpi = strtrim(string(dpi),2)

		command = ['convert']
		if keyword_set(trim_bbox) then begin
			; only trim if the epstool call failed
			if trim_err ne '' then begin
				command=[command,'-trim']
			endif
		endif
		command = [command,'-antialias','-density',dpi,pfile,newfile]
		print,strjoin(command,' ')
		spawn,command,res,/noshell
	endif

	plotfile = 'null'

end
