
;+
; NAME: 
;    OBJ_INFO
;       
; PURPOSE: 
;    For a list of objects (index) make color-color diagrams, size
;    vs brightness plot, find its family, and make its own 
;    FINDING CHART.
;
; WARNING: write_gif will not work if plot window is minimized.  i.e. you
;          won't have good gifs when you are done if it is minimized.
;	
;
; CALLING SEQUENCE: obj_info, pstruct, index, clr, dir=dir, radius=radius, 
;        gif=gif, ps=ps, outdir=outdir,fchart=fchart, all=all, 
;        noprompt=noprompt, silent=silent, nodisplay=nodisplay,
;	 hideradec=hideradec
;                       
; INPUTS: pstruct: photo structure.  
;         IMPORTANT NOTE: This MUST have objc_rowc as a continuously added 
;             number, not restarted at the beginning of each frame.  Such
;             boundaries are artificial and make it difficult to make 
;             finding charts centered on the object.  
;             This is done automatically by READ_PHOTO_COL.
;
;         index:  The objects for which to get info.
;         clr: The color to use when making the fchart.
;
; INPUT KEYWORD PARAMETERS: 
;           dir:      The directory holding the atlas images.
;           radius:   The finding chart for each object will have sides
;                     2*radius unless radius is too big. (e.g. radius > 2048)
;           radarcsec: radius in arcseconds (takes precedence over radius)
;           radarcmin: radius in arcminutes (takes precedence over radarcsec)
;           gif:      Set gif keyword to print gifs of all images in cwd.
;           ps:       Set ps keyword to print postscripts of all images.
;           outdir:   Where to put the output files
;           all:      If all is set, then all objects (not just stars) 
;                       are used for the locus of the color-color plots
;           noprompt: If noprompt is set, then there is no prompting
;           silent:   If silent is set, there will be no messages except
;                        for some errors.
;	    hideradec: If set it will not include ra dec on anything
;		       for example if you are showing unpublished results
;		       at a conference and want to be descreet.
;           hist:   Will hand this to plot_colors to use 2D histograms
;                   instead of scatter plots. Good for large catalogs.
;           radrange:  range on petrorads to plot. handed to plot_colors
;
; OUTPUTS: Files:  (only output if /gif or /ps is used)
;
;     a) The following files are created in the cwd for each object:
;
;            Obj-run-rerun-colorcamcol-field-id-type.extension
;
;        examples:
;            Obj-000259-1-3-0248-0234-atlas.gif
;            Obj-000259-1-3-0248-0234-atlas.ps
;            Obj-000259-1-3-0248-0234-colors.gif
;            Obj-000259-1-3-0248-0234-colors.ps
;            Obj-000259-1-3-0248-0234-fchart.gif
;            Obj-000259-1-3-0248-0234-fchart.ps
;
;     b) If index contains more that one object then a color-color plot is
;        made with all of them in it.  It uses the entire pstruct
;        to make the locus
;	The file name is:
;            Obj-000259-1-3-0248-0234-colors-multi.gif
;	with the info refering to the FIRST object in the list
;
; OPTIONAL OUTPUTS: fchart:  The image of the fchart may be returned.
;
;           
; CALLED ROUTINES:  EXTRACT_STARS:
;                      MAKE_FLAG_STRUCT
;                      FLAG_SELECT
;                   PLOT_COLORS
;                   BEGPLOT
;                   ENDPLOT
;                   GET_FAMILY:
;                      GET_ATLAS (calls many pros)
;                   FCHART
;                   DISPLAY_FCHART
;                   
;
; SUBROUTINES: OBJ_PROMPT_USER, OBJ_DISPLAY_FCHART
;
; EXAMPLES:  This will work on sdss3 if typed exactly:
;   (Until we move the data)
;
; IDL> file='/sdss3/data4/run109/tsObj-000109-3-0-0011.fit'
; IDL> read_photo_col, file, pstruct, struct_typ='all', nframes=20
;
; IDL> dir='/sdss3/data4/run109/'
; IDL> r=2
; IDL> index = 1002
; IDL> obj_info, pstruct, index, r, dir=dir
; 
;    Lets say you have a program that selects quasars.  It returns the
;      indices of the quasars. Also, you want to generate all the ps and gif
;      files at once without any keyboard prompting:
;
; IDL> findqso, pstruct, index
; IDL> obj_info, pstruct, index, r, dir=dir, /noprompt, /ps, /gif
;
;   If you only want to make the ps files:
;
; IDL> obj_info, pstruct, index, r, dir=dir, /noprompt, /nodisplay, /ps 
;
; REVISION HISTORY:  
;      Author: Erin Scott Sheldon  UMich 03/21/99
;	       David Johnston - changed filename formats to what is described 
;                    above. Fixed some bugs with the 'sky' program 5/12/99
;              E.S.S.  Made obj_display_fchart in to stand alone proc.
;                    display_fchart.pro
;              E.S.S.:
;              Changed order in names/titles: run-rerun-camcol-field-id
;              Addec radarcsec keyword.
;	       Added outdir keyword 02-Jul-2002
;	
;
;
;-                                        
;
;
;
;  Copyright (C) 2005  Erin Sheldon, NYU.  erin dot sheldon at gmail dot com
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; SUBROUTINE:  obj_prompt_user     subroutine of obj_info.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO obj_prompt_user, noprompt, next, max=max

 next=1
 if keyword_set(max) then if (max eq 1 ) then return

 if (not noprompt) then begin
    print,'-----------------------------'
    print,'Hit any key.  (q to quit)'
    print,'-----------------------------'
    key = get_kbrd(20)
    if (key eq 'q') then next=0
 endif

return
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; MAIN PROCEDURE
;
; SUBROUTINES  obj_prompt_user
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO  obj_info, pstruct, index, clr, $
               dir=dir, radius=radius, radarcsec=radarcsec, radarcmin=radarcmin,$
               fchart=fchart, objx=objx, objy=objy, $
               gif=gif, ps=ps, box=box, all=all, $
               outdir=outdir,$
               noprompt=noprompt, nodisplay=nodisplay, silent=silent, $
               nocolor=nocolor,nofamily=nofamily,nofchart=nofchart, $
               hideradec=hideradec, hist=hist,radrange=radrange,_extra=_extra

  if N_params() LT 3 then begin
     print,'-Syntax: obj_info, pstruct, index, clr, '
     print,'   dir=dir, radius=radius, '
     print,'   fchart=fchart, objx=objx, objy=objy,'
     print,'   gif=gif, ps=ps, box=box, all=all,'
     print,'   noprompt=noprompt, nodisplay=nodisplay, silent=silent'
     print,'   nocolor=nocolor, nofamily=nofamily, nofchart=nofchart,'
     print,'   hideradec=hideradec, hist=hist,radrange=radrange,_extra=_extra'
     print,''
     print,'Use  doc_library,"obj_info"  for help.'
	return
  endif

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Check keywords; 
; Set some parameters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF (NOT keyword_set(gif) ) THEN gif=0
  IF (NOT keyword_set(ps) ) THEN ps=0 
  IF (NOT keyword_set(noprompt)) THEN noprompt=0
  IF (NOT keyword_set(silent)) THEN silent = 0
  IF (NOT keyword_set(radius)) THEN radius = 300 ;; radius of fchart in pixels
  IF n_elements(radarcsec) NE 0 THEN radius = radarcsec/0.4
  IF n_elements(radarcmin) NE 0 THEN radius = radarcmin*60.0/0.4
  IF (NOT keyword_set(nodisplay)) THEN nodisplay = 0
  IF (NOT keyword_set(nofchart) ) THEN nofchart = 0
  IF (NOT keyword_set(nocolor) ) THEN nocolor = 0
  IF (NOT keyword_set(nofamily) ) THEN nofamily = 0
  IF (NOT keyword_set(hideradec) ) THEN hideradec = 0
  
  IF n_elements(outdir) EQ 0 THEN outdir=''

  IF (nodisplay AND gif) THEN BEGIN
      print,'Making a gif requires display.'
      return
  ENDIF 

  IF nodisplay THEN noprompt = 1

  if (clr lt 0 or clr gt 4) then begin
      print,'Color index must be in [0,4]'  ;;; check clr input
      return
  endif

  tags = tag_names(pstruct)
  run = strtrim(string(pstruct[0].run+1000000), 2)     ;; Used in the titles
  run=strmid(run,1,6)
	;now run has the required amout of zeros to match the tsObj 
	;file title

  ;; Old catalogs may not have rerun
  wrer = where(tags EQ 'RERUN', nwrer)
  IF nwrer NE 0 THEN BEGIN
      rerun='-'+strtrim(string(pstruct[0].(wrer[0])), 2)
  ENDIF ELSE BEGIN
      rerun = ''
  ENDELSE 
  camcol = strtrim(string(pstruct[0].camcol),2)
  colors=['u','g','r','i','z']

  showf = 10                  ;;; number of frames on each side of object
                                                     ;;;; to use for locus   
  oplot_str = pstruct[index]  ;; overplot indexed objects in color-color
      
  ntot = n_elements(pstruct)
  fmax = max(pstruct.field)
  fmin = min(pstruct.field)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Determine what part of pstruct to use for the locus
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  if keyword_set(all) then begin  ;; if all, then use everything for locus
      locus = lindgen( n_elements(pstruct) )
  endif else begin
      ;;;; stars are to be determined by their red flags  ;;;;;
      colorindex=2
      extract_stars, pstruct, colorindex, locus, /silent
  endelse
     
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Make a filled diamond symbol.
  ; To use it, set psym=8
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  frac = 1.0
  x = [-frac,0,frac,0,-frac]
  y = [0,frac,0,-frac,0]
  usersym,x,y,/fill

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Define the size of certain symbols
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  gifsymsize=1.0
  pssymsize=.7
;  psym=8
;  psym=4

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; If more that one object in index, make color-color 
  ; plots for whole locus and the objects.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;  loadct, 0, /silent
  max=n_elements(index)

  IF (NOT nocolor) THEN BEGIN 

      IF (not silent) THEN print,'-----------------------------'
      IF (max ne 1) THEN BEGIN

          objfield=pstruct(index(0)).field
          field = strtrim(string(objfield+10000), 2)
          field=strmid(field,1,4)
          id = strtrim(string(pstruct(index(0)).id+10000), 2)
          id=strmid(id,1,4)
          allname = outdir + 'Obj-'+run+rerun+'-'+camcol+'-'+field+'-'+id

          allps = allname + '-colors-multi.ps'
          allgif = allname + '-colors-multi.gif'

          IF (NOT nodisplay) THEN BEGIN
              if (not silent) then $
                print,'Making color-color plot for all input objects'
              plot_colors, pstruct(locus), oplot_str=oplot_str, /size, $
                symsize=gifsymsize,title=allname,hist=hist,radrange=radrange
              if (gif) then begin
                  WSHOW, 0, ICONIC=0 ;make sure it is showing
                  write_gif, allgif, tvrd()
              endif
          ENDIF
          IF (ps) then begin
              print,'-----------------------------'
              begplot, name=allps
              plot_colors, pstruct(locus), oplot_str=oplot_str, /size, $
                symsize=pssymsize,title=allname,hist=hist,radrange=radrange
              endplot,/noprint
              print,'-----------------------------'
          ENDIF
      ENDIF 
  ENDIF                         ; end (not nocolor)
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Now get info for each object in index
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  FOR i=0, max-1 DO BEGIN
     
      next=0

      IF (NOT nocolor OR i NE 0) THEN BEGIN 
          obj_prompt_user, noprompt, next, max=max
      ENDIF ELSE next = 1
     
     
      if (not next) then return

      jj=index[i]
      object=pstruct[jj]

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ; Build of the names of various output files
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      objfield=object.field
      field = strtrim(string(objfield+10000), 2)
      field=strmid(field,1,4)	
      id = strtrim(string(object.id+10000), 2)
      id=strmid(id,1,4)
      name = outdir + 'Obj-'+run+rerun+'-'+camcol+'-'+field+'-'+id
      colps = name + '-colors.ps'
      colgif = name + '-colors.gif'
      atlasps = name + '-atlas.ps'
      atlasgif = name + '-atlas.gif'
      fchartps   = name + '-fchart.ps'
      fchartgif   = name + '-fchart.gif'
     
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;  Pick out stars within (objfield+showf) and (objfield-showf) for 
      ;  stellar locus.  If we have fewer that (2*showf + 1) fields  or we 
      ;  are closer to the first or last field than showf, the we must
      ;  truncate the range or shift it respectively.
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      IF (NOT nocolor) THEN BEGIN

                            ;;;; First the case of not enough fields 
          if ( (fmax-fmin) lt (2*showf +1) ) then begin
              w=lindgen(ntot)
                            ;;;; Now check for edge effects
          endif else begin
       
              fdiff1 = objfield - fmin
              fdiff2 = fmax - objfield
              if (fdiff1 lt showf) then begin
                  grabmin = objfield - fdiff1
                  grabmax = objfield + showf + (showf - fdiff1)
              endif else if (fdiff2 lt showf) then begin
                  grabmax = objfield + fdiff2
                  grabmin = objfield - showf - (showf - fdiff2)
              endif else begin
                  grabmax = objfield + showf
                  grabmin = objfield - showf
              endelse
              w = where(pstruct(locus).field ge grabmin $
                        and pstruct(locus).field le grabmax)
          endelse

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ; Make the color-color plots and size vs brightness plot
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

          if (not silent) then print,'Plotting Colors For Object id: ',id

;         loadct,0,/silent
          IF (NOT nodisplay) THEN BEGIN
              plot_colors, pstruct(locus[w]), oplot_str=object, /size,$
                symsize=gifsymsize,title=name,hist=hist,radrange=radrange
              if (gif) then begin
                  WSHOW, 0, ICONIC=0 ;make sure it is showing
                  write_gif, colgif, tvrd()
              endif	
          ENDIF 
          if (ps) then begin
              print,'-----------------------------'
              begplot, name=colps
              plot_colors, pstruct(locus[w]), oplot_str=object, /size, $
                psymsize=pssymsize,title=name,hist=hist,radrange=radrange
              endplot,/noprint
              print,'-----------------------------'
          endif
          obj_prompt_user, noprompt, next
          if (not next) then return
      ENDIF ;; end of (not nocolor)
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ; Find the object's family and make the plots of the atlas images.
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
      IF (NOT nofamily) THEN BEGIN
          if (not silent) then print,'Getting Family For Object id: ',id

;         loadct,0,/silent
          IF (NOT nodisplay) THEN BEGIN 
              get_family, pstruct, jj, dir=dir,silent=silent,$
                hideradec=hideradec, _extra=_extra
              if (gif) then begin
                  WSHOW, 0, ICONIC=0 ;make sure it is showing
                  write_gif, atlasgif, tvrd()
              endif
          ENDIF 
          if (ps) then begin
              print,'-----------------------------'
              begplot, name=atlasps
              get_family, pstruct, jj, dir=dir,/silent,hideradec=hideradec, $
                _extra=_extra
              endplot,/noprint
              print,'-----------------------------'
          endif

          IF (NOT nofchart ) THEN BEGIN 
              obj_prompt_user, noprompt, next
              if (not next) then return
          ENDIF

      ENDIF  ;;; end of (not nofamily)

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;; create the finding fchart
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      IF (NOT nofchart) THEN BEGIN 
          fchart, pstruct, jj, radius, clr, fchart, dir=dir, $
            objx=objx, objy=objy, impos=impos, silent=silent
	
          display_fchart, fchart, object, objx, objy, clr, $
             box=box, gif=gif, ps=ps, fnameps=fchartps, $
             fnamegif=fchartgif, silent=silent, nodisplay=nodisplay, $
             hideradec=hideradec, _extra=_extra
      ENDIF 

  ENDFOR 

  return
end






















