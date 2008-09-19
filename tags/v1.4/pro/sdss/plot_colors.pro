;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME:  
;    PLOT_COLORS
;       
; PURPOSE:  
;       make color-color plots from sloan colors.  Plot one set of data
;	from obj_struct.  Over plot, if requested, from oplot_str.
;
;	DEFAULTS are COUNTS_MODEL for colors, PETRORAD vs. PETROCOUNTS for
;       size vs mag plots.  PLOT_COLORS will search for other arrays if
;       these are not present.  If the keywords /fibercounts or /psfcounts
;       are given, then these arrays are used (if present) for both
;       colors and magnitude.
;
;       NOTE: For stars, COUNTS_MODEL is closer to psfcounts than petrocounts or 
;             fibercounts.  Also, COUNTS_MODEL give the best colors for galaxies.
;             Thus, it is best for showing both stars and galaxies on the same 
;             color-color diagrams. That is why it is the default. If you are
;             just plotting stars and quasars, then PSFCOUNTS may be the way
;             to go; set the /psfcounts keyword.
;
; CALLING SEQUENCE:  
;     plot_colors, obj_struct, oplot_str=oplot_str,index=index, size=size, 
;            carr=carr,opcarr=opcarr, psfcounts=psfcounts, fibercounts=fibercounts, 
;            psym=psym,title=title,hist=hist,radrange=radrange,
;            oplot_clr=oplot_clr, _extra=_extra
;
; INPUTS: obj_struct:  photo objc structure.  
;
; OPTIONAL INPUTS:
;     index:       indexes on obj_struct to use in plotting
;     oplot_str:   struct to overplot
;     oplot_clr: The color of the overplotted symbols.  If this is set, 
;               the procedure simpctable is called, which sets certain 
;               system variables (the value of these depends on the device and
;               if the device is X, the number of colors available):
;
;              !black
;              !white
;              !red
;              !green
;              !blue
;              !yellow
;              !cyan or !lightblue
;              !magenta
;              
;            Therefore, if you don't want to remember the numbers, just
;            call simpctable before calling plot_colors, and use
;                         oplot_clr=!magenta  
;            or whatever. You can get back to black and white with loadct,0
;            Note that the background will always be set to black and the 
;            axes white, and these are reversed when you send to postscript.
;            WARNING: doesn't work well with the /hist keyword
;
;     radrange:  the range on petrorad to plot
;     psym: The over plotting symbol.  Default is a cross-hair.
;     title=title: title for upper left hand plot (g-r) vs (u-g)
;     _extra=_extra   any extra plotting stuff. 
;
; INPUT KEYWORD PARAMETERS:
;     /size: if set, oplot_colors also plots size v 
;            brightness.  DEFAULT is PETROCOUNTS vs. PETRORAD
;     /psfcounts:   If set, use psfcounts for colors and magnitude
;     /fibercounts: if set, use fibercounts for colors and magnitude
;           NOTE: Defaults are 1. COUNTS_MODEL for colors
;                              2. PETROCOUNTS for magnitude if /size
;     /hist:  use 2D histograms instead of scatter plots , displays them 
;              with tvim2
;
; OPTIONAL OUTPUT ARRAYS:  
;    carr:  (optional) output struct containing colors for obj_struct 
;    opcarr:  (optional) output struct containing colors for oplot_str
;
; 
; PROCEDURE: 
;	
;	
;
; REVISION HISTORY:  Tim Mckay UM
;		     Erin Sheldon UM  2/6/99  added over-plotting, comments
;		     Dave Johnston UC 5/15/99 made DEFAULT over-plot symbol 
;                             to be crosshairs
;		     Dave Johnston 7/23/99 added 2D histogram option instead of
;				scatter plots. This is good for speed and also
;				appearance. Added radrange as a keyword
;				this is the range in petrorad to plot.		
;                    Erin Sheldon Made plots square
;                    Erin Sheldon 05/04/00 Added color overplotting. Improved
;                         error handling by checking for existence of 
;                         magnitude arrays and size arrays with subroutine
;                         PLOT_COLORS_CHECK_TAGS. Improved memory
;                         usage.
;                                      
;-                                        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


PRO plot_colors_check_tags, obj_tags, wcolor, wmag, wsize, size=size, $
                            colstring=colstring,addstr=addstr

  IF n_elements(addstr) EQ 0 THEN addstr=''
  check = 0

  IF n_elements(colstring) NE 0 THEN BEGIN

      wcolor_tmp = where( obj_tags EQ colstring[0], ncolor)
      IF ncolor EQ 0 THEN BEGIN 
          print,'No '+addstr+'tag '+colstring
      ENDIF ELSE BEGIN
          print,'Using PSFCOUNTS for '+addstr+'colors'
          wcolor = wcolor_tmp
          check = 1
      ENDELSE 

  ENDIF 

  IF n_elements(wcolor) EQ 0 THEN BEGIN ;will go here if colstring not found

      wcolor = where( obj_tags EQ 'COUNTS_MODEL', nmod)
      IF nmod EQ 0 THEN BEGIN 
          wcolor = where( obj_tags EQ 'FIBERCOUNTS', nfib)
          IF nfib EQ 0 THEN BEGIN
              wcolor = where( obj_tags EQ 'PETROCOUNTS', npet)
              IF npet EQ 0 THEN BEGIN 
                  wcolor = where( obj_tags EQ 'PSFCOUNTS', npsf)
                  IF npsf EQ 0 THEN BEGIN 
                      print,'No '+addstr+' magnitude tags found. Searched for COUNTS_MODEL ',$
                        'FIBERCOUNTS PETROCOUNTS AND PSFCOUNTS'
                      wcolor = -1
                      return
                  ENDIF ELSE print,'Using PSFCOUNTS for '+addstr+'colors'
              ENDIF ELSE print,'Using PETROCOUNTS for '+addstr+'colors'
          ENDIF ELSE print,'Using FIBERCOUNTS for '+addstr+'colors'
      ENDIF ELSE print,'Using COUNTS_MODEL for '+addstr+'colors'
      

  ENDIF 
      
  IF keyword_set(size) THEN BEGIN 

      IF check THEN BEGIN 
          wmag = wcolor
      ENDIF ELSE BEGIN 
          wmag = where(obj_tags EQ 'PETROCOUNTS',npet)
          IF npet EQ 0 THEN BEGIN 
              wmag = wcolor
          ENDIF 
      ENDELSE 
      print,'Using '+obj_tags[wmag]+' '+addstr+'for magnitude'

  ENDIF 

  IF keyword_set(size) THEN BEGIN 

      wsize = where(obj_tags EQ 'PETRORAD', nrad)
      IF nrad EQ 0 THEN BEGIN 
          wsize = where(obj_tags EQ 'PETROR90', nrad90)
          IF nrad90 EQ 0 THEN BEGIN 
              wsize = where(obj_tags EQ 'PETR0R50', nrad50)
              IF nrad50 EQ 0 THEN BEGIN
                  print,'No '+addstr+' size arrays found. Searched PETRORAD, ',$
                    'PETRO50 AND PETRO90'
                  size=0
              ENDIF ELSE print,'Using PETROR50 for '+addstr+'size'
          ENDIF  ELSE print,'Using PETROR90 for '+addstr+'size'
      ENDIF  ELSE print,'Using PETRORAD for '+addstr+'size'

  ENDIF 

  return
END 

PRO plot_colors, obj_struct, oplot_str=oplot_str,index=index, size=size, $
                 carr=carr, opcarr=opcarr, $
                 psfcounts=psfcounts, fibercounts=fibercounts,$
                 psym=psym,title=title,hist=hist,$
                 radrange=radrange,oplot_clr=oplot_clr, $
                 _extra=_extra

  IF N_params() LT 1 THEN BEGIN 
      print,'plot_colors, obj_struct, oplot_str=oplot_str,index=index, size=size, carr=carr,opcarr=opcarr, psfcounts=psfcounts, fibercounts=fibercounts, _extra=_extra,psym=psym,title=title,hist=hist,radrange=radrange,oplot_clr=oplot_clr, _extra=_extra'
      return
  ENDIF 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Some parameters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  nobj = n_elements(obj_struct)
  IF n_elements(index) NE 0 THEN BEGIN 
      nindex = n_elements(index)
      IF nindex LE nobj THEN BEGIN
          s=index 
      ENDIF ELSE BEGIN
          print,'input index array is too large'
          return
      ENDELSE 
  ENDIF ELSE BEGIN 
      s=lindgen(nobj)
  ENDELSE 

  IF keyword_set(psfcounts) AND keyword_set(fibercounts) THEN BEGIN 
      print,'Conflicting keywords petrocounts and fibercounts'
      return
  ENDIF 
  IF keyword_set(psfcounts) THEN colstring = 'PSFCOUNTS'
  IF keyword_set(fibercounts) THEN colstring='FIBERCOUNTS'

  if keyword_set(hist) then hist=1 else hist=0
  if n_elements(radrange) eq 0 then radrange=[0,6]

  aspect = 1.
  nostr = n_elements(oplot_str)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; save old versions of system variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  backold = !p.background
  p_old=!p.multi
  psymold = !p.psym
  xstyleold = !x.style
  ystyleold = !y.style

  !p.psym = 3
  !x.style = 1
  !y.style = 1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Check for tags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  obj_tags = tag_names(obj_struct)

  plot_colors_check_tags, obj_tags, wcolor, wmag, wsize, size=size, colstring=colstring
  IF wcolor[0] EQ -1 THEN return

  IF nostr NE 0 THEN BEGIN 
      print
      IF keyword_set(size) THEN oplot_size = size
      oplot_tags = tag_names(oplot_str)
      plot_colors_check_tags, oplot_tags, wcolor_oplot, wmag_oplot, wsize_oplot, $
        size=oplot_size, colstring=colstring, addstr='overplot structure '
      
      ;; check if found oplot tags
      IF wcolor_oplot[0] EQ -1 THEN nostr = 0

  ENDIF 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Set up special psym which works well for few objects.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF NOT keyword_set(psym) THEN BEGIN 
      ;;oplot symbol is crosses big enough to go edge to edge
      if nostr eq 0 then ss=1
      if nostr eq 1 then ss=100
      if nostr gt 1 then ss=9
      if nostr gt 10 then ss=4 
      
      ;;if there are many objects then you don't want lines edge
      ;;to edge because it would be too messy. so just regular
      ;;sized crosses	
      ;;big enough to go all the way to the edge otherwise
	
      usersym,[-1,1,0,0,0]*ss,[0,0,0,1,-1]*ss
      psym = 8
  ENDIF

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; setup color table 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF keyword_set(oplot_clr) AND (nostr NE 0) THEN BEGIN 
      simpctable
      clruse = oplot_clr

      IF (!d.flags AND 1) THEN BEGIN
          regcolor = !black
          !p.background = !white
      ENDIF ELSE BEGIN 
          regcolor = !white
          !p.background = !black
      ENDELSE 
  ENDIF
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;   define colors and output structures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  carr=findgen(4,n_elements(s))

  carr[0,*]=obj_struct[s].(wcolor[0])[0]-obj_struct[s].(wcolor[0])[1]
  carr[1,*]=obj_struct[s].(wcolor[0])[1]-obj_struct[s].(wcolor[0])[2]
  carr[2,*]=obj_struct[s].(wcolor[0])[2]-obj_struct[s].(wcolor[0])[3]
  carr[3,*]=obj_struct[s].(wcolor[0])[3]-obj_struct[s].(wcolor[0])[4]

  if nostr NE 0 then begin
      ;; Plotting procedures require arrays  ;;;;;
      opcarr=fltarr(4,nostr)
      opcarr[0,*] = oplot_str.(wcolor_oplot[0])[0]-oplot_str.(wcolor_oplot[0])[1]
      opcarr[1,*] = oplot_str.(wcolor_oplot[0])[1]-oplot_str.(wcolor_oplot[0])[2]
      opcarr[2,*] = oplot_str.(wcolor_oplot[0])[2]-oplot_str.(wcolor_oplot[0])[3]
      opcarr[3,*] = oplot_str.(wcolor_oplot[0])[3]-oplot_str.(wcolor_oplot[0])[4]
  endif
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;   make plots
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  !p.multi=[0,2,2]
	
  ;;;;;;;;;;;;;;;;;;;;;
  ;; g-r vs. u-g
  ;;;;;;;;;;;;;;;;;;;;;

  pytitle='g-r'	
  pxtitle='u-g'
  xrange = [-1., 4.]
  yrange = [-1., 3.]

  if hist then begin
      ploth, carr[0,*], carr[1,*], xrange=xrange, yrange=yrange, $
        title=title, xtitle=pxtitle, ytitle=pytitle,/silent, color=regcolor, _extra=_extra
  endif else begin
      aplot, aspect, carr[0,*], carr[1,*], $
        xrange=xrange, yrange=yrange,xtitle=pxtitle,ytitle=pytitle,$
        title=title, color=regcolor, background=background, _extra=_extra
  endelse	
  if nostr NE 0 then begin
      oplot,opcarr[0,*],opcarr[1,*],psym=psym,color=clruse, _extra=_extra
  endif
  
  ;;;;;;;;;;;;;;;;;;;;;
  ;;  r-i vs. g-r
  ;;;;;;;;;;;;;;;;;;;;;

  pxtitle='g-r'	
  pytitle='r-i'
  xrange = [-1., 3.]
  yrange = [-1., 3.]

  if hist then begin
      ploth,carr[1,*],carr[2,*],xrange=xrange,yrange=yrange,$
        xtitle=pxtitle,ytitle=pytitle ,/silent, color=regcolor, _extra=_extra
  endif else begin
      aplot, aspect, carr[1,*],carr[2,*],xrange=xrange,yrange=yrange,$
        color=regcolor,xtitle=pxtitle,ytitle=pytitle, $
        background=background, _extra=_extra
  endelse
  if nostr NE 0 then begin
      oplot,opcarr[1,*],opcarr[2,*],psym=psym,color=clruse,_extra=_extra
  endif

  ;;;;;;;;;;;;;;;;;;;;;
  ;;  i-z vs. r-i
  ;;;;;;;;;;;;;;;;;;;;;

  pxtitle='r-i'	
  pytitle='i-z'
  xrange = [-1., 3.]
  yrange = [-1., 3.]

  if hist then begin
      ploth,carr[2,*],carr[3,*],xrange=xrange,yrange=yrange,$
        xtitle=pxtitle,ytitle=pytitle,/silent, color=regcolor, _extra=_extra
  endif else begin
      aplot, aspect, carr[2,*],carr[3,*],$
        xrange=xrange,yrange=yrange, color=regcolor, $
        xtitle=pxtitle,ytitle=pytitle, background=background,_extra=_extra
  endelse
  IF nostr NE 0 then begin
      oplot,opcarr[2,*],opcarr[3,*],psym=psym, color=clruse, _extra=_extra
  endif
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Plot brightness vs size if requested
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  if keyword_set(size) then begin

      pytitle = obj_tags[wsize]+'[2]'
      pxtitle = obj_tags[wmag]+'[2]'

      xrange = [12., 23.]

      if hist then begin
          ploth,obj_struct(s).(wmag[0])[2],obj_struct(s).(wsize[0])[2],$
            xrange=xrange,yrange=radrange,xtitle=pxtitle,$
            ytitle=pytitle,/silent, xstyle=1, ystyle=1, $
            color=regcolor, _extra=_extra
      endif else begin
          aplot, aspect, obj_struct(s).(wmag[0])[2],$
            obj_struct(s).(wsize[0])[2],xtitle=pxtitle,ytitle=pytitle, $
            xrange=xrange,yrange=radrange, xstyle=1, ystyle=1, $
            color=regcolor, background=background,_extra=_extra
      endelse

      if keyword_set(oplot_size) then begin
          oplotmag = fltarr(nostr)
          oplotrad = fltarr(nostr)

          oplotmag[*]=oplot_str.(wmag_oplot[0])[2]
          oplotrad[*]=oplot_str.(wsize_oplot[0])[2]

          oplot,oplotmag,oplotrad,psym=psym,color=clruse, _extra=_extra
      endif
      !p.multi=p_old
  endif
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; reset system variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  !p.background = backold
  !p.psym = psymold
  !p.multi=p_old
  !x.style = xstyleold
  !y.style = ystyleold

return

END








