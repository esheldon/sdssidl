PRO tvim2, a, $
           scale=scale, range=range, xrange=xrange, yrange=yrange, $
           aspect=aspect,$
           title=title, xtitle=xtitle, ytitle=ytitle, subtitle=subtitle, $
           noframe=noframe, nolabels=nolabels,$
           invbw=invbw, max_color=max_color, position=pos, $
           noCenter=noCenter, $
           _extra=extra_key

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
; NAME:   
;    tvim2
;
; USEAGE:   tvim2,a
;
;           tvim2,a,title=,xtitle=,ytitle=,xrange=,yrange=,subtitle=,$
;                   scale=,range=,/noframe,aspect=
;
; PURPOSE:  
;    Display an image.
;            1. numbered color scale 
;            2. plot title
;            3. annotated x and y axis 
;            4. simplified OPLOT capability
;
; INPUT    a           image quantity
;
; Optional keyword input:
;
;	   **NOTE**: This routine now uses the _extra mechanism to pass
;		     keywords for put_clr_scl.
;
;          title       plot title
;
;          xtitle      x axis title
; 
;          ytitle      y axis title
;
;	   subtitle    x axis subtitle
;
;          xrange      array spanning entire x axis range.  
;                      NOTE:  TVIM2 only uses XRANGE(0) and
;				XRANGE(N_ELEMENTS(XRANGE)-1).
;
;          yrange      array spanning entire y axis range.  
;                      NOTE:  TVIM2 only uses YRANGE(0) and
;				YRANGE(N_ELEMENTS(YRANGE)-1).
;
;          scale       if set draw color scale.  SCALE=2 causes steped
;                      color scale
;
;          range       two or three element vector indicating physical
;                      range over which to map the color scale.  The
;                      third element of RANGE, if specified, sets the
;                      step interval of the displayed color scale.  It
;                      has no effect when SCALE is not set. E.g.,
;                      RANGE=[0., 1., 0.1] will cause the entire color
;                      scale to be mapped between the physical values
;                      of zero and one; the step size of the displayed 
;                      color scale will be set to 0.1.
;
;          aspect      the x over y aspect ratio of the output image
;                      if not set aspect is set to (size_x/size_y) of the
;                      input image. If set it -1, no aspect ratio is forced. 
;
;          /noCenter:  if set, don't center the display
;
;          noframe     if set do not draw axis box around image
;
;	   nolabels    If set, inhibits labels on plot and scale.
;	  
;	   invbw       To invert the color table ie. array=!d.n_colors-1-array
;		       before calling tv. The deault is invbw=1 for the
;		       postscript device and invbw=0 else.		
;
;
; SIDE EFFECTS:        Setting SCALE=2 changes the color scale using the
;                      STEP_CT procedure.  The color scale may be returned
;                      to its original state by the command, STEP_CT,/OFF
;
; PROCEDURE            TVIM first determins the size of the plot data window
;                      with a dummy call to PLOT.  When the output device is
;                      "X", CONGRID is used to scale the image to the size of
;                      the available data window.  Otherwise, if the output
;                      device is Postscript, scaleable pixels are used in the
;                      call to TV.  PUT_COLOR_SCALE draws the color scale and
;                      PLOT draws the x and y axis and titles.
;
; DEPENDENCIES:        PUT_COLOR_SCALE, STEP_CT
;
; AUTHOR:              Paul Ricchiazzi    oct92 
;                      Earth Space Research Group, UCSB
;
;                      Modified version of TVIM by Jeff Bloch, SST-9, LANL
;
;			1.12	10/13/95
;		       Added invbw keyword to invert the color table. 
;		       The default is invbw=1 for postscript and invbw=0 else.
;		       Changed a line to use bytscl function.
;				David Johnston UChicago July 1999
;                      Centered up the display. Comments, Cleaned up. 
;                      Erin Scott Sheldon UMich 11/24/99
;
;-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF n_params() EQ 0 THEN BEGIN
      print,'-Syntax: tvim2, a, '
      print,'   scale=scale, range=range, xrange=xrange, yrange=yrange, '
      print,'   aspect=aspect,'
      print,'   title=title, xtitle=xtitle, ytitle=ytitle, subtitle=subtitle, '
      print,'   noframe=noframe, nolabels=nolabels,'
      print,'   invbw=invbw, '
      print,'   _extra=extra_key'
      print
      print,'For more help use doc_library,"tvim2"'
      return
  ENDIF 

  ;IF !p.multi[1] EQ 0 THEN !p.multi[1] = 1
  ;IF !p.multi[2] EQ 0 THEN !p.multi[2] = 1

  IF n_elements(invbw) EQ 0 THEN BEGIN 
            ; If device is postscript, reverse color by default
      IF (!d.flags AND 1) THEN invbw = 1 ELSE invbw = 0
  ENDIF 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Some parameters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF n_elements(max_color) EQ 0 THEN max_color=!d.n_colors-1
  IF n_elements(title) EQ 0 THEN title = ''
  IF n_elements(xtitle) EQ 0 THEN xtitle=''
  IF n_elements(ytitle) EQ 0 THEN ytitle=''
  IF n_elements(subtitle) EQ 0 THEN subtitle=''
  IF keyword_set(scale) THEN doscale = 1 ELSE doscale=0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Statistics of array
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  amax=float(max(a))
  amin=float(min(a))
  IF amin EQ amax THEN amax = amin+1
  IF n_elements(range) EQ 0 THEN range=[amin, amax]
  IF range(0) EQ range(1) THEN range(1)=range(1)+1

  sz=size(a)
  nx=sz(1)
  ny=sz(2)

  nxm=nx-1
  nym=ny-1

  IF n_elements(xrange) EQ 0 THEN BEGIN
      xrng=[ -0.5, nxm+0.5]
  ENDIF ELSE BEGIN
      xrng=[xrange(0), xrange(n_elements(xrange)-1)]
  ENDELSE 

  IF n_elements(yrange) EQ 0 THEN BEGIN
      yrng = [-0.5,nym+0.5]
  ENDIF ELSE BEGIN
      yrng = [yrange(0), yrange(n_elements(yrange)-1)]
  ENDELSE 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Deal with aspect ratio and drawing of scale
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; Info is needed to gaurantee deal with aspect ratio.
    plot, [0,1],[0,1],/nodata,xstyle=4,ystyle=4
    px=!x.window*!d.x_vsize
    py=!y.window*!d.y_vsize

    ;pos = plotposition()
    ;px = [ pos[0], pos[2] ]*!d.x_vsize
    ;py = [ pos[1], pos[3] ]*!d.y_vsize

    xsize=px(1)-px(0)
    ysize=py(1)-py(0)

    ;; info is needed for drawing scale
    if doscale then xsize = xsize - 50*!d.x_vsize/700.

    if n_elements(aspect) eq 0 then aspect = float(nx)/ny
    ;; if aspect is -1 then we don't force aspect ratio
    if aspect ne -1 then begin
        if xsize gt ysize*aspect then begin
            xsize=ysize*aspect 
        endif else begin 
            ysize=xsize/aspect 
        endelse
        px(1)=px(0)+xsize
        py(1)=py(0)+ysize
    endif

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; center up the display  E.S.S.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF NOT keyword_set(nocenter) THEN dcenter, xsize, ysize, px, py, /silent

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; draw color scale
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF doscale THEN BEGIN 
      s0=float(range(0))
      s1=float(range(1))
      IF s0 GT s1 THEN invert = 1
      IF keyword_set(invbw) THEN invert=1
      IF n_elements(range) EQ 3 THEN BEGIN 
          s2 = range(2)
          range = range(0:1)
      ENDIF ELSE BEGIN 
          rng=alog10(abs(s1-s0))
          IF rng LT 0. THEN pt = fix(alog10(abs(s1-s0))-.5) $
          ELSE pt = fix(alog10(abs(s1-s0))+.5)
          s2=10.^pt
          tst=[.05,.1,.2,.5,1.,2.,5.,10]
          ii=where(abs(s1-s0)/(s2*tst) le 16)
          s2=s2*tst(ii(0))
      ENDELSE 
      xs=px(1) + 9*!d.x_vsize/700.
      ys=py(0)
      ysize=py(1)-py(0)
      IF scale EQ 2 THEN step_ct, [s0,s1], s2
  ENDIF 

  ;; D.J.
  aa = bytscl(a,min=range(0),max=range(1),top=max_color)
  
  IF invbw EQ 1 THEN aa = max_color-aa

  IF (!d.flags AND 1) EQ 0 THEN BEGIN ;X window
      tv, congrid(aa,xsize,ysize), px(0), py(0)
      pos = [px(0), py(0), px(1), py(1)]
  ENDIF ELSE BEGIN 
      pos = [px(0), py(0), px(1), py(1)] ;postscript
      tv, aa, px(0), py(0), xsize=xsize, ysize=ysize, /device
  ENDELSE 

  IF doscale THEN BEGIN 
      IF ( total(!p.multi(1:2)) GT 4 ) THEN charsize=0.5
      IF ( total(!p.multi(1:2)) GT 8 ) THEN charsize=0.25
      put_clr_scl, xs, ys, range, s2, ysize=ysize,$
        invert=invert, nolabels=nolabels, charsize=charsize,$
        _extra=extra_key
  ENDIF 

  IF keyword_set(noframe) OR keyword_set(nolabels) THEN BEGIN 
      plot, [0,0], [0,0], xstyle=5, ystyle=5, $
        title=title,xtitle=xtitle,ytitle=ytitle, subtitle=subtitle, $
        xrange=xrng, yrange=yrng, position=pos, /noerase, /device, /nodata,$
        _extra=extra_key
  ENDIF ELSE BEGIN 
      plot, [0,0], [0,0], xstyle=1, ystyle=1, $
        title=title, xtitle=xtitle, ytitle=ytitle, subtitle=subtitle, $
        xrange=xrng, yrange=yrng, position=pos, /noerase, /device, /nodata,$
        _extra=extra_key
  ENDELSE 
  
  IF (NOT keyword_set(noframe)) AND keyword_set(nolabels) THEN BEGIN 
      axis,xaxis=1,xtickname=strarr(10)+" ",$
        _extra=extra_key
      axis,xaxis=0,xtickname=strarr(10)+" ",$
        _extra=extra_key
      axis,yaxis=1,ytickname=strarr(10)+" ",$
        _extra=extra_key
      axis,yaxis=0,ytickname=strarr(10)+" ",$
        _extra=extra_key
  ENDIF 

END 





