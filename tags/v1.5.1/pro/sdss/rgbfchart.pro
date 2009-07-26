
;+
;
; NAME:
;    RGBFCHART
;       
; PURPOSE:
;    Create a color finding chart for the input ra/dec position
;    Calls FIND_RADEC_FCHART to create an r-band fchart, then constructs
;    the g and i fcharts.  Then RGBVIEW is called to create and display
;    the color image.  Various images can be written or the postscript
;    device can be opened before calling RGBFCHART
;
; CALLING SEQUENCE:
; THERE ARE TOO WAYS TO CALL RGBFCHART:
;    rgbfchart, ra, dec, $ OR
;                    run, rerun, camcol, field, id, 
;                    $                ; How big is the image?
;                    /pixels, $
;                    /arcminutes, $
;                    /arcseconds, $
;                    $           ; radius of chart
;                    radius=, $
;                    $
;                    runuse=,   $ ; sometimes don't want to use first run found when ra/dec searching
;                    $
;                    $           ; Should we mark positions?
;                    /markPosition, $
;                    markStruct=, $ ; mark style structure for ra/dec (object)
;                    extra_markStruct=, $ ; extra marking structs
;                    $           ; Draw N-E direction arrows?
;                    /directions, $
;                    $           ; display parameters for rgbview
;                    nonlinearity=, $
;                    alpha=, $
;                    low_cut=, $
;                    /sdss, $
;                    /addu,$
;                    /prompt, $
;                    $           ; For writing images
;                    jpegfile=, pngfile=, $
;                    expand=, $
;                    jpegfchart=, pngfchart=,$
;                    z_resolution=, $
;                    $           
;                    $           ; These can be returned
;                    $           
;                    fu=, fg=, fr=, fi=, fz=, $
;                    struct=, $ ; struct for fields used
;                    photoid=, $ ; index of closest object
;                    objx=, objy=, $
;                    useind=, $
;                    imtot=, $
;                    $           ; Style for X or Z display
;                    addtitle=, title=, xtitle=, $
;                    order=, $
;                    /nodisplay, $
;                    rmap=, gmap=, bmap=, $ ; 8-bit color maps
;                    astrans=,$ ; Use the astrans to find radec?
;                    status=, $ ; exit status
;                    maguse=
;
;         FOR COMPATABILITY ONLY
;               saturation=saturation,contrast=contrast, gamma=gamma,$
;               giffile=giffile, giffchart=giffchart, clip=clip,maxsize=
;
; INPUTS: 
;    Either of these:
;      ra/dec: coordinates in degrees.  This position will be marked in the 
;           image if the /markPosition keyword is sent, and the mark style 
;           can be controlled with the markstruct keyword.  Other objects 
;           may be marked using the extra_markstruct input keyword.
;      run,rerun,camcol,field,id: SDSS id info for object chart is built 
;            around.
;
; OPTIONAL INPUTS:
;    /pixels: Use pixel units for all entered quantities.
;    /arcminutes: Use arcminte units (default)
;    /arcseconds: Use arcsecond units.
;    radius:  Half length of the square finding chart in specifiied units.
;
;    runuse: find_radec may return more than one run. Set this value
;            to an integer to choose which run.
;
;    /markPosition:  Mark the position of entered object or nearest object if 
;                    ra/dec was specified.  Default is a cross.
;    markStruct=: Mark structure describing the mark. e.g
;         markstruct = {ra: 200.0, dec: 0.0, type: 'circle', radius: 0.5}
;         for more options, see markstruct_copy_defaults.pro
;    extra_markStruct=: other positions to be marked.
;
;    /direction:  Print N-W direction arrows.
;
;    nonlinearity: scaled image = asihn(nonlinearity*alpha*(im-sky))/nonlin
;    alpha:
;    low_cut: Lowest value in image to use
;    /sdss:   Use SDSS parameters to rescale counts to energy.
;    /addu:  add the u-band to the g image
;    /prompt: Use a prompt in rgbview to interactively change display
;             parameters.
;
;    jpegfile: File to write full-res jpeg to.
;    pngfile:  same for png
;    expand=: Integer factor by which to expand the image. 
;    jpegfchart:  Name of jpeg file to write a finding chart. 
;                  If sent write_jpeg will be called and the image will be 
;                  written from the X-window.
;    pngfchart: png finding chart file.
;    z_resolution: resoluton in z-buffer, used when no display exists.
;    addtitle: Additional title to tack on
;    title: Title
;    xtitle: xtitle
;    order: IDL image order
;
;    /astrans: use astrans method to find radec
;    maguse: which magnitude used.
;
;
; OPTIONAL OUTPUTS:
;    fu, fg, fr, fi, fz: finding charts individual bands
;    struct: the PHOTO structure read in to make finding chart
;    objx, objy: positions in finding chart.
;    useind: index of objects used as "center".
;    photoid: index into struct of nearest object to ra/dec
;    imtot: the bytescaled total rgb image
;    rmap=rmap, gmap=gmap, bmap=bmap: 
;          If on 8-bit display, one can set these keywords to 
;          a  named variable which will be set to the color map
;    status: if status ne 0 then something went wrong
;
; CALLED ROUTINES:
;    FIND_RADEC_FCHART (calls many programs)
;    FCHART
;    RGBVIEW
;    CIRC_RADEC
; 
; PROCEDURE: 
;    
;	
;
; REVISION HISTORY:
;    Created ??-??-2001.  Documentation added 3-Jul-2002
;              Erin S. Sheldon UofMich
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


PRO rgbfchart_mark_image, image, markStruct_in, $
                               mapStruct=mapStruct, $
                               xoffset=xoffset, yoffset=yoffset, $
                               order=order, cfac=cfac

  ;; Make a copy since we will modify it
  markStruct = markStruct_in

  IF n_elements(cfac) EQ 0 THEN cfac=1

  ;; Some statistics
  imsize = size(image)
  ny = imsize[2]
  ss = imsize[1]

  ;; In pixels
  defrad = ss/10.0

  ;; radius
  ;; Checking this only works if the defaults have been defined!
  IF tag_exist(markStruct,'radius') THEN BEGIN 

      markStruct.radius = markStruct.radius*cfac

  ENDIF ELSE BEGIN 

      IF tag_exist(markStruct,'x') THEN nmark=n_elements(markStruct.x) $
      ELSE nmark = n_elements(markStruct.ra)

      IF nmark EQ 1 THEN BEGIN 
          markRadius = defrad
      ENDIF ELSE BEGIN 
          markRadius = [defrad, replicate(defrad/2.0,nMark-1)]
      ENDELSE 

      markStruct=create_struct(markStruct, 'radius', markRadius)
  ENDELSE 

  xshift = -0.5
  yshift = -0.5

  IF tag_exist(markStruct,'x') AND tag_exist(markStruct,'y') THEN BEGIN 
      ;; Add shift because plotting programs use center of pixel as
      ;; (0,0)

      marky = markstruct.y
      IF n_elements(order) NE 0 THEN BEGIN 
          IF order[0] EQ 1 THEN BEGIN 
              marky = (ny-1) - marky
          ENDIF 
      ENDIF 

      markstruct.x = markstruct.x + xshift
      markstruct.y = marky + yshift

      markxy, markStruct
  ENDIF ELSE IF (tag_exist(markStruct,'ra') AND $
                 tag_exist(markStruct,'dec')) THEN BEGIN 

      IF n_elements(mapStruct) EQ 0 THEN BEGIN 
          print,'You must enter mapStruct to circle ra/dec'
          return
      ENDIF 
;stop
      mark_radec, mapStruct, markStruct, $
        xoffset=xoffset, yoffset=yoffset, $
        order=order, ny=ny

  ENDIF ELSE BEGIN 
      print,'You must enter x/y or ra/dec'
      return
  ENDELSE 


END 

;; Only send a single structure
PRO rgbfchart_titles, struct, ra, dec, title, xtitle, ytitle, $
                           addtitle=addtitle, hideradec=hideradec, $
                           inxtitle=inxtitle, intitle=intitle

  colors=['u','g','r','i','z'] 

  tags = tag_names(struct)

  runstr = run2string(struct.run)

  fieldstr = field2string(struct.field)
  camcolstr = strtrim(string(struct.camcol),2)
  idstr = strtrim(string(struct.id), 2)

  ;; Set up titles
  wrer = where(tags EQ 'RERUN', nwrer)
  IF nwrer NE 0 THEN BEGIN
      rerunstr='-'+strtrim(string(struct.(wrer[0])), 2)
  ENDIF ELSE BEGIN
      rerunstr = ''
  ENDELSE 

  IF n_elements(addtitle) EQ 0 THEN addtitle=''
  tmptitle=runstr+rerunstr+'-'+camcolstr+'-'+fieldstr+'-'+idstr
  IF n_elements(intitle) EQ 0 THEN title=tmptitle ELSE title=intitle

  IF (NOT keyword_set(hideradec) ) THEN BEGIN 
      radecstr, ra, dec, rastr, decstr
      title = title + '  ' +rastr+' : ' + decstr
  ENDIF 

  title = title+'   '+addtitle

  ytitle='Offset (arcminutes)'

  wmag = sdss_maguse(struct, maguse=maguse, silent=silent)

  IF n_elements(inxtitle) EQ 0 AND wmag NE -1 THEN BEGIN 

      c=struct.(wmag)
      xtitle=''
      FOR kk=0, 4 DO BEGIN
          IF (xtitle NE '') THEN xtitle = xtitle+'  '
          mag = strmid( strtrim(string(c[kk]),2), 0, 5)
          xtitle = xtitle + colors[kk]+'='+mag
      ENDFOR

      addtit = ''
      FOR kk=0,3 DO BEGIN
          diff = strmid(strtrim(string(c[kk] - c[kk+1]),2), 0, 5)
          addtit = addtit + '  '+colors[kk]+'-'+colors[kk+1]+'='+diff
      ENDFOR
      xtitle = xtitle + addtit  

  ENDIF 


END 

PRO rgbfchart_mark_radec_arrvals, nra, default, name, arrval, retval

  nArrval = n_elements(arrval)

  IF nArrval NE nra THEN BEGIN 
      IF nArrval EQ 0 THEN BEGIN 
          IF name EQ 'mark_radius' THEN BEGIN 
              retval = default
              IF (nra-1) GT 0 THEN BEGIN 
                  retval = [default, replicate(default/2.0,nra-1)]
              ENDIF 
          ENDIF ELSE BEGIN 
              retval = replicate(default, nra)
          ENDELSE 
      ENDIF ELSE IF nArrval EQ 1 THEN BEGIN 
          retval = replicate(arrval[0], nra)
      ENDIF ELSE BEGIN 
          message,$
            name+' must be single number or same size as ' + $
            'mark_ra, mark_dec'
      ENDELSE 
  ENDIF ELSE BEGIN 
      retval = arrval
  ENDELSE 

END 

PRO rgbfchart_axis, xlabels, ylabels, xtitle, ytitle, title, $
                    axiscolor=axiscolor

  axis,xaxis=0,xticks=4,xtickn=xlabels, xtitle=xtitle
  axis,xaxis=1,xticks=4,xtickn=[' ',' ',' ',' ',' ']
  axis,yaxis=0,yticks=4,ytickn=ylabels, ytitle=ytitle
  axis,yaxis=1,yticks=4,ytickn=[' ',' ',' ',' ',' ']

END 

PRO rgbfchart_display, imtot, xlabels, ylabels, xtitle, ytitle, title,$
                       color_im=color_im, rmap=rmap, gmap=gmap, bmap=bmap,$
                       order=order

  fi = reform( imtot[2,*,*] )
  implot_setup, fi, xsize, ysize, px, py, xrng, yrng, /center
  
  IF !d.name EQ 'PS' THEN BEGIN 

      device,/color
      tvlct,indgen(256),indgen(256),indgen(256)
      
      pos = [px[0], py[0], px[1], py[1]]
      tv, imtot,true=1,px[0],py[0], xsize=xsize, ysize=ysize, /device,$
        order=order

  ENDIF ELSE BEGIN 

      IF !d.n_colors LE 255 THEN BEGIN ;; 8-bit display
          tv, congrid(color_im, xsize, ysize), px[0],py[0],$
            order=order
          tvlct, rmap, gmap, bmap
          pos = [px[0], py[0], px[1], py[1]]
      ENDIF ELSE BEGIN 
          tv, congrid(imtot, 3, xsize, ysize),true=1,px[0],py[0],$
            order=order
          pos = [px[0], py[0], px[1], py[1]]
      ENDELSE 

  ENDELSE 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Establish the data coordinates and 
  ;; place title
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  plot, [0,0], [0,0], xstyle=5, ystyle=5, $
    title=title, $
    xrange=xrng, yrange=yrng, position=pos,$
    /noerase, /device, /nodata

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Put labels on the finding chart
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  rgbfchart_axis, xlabels, ylabels, xtitle, ytitle, title, $
    axiscolor=axiscolor


END 

PRO rgbfchart_zbuff, im, xlabels, ylabels, xtitle, ytitle, title, order=order

  implot_setup, im, xsize, ysize, px, py, xrng, yrng, /center
  
  tv, congrid(im, xsize, ysize),px[0],py[0], order=order
  pos = [px[0], py[0], px[1], py[1]]

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Establish the data coordinates and 
  ;; place title
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  plot, [0,0], [0,0], xstyle=5, ystyle=5, $
    title=title, $
    xrange=xrng, yrange=yrng, position=pos,$
    /noerase, /device, /nodata

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Put labels on the finding chart
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  rgbfchart_axis, xlabels, ylabels, xtitle, ytitle, title, $
    axiscolor=axiscolor

END 



;; Possible tags
;markStruct = {ra:0d, dec:0d, $
;              type: 0, $
;              radius:0.0, $
;              hole_fraction: 0.0, $
;              color: "red", $
;              linestyle: 0}

PRO rgbfchart, ra, dec, camcol, field, id, $ ; ra,dec can actually be run/rerun
                    $           ; Units for all lenghts. Default arcminutes
                    pixels=pixels, $
                    arcminutes=arcminutes, $
                    arcseconds=arcseconds, $
                    $           ; radius of chart
                    radius=radius, $
                    $
                    runuse=runuse,   $ ; sometimes don't want to use first run found when ra/dec searching
                    $
                    $           ; Should we mark positions?
                    markPosition=markPosition, $
                    markStruct=markStruct, $ ; mark style structure for ra/dec (object)
                    extra_markStruct=extra_markStruct, $ ; extra marking structs
                    $           ; Draw N-E direction arrows?
                    directions=directions, $
                    $           ; display parameters for rgbview
                    nonlinearity=nonlinearity, $
                    alpha=alpha, $
                    low_cut=low_cut, $
                    sdss=sdss, $
                    addu=addu,$
                    prompt=prompt, $
                    $           ; For writing images
                    jpegfile=jpegfile, $
                    pngfile=pngfile, $
                    expand=expand, $
                    jpegfchart=jpegfchart, $
                    pngfchart=pngfchart,$
                    z_resolution=z_resolution, $
                    $           
                    $           ; These can be returned
                    $           
                    fg=fg, fr=fr, fi=fi, $
                    fu=fu, fz=fz, $
                    struct=struct, $ ; struct for fields used
                    photoid=photoid, $ ; index of closest object
                    objx=objx, objy=objy, $
                    useind=useind, $
                    imtot=imtot, $
                    $           ; Style for X or Z display
                    addtitle=addtitle, title=title, xtitle=xtitle, $
                    order=order, $
                    nodisplay=nodisplay, $
                    rmap=rmap, gmap=gmap, bmap=bmap, $ ; 8-bit color maps
                    astrans=astrans,$ ; Use the astrans to find radec?
                    status=status, $ ; exit status
                    maguse=maguse,$
                    $           ; For compatability only
                    maxsize=maxsize, $ 
                    saturation=saturation,contrast=contrast, gamma=gamma,$
                    giffile=giffile, giffchart=giffchart, clip=clip

  status = 1
  IF n_params() LT 2 THEN BEGIN 
      print,'-Syntax: '
      print,'    rgbfchart, ra, dec --OR-- run,rerun,camcol,field,id, ' ;
      print,'                    ; Units for all lenghts. Default arcminutes'
      print,'    /pixels, /arcminutes, /arcseconds, '
      print,'                    ; radius of chart'
      print,'    radius=, '
      print,'    '
      print,'    runuse=,    ; sometimes dont want to use first run found when ra/dec searching'
      print,'                    ; Should we mark positions?'
      print,'    /markPosition, markStruct=, extra_markStruct=, '
      print,'                    ; Draw N-E direction arrows?'
      print,'    /directions, '
      print,'                    ; display parameters for rgbview'
      print,'    nonlinearity=, alpha=, low_cut=, '
      print,'    /sdss, '
      print,'    /addu,'
      print,'    /prompt, '
      print,'                    ; For writing images'
      print,'    jpegfile=, pngfile=, '
      print,'    expand=, '
      print,'    jpegfchart=, pngfchart=, '
      print,'    z_resolution=, '
      print,'    '
      print,'                    ; These can be returned'
      print,'    '
      print,'    fu=, fg=, fr=, fi=, fz=, '
      print,'    struct=,  ; struct for fields used'
      print,'    photoid=,  ; index of closest object'
      print,'    objx=, objy=, '
      print,'    useind=, '
      print,'    imtot=, '
      print,'                    ; Style for X or Z display'
      print,'    addtitle=, title=, xtitle=, '
      print,'    order=, '
      print,'    nodisplay=, '
      print,'    rmap=, gmap=, bmap=,  ; 8-bit color maps'
      print,'    /astrans, ; Use the astrans to find radec?'
      print,'    status=,  ; exit status'
      print,'    maguse='      
      return
  ENDIF 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; parameters
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  colors=['u','g','r','i','z'] 
  ;; time the result
  time = systime(1)

  ;; PS device?
  IF !d.name EQ 'PS' THEN BEGIN 
      !p.thick=1
      !x.thick=1
      !y.thick=1
      !p.charsize=1
      !p.charthick=1
  ENDIF 

  ;; true color display?
  IF !d.n_colors GT 256 THEN true_color = 1 ELSE true_color = 0

  ;; What units? By default, arcminutes.
  ;; We always work in pixel coordinates. Multiply all lengths by cfac

  IF keyword_set(pixels) THEN BEGIN 
      cfac = 1.0
  ENDIF ELSE IF keyword_set(arcseconds) THEN BEGIN 
      cfac = 1.0/0.4
  ENDIF ELSE BEGIN ;; Default is arcminutes
      cfac = 60.0/0.4
  ENDELSE 

  rad_default = 300             ; pixels
  IF n_elements(radius) EQ 0 THEN BEGIN 
      radius = rad_default/cfac
  ENDIF 

  IF n_elements(markStruct) EQ 0 AND keyword_set(markPosition) THEN BEGIN 
      markStruct = {hole_fraction:1./3.}
  ENDIF 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Will we write a jpeg or png file of the finding chart? 
  ;; Note this is different than just a jpeg or png write without
  ;; annotation
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF n_elements(jpegfchart) NE 0 OR n_elements(pngfchart) NE 0 THEN BEGIN 
      write_fchart_image = 1
  ENDIF ELSE write_fchart_image=0

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; If no display or not true color display, we will need to use
  ;; the stacked z buffer method to write the true-color finding
  ;; chart
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF (  ((NOT true_color) AND write_fchart_image) OR $
        (keyword_set(nodisplay) AND write_fchart_image) ) THEN BEGIN
      stack_zbuff = 1
  ENDIF ELSE stack_zbuff = 0
  IF n_elements(z_resolution) EQ 0 THEN BEGIN 
      zres = [775,670] 
  ENDIF ELSE zres=z_resolution

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; make r-band finding chart
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  delvarx, photoid, struct
  IF n_params() EQ 5 THEN BEGIN 
      
      id_input = 1

      run = ra
      rerun = dec
      read_tsobj, [run,rerun,camcol], struct, start=field-1, nf=3

      IF n_elements(struct) EQ 0 THEN return

      photoid = where(struct.field EQ field AND struct.id EQ id, nw)
      IF nw EQ 0 THEN BEGIN 
          print,'No such id in this run/rerun/camcol/field'

          delvarx, struct, photoid, objx, objy
          return
      ENDIF 
      
      clr = 2
      fchart, struct, photoid, radius*cfac, clr, fr, $
        objx=objx, objy=objy

      ;; In this case, we will mark the objx, objy not ra/dec
      IF n_elements(markStruct) NE 0 THEN BEGIN 
          IF NOT tag_exist(markStruct,'x') THEN $
            markStruct = create_struct(markStruct,'x',0d)
          IF NOT tag_exist(markStruct,'y') THEN $
            markStruct = create_struct(markStruct,'y',0d)

          markStruct.x = objx
          markStruct.y = objy
      ENDIF 

  ENDIF ELSE BEGIN 
      
      id_input = 0

      find_radec_fchart, $
        ra, dec, $
        fchart=fr, $
        pstruct=struct, $
        useind=useind, $
        radius=radius, $
        pixels=pixels, $
        arcminutes=arcminutes, $
        arcseconds=arcseconds, $
        clr=2, photoid=photoid, runuse=runuse, $
        objx=objx, objy=objy,/nodisplay, astrans=astrans

      IF n_elements(fr) EQ 0 THEN BEGIN 
          delvarx, struct, photoid, objx, objy
          return
      ENDIF 

      ;; In this case, we mark the input ra/dec, not position of
      ;; nearest object
      IF n_elements(markStruct) NE 0 THEN BEGIN 
          IF NOT tag_exist(markStruct,'ra') THEN $
            markStruct = create_struct(markStruct,'ra',0d)
          IF NOT tag_exist(markStruct,'dec') THEN $
            markStruct = create_struct(markStruct,'dec',0d)

          markStruct.ra  = ra
          markStruct.dec = dec
      ENDIF 

  ENDELSE 

  frsize = size(fr)
  nx = frsize[1]
  ny = frsize[2]

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; now make g and i finding charts
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  clr = 1
  fchart, struct, photoid, radius*cfac, clr, fg, $
    /shift2r

  clr = 3
  fchart, struct, photoid, radius*cfac, clr, fi, $
    /shift2r

  ;; should we add in the u band?
  IF keyword_set(addu) THEN BEGIN 
      clr=0
      fchart, struct, photoid, radius*cfac, clr, fu
      fg = (fg + fu)
  ENDIF ELSE IF arg_present(fu) THEN BEGIN 
      clr=0
      fchart, struct, photoid, radius*cfac, clr, fu
  ENDIF 

  IF arg_present(fz) THEN BEGIN 
      clr=4
      fchart, struct, photoid, radius*cfac, clr, fz
  ENDIF 

  IF keyword_set(directions) THEN BEGIN 
      angle = angle_rowcol2radec(struct[photoid].run, $
                                 struct[photoid].camcol, $
                                 struct[photoid].field, $
                                 2)
  ENDIF 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Get the mapping from ra/dec to row,col in r-band
  ;; for marking positions. There is an offset between the
  ;; actual object position and the row,col.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF (n_elements(markStruct) NE 0 OR $
      n_elements(extra_markstruct) NE 0) THEN BEGIN 

      w=where(struct.field EQ struct[photoid].field)
      map_order = 1
      create_photomap, $
        struct[w].ra, struct[w].dec, struct[w].rowc[2], struct[w].colc[2], $
        mapStruct, map_order=map_order, $
        ra_center = ramatch, dec_center=decmatch
      
      xoffset = objx - struct[photoid].colc[2]
      yoffset = objy - struct[photoid].rowc[2]

  ENDIF 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Parameters for the annotation of the finding chart
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF id_input THEN BEGIN 
      ira = struct[photoid].ra
      idec = struct[photoid].dec
  ENDIF ELSE BEGIN 
      ira = ra[0]
      idec = dec[0]
  ENDELSE 
  rgbfchart_titles, struct[photoid], ira, idec, title, xtitle, ytitle, $
    addtitle=addtitle, hideradec=hideradec, $
    inxtitle=xtitle, intitle=title

  ss = size(fr)
  sx = ss[1]*.4/60.0
  sy = ss[2]*.4/60.0

  ;;;; make x and y tick labels
  xlabels = [ntostr(-2*sx/4.0, 5), $
             ntostr(-sx/4.0, 5)  , $
             '0.0', $
             ntostr(sx/4.0,4), $
             ntostr(2*sx/4.0,4)]
  ylabels = xlabels

  
  isky = 0.
  rsky = 0.
  gsky = 0.

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; This will write to the device that is open with no frame
  ;; unless /nodisplay
  ;; Also to jpeg or png files with no annotation
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  rgbview, fi, fr, fg, $
    order=order, $
    nonlinearity=nonlinearity, $
    alpha=alpha, $
    imtot=imtot, $
    rsky=isky, gsky=rsky, bsky=gsky, $
    /noframe, prompt=prompt, low_cut=low_cut, $
    rmap=rmap, gmap=gmap, bmap=bmap, expand=expand, $
    color_im=color_im,jpegfile=jpegfile, pngfile=pngfile, sdss=sdss, $
    /nodisplay

  ;; Under certain circumstances, we need to do a stacked zbuffer
  ;; to write the finding chart: no display or PS or 8-bit display, etc.
  IF stack_zbuff THEN BEGIN 

      devold = !d.name
      setupplot, 'Z'
      device, set_resolution=zres

      tf = reform(imtot[0,*,*])
      rgbfchart_zbuff, tf, xlabels, ylabels, xtitle, ytitle, title, $
        order=order

      IF n_elements(markStruct) NE 0 THEN BEGIN 
          rgbfchart_mark_image, $
            fr, markStruct, $
            mapStruct=mapStruct, $
            xoffset=xoffset, yoffset=yoffset, $
            order=order, cfac=cfac
      ENDIF 
      IF n_elements(extra_markStruct) NE 0 THEN BEGIN 
          rgbfchart_mark_image, $
            fr, extra_markStruct, $
            mapStruct=mapStruct, $
            xoffset=xoffset, yoffset=yoffset, $
            order=order, cfac=cfac
      ENDIF 

      IF keyword_set(directions) THEN BEGIN 
          plot_ne_arrows, angle, fracsize=0.05, order=order
      ENDIF 

      tmp = tvrd()
      tsz = size(tmp,/dim)
      imtot2 = bytarr(3, tsz[0], tsz[1])
      imtot2[0,*,*] = tmp

      tf = reform(imtot[1,*,*])
      rgbfchart_zbuff, tf, xlabels, ylabels, xtitle, ytitle, title, $
        order=order

      IF n_elements(markStruct) NE 0 THEN BEGIN 
          rgbfchart_mark_image, $
            fr, markStruct, $
            mapStruct=mapStruct, $
            xoffset=xoffset, yoffset=yoffset, $
            order=order, cfac=cfac
      ENDIF 
      IF n_elements(extra_markStruct) NE 0 THEN BEGIN 
          rgbfchart_mark_image, $
            fr, extra_markStruct, $
            mapStruct=mapStruct, $
            xoffset=xoffset, yoffset=yoffset, $
            order=order, cfac=cfac
      ENDIF 

      IF keyword_set(directions) THEN BEGIN 
          plot_ne_arrows, angle, fracsize=0.05, order=order
      ENDIF 


      tmp = tvrd()
      imtot2[1,*,*] = tmp

      tf = reform(imtot[2,*,*])
      rgbfchart_zbuff, tf, xlabels, ylabels, xtitle, ytitle, title, $
        order=order

      IF n_elements(markStruct) NE 0 THEN BEGIN 
          rgbfchart_mark_image, $
            fr, markStruct, $
            mapStruct=mapStruct, $
            xoffset=xoffset, yoffset=yoffset, $
            order=order, cfac=cfac
      ENDIF 
      IF n_elements(extra_markStruct) NE 0 THEN BEGIN 
          rgbfchart_mark_image, $
            fr, extra_markStruct, $
            mapStruct=mapStruct, $
            xoffset=xoffset, yoffset=yoffset, $
            order=order, cfac=cfac
      ENDIF 

      IF keyword_set(directions) THEN BEGIN 
          plot_ne_arrows, angle, fracsize=0.05, order=order
      ENDIF 

      tmp = tvrd()
      imtot2[2,*,*] = tmp

      IF n_elements(jpegfchart) NE 0 THEN BEGIN 
          print,'Writing JPEG fchart: ',jpegfchart
          write_jpeg, jpegfchart, imtot2, true=1, quality=75
      ENDIF 
      IF n_elements(pngfchart) NE 0 THEN BEGIN 
          print,'Writing PNG fchart: ',pngfchart
          IF float(!version.release) LT 5.4 THEN BEGIN 
              
              ;; In old version png is written out flipped
              imtot2[0,*,*] = rotate(rotate(reform(imtot2[0,*,*]),1),4)
              imtot2[1,*,*] = rotate(rotate(reform(imtot2[1,*,*]),1),4)
              imtot2[2,*,*] = rotate(rotate(reform(imtot2[2,*,*]),1),4)

              write_png, pngfchart, imtot2
              
          ENDIF ELSE BEGIN 
              write_png, pngfchart, imtot2
          ENDELSE 
      ENDIF 
      setupplot, devold

  ENDIF 

  IF NOT keyword_set(nodisplay) THEN BEGIN 

      rgbfchart_display, imtot, xlabels, ylabels, xtitle, ytitle, title,$
        order=order

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Mark points on the finding chart
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      IF n_elements(markStruct) NE 0 THEN BEGIN 
          rgbfchart_mark_image, $
            fr, markStruct, $
            mapStruct=mapStruct, $
            xoffset=xoffset, yoffset=yoffset, $
            order=order, cfac=cfac
      ENDIF 
      IF n_elements(extra_markStruct) NE 0 THEN BEGIN 
          rgbfchart_mark_image, $
            fr, extra_markStruct, $
            mapStruct=mapStruct, $
            xoffset=xoffset, yoffset=yoffset, $
            order=order, cfac=cfac
      ENDIF 

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Put North-East lines on finding chart
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      IF keyword_set(directions) THEN BEGIN 
          angle = $
            angle_rowcol2radec(struct[photoid].run, $
                               struct[photoid].camcol, $
                               struct[photoid].field, 2)
          plot_ne_arrows, angle, fracsize=0.05, order=order
      ENDIF 


      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; output images of the finding chart
      ;; These will be read from the display
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      IF n_elements(giffchart) NE 0 THEN BEGIN 
          print,'Writing GIF no longer supported in IDL'
      ENDIF 
      IF n_elements(jpegfchart) NE 0 AND NOT stack_zbuff THEN BEGIN 
          IF true_color THEN BEGIN 
              print,'Writing JPEG fchart: ',jpegfchart
              write_jpeg, jpegfchart, tvrd(true=1), true=1, quality=75
          ENDIF ELSE BEGIN 
              print,'Cannot write JPEG from 8-bit display: try PNG format'
          ENDELSE 
      ENDIF 
      IF n_elements(pngfchart) NE 0 AND NOT stack_zbuff THEN BEGIN 
          print,'Writing PNG fchart: ',pngfchart

          IF true_color THEN BEGIN 

              IF float(!version.release) LT 5.4 THEN BEGIN 
                  tmp = tvrd(true=1)
                  
                  ;; In old version png is written out flipped
                  tmp[0,*,*] = rotate(rotate(reform(tmp[0,*,*]),1),4)
                  tmp[1,*,*] = rotate(rotate(reform(tmp[1,*,*]),1),4)
                  tmp[2,*,*] = rotate(rotate(reform(tmp[2,*,*]),1),4)
                  
                  write_png, pngfchart, tmp
                  
              ENDIF ELSE BEGIN 
                  write_png, pngfchart, tvrd(true=1)
              ENDELSE 
          ENDIF ELSE BEGIN 
              IF float(!version.release) LT 5.4 THEN BEGIN 
                  tmp = tvrd()
                  tmp = rotate( rotate(tmp,1), 4)
                  write_png, pngfchart, tmp, rmap, gmap, bmap
              ENDIF ELSE BEGIN 
                  write_png, pngfchart, tvrd(), rmap, gmap, bmap
              ENDELSE 
          ENDELSE 
      ENDIF 
  ENDIF 

  ptime, systime(1)-time
  status=0

  return
END 
