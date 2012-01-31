
;+
;
; NAME:
;    RGBVIEW
;       
; PURPOSE:
;    Create and display a RGB image from red, green, and blue input images.
;    This program designed for use with astronomical images: 
;      * The image has the "sky" removed, so that the sky is black.
;      * An asinh stretch is used, as described in Lupton et al.
;        astro-ph/0312483  
;
;    SDSS should use red=i, grn=r, blue=g although this will have false
;    color. This can be input for each image, or the median is used.
;
;    If the device is postscript, then a color postscript is made.
;    NOTE: The color map is NOT inverted so there is often large amounts
;          of black space.
;
;    Works best on devices with 16 million+ colors, but will work sensibly
;    with any number of available colors (e.g. 256 in postscript)
;
;    Also note that the number of colors on your display does not affect
;    the optional jpeg/png files that RGBVIEW outputs because it uses full
;    color range when producing them.
;
;    WARNING: this program can use lots of memory. It makes 
;    a copy of each image for speed: each image is converted to float and
;    sky subtracted. If your images are already sky-subtracted and float then
;    you should modify this program.
;
; CALLING SEQUENCE:
;    rgbview, rr, gg, bb, $
;             $                  ; control the stretch
;             nonlinearity=nonlinearity, $
;             alpha=alpha, $
;             scales=scales, $   ; multiply images by this number
;             sdss=sdss, $       ; set scales for sdss
;             $                  ; range in image
;             low_cut=low_cut, high_cut=high_cut, $
;             $                  ; sky is subtracted
;             rsky=rsky, gsky=gsky, bsky=bsky, $
;             nodisplay=nodisplay, $ ; display?
;             prompt=prompt, $  ;should we interact with user for display?
;             $                  ; Write jpeg or png
;             jpegfile=jpegfile, $
;             pngfile=pngfile, $
;             expand=expand, $   ; factor to expand pixels when writing image
;             $                  ; read from the screen? 
;             tvread=tvread, $
;             $                  ; some of the images used
;             color_im=color_im, imtot=imtot, $
;             $                  ; color map used for 8-bit
;             rmap=rmap, gmap=gmap, bmap=bmap, $
;             $                  ; some screen parameters
;             xrange=xrange, yrange=yrange, $
;             title=title, xtitle=xtitle, ytitle=ytitle, $
;             subtitle=subtitle, $
;             noframe=noframe, nolabels=nolabels, $
;             silent=silent, $
;             _extra=extra_key
;
; THESE KEYWORDS ARE KEPT FOR COMPATABILITY ONLY, AND WILL BE QUIETLY IGNORED
;             gamma=gamma, saturation=saturation, 
;             contrast=contrast, 
;             rsig=rsig, gsig=gsig, bsig=bsig, 
;             giffile=giffile ;gif no longer supported by IDL
;
; INPUTS: 
;    red, grn, blue: The red, green and blue images.  Images must be same size.
;
; OPTIONAL INPUTS:
;
;    nonlinearity: Image is scaled as
;                  asinh(alpha*nonlinearity*(image-sky))/nonlinearity The
;                  default is 8, which works well with alpha=0.02 for SDSS
;                  images.
;    alpha: default 0.02.  Controls where the linearity kicks in, i.e. where
;           the asihn behaves linearly.
;           For the HDF, I found that nonlinearity=8, alpha=1 works well.
;    scales: scale for each image 3 elem. array'
;
;    low_cut:      Lowest value in image to use in the image.
;    high_cut:     Highest value to use in the image.
;
;    rsky, gsky, bsky: input sky value of r,g,b images.  Avoides running
;                      sigma-clipping to find sky.  If not input, these values
;                      can be returned through these keywords.
;
;    jpegfile: If a string is sent in this keyword, rgbview will write a jpeg
;              file containing the image with this filename.  Written directly
;              at full resolution from the images unless /tvread is set.
;    pngfile: same as above except writes png image file.  
;    expand: Factor to expand pixels when writing image (not tvread)
;
;    xrange, yrange, noframe, nolabels: see tvim2
;    title,xtitle,ytitle,subtitle: Plot labels.
;    _extra=extra_key:  Other plotting keywords.
;
; KEYWORD PARAMETERS:
;    /sdss: if set, rescales images by energy based on sdss filters.  
;    /nodisplay: if set, no display is made.  You might use this if you are
;          just outputting the jpeg files, maybe in a batch job if used in
;          conjunction with /prompt
;    /prompt: if set then user can interactively change display parameters.
;    /tvread: write the image to file from the display rather than directly
;             from the composite image.
;    /silent:
;       
; OPTIONAL OUTPUTS: 
;    imtot: image containing ar, ag, ab in the form bytarr(3, n, m).  Can be 
;         input directly to write_jpeg to produce 24-bit image.
;    rmap,gmap,bmap: color map vectors.  These are the vectors used to display 
;         this image.  If using an 8-bit display, they can be sent to 
;
;                IDL> WRITE_PNG, filename, TVRD(), rmap, gmap, bmap
;           
;         If on 8-bit display, you might need to go back to BW linear display
;         The color map can be reset to BW linear with loadct,0
; 
;    color_im: a byte 2-d image containing the image used for display with the 
;        8-bit color maps above
;  
; CALLED ROUTINES: (lower case are built in IDL procedures)
;    DCENTER
;    SIGMA_CLIP
;    color_quan
;    bytscl
;    tv
;    tvlct
;    (device)
;    (write_gif)
;    (write_jpeg)
;
; PROCEDURE: 
;    
;
; REVISION HISTORY:
;    Author: Erin Scott Sheldon  UofMich  11/28/99
;                                             
;-      
;
;
;
;  Copyright (C) 2005  Erin Sheldon, NYU.  erin dot sheldon at gmail dot com
;
;    This program is free software; you can redistribute it and/or modify
;    it under the terms of version 2 of the GNU General Public License as 
;    published by the Free Software Foundation.
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


PRO rgbview_scalergb, rin, gin, bin, imtot, $
                      low=low, high=high, $
                      nonlinearity=nonlinearity, $
                      alpha=alpha, $
                      silent=silent

  
  IF n_elements(nonlinearity) EQ 0 THEN nonlinearity = 8
  IF n_elements(alpha) EQ 0 THEN alpha = 0.02
  IF n_elements(low) EQ 0 THEN low=0

  IF NOT keyword_set(silent) THEN BEGIN 
      print,'alpha = ',ntostr(alpha,5)
      print,'nonlinearity = ',ntostr(nonlinearity,5)
  ENDIF 

  ;; Create the total image, byte
  tmp = size(rin, /dim)
  nx = tmp[0]
  ny = tmp[1]
  imtot = bytarr(3, nx, ny)

  r=rin > 0
  b=bin > 0
  g=gin > 0

  I = r + g + b
  I[*] = I[*]/3.0

  ;; leave these zero in red,grn,blue output
  w=where(I GT 0, nw, comp=wcomp, ncomp=ncomp)

  IF nw EQ 0 THEN BEGIN 
      print,'WARNING: No good pixels. Image is all zero'
      return
  ENDIF ELSE BEGIN 
      ;; now create images scaled to 1.  The max of them must be equal to 1
      ;; at each pixel

      IF nonlinearity GT 0 THEN BEGIN 

          fofi_over_i = asinh( alpha*nonlinearity*I )/nonlinearity
          
          fofi_over_i[w] = fofi_over_i[w]/I[w]
          
          r[w]  = r[w]*fofi_over_i[w]
          g[w]  = g[w]*fofi_over_i[w]
          b[w]  = b[w]*fofi_over_i[w]

      ENDIF 
          

      factor = (r > g > b) > 1
      finv = 1./factor
      r = r*finv
      g = g*finv
      b = b*finv
  ENDELSE 

  imtot[0,*,*] = byte( (floor(temporary(r)*256.0) > 0) < 255 )
  imtot[1,*,*] = byte( (floor(temporary(g)*256.0) > 0) < 255 )
  imtot[2,*,*] = byte( (floor(temporary(b)*256.0) > 0) < 255 )

END 

PRO rgbview_setupplot, nx, ny, aspect, xsize, ysize, px, py, xrng, yrng

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Set up plot
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  plot, [0,1],[0,1],/nodata,xstyle=4,ystyle=4
  px=!x.window*!d.x_vsize
  py=!y.window*!d.y_vsize
  xsize=px[1]-px[0]
  ysize=py[1]-py[0]
  
  IF xsize GT ysize*aspect THEN xsize=ysize*aspect ELSE ysize=xsize/aspect 
  px[1]=px[0]+xsize
  py[1]=py[0]+ysize

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
  ;; center up the display
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  dcenter, xsize, ysize, px, py, /silent

END 

PRO rgbview, rr, gg, bb, $
             $                  ; control the stretch
             nonlinearity=nonlinearity, $
             alpha=alpha, $
             scales=scales, $   ; scale for each image: 3 elem. array
             sdss=sdss, $       ; set scales for sdss
             $                  ; range in image
             low_cut=low_cut, high_cut=high_cut, $
             $                  ; input sky values (or returned)
             rsky=rsky, gsky=gsky, bsky=bsky, $
             subtract_sky=subtract_sky, $
             nodisplay=nodisplay, $ ; display?
             order=order, $
             prompt=prompt, $  ; should we interact with user for display pars?
             $                  ; Write jpeg or png
             jpegfile=jpegfile, $
             pngfile=pngfile, $
             expand=expand, $   ; should we expand the pixels a bit?
             $                  ; read from the screen? 
             tvread=tvread, $
             $                  ; some of the images used
             imtot=imtot, color_im=color_im, $
             $                  ; color map used for 8-bit
             rmap=rmap, gmap=gmap, bmap=bmap, $
             $                  ; some screen parameters
             xrange=xrange, yrange=yrange, $
             title=title, xtitle=xtitle, ytitle=ytitle, $
             subtitle=subtitle, $
             noframe=noframe, nolabels=nolabels, $
             silent=silent, $
             _extra=extra_key, $
             $                  ; For compatability
             gamma=gamma, saturation=saturation, $
             rsig=rsig, gsig=gsig, bsig=bsig, $
             contrast=contrast, giffile=giffile


  IF n_params() EQ 0 THEN BEGIN
      print,'-Syntax: '
      print,' rgbview, rr, gg, bb, $'
      print,'          $                  ; control the stretch'
      print,'          nonlinearity=nonlinearity, $'
      print,'          alpha=alpha, $'
      print,'          scales=scales, $ ; scale for each image: 3 elem. array'
      print,'          /sdss, $         ; set scales for sdss'
      print,'          $                  ; range in image'
      print,'          low_cut=low_cut, high_cut=high_cut, $'
      print,'          $                  ; input sky values (or returned)'
      print,'          rsky=rsky, gsky=gsky, bsky=bsky, $'
      print,'          /nodisplay,      $ ; display?'
      print,'          /prompt,        $  ; should we interact with user for display pars?'
      print,'          $                  ; Write jpeg or png'
      print,'          jpegfile=jpegfile, $'
      print,'          pngfile=pngfile, $'
      print,'          expand=expand, $ ; factor to expand pixels when writing image'
      print,'          /tvread, $         ; read from the screen?'
      print,'          $                  ; some of the images used'
      print,'          imtot=imtot, color_im=color_im, $'
      print,'          $                  ; color map used for 8-bit'
      print,'          rmap=rmap, gmap=gmap, bmap=bmap, $'
      print,'          $                  ; some screen parameters'
      print,'          xrange=xrange, yrange=yrange, $'
      print,'          title=title, xtitle=xtitle, ytitle=ytitle, $'
      print,'          subtitle=subtitle, $'
      print,'          /noframe, /nolabels, $'
      print,'          /silent, $'
      print,'          _extra=extra_key'
      return
  ENDIF 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Set up parameters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; titles and stuff for axes.
  IF n_elements(title) EQ 0 THEN title = ''
  IF n_elements(xtitle) EQ 0 THEN xtitle=''
  IF n_elements(ytitle) EQ 0 THEN ytitle=''
  IF n_elements(subtitle) EQ 0 THEN subtitle=''

  ;; how to scale the image
  IF n_elements(nonlinearity) EQ 0 THEN nonlinearity = 8
  IF n_elements(alpha) EQ 0 THEN alpha = 0.02
  IF n_elements(scales) EQ 0 THEN BEGIN 
      IF keyword_set(sdss) THEN BEGIN 
          scales = [4.9,5.7,7.8]
      ENDIF ELSE scales = [1.0, 1.0, 1.0]
  ENDIF 
  ;; see how many colors we got
  max_color=!d.n_colors-1
  IF max_color LT 255 AND NOT keyword_set(silent) THEN BEGIN
      print
      print,'Only got ',ntostr(max_color+1),' colors for display'
      print
  ENDIF 

  IF max_color GT 10000000 THEN true_color=1 ELSE true_color=0
  IF ( (n_elements(jpegfile) NE 0) AND keyword_set(tvread) AND $
       (NOT true_color) ) THEN BEGIN 
      message,'You cannot read a jpeg from the display in 8-bit color mode',/inf
      message,'Try using a gif and /tvread'
  ENDIF 
  
  IF ( ( (n_elements(giffile) NE 0) OR (n_elements(jpegfile) NE 0) ) AND $
       keyword_set(tvread) AND keyword_set(nodisplay) ) THEN BEGIN 
      message,'Cannot read from display if /nodisplay is set'
  ENDIF 

  ;; prompting
  IF NOT keyword_set(prompt) THEN prompt = 0

  ;; Check current device
  ;; treat X and Z the same
  IF (!d.name EQ 'X') OR (!d.name EQ 'Z') THEN doX = 1 ELSE doX = 0
  
  ;; Check size of arrays
  szr = size(rr)
  szg = size(gg)
  szb = size(bb)

  IF (szr[4] NE szg[4]) OR (szr[4] NE szb[4]) THEN BEGIN
      print,'Arrays must be of same size'
      return
  ENDIF 

  nx = szr[1]
  ny = szr[2]
  aspect = float(nx)/ny

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Create a floating point image, and do the sky subtraction if needed (default
; is to assume it's already sky-subtracted
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF keyword_set(subtract_sky) THEN BEGIN 
      IF n_elements(rsky) EQ 0 THEN rsky = median(rr)
      IF n_elements(gsky) EQ 0 THEN gsky = median(gg)
      IF n_elements(bsky) EQ 0 THEN bsky = median(bb)

      IF NOT keyword_set(silent) THEN BEGIN 
          print
          print,'subtracting rsky ',rsky
          print,'subtracting gsky ',gsky
          print,'subtracting bsky ',bsky
      ENDIF 

      ;; sky subtract and convert to float
      red  = float(scales[0])*(rr - rsky)
      grn  = float(scales[1])*(gg - gsky)
      blue = float(scales[2])*(bb - bsky)
  ENDIF ELSE BEGIN 
      red  = float(scales[0])*rr
      grn  = float(scales[1])*gg
      blue = float(scales[2])*bb
  ENDELSE 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; byte scale each of the images
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  continue = 1

  WHILE continue DO BEGIN 

      rgbview_scalergb, red, grn, blue, imtot, $
        nonlinearity=nonlinearity, $
        alpha=alpha, $
        silent=silent, low=low_cut, high=high_cut

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Create pseudo color image for display
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      IF (max_color LE 255) THEN BEGIN 
          color_im = color_quan(imtot, 1, rmap, gmap, bmap)
      ENDIF 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Output to proper device (unless nodisplay)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      IF NOT keyword_set(nodisplay) THEN BEGIN 

;          rgbview_setupplot, nx, ny, aspect, xsize, ysize, $
;            px, py, xrng, yrng
          
          implot_setup, red, xsize, ysize, px, py, xrng, yrng, /center

          IF doX THEN BEGIN     ;X window
              IF max_color LE 255 THEN BEGIN ;; 8-bit display
                  tv, congrid(color_im, xsize, ysize), px[0],py[0], $
                    order=order
                  tvlct, rmap, gmap, bmap
                  pos = [px[0], py[0], px[1], py[1]]
              ENDIF ELSE BEGIN 
                  tv, congrid(imtot, 3, xsize, ysize),true=1,px[0],py[0], $
                    order=order
                  pos = [px[0], py[0], px[1], py[1]]
              ENDELSE 
         ENDIF ELSE BEGIN      ;Postscript
              device,/color
              tvlct,indgen(256),indgen(256),indgen(256)

              pos = [px[0], py[0], px[1], py[1]]
              tv, imtot,true=1,px[0],py[0], xsize=xsize, ysize=ysize, $
                /device, order=order
          ENDELSE 

          IF keyword_set(noframe) OR keyword_set(nolabels) THEN BEGIN 
              plot, [0,0], [0,0], xstyle=5, ystyle=5, $
                title=title,xtitle=xtitle,ytitle=ytitle, subtitle=subtitle, $
                xrange=xrng, yrange=yrng, position=pos, $
                /noerase, /device, /nodata
          ENDIF ELSE BEGIN 
              plot, [0,0], [0,0], xstyle=1, ystyle=1, $
                title=title,xtitle=xtitle,ytitle=ytitle, subtitle=subtitle, $
                xrange=xrng, yrange=yrng, position=pos,$
                /noerase, /device, /nodata
          ENDELSE 
          
          IF (NOT keyword_set(noframe)) AND keyword_set(nolabels) THEN BEGIN 
              axis,xaxis=1,xtickname=strarr(10)+" "
              axis,xaxis=0,xtickname=strarr(10)+" "
              axis,yaxis=1,ytickname=strarr(10)+" "
              axis,yaxis=0,ytickname=strarr(10)+" "
          ENDIF  

          IF (prompt) AND doX THEN BEGIN 
              print
              ans = ' '
              print,format='($, "Command: (n nonlinearity) (a alpha)")'
              read,ans
              ans = STRLOWCASE(ntostr(ans,1,0))
              CASE ans OF 
                  'n': BEGIN
                          print,format='($, "New nonlinearity")'
                          read, nonlinearity
                          nonlinearity = nonlinearity > 0
                       END 
                  'a': BEGIN
                          print,format='($, "New alpha")'
                          read, alpha
                          alpha = alpha > 0.
                       END
                  ELSE: continue = 0
              ENDCASE 
          ENDIF ELSE continue = 0 
      ENDIF ELSE continue = 0; nodisplay

  ENDWHILE 
  IF NOT keyword_set(silent) THEN print

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; output the image if requested
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF n_elements(jpegfile) NE 0 THEN BEGIN 

      IF n_elements(quality) EQ 0 THEN qual=75 ELSE BEGIN
          qual = quality < 100 > 0
      ENDELSE 

      IF NOT keyword_set(silent) THEN BEGIN 
          print
          print,'Writing jpeg file: ',jpegfile
          print,'Using quality: ',ntostr(qual)
      ENDIF 
      IF keyword_set(tvread) THEN BEGIN
          write_jpeg,jpegfile,tvrd(true=1),true=1,quality=qual
      ENDIF ELSE BEGIN

          IF n_elements(expand) NE 0 THEN BEGIN 
              expand = fix(expand)

              ;; /sample just replicates 
              imtot2 = rebin( imtot, 3, expand*nx, expand*ny , /sample)
              write_jpeg,jpegfile,imtot2,true=1,quality=qual, order=order
          ENDIF ELSE BEGIN 
              write_jpeg,jpegfile,imtot,true=1,quality=qual, order=order
          ENDELSE 
      ENDELSE 
  ENDIF 

  IF n_elements(pngfile) NE 0 THEN BEGIN 

      IF NOT keyword_set(silent) THEN print,'Writing png file: ',pngfile

      ;; get image from display?
      IF keyword_set(tvread) THEN BEGIN 
          IF true_color THEN BEGIN 
              tmp = tvrd(true=1)
              
              IF float(!version.release) LT 5.4 THEN BEGIN 
                  ;; In old version png is written out flipped
                  tmp[0,*,*] = rotate(rotate(reform(tmp[0,*,*]),1),4)
                  tmp[1,*,*] = rotate(rotate(reform(tmp[1,*,*]),1),4)
                  tmp[2,*,*] = rotate(rotate(reform(tmp[2,*,*]),1),4)
              ENDIF 

              write_png, pngfile, tmp
          ENDIF ELSE BEGIN 
              write_png, pngfile, tvrd(), rmap, gmap, bmap
          ENDELSE 
      ENDIF ELSE BEGIN 

          IF float(!version.release) LT 5.4 THEN BEGIN 
              ;; In old version png is written out flipped
              imtot[0,*,*] = rotate(rotate(reform(imtot[0,*,*]),1),4)
              imtot[1,*,*] = rotate(rotate(reform(imtot[1,*,*]),1),4)
              imtot[2,*,*] = rotate(rotate(reform(imtot[2,*,*]),1),4)
          ENDIF 
          
          IF n_elements(expand) NE 0 THEN BEGIN 
              expand = fix(expand)
              
              ;; /sample just replicates 
              imtot2 = rebin( imtot, 3, expand*nx, expand*ny , /sample)
              write_png,pngfile,imtot2
          ENDIF ELSE BEGIN 
              write_png,pngfile,imtot
          ENDELSE 

          IF float(!version.release) LT 5.4 THEN BEGIN 
              ;; In old version png is written out flipped
              imtot[0,*,*] = rotate(rotate(reform(imtot[0,*,*]),1),4)
              imtot[1,*,*] = rotate(rotate(reform(imtot[1,*,*]),1),4)
              imtot[2,*,*] = rotate(rotate(reform(imtot[2,*,*]),1),4)
          ENDIF 
          
      ENDELSE 

  ENDIF 

return
END 
