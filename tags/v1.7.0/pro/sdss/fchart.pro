PRO fchart, pstruct, index, radius, clr, fchart, dir=dir, $
            shift2r=shift2r, $
            objx=objx, objy=objy, impos=impos, silent=silent, $
            maxsize=maxsize, nonoise=nonoise, useind=useind
  

;+
; NAME: 
;    FCHART
;       
; PURPOSE: 
;    Create a finding chart for an SDSS object.
;	
; CALLING SEQUENCE: 
;
;    fchart, pstruct, index, radius, clr, fchart, dir=dir, 
;            /shift2r, 
;            objx=objx, objy=objy, impos=impos, silent=silent, 
;            nonoise=nonoise, useind=useind
;
; COMMENTS:  The wrapper obj_info is a very convenient way to use FCHART
;            since it makes a nice display.
;
; INPUTS: 
;    
;    pstruct:  A photo structure.
;              IMPORTANT NOTE: This MUST have certain tags, such as
;              rowc,colc,field,id,parent,nchild
;
;    index:    The index of the object in the photo structure which needs
;              a finding chart.
;    radius:   The box which the object is in will have sides 2*radius
;              unless radius is too big. (e.g. bigger that 2048 pixels)
;    clr:      The color used to make the finding chart. This follows the
;              photo convention: 0,1,2,3,4 -> u,g,r,i,z
;
; INPUT KEYWORD PARAMETERS:
;         dir:  The directory to find the atlas images.
;         /shift2r: Perform sub-pixel shifts based on astrometry to align the
;                   atlas images with the r-band.  For some reason, only
;                   integer shifts were stored in the atlas images.  This slows
;                   things down only a small amount.
;         /silent: If silent is set, nothing is printed except some errors
;         /nonoise: if set, no noise is added to image.
;         maxsize: deprecated
;         
; OUTPUTS: 
;
;    fchart:  The image of the finding chart. 
;
; OPTIONAL OUTPUTS: 
;    objx: The x-position of the input object in pixels in image
;               coordinates (as opposed to photo [colc,rowc]
;    objy: Same for y.
;    impos: Absolute position of left hand corner of image as defined
;           by this equation:
;           impos[0] = colc 
;           impos[1] = rowc + 1361.*(field - fieldmin)
;
; CALLED ROUTINES: 
;
;    GET_FAMILY
;    GET_ATLAS
; 
; PROCEDURE: 
;  Create a finding chart around an input object from all the objects nearby.
;  The trick is to use atlas images of objects in such a way that flux is
;  only added once.  I choose nchild == 0 to do this.
;	
;  From here, the atlas image of each good neighbor is the proper color is 
;  found and placed within the appropriate box.  The registration is determined
;  by comparing positions in the different colors.
;
;  Note the box may not center on the main object if it is less that 'radius'
;  from either the edge of the frame in the column direction. This is also, 
;  true if it is near either end of the series of frames read into pstruct.
;
;
; REVISION HISTORY: 
;     Author  Erin Scott Sheldon UM 03/21/99
;     Dave Johnston - was adding way too much noise
;        to background in some cases , now it just adds a 
;        trace amount of noise to background 
;        sky rms sqrt(30)  5/15/99	
;     Now allows objects with center outside image to 
;     contribute light to the image. Object centers must
;     be within maxsize.  14-NOV-2000
;     30-Aug-2002: Better way to calculate positions: just use r-band
;         positions and translate the other bands to r-band using the
;         bottom-left corners of the atlas images.
;     11-Sep-2002: 
;         -Fixed bug where light from some objects was added
;          twice if the objects spanned more than one field.  
;         -Fixed bug where some objects were not being used.
;                -- E.S.S.
;         -Added optional sub-pixel shifting, so can get better alignment with
;         the r-band (for color images..)
;     23-Feb-2005:  Use new read_atlas routine.  E.S.S.
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

  

  IF N_params() LT 4 THEN BEGIN 
      print,'-Syntax: fchart, pstruct, index, radius, clr, fchart, '
      print,'         dir=dir, '
      print,'         /shift2r, '
      print,'         objx=objx, objy=objy, impos=impos, silent=silent,'
      print,'         maxsize=maxsize, nonoise=nonoise, useind=useind'
      print,''
      print,'Use  doc_library,"fchart" for more help.'
      return
  ENDIF 
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; check some keywords, set parameters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  sdssidl_setup, /silent
  colors=['u','g','r','i','z']
  index = index[0]

  sky = 0L

  ;; this tells us not to add flux twice
  ;; old reruns don't have this set
  DUPLICATE = 2L^2

  IF (clr LT 0) OR (clr GT 4) THEN BEGIN 
      print,'Color index must be in [0,4]'
      return
  ENDIF 

  ;; Should we shift the image so it will align better (at sub-pixel
  ;; level) with the r-band?  This is important for color images

  IF keyword_set(shift2r) AND clr NE 2 THEN BEGIN 

      trans = sdss_read('astrans',$
                        pstruct[index].run,  pstruct[index].camcol, $
                        bandpass=clr, rerun=pstruct[index].rerun, $
                        node=node, inc=inc, /silent)
      rtrans = sdss_read('astrans',$
                         pstruct[index].run,  pstruct[index].camcol, $
                         bandpass=2, rerun=pstruct[index].rerun, $
                         node=rnode, inc=rinc, /silent)

      IF NOT keyword_set(silent) THEN $
        print,'Doing sub-pixel shifts to align with r-band'
      doshift=1 
  ENDIF ELSE doshift=0

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; the object
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  object = pstruct[index]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; make sure picture area as defined by radius does not extend  ;;;
; beyond the bounds of sloan frame in the column direction     ;;;
; Also, since all we have is pstruct, we must make sure that   ;;;
; rows are within bounds set by pstruct                        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; make an absolute variable rrowc from which to calculate
  ;; positions
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; use r-band positions
  usecolor = 2

  fieldmin = 0L
  rrowc = pstruct.rowc[usecolor]
  rrowc = rrowc + 1361.*(pstruct.field - fieldmin)

  rcolc = pstruct.colc[usecolor]

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; This method of calculating the minrow/maxrow is not that good, it
  ;; will definitely be too large/small.  But it should only mattter
  ;; in the first or last frame where radius will go out of the bounds
  ;; Note the 64 is due to overlaps
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  tmp1=min(rrowc, max=tmp2)
  f=object.field
  
  CASE 1 OF
      (f EQ min(pstruct.field)) : BEGIN
          offset=rrowc[index] - tmp1
          IF (NOT keyword_set(silent)) THEN BEGIN 
              print
              print,'OFFSET AT BEGINNING: ',ntostr(offset)
          ENDIF 
          minrow=tmp1 - offset
          maxrow=tmp2
      END
      (f EQ max(pstruct.field)) : BEGIN
          offset = (1489 - 1) - object.rowc[usecolor]
          IF (NOT keyword_set(silent)) THEN BEGIN
              print
              print,'OFFSET AT END: ',ntostr(offset)
          ENDIF 
          maxrow = tmp2 + offset
          minrow=tmp1
      END
      ELSE: BEGIN 
          minrow=tmp1
          maxrow=tmp2
      END
  ENDCASE
  
  maxcol = 2048.-1  ;; note mincol is just 0

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; may have to trim down radius if its too big
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;  radius = long(radius)
  r2 = rnd(2*radius)
  diffrow=maxrow-minrow
  IF (r2 GE diffrow) THEN BEGIN 
      print,'Radius too big.  Trimming'
      radius=diffrow/2
      r2=rnd(2*radius)
  ENDIF 
  IF (r2 GE maxcol) THEN BEGIN 
      print,'Radius*2 is larger than field width.  Trimming.'
      radius=(maxcol+1)/2; + 1
      r2=rnd(2*radius)
  ENDIF 
  
  ;;; absolute position of object  ;;;
  objpos = [rcolc[index], rrowc[index]]

  ;;; objects initial relative position in image 
  objx = radius-.5
  objy = radius-.5

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; find center of the image.  Begins with object centered.
  ;; Check columns(x) first. Note center and relative position
  ;; of object may change
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  center=objpos
  diff=objpos[0]-radius
  IF (diff LT 0) THEN BEGIN 
      center[0]=center[0] - diff
      objx = objx + diff
  ENDIF ELSE BEGIN 
      diff=maxcol - (objpos[0] + radius)
      IF (diff LT 0) THEN BEGIN 
          center[0]=center[0] + diff
          objx = objx - diff
      ENDIF 
  ENDELSE 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Check rows (y) 
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  diff=(objpos[1]-radius) - minrow
  IF (diff LT 0) THEN BEGIN 
      center[1]=center[1] - diff
      objy=objy + diff
  ENDIF ELSE BEGIN 
      diff=maxrow - (objpos[1] + radius)
      IF (diff LT 0) THEN BEGIN 
          center[1]=center[1] + diff
          objy = objy - diff
      ENDIF 
  ENDELSE 

;  objx=objx-1.
;  objy=objy-1.

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Bottom left corner in absolute frame. This  will be the origin 
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  lcorner = [center[0] - radius, center[1] - radius]

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; the region covered by the image
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  col = [center[0] - radius, center[0] + radius]
  row = [center[1] - radius, center[1] + radius]
  impos = [col[0], row[0]]

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Print out stuff
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  id = ntostr(object.id)
  IF (NOT keyword_set(silent)) THEN BEGIN 
      print,''
      print,'-----------------------------'
      print,'Building Finding Chart From ',$
            colors[clr],' Images For Object id ',id
      strr2=ntostr(r2)
      print,'Chart size: [',strr2,',',strr2,']'
      print,'Object center is at: [',ntostr(long(objx)),',',$
            ntostr(long(objy)),']'
      print,'-----------------------------'
      
  ENDIF 

  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Define the finding chart
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  fchart = replicate(sky,r2,r2)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; select stuff within box of side 2*radius = r2. Centers can be
  ;; within some distance of the bounding box.  This distance is 
  ;; determined by how big we think the object is. 
  ;; 
  ;; Throw away parents
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;  max_cdist = maxsize[0]
;  max_rdist = maxsize[1]

  max_cdist = 500
  max_rdist = 500
  useind = where( pstruct.run EQ object.run AND $
                  rcolc LE (col[1]+max_cdist) AND $
                  rcolc GE (col[0]-max_cdist) AND $
                  rrowc LE (row[1]+max_rdist) AND $
                  rrowc GE (row[0]-max_rdist) AND $
                  pstruct.nchild EQ 0, count )

  keep = bytarr(count)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;  Find the atlas images of objects (see documentation above)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF (count NE 0) THEN BEGIN 

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;  Get the atlas images and place them in chart
      ;;  NOTE: Speed of this is limitied by get_atlas 
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      IF (NOT keyword_set(silent)) THEN BEGIN 
          printstr = 'Checking '+ntostr(count)+' neighbors'
          print,printstr
      ENDIF 
      
      FOR i=0, count-1 DO BEGIN 
          IF ( ( (i EQ 0) OR (i EQ count-1) OR  ((i+1) MOD 40) EQ 0 ) $
               AND NOT keyword_set(silent)  ) THEN print,'.',format='(a,$)'
          gi=useind[i]

          ;; Need field for astrans info
          field_current = pstruct[gi].field

          read_atlas, pstruct, index=gi, clr=clr, image=im, $
               col0=col0, row0=row0, dcol=dcol, drow=drow,/silent
 
          IF n_elements(im) EQ 0 THEN GOTO,jump

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; find sub-pixel offsets from r-band.  The info in the atlas is
          ;; close, but no cigar. For some reason Robert only stored the shifts
          ;; as integers.
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

          IF doshift THEN BEGIN 
              ra = pstruct[gi].ra
              dec = pstruct[gi].dec

              eq2rowcol, trans,  node,  inc,  field_current, ra, dec, $
                trow,  tcol
              eq2rowcol, rtrans, rnode, rinc, field_current, ra, dec, $
                trrow, trcol
          
              adcol = tcol - trcol
              adrow = trow - trrow

              dcolshift = (dcol[clr] - adcol)
              drowshift = (drow[clr] - adrow)

              IF (abs(dcolshift) LT 2) AND (abs(drowshift) LT 2) THEN BEGIN 
                  im = fshift(im,dcolshift,drowshift)
              ENDIF 
          ENDIF 

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; Get r-band position of bottom left corner of atlas 
          ;; in its field (col0 same for field and absolute coord) 
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

          row0 = row0[2]
          col0 = col0[2]

          ;; Convert to absolute position
          rowold = row0
          row0 = row0 + 1361.*(pstruct[gi].field - fieldmin)

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; put row0 and col0 in our image coordinates 
          ;; lcorner is the absolute position of the 
          ;; bottom left hand corner of our image  
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

          im_col0 = long(col0 - lcorner[0])
          im_row0 = long(row0 - lcorner[1])

          sz = size(im)
          imcols = sz[1]
          imrows = sz[2]

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; The pixels of atlas image in field coords
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

          imy = lindgen(imrows) + rowold

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; The pixels of atlas image in fchart coords 
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

          x = lindgen(imcols) + im_col0
          y = lindgen(imrows) + im_row0

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; Positions which are acutally in fchart and
          ;; which don't spill over into another frame
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

          wx = where( x GT 0 AND x LT r2, nwx)
          wy = where( y GT 0 AND y LT r2,nwy)

          IF (nwx NE 0) AND (nwy NE 0) THEN BEGIN 

              keep[i] = 1

              maxwx=max(wx)
              minwx=min(wx)
              maxwy=max(wy)
              minwy=min(wy)

              maxx=x[maxwx]
              minx=x[minwx]
              maxy=y[maxwy]
              miny=y[minwy]

              ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
              ;; Place the atlas images. 
              ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

              ;; copy into fchart
              fchart[minx:maxx,miny:maxy] = $
                fchart[minx:maxx,miny:maxy] + im[minwx:maxwx, minwy:maxwy]
                
          ENDIF 
          delvarx,im
          jump:
      ENDFOR 
      IF (NOT keyword_set(silent)) THEN BEGIN
          print
          print,'-----------------------------'
      ENDIF 

  ENDIF ELSE BEGIN ;;;; if count was zero, then no objects were placed! ;;;
      print, 'No objects were found.  Finding Chart is empty!'
  ENDELSE 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Add noise (if requested) and correct saturation, etc
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;  w=where(fchart EQ 0, nw)
;  IF nw NE 0 THEN fchart[w] = 1000

  nw=0L
  IF NOT keyword_set(nonoise) THEN BEGIN 
      w = where(fchart EQ sky, nw) ;Places to add noise

      ;; Add noise for sky of about 30.
      IF nw NE 0 THEN BEGIN
          fchart[w] = fchart[w] + sqrt( 30.0 )*randomn(seed,nw)
      ENDIF 
  ENDIF 

;  l = where(fchart LT 0, nl)
;  IF nl NE 0 THEN fchart[l] = 32767

  wkeep = where(keep,nw)
  IF nw EQ 0 THEN useind=-1 ELSE useind = useind[ wkeep ]

  return
END 
