
;+
;
; NAME: 
;    mark_radec
;       
; PURPOSE: 
;    Marks an ra and dec position on the current display using the input
;    mapping structure.  The default mark is a cross centered on the
;    position, with a hole at the center
;
; CALLING SEQUENCE: 
;    mark_radec, mapStruct, radius, ra, dec
;      
;                 
;
; INPUTS: mapStruct: Structure containing the info needed to map ra/dec to x-y
;            position.  The map structure is created using the CREATE_PHOTOMAP
;            procedure.  procedure.  
;         radius: radius of the mark in data coordinates.
;         ra, dec: the ra and dec to be circled.
;
; INPUT KEYWORD PARAMETERS:
;         /cross: use a cross to mark the ra/dec.
;         hole_fraction: leave a central hole in the cross with this fraction 
;                   of radius.
;
;         /box: use a box instead of a circle.  radius will be side of box.
;
;         /ellipse: use an ellipse. Largest side of ellipse is set to radius.
;         axis_ratio: The axis ratio of the ellipse
;         pos_angle: position angle of ellipse in degrees.
;
;         order=: order=0 (default) means images was plotted 0,0 in bottom
;                 left,  order=1 is top left.  Note objx,objy
;                 is still given in the original coord. system. User must input
;                 ny for the transformation.
;         ny=: number of pixels in the y-direction.
;         /silent: Shut off the messages except errors.
;
;       
; OUTPUTS: A mark is drawn on the current device.
;
; EXAMPLE:
;  map_order = 1
;  create_photomap, $
;    map_order, $
;    pstruct.ra, pstruct.dec, pstruct.rowc[2], pstruct.colc[2], mapStruct
;
;  ;.....plot some image
;  xoffset = objx - pstruct[id].colc[2]
;  yoffset = objy - pstruct[id].rowc[2]
;
;  mark_radec, mapStruct, ra, dec, 30.0, $
;     /cross, holeradius = 10.0, $
;     xoffset=xoffset, yoffset=yoffset
;
; CALLED ROUTINES: 
;    CREATE_PHOTOMAP
;    TVCIRCLE
;    TVBOX
;    PLOT_CROSS
;
; REVISION HISTORY:
;	Author: Erin Scott Sheldon UChicago  18-Oct-2004
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


PRO mark_radec_syntax

  print,'-Syntax: mark_radec, mapStruct, radius, ra, dec, '
  print,'                 -- OR --'
  print,'         mark_radec, mapStruct, markStruct, '
  print,'            $'
  print,'            $               ; To shift from row/col'
  print,'            xoffset=xoffset, yoffset=yoffset, $'
  print,'            $               ; a code for mark style'
  print,'            mark_type=mark_type, $'
  print,'            $               ; cross'
  print,'            cross=cross, $'
  print,'            hole_fraction=hole_fraction, $'
  print,'            $               ; box'
  print,'            box=box, $'
  print,'            $               ; circle'
  print,'            circle=circle, $'
  print,'            $               ; ellipse'
  print,'            ellipse=ellipse, $'
  print,'            axis_ratio=axis_ratio, $'
  print,'            pos_angle=pos_angle, $'
  print,'            $               ; image order'
  print,'            order=order, $'
  print,'            ny=ny, $'
  print,'            silent=silent, $'
  print,'            $               ; an added shift for display'
  print,'            xshift=xshift, yshift=yshift, $'
  print,'            _extra=_extra'
  print,''
  print,'Use doc_library,"mark_radec"  for more help.'  
  return

END 

PRO mark_radec, mapStruct, radius_OR_markStruct, ra, dec, $
                $               ; To shift from row/col
                xoffset=xoffset, yoffset=yoffset, $
                $               ; a code for mark style
                mark_type=mark_type, $
                $               ; cross
                cross=cross, $
                hole_fraction=hole_fraction, $
                $               ; box
                box=box, $
                $               ; circle
                circle=circle, $
                $               ; ellipse
                ellipse=ellipse, $
                axis_ratio=axis_ratio, $
                pos_angle=pos_angle, $
                color=color, $
                linestyle=linestyle, $
                $               ; image order
                order=order, $
                ny=ny, $
                silent=silent, $
                $               ; an added shift for display
                xshift=xshift, yshift=yshift, $
                _extra=_extra

  np = n_params()
  IF (np LT 2) THEN BEGIN 
      mark_radec_syntax
      return
  ENDIF ELSE BEGIN 
      rd = size(radius_OR_markStruct, /tname)
      IF rd EQ 'STRUCT' THEN BEGIN 
          markStruct = radius_OR_markStruct

          markstruct_extract, $
            markStruct, $
            mark_radius=radius, $
            ra=ra, dec=dec, $
            mark_type=mark_type, $
            hole_fraction=hole_fraction, $
            axis_ratio=axis_ratio, $
            pos_angle=pos_angle, $
            color=color, $
            linestyle=linestyle

      ENDIF ELSE BEGIN 
          IF np NE 4 THEN BEGIN 
              mark_radec_syntax
              return
          ENDIF 
          radius = radius_OR_markStruct
      ENDELSE 
  ENDELSE 

  nra = n_elements(ra)
  ndec = n_elements(dec)

  IF nra NE ndec THEN BEGIN 
      message,'ra, dec must be same size'
  ENDIF 

  IF NOT keyword_set(silent) THEN silent = 0

  IF n_elements(xoffset) EQ 0 THEN xoffset=0
  IF n_elements(yoffset) EQ 0 THEN yoffset=0

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; By default, the shift is -0.5.
  ;; This is because the display programs we use take the *center* of
  ;; the pixel as the origin
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF n_elements(xshift) EQ 0 THEN xshift = -0.5
  IF n_elements(yshift) EQ 0 THEN yshift = -0.5

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Use the mapping to convert ra/dec to row/col.  Then shift the row/col
  ;; to the actual image coordinates using the offsets
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; map ra/dec to row/col
  rd2xy, ra, dec, mapStruct.astr_struct, x1, x2
  kmap, x1, x2, row, col, mapStruct.kx, mapStruct.ky

  ;; map row/col to x/y on the image
  xc = col + xoffset
  yc = row + yoffset

  ;; What is the image order?
  IF n_elements(order) NE 0 THEN BEGIN 
      IF order[0] EQ 1 THEN BEGIN 
          IF n_elements(ny) EQ 0 THEN message,'Must input ny if order= is sent'
      
          yc = (ny-1) - yc
      ENDIF 
  ENDIF 

  xc = xc + xshift
  yc = yc + yshift

  markxy, radius, xc, yc, $
    $                           ; a code for mark style
    mark_type=mark_type, $
    $                           ; cross (default)
    cross=cross, $
    hole_fraction=hole_fraction, $
    box=box, $
    $                           ; circle
    circle=circle, $
    $                           ; ellipse
    ellipse=ellipse, $
    axis_ratio=axis_ratio, $
    pos_angle=pos_angle, $
    color=color, $
    linestyle=linestyle, $
    _extra=_extra


return
end
























