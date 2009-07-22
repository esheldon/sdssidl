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

FUNCTION markstruct_mark_radec_getcolor, colorString

  comm = 'color = !'+colorString
  IF NOT execute(comm) THEN BEGIN 
      message,'Could not get color: '+colorString
  ENDIF 

  return,color

END 

PRO markstruct_mark_radec_checktags, markStruct

  tags = tag_names(markStruct)

  match, tags, ['RA', 'DEC'], mtags, mradec

  IF n_elements(mtags) NE 2 THEN BEGIN 
      message,'markStruct must contain ra/dec '
  ENDIF 

END 

PRO markstruct_mark_radec, image, mapStruct, markStruct, $
                           xoffset=xoffset, yoffset=yoffset, $
                           order=order, cfac=cfac, $
                           xshift=xshift, yshift=yshift

  COMMON markstruct_block, $
    defMarkRA, defMarkDEC, defMarkX, defMarkY, $
    defMarkType, defMarkHole_Fraction, defMarkLineStyle, $
    defMarkRadius, defMarkColor, $
    defaultMarkStruct

  ;; Make sure the tags we absolutely need input are already there
  markstruct_mark_radec_checktags, markStruct

  ;; Copy in defaults for the tags that were not input
  markstruct_copy_defaults, markStruct

  ;; Some statistics
  nMark = n_elements(markStruct.ra)
  imsize = size(image)
  ny = imsize[2]
  ss = imsize[1]

  defrad = ss/10.0

  IF n_elements(cfac) EQ 0 THEN cfac=1

  ;; radius
  ;; Checking this only works if the defaults have been defined!
  w=where(markStruct.radius EQ defMarkRadius,nw)
  IF nw NE 0 THEN BEGIN
      IF nw EQ n_elements(markStruct.radius) THEN BEGIN 
          IF (nMark-1) GT 0 THEN BEGIN 
              markRadius = [defrad, replicate(defrad/2.0,nMark-1)]
          ENDIF ELSE BEGIN 
              markRadius = defrad
          ENDELSE 
      ENDIF
  ENDIF ELSE BEGIN 
      ;; Convert radius to pixels if needed
      markRadius = markStruct.radius*cfac
  ENDELSE 
  

  ;; This is because the display programs end up shifting the data 
  ;; coords by 0.5

  FOR i=0L, nMark-1 DO BEGIN 

      color = markstruct_mark_radec_getcolor(markStruct.color[i])

      IF markStruct.x[i] NE defMarkX THEN BEGIN 

          IF n_elements(xshift) EQ 0 THEN xshift = -0.5
          IF n_elements(yshift) EQ 0 THEN yshift = -0.5

          markx = markStruct.x[i]
      
          IF keyword_set(order) THEN BEGIN 
              marky = (ny-1) - markStruct.y[i]
          ENDIF ELSE BEGIN 
              marky = markStruct.y[i]
          ENDELSE 

          markx = markx + xshift
          marky = marky + yshift

          markxy, $
            markRadius[i], markx, marky, $
            mark_type = markStruct.type[i], $
            hole_fraction=markStruct.hole_fraction[i], $
            color=color, linestyle=markStruct.linestyle[i], $
            $
            order=order, ny=ny
          
          beg = 1L
          
      ENDIF ELSE BEGIN 
          
          mark_radec, $
            mapStruct, $
            markRadius[i], $
            markStruct.ra[i], markStruct.dec[i],$
            $
            xoffset=xoffset, yoffset=yoffset, $
            $
            mark_type = markStruct.type[i], $
            hole_fraction=markStruct.hole_fraction[i], $
            color=color, linestyle=markStruct.linestyle[i], $
            xshift = xshift, yshift=yshift, $
            $
            order=order, ny=ny

      ENDELSE 
  ENDFOR 


END 
