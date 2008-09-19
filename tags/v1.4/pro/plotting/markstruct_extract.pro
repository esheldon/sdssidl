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
FUNCTION markStruct_extract_getcolor, color_array

  ncolor = n_elements(color_array)
  color = lonarr(ncolor)
  IF ncolor EQ 1 THEN color=0L ELSE color = lonarr(ncolor)

  FOR i=0L, ncolor-1 DO BEGIN 
      comm = 'color['+ntostr(i)+'] = !'+color_array[i]
      IF NOT execute(comm) THEN BEGIN 
          message,'Could not get color: '+colorString
      ENDIF 
  ENDFOR 

  return,color

END 

PRO markStruct_extract, markStruct, $
                        mark_radius=radius, $
                        x=x, $
                        y=y, $
                        ra=ra, $
                        dec=dec, $
                        mark_type=mark_type, $
                        hole_fraction=hole_fraction, $
                        axis_ratio=axis_ratio, $
                        pos_angle=pos_angle, $
                        color=color, $
                        linestyle=linestyle
  
  IF tag_exist(markStruct,'radius') THEN radius = markStruct.radius

  IF tag_exist(markStruct,'x') THEN x = markStruct.x
  IF tag_exist(markStruct,'y') THEN y = markStruct.y

  IF tag_exist(markStruct,'ra') THEN ra = markStruct.ra
  IF tag_exist(markStruct,'dec') THEN dec = markStruct.dec

  ;; The markstruct always takes precedence
  IF tag_exist(markStruct,'type') THEN mark_type = markStruct.type
  IF tag_exist(markStruct,'hole_fraction') THEN $
    hole_fraction = markStruct.hole_fraction
  IF tag_exist(markStruct,'axis_ratio') THEN $
    axis_ratio = markStruct.axis_ratio
  IF tag_exist(markStruct,'pos_angle') THEN pos_angle = markStruct.pos_angle

  IF tag_exist(markStruct,'color') THEN $
    color = markstruct_extract_getcolor(markStruct.color)
  IF tag_exist(markStruct,'linestyle') THEN linestyle = markStruct.linestyle

END 
