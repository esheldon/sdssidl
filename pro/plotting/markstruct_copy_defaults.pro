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
PRO markstruct_copy_defaults, markStruct, invbw=invbw

  COMMON markstruct_block, $
    defMarkRA, defMarkDEC, defMarkX, defMarkY, $
    defMarkType, defMarkHoleFraction, defMarkLineStyle, $
    defMarkRadius, defMarkAxisRatio, defMarkPosAngle, defMarkColor, $
    defaultMarkStruct

  IF n_elements(defaultMarkStruct) EQ 0 THEN BEGIN 
      
      defMarkRA   = -9999d
      defMarkDEC  = -9999d
      defMarkX = -9999d
      defMarkY = -9999d
      defMarkType = 'cross'
      defMarkRadius = -9999.
      defMarkAxisRatio = 1
      defMarkPosAngle = 0
      defMarkHoleFraction = 1.0/3.0
      defMarkColor = 'p.color'
      defMarkLineStyle = 0


      defaultMarkStruct = {                                       $
                            ra:   defMarkRA,                      $
                            dec:  defMarkDEC,                     $
                            X: defMarkX,                          $
                            Y: defMarkY,                          $
                            type: defMarkType,                    $
                            radius: defMarkRadius,                $
                            axis_ratio:defMarkAxisRatio,          $
                            pos_angle:defMarkPosAngle,            $
                            hole_fraction: defMarkHoleFraction,   $
                            color: defMarkColor,                  $
                            linestyle: defMarkLineStyle           $
                          }
  ENDIF 
  
  ;; A form of the mark struct is input. Copy input tags
  IF n_elements(markStruct) NE 0 THEN BEGIN 
      
      ;; check for existing tags
      defTags = tag_names(defaultMarkStruct)
      tags = tag_names(markStruct)

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; If ra/dec are not input, it is assumed that there is only 
      ;; one mark. 
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      wTagRA=where(tags EQ 'RA', nTagRA)
      IF nTagRA EQ 0 THEN nMark = 1 ELSE nMark = n_elements(markStruct.ra)

      match, tags, defTags, m, md
      nMatch = n_elements(md)
      nUnMatched = n_elements(defTags) - nMatch
      IF nUnMatched GT 0 THEN BEGIN 

          remove, md, defTags
          FOR i=0L, nUnmatched-1 DO BEGIN 
              CASE defTags[i] OF
                  'RA': addval = replicate(defMarkRA, nMark)
                  'DEC': addval = replicate(defMarkDEC, nMark)
                  'X': addval = replicate(defMarkX, nMark)
                  'Y': addval = replicate(defMarkY, nMark)
                  'TYPE': addval = replicate(defMarkType, nMark)
                  'RADIUS': addval = replicate(defMarkRadius, nMark)
                  'AXIS_RATIO': addval = replicate(defMarkAxisRatio, nMark)
                  'POS_ANGLE': addval = replicate(defMarkPosAngle, nMark)
                  'HOLE_FRACTION': BEGIN 
                      addval = replicate(defMarkHoleFraction, nMark)
                  END 
                  'COLOR': addval = replicate(defMarkColor, nMark)
                  'LINESTYLE': addval = replicate(defMarkLinestyle, nMark)
                  ELSE: message,'unknown tag '+defTags[i]
              ENDCASE 
              IF nMark EQ 1 THEN addval = addval[0]

              add_tag, temporary(markStruct),defTags[i],addval,markStruct

          ENDFOR 
      ENDIF 
          
  ENDIF ELSE BEGIN
      markStruct = defaultMarkStruct
  ENDELSE 

END 
