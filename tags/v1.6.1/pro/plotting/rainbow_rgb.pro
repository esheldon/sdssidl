;+
; NAME:
;  RAINBOW_RGB()
;
;
; PURPOSE:
;  Produce rgb colors based on hue.  Wrapper for COLOR_CONVERT so the
;  user only has to supply the hue, although luminance and saturation can
;  optionally be sent.
;  
; CALLING SEQUENCE:
;  color = rainbow_rgb(hue, luminance=luminance, sataturation=saturation)
;
; INPUTS:
;  hue:  hue on the color wheel: [0,360] from red-green-blue and back.
;
;
; OPTIONAL INPUTS:
;  luminance=: In [0,1]
;  saturation: In [0,1]
;
; KEYWORD PARAMETERS:
;
;
; OUTPUTS:
;  The corresponding rgb color which can be sent to IDL plotting routines.
;
; EXAMPLE:
;  color = rainbow_rgb(125)
;  oplot, data, color=color
;
;
; MODIFICATION HISTORY:
;  Created:  ??-July-2005.  Erin Sheldon, UChicago
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


FUNCTION rainbow_rgb, hue, luminance=luminance, sataturation=saturation

  IF n_elements(hue) EQ 0 THEN BEGIN 
      print,'-Syntax:  color = rainbow_rgb(hue, luminance=, saturation=)'
      print
      print,'  hue is somewhere along the color wheel in [0,360] from '
      print,'                red-green-blue and back'
      print,'  lum and sat are in [0,1]'
      return,-1
  ENDIF 

  nhue = n_elements(hue)

  ;; Luminance

  deflum = 0.5
  nlum = n_elements(lumniance)
  IF nlum NE 0 THEN BEGIN 
      IF nlum EQ 1 THEN BEGIN 
          lum = replicate( luminance[0] > 0 < 1, nlum)
      ENDIF ELSE IF nlum EQ nhue THEN BEGIN 
          lum = luminance > 0 < 1
      ENDIF ELSE BEGIN 
          message,'luminance must be scalar or same size as hue'
      ENDELSE 
  ENDIF ELSE BEGIN 
      lum = replicate(deflum, nhue)
  ENDELSE 




  defsat = 0.7
  nsat = n_elements(saturation)
  IF nsat NE 0 THEN BEGIN 
      IF nsat EQ 1 THEN BEGIN 
          sat = replicate( saturation[0] > 0 < 1, nsat)
      ENDIF ELSE IF nsat EQ nhue THEN BEGIN 
          sat = saturation > 0 < 1
      ENDIF ELSE BEGIN 
          message,'saturation must be scalar or same size as hue'
      ENDELSE 
  ENDIF ELSE BEGIN 
      sat = replicate(defsat, nhue)
  ENDELSE 

  hues = (hue > 0 < 360.0)
  COLOR_CONVERT, hues, lum, sat, R, G, B, /HLS_RGB

  color = R + 256L*(G+256L*B)
  return,color

END 
