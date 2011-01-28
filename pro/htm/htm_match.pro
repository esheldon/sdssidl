;+
; NAME:
;  HTM_MATCH
;
;
; PURPOSE:
;  Match two sets of ra/dec points using the Heierarchical 
;  Triangular Mesh code.  This is an IDL wrapper for htmmatchc
;  IDL DLM, and uses the DLM htm_index when these are not sent.
;  This is very efficient for large search angles and large lists.
;  May seem slow otherwise due to overhead creating htm indices.
;
;
; CATEGORY:
;  Astronomy.
;
;
; CALLING SEQUENCE:
;   htm_match, ra1, dec1, ra2, dec2, angle, m1, m2, distance12, $
;               maxmatch=, $
;               htmid2=, htmrev2=, $
;               minid=, maxid=, $
;               depth=, 
;               file=, 
;               status=
;
;
; INPUTS:
;  ra1,dec1,ra2,dec2: ra,dec lists. Must be double precision.
;  angle: The search angle in radians.  May be a scalar or an
;     array same length as ra1,dec1.
;
;
; KEYWORD PARAMETERS:
;  maxmatch=: The maximum number of allowed matches per point. Defaults
;      to 1.  Setting maxmatch=0 will return all matches.
;      Note the default used to be to return all matches.
;  depth=: the depht of the HTM tree.  Defaults to 10.
;  htmid2=: the htm indexes for the second list.  If not sent
;      they are generated internally.
;  htmrev2=: The result of
;           h=histogram(htmid2-min(htmid20), min=0, rev=htmrev2) 
;      If not sent will be created internally.
;  minid=, maxid=: If htmrev2 is sent with these, there is no need
;      to calculate htmid2.
;  file=: A file into which will be written the indices and distances.
;         When this keyword is sent, no arrays are returned.  This is
;         useful when the match data will not fit into memory.
;         The file is an unformatted binary file. It can be read with 
;         htm_readbin() and converted to a FITS file with htm_bin2fits
;
; OUTPUTS:
;  m1,m2: The indices of matches in list 1 and 2.
;  distance12:  The distance between matches in radians.
;
;
; OPTIONAL OUTPUTS:
;  status=: 0 is all well, 2 is no matches, 1 (or something else) 
;      is failure.
;
; RESTRICTIONS:
;  The C++ DLM htmmatchc must be compiled and in your IDL_DLM_PATH
;
; EXAMPLE:
;
;
;
; MODIFICATION HISTORY:
;  Working version: 19-April-2006  Erin Sheldon NYU
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

PRO htm_match, ra1, dec1, ra2, dec2, angle, m1, m2, distance12, $
               maxmatch=maxmatch, $
               htmid2=htmid2, $
               htmrev2=htmrev2, $
               minid=minid, maxid=maxid, $
               depth=depth, $
               file=file, $
               status=status

  status = 1
  m1=-1
  m2=-1
  distance12=-1d
  IF n_params() LT 7 THEN BEGIN  
      print,'-Syntax: htm_match, ra1, dec1, ra2, dec2, angle, match1, match2, d12, '
      print,'           [ maxmatch=, '
      print,'             htmid2=, htmrev2=, minid=, maxid=, depth=, file=, status=]'
      print,'ra/dec in degrees, double precision'
      print,'angle in radians.  May be an array of same length as ra1,dec1'
      print,'status=0 is success, status=2 no matches, otherwise failure'
      return
  ENDIF 

  nra1  = n_elements(ra1)
  ndec1 = n_elements(dec1)
  nra2  = n_elements(ra2)
  ndec2 = n_elements(dec2)
  nangle = n_elements(angle)

  IF nra1 NE ndec1 THEN BEGIN 
      message,'ra1 and dec1 must be same length',/inf
      return
  ENDIF 
  IF nra2 NE ndec2 THEN BEGIN 
      message,'ra2 and dec2 must be same length',/inf
      return
  ENDIF 
  IF nangle EQ 1 THEN BEGIN 
      angle = double(angle)
  ENDIF ELSE IF nangle NE nra1 THEN BEGIN 
      message,'angle must be a scalar or same size as ra1,dec1',/inf
      return
  ENDIF 

  IF n_elements(depth) EQ 0 THEN depth=10

  ;; Has the user given us the reverse indices?
  nhtmrev2 = n_elements(htmrev2)
  IF nhtmrev2 EQ 0 THEN BEGIN

      ;; Has the user given us the indices?
      nhtmid2 = n_elements(htmid2)
      IF nhtmid2 NE 0 THEN BEGIN 
          IF nhtmid2 NE nra2 THEN BEGIN 
              message,'Input htmid2 must be same length as ra2/dec2',/inf
              return
          ENDIF 
          ;;print,'Using input htmid2'
      ENDIF ELSE BEGIN 
          ;;print,'Creating htmid2'

          ;; IDL has a bug that it will give a compile error if the
          ;; htm_index() DLM wasn't loaded (doesn't happen for procs).
          ;; we wrap this in a command string to avoid that.
          command = 'htmid2 = htm_index(ra2, dec2, depth)'
          IF NOT execute(command) THEN BEGIN 
              message,'Could not create htm indices. htm_index()',/inf
              return
          ENDIF 
      ENDELSE 

      minid = min(htmid2, max=maxid)
      ;;print,'histogramming htmid2'

      IF NOT isarray(htmid2) THEN BEGIN 
          hist = histogram([htmid2]-minid, min=0, rev=htmrev2)      
      ENDIF ELSE BEGIN 
          hist = histogram(htmid2-minid, min=0, rev=htmrev2)      
      ENDELSE 

  ENDIF ELSE BEGIN 
      ;;print,'Using input reverse indices'
      IF n_elements(htmid2) NE 0 THEN minid=min(htmid2,max=maxid)
      IF n_elements(minid) EQ 0 OR n_elements(maxid) EQ 0 THEN BEGIN 
          message,'If you enter htmrev2, then you must also enter minid/maxid or htmid2',/inf
      ENDIF 
  ENDELSE 

  IF n_elements(file) NE 0 THEN outfile = expand_tilde(file)

  ;; Run the matching code
  ;;print,'Calling C++ match code'
  htmmatchc, $
    ra1, dec1, ra2, dec2, htmrev2, minid, maxid, $
    angle, m1, m2, distance12, $
    maxmatch=maxmatch, depth=depth, file=outfile, status=cstatus

  status = cstatus


END 

