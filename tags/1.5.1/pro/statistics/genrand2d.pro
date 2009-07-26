
;+
;
; NAME:  
;    GENRAND2D
;       
; PURPOSE:  
;    Generate random numbers from a 2d probability function.
;	
;
; CALLING SEQUENCE:
;      genrand2d, prob, xvals, yvals, nrand, xrand, yrand, $
;                 double=double, nxbins=nxbins, nybins=nybins, /plt
;                 
;
; INPUTS: 
;    prob: input probablility function.  
;    xvals, yvals: x-y values corresponding to the surface prob z values
;    nrand: How many points to generate from the probability function.
;
; KEYWORD PARAMETERS:
;         /double: Return double precision?
;         /plt: make a plot of the generated data and input distribution
;         nxbins=nxbins, nybins=nybins: number of bins to use in plot
;       
; OUTPUTS: 
;    xrand, yrand:  arrays of random numbers generated from prob
; 
; PROCEDURE: 
;  Generates random numbers from the input distribution by generating x,y,z
;           values uniformly and keeping those that lie beneath the probability
;           surface.
;
; REVISION HISTORY:
;	Author: Erin Scott Sheldon  UofChicago 02-Mar-2003
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

pro genrand2d, prob, xvals, yvals, nrand, xrand, yrand, $
               double=double, plt=plt, $
               nxbins=nxbins, nybins=nybins


  if n_params() lt 4 then begin
	print,'genrand2d, prob, xvals, yvals, nrand, xrand, yrand, /double, /plt, nxbins=, nybins='
	return
  endif

  ;; This way only one seed is generated per IDL session.  
  common genrand_seed_block, seed

  nx = n_elements(xvals)
  ny = n_elements(yvals)

  ss = size(prob)

  if ss[0] ne 2 then message,'Probability surface must be a 2d array'

  if (nx ne ss[1]) or (ny ne ss[2]) then begin
	print,'size(xvals) must equal x-size of prob, same for yvals'
	return
  endif

  if not keyword_set(double) then double = 0

  ;; generate the random points

  xrand = dblarr(nrand)
  yrand = xrand

  xi = lindgen(nx)
  yi = lindgen(ny)

  minx = min(xvals, max=maxx)
  miny = min(yvals, max=maxy)

  maxprob = max(prob)
  minprob = 0d

  ngood = 0l
  nbad = nrand
  bad = lindgen(nbad)

  while ngood lt nrand do begin 
      
;      if not keyword_set(silent) then begin 
;          if ngood eq 0 then print,'generating ',ntostr(nbad) $
;          else print,'re-generating ',ntostr(nbad)
;      endif 
      
      randx = arrscl(randomu(seed, nbad, /double), $
                     minx, maxx, $
                     arrmin=0d, arrmax=1d )
      
      randy = arrscl(randomu(seed, nbad, /double), $
                     miny, maxy, $
                     arrmin=0d, arrmax=1d )
      
      randprob = arrscl(randomu(seed, nbad, /double), $
                        minprob, maxprob, $
                        arrmin=0d, arrmax=1d )

      rxi = interpol(xi, xvals, randx)
      ryi = interpol(yi, yvals, randy)

      interp_prob = interpolate(prob, rxi, ryi)
      
      tgood = where(randprob le interp_prob, ntgood, $
                    comp=tbad, ncomp=ntbad)
      
      if ntgood ne 0 then begin  
          
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; we found some good ones
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

          good_ids = bad[tgood]
          
          ;; copy in the good ones
          xrand[good_ids] = randx[tgood]
          yrand[good_ids] = randy[tgood]

          ngood = ngood + ntgood
          
          if ntbad ne 0 then bad = bad[tbad]
          nbad = ntbad
          
      endif 
      
      if not double then begin 
          xrand = float( temporary(xrand) )
          yrand = float( temporary(yrand) )
      endif 

  endwhile 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; plot the results
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  if keyword_set(plt) then begin
     ploth, xrand, yrand, /silent, xtitle='X rand', ytitle='Y rand', title='2-d Prob', nxbins=nxbins, nybins=nybins
     contour, prob, xvals, yvals, /overplot, nlevels=3

     legend,'Input distribution',line=0,/right,box=0

  endif

return
end









