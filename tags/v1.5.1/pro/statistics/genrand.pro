
;+
;
; NAME:  
;    GENRAND
;       
; PURPOSE:  
;    Generate random numbers from an input probability function.
;	
; CALLING SEQUENCE:
;      genrand, prob, xvals , nrand, [, rand, /cumulative, /double, /quadratic,
;      plt=plt, bin=bin])
;                 
; INPUTS: 
;    prob: input probablility function.  
;    xvals: abscissae
;    nrand: How many points to generate from the probability function.
;
; KEYWORD PARAMETERS:
;         method=method: method=0 (default) use integral technique
;                        method=1 use 2-d cut technique 
;         cumulative: tells the program that prob is the cumulative 
;	         probablility.  Using this option saves processing time.
;                Note for method=1 this is not applicable.
;         /double: use double precision?
;         /quadratic: use quadratic interpolation? Ignored for idl versions
;                     less than 5.4
;         /plt: plot histogram if requested
;         bin: how to bin the histogram (irrelevent if not /plt )
;       
; OUTPUTS: 
;    rand:  an array of random numbers generated from prob. size nrand
; 
; PROCEDURE: 
;  Method=0 (default)
;    Generates random numbers from the input distribution by inverting
;    the cumulative probability function.  Performs interpolation
;    using the interpol function.
;	
;  Method=1
;  Generates random numbers from the input distribution by generating x,y
;           values uniformly and keeping those that lie beneath the 
;           probability distribution
;  
; REVISION HISTORY:
;	Author: Erin Scott Sheldon  UofM  1/??/99
;       March 2003: Use interpolation.  Introduced method=1 E.S.S.
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

pro genrand, prob, xvals , nrand, rand, $
             cumulative=cumulative, plt=plt, bin=bin, $
             double=double,old=old, quadratic=quadratic, method=method, $
             _extra=_extra



  if n_params() lt 3 then begin
	print,'Syntax: genrand, prob, xvals , nrand , rand, /cumulative, '+$
          'method=, /double, /quadratic, /plt,bin=, )'
        print,'Outputs a set of random numbers generated from prob.'
        print,'Use doc_library, "genrand" for more help'
	return
  endif

  ;; This way only one seed is generated per IDL session.  
  common genrand_seed_block, seed

  nx = n_elements(xvals)
  if (nx ne n_elements(prob)) then begin
	print,'prob and xvals must be of same size'
	return
  endif
  if not keyword_set(double) then double = 0

  if n_elements(method) eq 0 then method = 0

  if method eq 0 then begin 

      ;;;;;;;;;;;;;;;;;;;;;;;
      ;; integral method
      ;;;;;;;;;;;;;;;;;;;;;;;

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;  if prob is not a cumulative probability function 
      ;;  we need to generate one from prob
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      if not keyword_set(cumulative)  then begin
          if double then intfunc,double(prob), xvals, cumdist $
          else intfunc,float(prob), xvals, cumdist
      endif else begin
          cumdist=prob
      endelse
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; normalize probability function 
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
      norm = cumdist[nx-1]
      cumdist = cumdist/norm

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; generate a set of uniform random numbers from 0 to 1. store in
      ;; the array testrand
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      testrand = randomu(seed,nrand)
 
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; generate numbers from distribution prob 
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      if float(!version.release) ge 5.4 then begin 
          rand = interpol(xvals, cumdist, testrand,quadratic=quadratic)
      endif else begin 
          rand = interpol(xvals, cumdist, testrand)
      endelse 
 
  endif else begin 

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; monte-carlo 2d cut method
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      rand = dblarr(nrand)

      minx = min(xvals, max=maxx)
      maxprob = max(prob)
      minprob = 0d

      ngood = 0l
      nbad = nrand
      bad = lindgen(nbad)

      while ngood lt nrand do begin 

          randx = arrscl(randomu(seed, nbad, /double), $
                         minx, maxx, $
                         arrmin=0d, arrmax=1d )

          randprob = arrscl(randomu(seed, nbad, /double), $
                         minprob, maxprob, $
                         arrmin=0d, arrmax=1d )

          if float(!version.release) ge 5.4 then begin 
              interp_prob = interpol(prob, xvals, randx,quadratic=quadratic)
          endif else begin 
              interp_prob = interpol(prob, xvals, randx)
          endelse 

          tgood = where(randprob le interp_prob, ntgood, $
                        comp=tbad, ncomp=ntbad)

          if ntgood ne 0 then begin  

              ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
              ;; we found some good ones
              ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

              good_ids = bad[tgood]

              ;; copy in the good ones
              rand[good_ids] = randx[tgood]

              ngood = ngood + ntgood

              if ntbad ne 0 then bad = bad[tbad]
              nbad = ntbad

          endif 

      endwhile 

      if not double then rand = float(temporary(rand))

  endelse 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; plot the results
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  if keyword_set(plt) then begin
     if (not keyword_set(bin) ) then bin=2.*(xvals[1] - xvals[0])
     plothist,rand, xhist, yhist, bin=bin, /norm

     nxhist = n_elements(xhist)
     if nxhist eq 1 then ii = 0 else ii = long( float(nxhist)/2.0 )
     probval = interpol(prob, xvals, xhist[ii])
     ratio = float(yhist[ii])/probval

     if (not keyword_set(cumulative) ) then begin
	   oplot, xvals, prob*ratio,color=!red,thick=2
     endif
  endif

  if n_elements(cumdist) ne 0 then cumulative = cumdist

return
end
