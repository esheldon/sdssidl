;+
; NAME:
;   match_hist
;
; PURPOSE:
;   Get a subset ind2 from array2 such that the histogram of array2[ind2]
;   is proportional to that of another array array1.  i.e.
;       hist1=histogram(array1, binsize=binsize, min=min, max=max)
;       hist2=histogram(array2[ind2], binsize=binsize, min=min, max=max)
;   Then hist1 is proportional to hist2.  Note if the modifiers min=, max= 
;   are sent the final match may also be over a subset of array1.
;
; CALLING SEQUENCE:
;   indices2 = match_hist(array1, array2, binsize, min=, max=, indices1=)
;
; INPUTS:
;   array1: An array.
;   array2: A second array; a subset will be drawn from this array such that
;       the histograms of array1 and array2 match.
;   binsize: The binsize for the histogram.
;
; OPTIONAL INPUTS:
;   min=: The minimum value for the histogram.
;   max=: The maximum value for the histogram.
;
; OUTPUTS:
;   indices2: Indices such that the histogram of array2[ind2] match that of
;       array1.  These indices will be a random sampling of the elements from
;       each bin of the histogram, drawn such that the heights of the two 
;       histograms are proportional.  Note if the modifiers min= or max= 
;       are sent you should also use the indices1 keyword to retrieve the 
;       subset of array1 that was matched.  
;
; OPTIONAL OUTPUTS:
;   indices1: The indices from array1 that were matched.  This may be useful 
;       if the modifiers min= or max= were sent.
;
; EXAMPLE:
;   binsize=0.1
;   ind2 = match_hist(z1, z2, binsize, min=0.0, max=1.0, indices1=ind1)
;
; MODIFICATION HISTORY:
;   Created: Sometime in 2007, Erin Sheldon, NYU
;
;-

function match_hist, x1, x2, binsize, min=min, max=max, $
                     x1h=x1h, x2h=x2h, new_x2h=new_x2h, $
                     indices1=indices1
    
  if n_params() lt 3 then begin 
      print,'Syntax error'
      print,'  usage: indices2 = match_hist(x1, x2, binsize, min=, max=, indices1=)'
      print,'  Chooses values of x2 such that the histograms of x1 and x2 are proportional'
      message,'  Halting'
  endif 

  if n_elements(min) eq 0 then min = min(x1)
  if n_elements(max) eq 0 then max = max(x1)

  ;; Histogram the first array
  if arg_present(indices1) then begin
      x1h = histogram(x1, min=min, max=max, binsize=binsize, rev=rev1)
  endif else begin
      x1h = histogram(x1, min=min, max=max, binsize=binsize)
  endelse

  ;; second histogram
  x2h = histogram(x2, min=min, max=max, binsize=binsize, rev=rev2)

  nbin = n_elements(x1h)

  ;; The percentage of x2 to keep in each bin
  perc_keep = fltarr(nbin)
  ratio = fltarr(nbin)
  new_x2h = lonarr(nbin)
  
  wh = where(x1h GT 0 AND x2h GT 0, ngood)

  if ngood eq 0 then begin 
      print,'No values from x2 found in x1 range ' + $
        '['+ntostr(min)+', '+ntostr(max)+']'
      return,-1
  endif 

  ;; normalize by the maximum in x1 histogram
  ratio[wh] = float(x1h[wh])/x2h[wh]
  maxratio = max(ratio, maxi)

  ;colprint, x1h[wh], x2h[wh]

  ;pplot, ratio[wh], psym=10
  ;pplot, ratio[wh], psym=8,/over,color=c2i('red')

  ;; The percentage to keep
  perc_keep[wh] = ratio[wh]/maxratio

  ;; pointer array to keep track of the ones from x2 we want
  ptrlist2 = ptrarr(nbin)
  if arg_present(indices1) then ptrlist1 = ptrarr(nbin)

  for j=0l, nbin-1 do begin 

      if x1h[j] gt 0 then begin 

          if rev2[j] ne rev2[j+1] then begin 
              w = rev2[ rev2[j]:rev2[j+1]-1 ]
              
              nrtot = n_elements(w)
              
              if perc_keep[j] lt 1 then begin 
                  ;; randomly choose perc_keep of them
                  nkeep = round(perc_keep[j]*nrtot) > 1
                  ind = lindgen(nrtot)
                  ;; now randomize the order of the indices and keep the
                  ;; first nkeep of them
                  s = sort( randomu(seed, nrtot) )
                  ind = ind[s]
                  ind = ind[0:nkeep-1]
                  
                  w = w[ind]
                  
              endif else begin
                  nkeep = nrtot
              endelse 

              ptrlist2[j] = ptr_new(w, /no_copy)
              new_x2h[j] = nkeep
             
              if arg_present(indices1) then begin
                  w1 = rev1[ rev1[j]:rev1[j+1]-1]
                  ptrlist1[j] = ptr_new(w1,/no_copy)
              endif
              
          endif 
      endif 

  endfor 

  indices2 = combine_ptrlist(ptrlist2)
  if arg_present(indices1) then indices1 = combine_ptrlist(ptrlist1)

  return, indices2
                 
end 
