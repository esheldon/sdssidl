;+
; NAME:
;  SDSS_UTIL__DEFINE  (An IDL Class File)
;
; PURPOSE:
;  Defines a set of utility methods usefule for SDSS data manipulation.
;  Generally one uses the "sdss" class which inherits this one and
;  some others.
;
; CATEGORY:
;  SDSS 
;
; CALLING SEQUENCE:
;  su=obj_new('sdss_util')
;
; METHODS:
;  Use the methods procedure to list the methods of this file.
;     IDL> methods, 'sdss_util'
;  And the doc_method procedure to get documentation on individual
;  methods.
;     IDL> doc_method, 'sdss_util::photoid'
;
;  A quick list of useful methods:
;  
;    ::photoid() - return a unique id given run,rerun,camcol,field,id
;                  or a struct containing those tags..
;    ::photoid_extract: Convert a photoid to the corresponding run,rerun...
;    ::histid() - Return a tree structure for a given set of sdss ids
;                 with the leaves as indices.
;    ::histid_destroy - free the pointers in the tree
;    ::histid_print - print the tree structure.
;    ::psfrec: Reconstruct the psf image given a psf structure as read
;              with sdss_files::psfield_read()
;
; EXAMPLES:
;   IDL> su=obj_new('sdss_util')
;   IDL> phid = su->photoid(run,rerun,camcol,field,id)
;   ;; Get tree structure for objects by run,rerun
;   IDL> histid = su->histid(run,rerun)
;   ;; Get tree structure for objects by run,rerun,camcol
;   IDL> histid = su->histid(run,rerun,camcol)
;
;
; TODO:
;   Move many of the procedures in the sdss directory into this class.
; 
;
; MODIFICATION HISTORY:
;  Created: Mid-2005  Erin Sheldon Uchicago
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

function sdss_util::init
  return,1
end 


function sdss_util::photoid, run, rerun, camcol, field, id

  on_error, 2
  ten=ulong64(10)
  p1 = 0l
  p2 = 6l
  p3 = 11l
  p4 = 12l
  p5 = 15l

  np = n_params()
  if np eq 1 then begin 

      if size(run,/tname) ne 'STRUCT' then begin 
          message,'Syntax: superid=sdss_photoid(run,rerun,camcol,field,id OR struct)'
      endif 

      tags = tag_names(run)
      match,tags,['RUN','RERUN','CAMCOL','FIELD','ID'],min,mt
      if n_elements(mt) ne 5 then begin 
          message,'Input structure must contain run,rerun,camcol,field,id'
      endif 

      super = $
        ulong64(run.id)*ten^p1     + $
        ulong64(run.field)*ten^p2   + $
        ulong64(run.camcol)*ten^p3 + $
        ulong64(run.rerun)*ten^p4  + $
        ulong64(run.run)*ten^p5

  endif else if np eq 5 then begin 

      nr   = n_elements(run)
      nrer = n_elements(rerun)
      nc   = n_elements(camcol)
      nf   = n_elements(field)
      nid  = n_elements(id)
      
      if total_int([nr,nrer,nc,nf,nid]) ne 5*nr then begin 
          message,'All arrays must be same size'
      endif 
      
      super = $
        ulong64(id)*ten^p1     + $
        ulong64(field)*ten^p2   + $
        ulong64(camcol)*ten^p3 + $
        ulong64(rerun)*ten^p4  + $
        ulong64(run)*ten^p5

  endif else begin 
      message,'Syntax: superid=sdss_photoid(run,rerun,camcol,field,id OR struct)'
  endelse 

  return,super

end 

pro sdss_util::photoid_extract, photoid, run, rerun, camcol, field, id

  if n_params() lt 1 then begin 
      message,'-Syntax: su->photoid_extract, photoid, run, rerun, camcol, field, id',/inf
      message,' super index should not be floating point'
  endif 

  ten = 10ull

  p1 = 0l
  p2 = 6l
  p3 = 11l
  p4 = 12l
  p5 = 15l

  run = $
    super/ten^p5
  rerun = $
    super/ten^p4 - run*ten^(p5-p4)
  camcol = $
    super/ten^p3 - run*ten^(p5-p3) - rerun*ten^(p4-p3)
  field = $
    super/ten^p2 - run*ten^(p5-p2) - rerun*ten^(p4-p2) - camcol*ten^(p3-p2)
  id =  $
    super/ten^p1 - run*ten^(p5-p1) - rerun*ten^(p4-p1) - camcol*ten^(p3-p1) - field*ten^(p2-p1)

  run = long(run)
  rerun = fix(rerun)
  camcol = fix(camcol)
  field = fix(field)
  id = long(id) 

  return

end 





function sdss_util::_histid_histogram, data, rev=rev

  if n_elements(data) eq 1 then begin 
      hist = histogram([data], rev=rev)
  endif else begin 
      hist = histogram(data, rev=rev)
  endelse 

  return, hist
end 

function sdss_util::_histid_checkpars, np, runs, reruns, camcols, fields

  nruns = n_elements(runs)
  nreruns = n_elements(reruns)
  ncamcols = n_elements(camcols)
  nfields = n_elements(fields)

  case np of
      1: return,1
      2: begin 
          if nreruns ne nruns then begin 
              message,'runs,reruns must be same length',/inf
              return,0 
          endif else return,1
      end 
      3: begin 
          if nreruns ne nruns or ncamcols ne nruns then begin
              message,'runs,reruns,camcols must be same length',/inf
              return,0 
          endif else return,1
      end 
      4: begin 
          if (nreruns ne nruns or $
              ncamcols ne nruns or $
              nfields ne nruns) then begin 
              message,'runs,reruns,camcols,fields must be same length',/inf
              return,0
          endif else return,1
      end  
      else:
  endcase 

end 

function sdss_util::histid, runs, reruns, camcols, fields, check=check, status=status
  
  on_error, 2

  status = 1
  np = n_params()

  if np lt 1 or np gt 4 then begin 
      message,'-Syntax: idstruct = sdss_histid(runs, [reruns, camcols, fields, status=])'
  endif 

  if not self->_histid_checkpars(np, runs, reruns, camcols, fields) then begin 
      return,-1
  endif 

  ;; node structures. 
  ;; depth     meaning
  ;;  1    only runs indexed
  ;;  2    runs/reruns
  ;;  3    runs/reruns/camcols
  ;;  4    runs/reruns/camcols/fields

  ;; indices will remain null except at the leaf of the tree
  basestruct   = {depth:0, nleaves:0l, nobj:0l, $
                  nruns:0, runs: ptr_new(), nind:0l, indices:ptr_new()}

  runstruct    = {run:0l, $
                  nreruns:0,  reruns:  ptr_new(), nind:0l, indices:ptr_new()}
  rerunstruct  = {rerun:0,$
                  ncamcols:0, camcols: ptr_new(), nind:0l, indices:ptr_new()}
  camcolstruct = {camcol:0,  $
                  nfields:0,  fields:  ptr_new(), nind:0l, indices:ptr_new()}
  fieldstruct  = {field:0, nind:0l, indices:ptr_new()}

  ;; histogram the runs
  run_h = self->_histid_histogram(runs, rev=rrev)
  wrun_h = where(run_h ne 0, nwrun_h)
  
  ;; fill in the main run struct
  basestruct.depth = np
  basestruct.nruns = nwrun_h
  basestruct.runs = ptr_new(replicate(runstruct, nwrun_h))
  pruns = basestruct.runs

  for ri=0l, nwrun_h-1 do begin 
      
      ;; indices of this run in histogram
      wrun_h_i = wrun_h[ri]
      
      ;; indices of requests with this run
      wrun = rrev[ rrev[wrun_h_i]:rrev[wrun_h_i+1] -1 ]

      ;; copy in run
      (*pruns)[ri].run = runs[wrun[0]]

      ;; are there reruns?
      if np gt 1 then begin 
          ;; histogram the reruns
          rerun_h = self->_histid_histogram(reruns[wrun], rev=rrrev)
          wrerun_h = where(rerun_h ne 0, nwrerun_h)
          
          ;; set rerun structures, and get a pointer to them
          (*pruns)[ri].nreruns = nwrerun_h
          (*pruns)[ri].reruns = $
            ptr_new(replicate(rerunstruct, nwrerun_h))
          
          preruns = (*pruns)[ri].reruns
          for rri=0l, nwrerun_h-1 do begin 

              ;; indices of this rerun in rerun histogram
              wrerun_h_i = wrerun_h[rri]
              
              ;; indices of requests with this run-rerun
              wrerun = rrrev[ rrrev[wrerun_h_i]:rrrev[wrerun_h_i+1]-1]
              wrerun = wrun[wrerun]

              ;; copy in rerun
              (*preruns)[rri].rerun = reruns[wrerun[0]]

              ;; are there camcols?
              if np gt 2 then begin               
                  ;; histogram the camcols
                  hamcol_h = self->_histid_histogram(camcols[wrerun], rev=crev)
                  wcamcol_h = where(hamcol_h ne 0, nwcamcol_h)
                  
                  ;; set camcols structures, and get a pointer to them
                  (*preruns)[rri].ncamcols = nwcamcol_h
                  (*preruns)[rri].camcols = $
                    ptr_new(replicate(camcolstruct, nwcamcol_h))

                  pcamcols = (*preruns)[rri].camcols                  
                  for ci=0l, nwcamcol_h-1 do begin 
                      
                      ;; indices of this camcol in camcol histogram
                      wcamcol_h_i = wcamcol_h[ci]
                      
                      ;; indices of requests with this run-rerun-camcol
                      wcamcol = crev[ crev[wcamcol_h_i]:crev[wcamcol_h_i+1] -1]
                      wcamcol = wrerun[wcamcol]

                      ;; copy in camcol
                      (*pcamcols)[ci].camcol = camcols[wcamcol[0]]

                      ;; are there fields?
                      if np gt 3 then begin 
                      
                          ;; histogram the fields
                          field_h = $
                            self->_histid_histogram(fields[wcamcol], rev=frev)
                          wfield_h = where(field_h ne 0, nwfield_h)
                          
                          ;; set fields structures, and get a pointer to them
                          (*pcamcols)[ci].nfields = nwfield_h
                          (*pcamcols)[ci].fields = $
                            ptr_new(replicate(fieldstruct,nwfield_h))

                          pfields = (*pcamcols)[ci].fields
                          for fi=0l, nwfield_h-1 do begin 
                              
                              ;; indices of this field in field histogram
                              wfield_h_i = wfield_h[fi]
                              
                              ;; indices of requests with this 
                              ;; run-rerun-camcol-field
                              wfield = $
                                frev[ frev[wfield_h_i]:frev[wfield_h_i+1] -1 ]
                              wfield = wcamcol[wfield]

                              nind = n_elements(wfield)
                              (*pfields)[fi].field = fields[wfield[0]]
                              (*pfields)[fi].indices = ptr_new(wfield,/no_copy)
                              (*pfields)[fi].nind = nind

                              basestruct.nleaves = basestruct.nleaves+1
                              basestruct.nobj = basestruct.nobj + nind
                              basestruct.nleaves = basestruct.nleaves + 1
                          endfor ;; fields
                      endif else begin ;; np < 3
                          nind = n_elements(wcamcol)
                          (*pcamcols)[ci].indices = ptr_new(wcamcol, /no_copy)
                          (*pcamcols)[ci].nind = nind
                          basestruct.nleaves = basestruct.nleaves+1
                          basestruct.nobj = basestruct.nobj + nind
                          basestruct.nleaves = basestruct.nleaves + 1
                      endelse 
                  endfor ;; camcols
              endif else begin ;; np < 2
                  nind = n_elements(wrerun)
                  (*preruns)[rri].indices = ptr_new(wrerun, /no_copy)
                  (*preruns)[rri].nind = nind

                  basestruct.nobj = basestruct.nobj + nind
                  basestruct.nleaves = basestruct.nleaves + 1
              endelse 
          endfor ;; reruns
      endif else begin ;; np < 1
          nind = n_elements(wrun)
          (*pruns)[ri].indices = ptr_new(wrun, /no_copy)
          (*pruns)[ri].nind = nind

          basestruct.nobj = basestruct.nobj + nind
          basestruct.nleaves = basestruct.nleaves + 1
      endelse 
  endfor ;; runs

  if keyword_set(check) then begin 

      ;; Check total number of indices, as well as match the ids up to the
      ;; indexed objects

      ntotal = 0ULL

      print,'Checking'
      nn = n_elements(wstruct)
      for i=0ll, nn-1 do begin 
          
          ind = *wstruct[i].indices
          
          ntotal = ntotal + n_elements(ind)

          w=where(runs[ind] ne wstruct[i].run, nw)
          if nw ne 0 then message,'runs'

          if np gt 1 then begin 
              w=where(reruns[ind] ne wstruct[i].rerun, nw)
              if nw ne 0 then message,'reruns'
              
              if np gt 2 then begin 
                  w=where(camcols[ind] ne wstruct[i].camcol, nw)
                  if nw ne 0 then message,'camcols'
                  
                  if np gt 3 then begin 
                      w=where(fields[ind] ne wstruct[i].field, nw)
                      if nw ne 0 then message,'fields'
                  endif ;; fields
              endif ;; camcols
          endif ;; reruns
          ;; runs

      endfor 

      if ntotal ne n_elements(runs) then begin 
          message,'Total number of indices not equal input'
      endif 

  endif 

  status = 0
  return,basestruct


end 


pro sdss_util::histid_print, basestruct

  if n_params() lt 1 then begin
      message,'su->histid_print, baseStruct'
  endif 

  print,'Nobj:    '+ntostr(baseStruct.nobj)
  print,'Depth:   '+ntostr(basestruct.depth)
  print,'Nleaves: '+ntostr(baseStruct.nleaves)

  nruns = basestruct.nruns
  pruns = basestruct.runs
  for ri=0l, nruns-1 do begin

      run = (*pruns)[ri].run
      nreruns = (*pruns)[ri].nreruns
      if nreruns gt 0 then begin 

          preruns = (*pruns)[ri].reruns
          for rri=0l, nreruns-1 do begin 

              rerun = (*preruns)[rri].rerun

              ncamcols = (*preruns)[rri].ncamcols
              if ncamcols gt 0 then begin 
                  
                  pcamcols = (*preruns)[rri].camcols
                  for ci=0l, ncamcols-1 do begin 
                      camcol = (*pcamcols)[ci].camcol
                      
                      nfields = (*pcamcols)[ci].nfields

                      if nfields gt 0 then begin 

                          pfields = (*pcamcols)[ci].fields
                          for fi=0l, nfields-1 do begin 
                              field = (*pfields)[fi].field

                              nind = (*pfields)[fi].nind

                              print,run,rerun,camcol,field,nind
                          endfor 

                      endif else begin 
                          print,run,rerun,camcol
                      endelse 
                  endfor 

              endif else begin 
                  print,run,rerun
              endelse 

          endfor 

      endif else begin 
          print,run
      endelse 
  endfor 

end 



pro sdss_util::histid_destroy, basestruct

  on_error, 2

  if n_params() lt 1 then begin 
      message,'-Syntax: su->histid_destroy, baseStruct'
  endif 

  ;; we allow indices to be stored at all levels.  check base structure.  
  if ptr_valid(basestruct.indices) then begin 
      ptr_free, basestruct.indices
  endif 

  pruns = basestruct.runs
  if ptr_valid(pruns) then begin 
      
      ;; now loop over runs
      nruns = basestruct.nruns
      for ri=0l, nruns-1 do begin 

          ;; check for indices on basic run structure
          if ptr_valid( (*pruns)[ri].indices ) then begin 
              ptr_free, (*pruns)[ri].indices 
          endif 

          ;; reruns for this run?
          preruns = (*pruns)[ri].reruns
          if ptr_valid(preruns) then begin 

              ;; now loop over the reruns
              nreruns = (*pruns)[ri].nreruns
              for rri=0l, nreruns-1 do begin 

                  ;; check for indices on the base rerun structure
                  if ptr_valid( (*preruns)[rri].indices ) then begin 
                      ptr_free, (*preruns)[rri].indices 
                  endif 
                  
                  ;; camcols for this rerun?
                  pcamcols = (*preruns)[rri].camcols
                  if ptr_valid(pcamcols) then begin 

                      ;; now loop over the camcols
                      ncamcols = (*preruns)[rri].ncamcols
                      for ci=0l, ncamcols-1 do begin 

                          ;; check for indices on the base camcol structure
                          if ptr_valid( (*pcamcols)[ci].indices ) then begin 
                              ptr_free, (*pcamcols)[ci].indices 
                          endif 

                          ;; fields for this camcol?
                          pfields = (*pcamcols)[ci].fields
                          if ptr_valid(pfields) then begin 
                              
                              nfields = (*pcamcols)[ci].nfields
                              for fi=0l, nfields-1 do begin 

                                  ;; check for indices for this field
                                  pind = (*pfields)[fi].indices
                                  if ptr_valid(pind) then begin 
                                      ptr_free, pind
                                  endif 
                              endfor 
                              ptr_free, pfields
                          endif ;; there are fields for this camcol

                      endfor 
                      ptr_free,pcamcols
                  endif ;; there are camcols for this reruns
                  
              endfor 
              ptr_free, preruns
          endif ;; there are reruns for this run

      endfor 
      ptr_free, pruns

  endif ;; there are individual runs

end 











;; written by dave johnston, incorporated by es
function sdss_util::psfrec, pstruct, row, col, counts=counts

  np = (n_elements(pstruct) gt 0) + (n_elements(row) gt 0) + (n_elements(col) gt 0)
  if np lt 3 then begin 
      on_error, 2
      print,'-Syntax: psf_image = sdss_psfrec(pstruct, row, col, counts=)'
      message,'Halting'
  endif 

  rcs=.001
  
  nrow_b=(pstruct.nrow_b)[0]
  ncol_b=(pstruct.ncol_b)[0]
                                ;assumes they are the same for each eigen
                                ;so only use the 0 one
  rnrow=(pstruct.rnrow)[0]
  rncol=(pstruct.rncol)[0]

  nb=nrow_b*ncol_b
  coeffs=fltarr(nb)
  ecoeff=fltarr(3)
  cmat=pstruct.c
  
  
  for i=0l, nb-1 do coeffs[i]=(row*rcs)^(i mod nrow_b) * (col*rcs)^(i/nrow_b)
  
  for j=0,2 do begin
      for i=0l, nb-1 do begin
          ecoeff[j]=ecoeff[j]+cmat(i/nrow_b,i mod nrow_b,j)*coeffs[i]
      endfor	
  endfor
  p=(pstruct.rrows)[*,0]*ecoeff[0]+$
    (pstruct.rrows)[*,1]*ecoeff[1]+$
    (pstruct.rrows)[*,2]*ecoeff[2]
 
  if n_elements(counts) ne 0 then begin 
      p = p/total(p)*counts
  endif 

  p = reform(p,rncol,rnrow)
  return,p

end 

function sdss_util::cleanup
  return,1
end 

pro sdss_util__define

  struct = {$
             sdss_util, $
             _sdss_util_dummy: 0 $
           }

end 
