;+
; NAME:
;  FIELD_INDICES
;
;
; PURPOSE:
;  Takes arrays of run,rerun,camcol,field and finds the number of uniq hashs
;  (ie run-rerun-camcol-field congomerates) and returns the reverse_index (see
;  IDL "histogram" function docs) so that they can be grouped together easily
;  it also returns an index and a structure with {run,rerun,camcol,field}
;  such that the array str(index).run is equal to run etc. This attemps to be
;  clever and do things quickly and with minimal memory use
;
; CATEGORY:
;  SDSS specific routine.
;
;
; CALLING SEQUENCE:
;  field_indices, run, rerun, camcol, field, num, rev, index, str, h
;
; INPUTS:
;   run,rerun,camcol,field: in integer form
;
; OUTPUTS:
;   num:  the number of uniq field combos
;   rev: the reverse indices of histogram
;   index: described above, same number of elements as run,rerun...
;   str: array of num structures with run,rerun,camcol,field
;   h:   the histogram it self
;
; MODIFICATION HISTORY:
;  April 05 2003 Dave Johnston
;
;-


PRO field_indices,run,rerun,camcol,field,num,rev,index,str,h

  IF n_params() EQ 0 THEN BEGIN
      print,'-syntax field_indices,run,rerun,camcol,field,num,rev,index,str,h'
      return
  endif

  str0={run:0L,rerun:0L,camcol:0,field:0L}

  ten=ulong64(10)

  super=$
    ulong64(field)        + $
    ulong64(camcol)*ten^5 + $
    ulong64(rerun)*ten^6  + $
    ulong64(run)*ten^9

;a super index hash


  s=sort(super)
  us=sort(s)
  super=super(s)

  uni=uniq(super)
  num=n_elements(uni)

  n=n_elements(super)

  w=where(super NE shift(super,1))
;the new ones

;index=lonarr(n,/nozero)
  index=lonarr(n)
  val=0L

;give them consecutive tags so the histogram
;dones't have gazillions of empty bins

  str=replicate(str0,num)
  FOR i=0L, num-2 DO BEGIN
      index(w(i):w(i+1)-1)=val
      val=val+1
      str(i).run=run(s(w(i)))
      str(i).rerun=rerun(s(w(i)))
      str(i).camcol=camcol(s(w(i)))
      str(i).field=field(s(w(i)))
  ENDFOR
;last one special
  i=num-1
  index(w(i:*))=val
  str(i).run=run(s(w(i)))
  str(i).rerun=rerun(s(w(i)))
  str(i).camcol=camcol(s(w(i)))
  str(i).field=field(s(w(i)))

  index=index(us)
;unsort and histogram
  h=histogram(index,rev=rev)

  return
END
