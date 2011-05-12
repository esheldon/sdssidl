pro get_family, cat, pindex, parent, children, siblings, im, $
                dir=dir, clr=clr,$
                pmulti=pmulti, $
                field=field, run=run, camcol=camcol, maxsize=maxsize, $
                prompt=prompt, nodisplay=nodisplay, wsize=wsize, $
                maguse=maguse,$
                silent=silent,$
                hideradec=hideradec, trunc=trunc, _extra=extra


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
; NAME: 
;    GET_FAMILY
;
; PURPOSE: 
;    retrieves atlas images for an object and any other objects
;    associated with it in a blend.
;                                                                            
; Inputs: pstruct: the photo output structure. Must use sarahtags.     
;         index: the object's index in pstruct
;
; Optional inputs: dir: directory of atlas images
;                  field: field of the object. 
;		   run: run object is from
;                  camcol: camera column of object (1-6)
;                  maxsize: maximum size for images.  [500,500] is default.
;                  prompt: prompting for prompt=1
;                  nodisplay: no display if nodisplay =1
;                  silent: will run silently unless bad images or missing
;                          images are found
;                                                                   
; Outputs: parent: the array containing the index of the parent    
;	  children: the array containing the indices of the children 
;         siblings: the array containing the indices of the siblings  
;         im: the atlas image
;
; Examples:
;
;  View the family of an object and return family.
;
; IDL> struct = sdss_read('tsobj', run, camcol, field=field)
; IDL> index = 33
; IDL> get_family, struct, index, parent, children, siblings
;           				
; Author: Sarah Monk			
; Dave Johnston : only look in a vicinity of the original object
;	not the whole structure. Speeds it up considerably.
;                                                                 	
;-                          
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  common get_family_block,superindex,ssindex,unssindex
;the variable that sorts the cat, only do this once if at all
;basically a lookup table between the cat and the sorted cat 
;is set up without ever having to actually make the sorted cat

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; help message ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  if n_params() LT 2 then begin
      print,'Syntax: get_family, photo struct, index, '
      print,'           parent, children, siblings,image,'
      print,'           dir=dir, field=field, run=run, camcol=camcol, '
      print,'           maxsize=maxsize,prompt=prompt,nodisplay=nodisplay, '
      print,'           wsize=wsize, silent=silent, hideradec=hideradec,'
      print,'           _extra=extra'

      print,'* Photo struct must have id, field and parent tags.'
      print,'  (Using make_sarahtags instead of the default lensing taglist will work.'
      print,'Use doc_library,"get_family"  for more help'
      return
  endif


;just look in the vicinity not the entire structure
;so make a smaller structure to search
;don't forget to change the indices upon returning
;this speeds up this program when using big catalogs

  parent = -1L
  children = -1L
  siblings = -1L

  numcat = n_elements(cat)
  startcat = ( pindex[0] - 50 > 0 )
  endcat = ( pindex[0] + 50 < (numcat-1) )

  pstruct = cat(startcat:endcat)
  index = long(pindex[0] - startcat)

  superloc=100000l*pstruct.field+pstruct.id

;a super index of pstruct. Will be sorted if cat was already in 
;original sorted order because will never be more than 100000 
;objects in a field

  sss=superloc(sort(superloc))- superloc
  www=where(sss ne 0,unsorted)
  doit =(n_elements(ssindex) ne numcat)

;superindex may exist but is for a different catalog
;so need to make it again (because they are different sizes)

  sortflag=0
  if unsorted ne 0 then begin	
      sortflag=1
      madesuper=0
      if (n_elements(superindex) eq 0) or doit then begin
          doit_again:print,'making a NEW superindex'
                                ;this is a goto lablel
          print,'making superindex for sorting'
          superindex=100000l*cat.field+cat.id
          ssindex=sort(superindex)
          unssindex=sort(ssindex)
                                ;the unsorted index to go the other way
          madesuper=1
                                ;so now we have a lookup table defined by
                                ;cat_sort=cat(ssindex)
                                ;and cat=cat_sort(unssindex)
                                ;without having to actually make cat_sort
                                ;(possibly a big memory variable)
      endif   
      spindex=unssindex(pindex[0])
                                ;the index of our object in the sorted cat
      
      startcat=(spindex-50) > 0
      endcat=(spindex+50) < (numcat-1)
      pstruct=cat(ssindex(startcat:endcat))
                                ;now it will be the right local neighborhood
                                ;where the family lives
      index=spindex-startcat	
      
      if (madesuper eq 0) or 1 then begin
          superloc=100000l*pstruct.field+pstruct.id
          sss=superloc(sort(superloc))- superloc
          www=where(sss ne 0,unsorted)
          if unsorted ne 0 then goto, doit_again
               ;this is just to make sure that superindex has been made 
               ;for THIS particular cat so now check to see if pstruct
               ;is really sorted if not then must make a new superindex
               ;because even though it was of the right size it was not right.
      endif

  endif
;;;;;;;;;;;;;;        check some keywords                        ;;;;;;;;;;;

  if (not keyword_set(nodisplay) ) then nodisplay=0
  if (not keyword_set(prompt) ) then noprompt=1 else noprompt=0
  if (not keyword_set(hideradec) ) then hideradec = 0
  if keyword_set(silent) then silent=1 else silent=0
  IF NOT keyword_set(trunc) THEN trunc = 0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; print object's field & id ;;;;;;;;;;;;;;;;;;

  IF (NOT silent) THEN BEGIN 
      print,''
      print,'-----------------------------'
      print,'Your object has photo id:  '+ntostr(pstruct(index).id)
      print,'Your object is in field:  '+ntostr(pstruct(index).field)
  ENDIF 

;;;;;;;;;;;; does it have a parent? If yes, then... ;;;;;;;;;;;;;;;;;;;;;;;;

  IF (pstruct(index).parent NE -1) THEN BEGIN

      ;; The parent may not be in the structure in some rare cases
      parent=where(pstruct.field eq pstruct(index).field and $
                   pstruct.id eq pstruct(index).parent, nparent)
      
      ;; ret_* = indices in original structure.
      IF nparent NE 0 THEN BEGIN 
          ret_parent=parent+startcat
          if sortflag THEN ret_parent=ssindex(ret_parent)
      ENDIF ELSE ret_parent = parent
      ret_parent = ntostr(ret_parent)

;;;;;;;;;;;; if so, find brothers and sisters ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      siblings=where(pstruct.parent eq pstruct(index).parent and $
                     pstruct.field eq pstruct(index).field, nsib)

      ;; ret_* = indices in original structure.
      IF nsib NE  0 THEN BEGIN 
          ret_siblings=siblings+startcat
          if sortflag then ret_siblings=ssindex(ret_siblings)
      ENDIF 
      ret_siblings = ntostr(ret_siblings)

      IF (nsib EQ 1 AND (NOT silent)) THEN BEGIN
          print,'Your object is the faint version of:  ',ret_parent
      ENDIF ELSE IF (NOT silent) THEN BEGIN 
          print,'The parent of your object is:  ',ret_parent
          print,'The siblings of your object have indices:  ', ret_siblings
      ENDIF



;;;;;;;;;;; now find any children ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      children=where(pstruct.field eq pstruct(index).field and $
                     pstruct.parent eq pstruct(index).id, nchild)

      ;; ret_* = indices in original structure.
      if nchild NE 0 then begin
          ret_children=children+startcat
          if sortflag then ret_children=ssindex(ret_children)
      ENDIF ELSE ret_children = children
      ret_children = ntostr(ret_children)

      if (nchild EQ 0 and (not silent)) then begin 
          print,'Not a parent'
      endif else if (not silent) then begin
          print,'Has children with indices:  ',ret_children
      endif



;;;;;;;;;; if it doesn't have a parent, check for kids ;;;;;;;;;;;;;;;;;;;;;

  endif else begin
      if (not silent) then print,'Not a child'

      children=where(pstruct.field eq pstruct(index).field and $
                     pstruct.parent eq pstruct(index).id, nchild)

      ;; ret_* = indices in original structure.
      if nchild NE 0 then begin
          ret_children=children+startcat
          if sortflag then ret_children=ssindex(ret_children)
      ENDIF ELSE ret_children = children
      ret_children = ntostr(ret_children)

      if (nchild eq 1 and (not silent) ) then begin
          print,'The faint version of your object has index:  ',ret_children
      endif else if ( (not silent) AND (nchild NE 0) )then begin
          print,'Has children with indices:  ',ret_children
      endif

      if ((size(children))(0) eq 0 and (not silent) ) then begin 
          print,'Not a parent'
      endif
    


;;;;;;;;;;;;;;;;;;;;;;;;;;; and check for grandkids ;;;;;;;;;;;;;;;;;;;;;;;;;

      if nchild EQ 1 then begin

          gchildren=where(pstruct.field eq pstruct(index).field and $
                          pstruct.parent eq pstruct(children).id, ngchild)

          ;; ret_* = indices in original structure.
          if ngchild NE 0 then begin
              ret_gchildren=gchildren+startcat
              if sortflag then ret_gchildren=ssindex(ret_gchildren)
          endif	ELSE ret_gchildren = gchildren
          ret_gchildren = ntostr(ret_gchildren)

          if (ngchild NE 0 and (not silent) ) then begin 
              print,'The faint version of your object has '+$
                'children with indices: ',ret_gchildren

          endif else if (not silent) then begin
              print,'No children'
          endif
      endif
  endelse



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; display atlas images ;;;;;;;;;;;;;;;;;;;;;;


    IF (not keyword_set(nodisplay) ) then begin 

        if (not silent) then print,'Parent displayed first, then children'
      
        if keyword_set(wsize) then begin
            if (!d.x_size ne wsize(0) and !d.y_size ne wsize(1)) then begin
                window,xsize=wsize(0),ysize=wsize(1)
            endif
        endif
        pmulti_old = !p.multi

        ;;;;;;;;;;;;; not a grandparent -> show itself, kids
        if ((size(gchildren))(0) eq 0 and (size(children))(0) ne 0) then begin
            a = nchild + 1

            pchild = lindgen(nchild)
            IF n_elements(pmulti) EQ 0 THEN BEGIN 
                CASE 1 OF 
                    (a LE 5): !p.multi=[0,1,a]
                    ( (a GT 5) AND (a LE 10) ): !p.multi=[0,2,(a/2) + (a MOD 2)]
                    (a GT 10): BEGIN
                        IF trunc THEN pchild = lindgen(9)
                        !p.multi = [0,2,5]
                    END 
                ENDCASE 
            ENDIF ELSE BEGIN 
                !p.multi=pmulti
            ENDELSE 

            view_atlas, pstruct, index=index, imtot=im, clr=clr, /silent, $
                maguse=maguse, hideradec=hideradec
                ytitle='Input obj'
            for ich=0L, nchild-1 do begin 
                ii = children[ pchild[ich] ]
                view_atlas, pstruct, index=ii, imtot=im, clr=clr, $
                    maguse=maguse, hideradec=hideradec, $
                    _extra=extra,/silent  
            endfor

            if (not silent) then print,'Your object is the parent'
            !p.multi=pmulti_old

            ; return the indices to the indices of the big structure
            ; watch out for negative default values and remember that 
            ; you may have sorted

            if parent[0] NE -1 then begin
                if parent(0) gt 0 then begin
                    parent=parent+startcat
                    if sortflag then parent=ssindex(parent)
                endif
            ENDIF 
            if children[0] NE -1 then begin
                if children(0) gt 0 then begin
                    children=children+startcat
                    if sortflag then children=ssindex(children)
                endif
            ENDIF 
            if siblings[0] NE -1 then begin
                if siblings(0) gt 0 then begin
                    siblings=siblings+startcat
                    if sortflag then siblings=ssindex(siblings)
                endif
            ENDIF
            return
        endif 
      

        ;;;;;;;;;;;;; if _only_ has children.... ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        if (parent eq -1 and (size(children))(0) ne 0 and $
            (size(gchildren))(0) eq 0) then BEGIN

            a = nchild + 1
            IF n_elements(pmulti) EQ 0 THEN BEGIN 
                CASE 1 OF 
                    (a LE 5): !p.multi=[0,1,a]
                    ( (a GT 5) AND (a LE 10) ): !p.multi=[0,2,(a/2) + (a MOD 2)]
                    (a GT 10): BEGIN 
                        IF trunc THEN pchild = lindgen(9)
                        !p.multi = [0,2,5]
                    END 
                ENDCASE 
            ENDIF ELSE BEGIN 
                !p.multi=pmulti
            ENDELSE 

            view_atlas, pstruct, index=index, imtot=im, clr=clr, /silent, $
                maguse=maguse, hideradec=hideradec
                ytitle='Input obj'
            for ich=0L, nchild-1 do begin 
                ii = children[ pchild[ich] ]
                view_atlas, pstruct, index=ii, imtot=im, clr=clr, $
                    maguse=maguse, hideradec=hideradec, $
                    _extra=extra,/silent  
            endfor

            if (not silent) then print,'Your object is the parent'
        endif 

    
        ;;;;;;;;;;;;;;; if a grandparent -> show child, gchildren ;;;;;;;;;;;
        if ((size(gchildren))(0) ne 0) then begin
            a = ngchild + 1

            pgchild = lindgen(ngchild)
            IF n_elements(pmulti) EQ 0 THEN BEGIN 
                CASE 1 OF 
                    (a LE 5): !p.multi=[0,1,a]
                    ( (a GT 5) AND (a LE 10) ): !p.multi=[0,2,(a/2) + (a MOD 2)]
                    (a GT 10): BEGIN
                        IF trunc THEN pgchild = lindgen(9)
                        !p.multi = [0,2,5]
                    END 
                ENDCASE 
            ENDIF ELSE BEGIN 
                !p.multi=pmulti
            ENDELSE 
            
            view_atlas, pstruct, index=pchild, imtot=im, clr=clr, $
                ytitle='Faint of input', $
                maguse=maguse, hideradec=hideradec, $
                _extra=extra,/silent  
            for ich=0L, ngchild-1 do begin 
                ii=gchildren[pgchild[ich]]
                view_atlas, pstruct, index=ii, imtot=im, clr=clr, $
                    maguse=maguse, hideradec=hideradec, $
                    _extra=extra,/silent  
            endfor

            if (not silent) then print,'Your object is the (bright) parent'
        endif
      
        ;;;;;;;;;;;;; if has a parent, no kids -> show parent, siblings ;;;;;  
        if (parent ne -1 and (size(children))(0) eq 0) then BEGIN

            a = nsib + 1

            psib = lindgen(nsib)
            nsib_use = nsib
            IF n_elements(pmulti) EQ 0 THEN BEGIN 
                CASE 1 OF 
                    (a LE 5): !p.multi=[0,1,a]
                    ( (a GT 5) AND (a LE 10) ): !p.multi=[0,2,(a/2) + (a MOD 2)]
                    (a GT 10): BEGIN
                        IF trunc THEN BEGIN
                            nsib_use = 9
                            w=where(siblings NE index, nw)
                            wi = where(siblings EQ index, nwi)
                            psib = [ siblings(wi), siblings(w[0:7]) ]
                        ENDIF 
                        !p.multi = [0,2,5]
                    END 
                ENDCASE
            ENDIF ELSE BEGIN 
                !p.multi=pmulti
            ENDELSE 

            view_atlas, pstruct, index=parent, imtot=im, clr=clr, $
                ytitle='Faint of input', $
                maguse=maguse, hideradec=hideradec, $
                _extra=extra,/silent  
            

            for ich=0L, nsib_use-1 do begin 
                ii=siblings[psib[ich]]
                if ii lt n_elements(pstruct) then begin
                    if ii eq index then begin
                        ytitle = 'Input obj'
                    endif else begin
                        ytitle=''
                    endelse
                    view_atlas, pstruct, index=ii, imtot=im, clr=clr, $
                        ytitle=ytitle, $
                        maguse=maguse, hideradec=hideradec, $
                        _extra=extra,/silent  
 
                endif
            endfor

          
            if (not silent) then print,'Your object is a child'    
        endif
      
        ;;;;;;; if orphan ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        if (parent eq -1 and (size(children))(0) eq 0) then begin
            !p.multi=0

            view_atlas, pstruct, index=index, imtot=im, clr=clr, $
                ytitle='Input obj', $
                maguse=maguse, hideradec=hideradec, $
                _extra=extra,/silent  

            if (not silent) then print,'Your object is an orphan'
        endif
        !p.multi= pmulti_old

        if (not silent) then begin
            print,'-----------------------------'
            print,''
        endif
      
    ENDIF                         ;Checked nodisplay
  
;return the indices to the indices of the big structure
;watch out for negative default values and remember that you may have sorted
  
    IF parent[0] NE -1 THEN BEGIN 
        IF parent(0) GT 0 THEN BEGIN 
            parent=parent+startcat
            IF sortflag THEN parent=ssindex(parent)
        ENDIF 
    ENDIF 
    IF children[0] NE -1 THEN BEGIN 
        IF children(0) GT 0 THEN BEGIN 
            children=children+startcat
            IF sortflag THEN children=ssindex(children)
        ENDIF 
    ENDIF 
    IF siblings[0] NE -1 THEN BEGIN 
        IF siblings(0) GT 0 THEN BEGIN 
            siblings=siblings+startcat
            IF sortflag THEN siblings=ssindex(siblings)
        ENDIF 
    ENDIF 

  return
end






