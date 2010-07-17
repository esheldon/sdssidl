PRO plot_1color, obj_struct, color, oplot_str=oplot_str,index=index, size=size, carr=carr,opcarr=opcarr

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME:  
;    PLOT_1COLOR
;       
; PURPOSE:  
;       make color-color plot from sloan colors.  Plot one set of data
;	from obj_struct.  Over plot, if requested, from oplot_str
;	
;
; CALLING SEQUENCE:  
;   oplot_colors, obj_struct, color, oplot_str=oplot_str,
;	index=index, size=size, carr=carr, opcarr=opcarr
;      
;                 
;
; INPUTS: 
;         obj_struct:  photo objc structure.
;	  color:       indicates which colors to plot. 
;		       color = 1 for g-r vs u-g
;                      color = 2 for r-i vs g-r
;                      color = 3 for i-z vs r-i
;
;         oplot_str:   (optional) struct to overplot
;         index:       (optional) indexes on obj_struct to use in plotting
;	  size:        (optional) if set, oplot_colors also plots size v 
; 				brightness
;
; OUTPUTS:  
;           carr:  (optional) output struct containing colors for obj_struct 
;	    opcarr:  (optional) output struct containing colors for oplot_str
;
; OPTIONAL OUTPUT ARRAYS:  
;     carr:  (optional) output struct containing colors 
;            for obj_struct 
;     opcarr:  (optional) output struct containing colors
;	       for oplot_str
;
; INPUT KEYWORD PARAMETERS:
; 
; PROCEDURE: 
;	
;	
;
; REVISION HISTORY: 
;    Authro: Erin Sheldon UM  2/6/99
;	
;       
;                                      
;-                                        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

if N_params() eq 0 then begin
	print,'plot_1color, obj_struct, color, oplot_str=oplot_str,index=index, size=size, carr=carr, opcarr=opcarr'
	print,'color = 1 for g-r vs u-g'
	print,'color = 2 for r-i vs g-r'
	print,'color = 3 for i-z vs r-i'
	return
endif

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

if keyword_set(index) then begin
	s=index
endif else begin
	s=lindgen(n_elements(obj_struct))
endelse

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

p_old=!p.multi
!p.multi=[0,1,1]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Plot brightness vs size if requested
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  if keyword_set(size) then begin

	!x.title='petrorad'		
	!y.title='fibercounts'
	plot,obj_struct(s).fibercounts(2),obj_struct(s).petrorad(2),$
		psym=3,xrange=[12,25],yrange=[0,10]
	result=get_kbrd(20)
  endif

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;   define colors and output structures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ug=obj_struct(s).fibercounts(0)-obj_struct(s).fibercounts(1)
  gr=obj_struct(s).fibercounts(1)-obj_struct(s).fibercounts(2)
  ri=obj_struct(s).fibercounts(2)-obj_struct(s).fibercounts(3)
  iz=obj_struct(s).fibercounts(3)-obj_struct(s).fibercounts(4)
	
  carr=findgen(4,n_elements(ug))
  carr(0,*)=ug
  carr(1,*)=gr
  carr(2,*)=ri
  carr(3,*)=iz

  if keyword_set(oplot_str) then begin
	ug_oplot = oplot_str.fibercounts(0)-oplot_str.fibercounts(1)
	gr_oplot = oplot_str.fibercounts(1)-oplot_str.fibercounts(2)
	ri_oplot = oplot_str.fibercounts(2)-oplot_str.fibercounts(3)
	iz_oplot = oplot_str.fibercounts(3)-oplot_str.fibercounts(4)

	opcarr=findgen(4,n_elements(ug_oplot))
	opcarr(0,*)=ug_oplot
	opcarr(1,*)=gr_oplot
	opcarr(2,*)=ri_oplot
	opcarr(3,*)=iz_oplot
	sym=4
  endif

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;   make plots
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
  if (color eq 1) then begin
	!y.title='g-r'	
	!x.title='u-g'
	plot,ug,gr,psym=3,xrange=[-1.,4.0],yrange=[-1.,3.0]
	if keyword_set(oplot_str) then begin
	   oplot,ug_oplot,gr_oplot,psym=sym
        endif
  endif else if (color eq 2 ) then begin

	!x.title='g-r'	
	!y.title='r-i'
	plot,gr,ri,psym=3,xrange=[-1.,3.0],yrange=[-1.,2.5]
	if keyword_set(oplot_str) then begin
	   oplot,gr_oplot,ri_oplot,psym=sym
	endif
  endif else if (color eq 3 ) then begin

	!x.title='r-i'	
	!y.title='i-z'
   	plot,ri,iz,psym=3,xrange=[-1.,2.5],yrange=[-1.,2.5]
	if keyword_set(oplot_str) then begin
	   oplot,ri_oplot,iz_oplot,psym=sym
	endif
  endif else begin
	print,'Invalid color option: ',color
  endelse

  !x.title=''
  !y.title=''
  !p.multi=p_old
	
return

END








