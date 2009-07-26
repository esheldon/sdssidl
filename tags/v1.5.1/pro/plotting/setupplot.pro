
;+
;
; NAME:
;    SETUPPLOT
;       
; PURPOSE:
;    Set up default plotting parameters. Parameters are taken from
;    the !pslayout system variable, created by the pslayout procedure.
;    The user should put a copy of pslayout in their path and change
;    the defaults to suit them. Note pslayout contains tags for setting
;    up X and Z buffer as well as PS
;
;    Also runs simpctable if load_simpctable=1 (default), to create a set of 
;	 colors, and defsymbols to define the system variables !tsym for true-type 
;	 font symbols and !vsym for vector drawn font symbols, and !csym for use 
;	 with either true-type or vector drawn fonts.  Note, this is obsoleted
;    by the textoidl() program which allows one to type tex symbols.
;
;	 The simpctable method is also being deprecated, as the c2i function is
;	 a better method, allowing the user to indicate a color name which it
;	 converts to the appropriate color index.
;
; CALLING SEQUENCE:
;    setupplot [, type, /help, /test, true=true, /invbw, 
;		load_simpctable=, tmpdir=]
;
; OPTIONAL INPUTS:
;    type: if given, the type is set using set_plot, type
;	 load_simpctable=: 1 for true, 0 for false.  Currently on by default but
;		this will change as the c2i(color_name) method is better.  See c2i.pro
;		for details.
;    tmpdir=: Temporary directory used by setfont for setting the vector
;       font. Default is /tmp
; KEYWORD PARAMETERS:
;    /true: Can set /true to use true-type fonts. Can be used to override
;           the x_true and ps_true tags in !pslayout
;    /test: run a test showing all the symbols created by defsymbols.pro
;    /help: print simple syntax/help.
;    /invbw: flip colors
;
; OUTPUTS: 
;    None unless /test, in which case some plots are made.
;
; OPTIONAL OUTPUTS:
;    None
;
; CALLED ROUTINES:
;    PSLAYOUT
;    SIMPCTABLE
;    DEFSYMBOLS
; 
; REVISION HISTORY:
;    19-Mar-2001 Erin Scott Sheldon UofMich
;    2007-08-28: Added setfont calls for vector fonts.
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

PRO setupplot, type, help=help, test=test, true=true, invbw=invbw, $
               greyscale=greyscale, load_simpctable=load_simpctable, tmpdir=tmpdir



    if keyword_set(help) then begin 
        print,'-Syntax: setupplot, display_type, /help, /test, /true, $'
        print,'   /invbw, /greyscale, tmpdir='
        print
        print,' Will setup plotting parameters; sets system variables'
        print,' for various plotting symbols. '
        print,' use type="ps" for postscript "x" for x-window'
        print,' If type is not given, then it is determined from the !d.flags'
        print,'Use doc_library,"msetupplot"  for more help.'  
        return
    endif 

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Check input device type (optional)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    if n_elements(type) ne 0 then begin 
        if size(type,/tname) ne 'STRING' then begin
            print,'type must be a string'
            return
        endif 
        set_plot,type
    endif

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; run pslayout. Will define !pslayout (if not already defined)
    ;; which contains defaults for postscript output and other stuff,
    ;; including whether or not we should use true-type fonts (or 
    ;; postscript fonts if device is 'ps')
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; the .true tag is obsolete, x_true and ps_true are better, 
    ;; can control each 
    pslayout
    if n_elements(true) eq 0 then begin
        if !pslayout.true eq 1 then true=1 else true=0
    endif 

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; set up a color table
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    if keyword_set(greyscale) then begin 
        loadct,0
    endif else begin 
        if tag_exist(!pslayout, 'load_simpctable') then begin
			load_simpctable = !pslayout.load_simpctable
		endif else begin
			; old way
			load_simpctable = 1
		endelse
        if load_simpctable then begin
            simpctable
		endif
    endelse

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; set default background to white if requested
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    if keyword_set(invbw) then begin 
        !p.background=!white
        !p.color = !black
    endif 

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; set system variables in device dependent way
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    case !d.name of
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Defaults for the postscript output
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        'PS': begin 
            !p.thick = !pslayout.ps_thick
            !x.thick = !pslayout.ps_xthick
            !y.thick = !pslayout.ps_ythick
            !x.ticklen = !pslayout.ps_xticklen
            !y.ticklen = !pslayout.ps_yticklen
            !p.charsize = !pslayout.ps_charsize
            !p.charthick = !pslayout.ps_charthick

            ;; symbols/font defined same way for true and postscript fonts
            if keyword_set(true) or !pslayout.ps_true then begin
                !p.font=0
            endif else begin
                !p.font=-1
                if !pslayout.font_index ge 3 then begin
                    setfont, !pslayout.font_index, tmpdir=tmpdir
                endif
            endelse
        end

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Defaults for the X-windows display
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        'X': begin
            !p.thick = !pslayout.x_thick
            !x.thick = !pslayout.x_xthick
            !y.thick = !pslayout.x_ythick
            !x.ticklen = !pslayout.x_xticklen
            !y.ticklen = !pslayout.x_yticklen
            !p.charsize = !pslayout.x_charsize
            !p.charthick= !pslayout.x_charthick

            ;; use true-type fonts in X?
            if keyword_set(true) or !pslayout.x_true then begin
                !p.font = 1 

                ;; set the default font
                ;; make a dummy window
                if display_exists() then begin 
                    window,/free,/pixmap,xsize=1,ysize=1
                    fset=!pslayout.font
                    IF !pslayout.bold THEN fset=fset+' bold'
                    IF !pslayout.italic THEN fset=fset+' italic'
                    device,set_font=fset,/tt_font
                    wdelete,!d.window
              endif 

            endif else begin
                !p.font=-1
                if !pslayout.font_index ge 3 then begin
                    setfont, !pslayout.font_index, tmpdir=tmpdir
                endif
            endelse 
        end 
        'Z': begin
            !p.thick = !pslayout.z_thick
            !x.thick = !pslayout.z_xthick
            !y.thick = !pslayout.z_ythick
            !x.ticklen = !pslayout.z_xticklen
            !y.ticklen = !pslayout.z_yticklen
            !p.charsize = !pslayout.z_charsize
            !p.charthick= !pslayout.z_charthick

            ;; use true-type fonts in X?
            if keyword_set(true) or !pslayout.x_true then begin
                !p.font = 1 
                fset=!pslayout.font
                if !pslayout.bold then fset=fset+' bold'
                if !pslayout.italic then fset=fset+' italic'
                device,set_font=fset,/tt_font
            endif else begin
                !p.font=-1
                if !pslayout.font_index ge 3 then begin
                    setfont, !pslayout.font_index, tmpdir=tmpdir
                endif
            endelse 
          
            ;; set the resolution of the z-buffer
            device,set_resolution = !pslayout.z_resolution
        end 
        else: message,'type '+type+' unknown'
    endcase 
  
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Define new system variables to aid plotting 
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; golden ratio: send to aplot
    defsysv,'!gratio', exists=exists
    IF NOT exists THEN defsysv,'!gratio',1.36603




    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; define the plotting symbols: !tsym for true and !vsym for vector
    ;; and the common symbols in !csym using the set !p.font to determine
    ;; which to use. Note, this is obsoleted by the textoidl() program.
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    defsymbols





    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; demonstrate the symbols if requested 
    ; go 3 columns per page
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	dopage = 1
	column = 1
	if keyword_set(test) then begin 

		if type eq 'PS' then size=0.85 else size=1.5

		if keyword_set(true) then begin
			print,'Testing true type font'
			sym=!TSYM 
			symtags = tag_names(sym)
			symtags = '!!TSYM.' + symtags
			firstmess='Symbols defined in !!TSYM system variable'
		endif else begin
			print,'Testing vector drawn font'
			sym=!VSYM
			symtags = tag_names(sym)
			symtags = '!!VSYM.' + symtags
			firstmess='Symbols defined in !!VSYM system variable'
		endelse 

		ntags = n_elements(symtags)

		plot,[0],/nodata,ystyle=4,xstyle=4
		xyouts,0,1,firstmess

		xstep = .35
		ystep = .1

		ystart = 0.9
		y=ystart
		x = 0.0
		for i=0, ntags-1 do begin 

			if ( (i+1) mod 10) eq 0 then begin

				column=column+1
				if (column eq 4) or (i eq 0)  then begin
					column=1
					x = 0
					if strupcase(type) eq 'X' then key=get_kbrd(1)
					plot,[0],/nodata,ystyle=4,xstyle=4
				endif else  x = x + xstep
				y = ystart

				xyouts, x, y, symtags[i]+'  '+SYM.(i),charsize=size
			endif else begin
				if i ne 0 then y = y - ystep
				xyouts, x, y, symtags[i]+'  '+SYM.(i),charsize=size
			endelse 
		endfor 

	endif 

  return 
END 
