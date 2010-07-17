
;+
;
; NAME:
;   c2i()
;       
; PURPOSE:
;   Convert a color name(s) to a IDL color index for use with plotting routines.
;   This is a more robust method to simpctable
;
; CALLING SEQUENCE:
;   cindex = c2i(colornames, bits=, /all, /sample, /showcolors, rct=,gct=,bct=)'
;    
; COMMENTS:
;   Below are the common colors set for all devices.  On 24-bit 
;   devices more colors are defined, these can be shown with /showcolors.
;   Colors names are not case sensitive.
;
;       Black White Red Green Blue Yellow Cyan Magenta
;   
;       LightSteelBlue     SkyBlue         DarkSlateGrey
;       SlateGrey          LightBlue       MidnightBlue
;       NavyBlue           RoyalBlue       DodgerBlue
;       DarkBlue           Turquoise       DarkGreen
;       SeaGreen           ForestGreen     LightGreen
;       Sienna             Firebrick       Salmon
;       Orange             OrangeRed       HotPink
;       DeepPink           Violet          DarkRed
;       Purple
;   
;       grey0 grey1 ... grey100
;
;   On a true-color display, many more colors are defined: all the colors that
;   are in the rgb.txt file on unix systems (see the showrgb program). 
;   You can send the keyword /showcolors the command showrgb, which shows
;   the color names. 
;
;   On an 8-bit display or device (such as postscript) a new color table is
;   loaded. 33 colors above are defined plus greys grey00 to grey100.  
;   If you want all greys from 0 to 255, run loadct,0
;
;   The PPLOT.PRO program accepts color strings through the color= keyword.
;   When a string is sent it calls this program to get the color index.
;
; INPUTS: 
;    colorname: A color name(s) in string form, e.g. 
;       'DarkGreen', or ['blue','red']
;
; OPTIONAL INPUTS:
;   bits=: input the bits/pixel.  Must be either 8 or 24. If not set, 
;           the number of available colors will be determined.
;   /showcolors: run the program showrgb if it exists on the machine
;   /all: Return a structure containing info about all available colors
;       for this device.
;   /sample: Return a a sample of color.
;   /struct: The output is a structure with names and indices.
;
; OUTPUTS
;   index: The color index that can be sent to idl plotting routines, unless
;       the /all or /sample keywords are set..
;
; OUTPUTS: 
;
; OPTIONAL OUTPUTS:
;   colorlist=: Array of the standard colors listed above.
;   rct=, gct=, bct=: The red, green, and blue color arrays
;       loaded for 8-bit devices Useful for writing gif files from the 
;       plotting window. This only makes sense on 8-bit devices
;
; CALLED ROUTINES:
;    (TVLCT) if 8-bit device
; 
; EXAMPLE:
;    IDL> 
;    IDL> plot, [0], /nodata, yrange=[-1.2,1.2],xrange=[0,2.*!pi], $
;    IDL>    color=c2i('black'), background=c2i('white'), xstyle=1
;    IDL> x = findgen(300)/299.*2.*!pi
;    IDL> y = sin(x) + randomn(seed,300)/5.
;    IDL> oplot, x, y, color=c2i('blue')
;    IDL> oplot, x, sin(x), color=c2i('red')
;
; REVISION HISTORY:
;   Created: 2007-10-18, Erin Sheldon, NYU
;                                      
;-                                       
;  Copyright (C) 2007  Erin Sheldon, NYU.  erin dot sheldon at gmail dot com
;
;    This program is free software; you can redistribute it and/or modify
;    it under the terms of version 2 of the GNU General Public License as 
;    published by the Free Software Foundation.

; read the rgb.txt file
function c2i_read_rgb, file=file

    ; If you want your rgb.txt somewhere else, alter this.
    ; make sure there are no comment lines in the file
    if n_elements(file) eq 0 then begin
        dir=getenv('SDSSIDL_DIR')
        if dir eq '' then message,'SDSSIDL_DIR environment variable not set'
        file=filepath(root=dir, subdir=['data'], 'rgb.txt')
    endif

    nl = file_lines(file)
    tstruct = replicate({r:-1L, g:-1L, b:-1L, name:''},nl)

    openr, lun, file, /get_lun
    readf, lun, tstruct, format='(3I4,A25)'
    free_lun, lun

    ; add IDL color index
    tstruct.name = strlowcase(strtrim(tstruct.name, 2))
    ; usually no dupes, but this also sorts for us
    tstruct = tstruct[rem_dup(tstruct.name)]
    cstruct = replicate({r:0L, $
                         g:0L, $
                         b:0L, $
                         index8:-1L, $
                         index24:0L, $
                         name:''},nl)
    struct_assign, tstruct, cstruct, /nozero
    cstruct.index24 = cstruct.r + 256L*(cstruct.g+256L*cstruct.b)
    return, cstruct
 
end 

; This must be done slowly due to possible dups and preserve order
function c2i_match_cnames, cnames_in, all=all, struct=struct

    common c2i_block, cstruct, xsetup, dexist, bitsperpixel, rtable, gtable, btable


    if keyword_set(all) then begin
        if bitsperpixel eq 8 then begin
            match_cstruct=where(cstruct.index8 ge 0, nw)
            if nw eq 0 then message,'No eight-bit colors set'
        endif else begin
            match_cstruct = lindgen(n_elements(cstruct)) 
        endelse
    endif else begin
        cnames=strlowcase(cnames_in)
        nnames = n_elements(cnames)
        match_cstruct = lonarr(nnames)
        for i=0L, nnames-1 do begin
            w=where(cstruct.name eq cnames[i], nw)
            if nw eq 0 then message,'Unknown color name: '+cnames[i]
            match_cstruct[i] = w[0]
        endfor
        if nnames eq 1 then match_cstruct = match_cstruct[0] 
    endelse
    return, match_cstruct

end

function c2i_info2struct, names, indices
    n=n_elements(names)
    st = replicate({name:'', index:0L}, n)
    st.name = names
    st.index = indices
    return, st
end
function c2i_extract_colorinfo, match_cstruct, struct=struct

    common c2i_block, cstruct, xsetup, dexist, bitsperpixel, rtable, gtable, btable

    if bitsperpixel eq 8 then begin
        index = cstruct[match_cstruct].index8
    endif else begin
        index = cstruct[match_cstruct].index24
    endelse

    if keyword_set(struct) then begin
        names = cstruct[match_cstruct].name
        return, c2i_info2struct(names, index)
    endif else begin
        return, index
    endelse

end

pro c2i_create8

    common c2i_block, cstruct, xsetup, dexist, bitsperpixel, rtable, gtable, btable

    ; Make sure we have the 24-bit colors read.
    if n_elements(cstruct) eq 0 then begin
        cstruct = c2i_read_rgb()
    endif

    n_colors=!d.n_colors < 256

    cnames8 = $
        ['black','magenta','red','green','blue','yellow','cyan','white',$
        'lightsteelblue','skyblue','darkslategrey',$
        'slategrey',$
        'lightblue','midnightblue','navyblue', $
        'royalblue','dodgerblue','darkblue', $
        'turquoise','darkgreen','seagreen', $
        'forestgreen','lightgreen','sienna', $
        'firebrick','salmon','orange', $
        'orangered','hotpink','deeppink', $
        'violet','darkred','purple']

    nc = n_elements(cnames8)

    ; how many positions are left for other colors (such as the greys)?
    left = n_colors-nc

    ; Get some greys
    ngreys=101
    greys = lindgen(ngreys)
    greynames = 'grey'+string(strtrim(greys,2))

    cnames8 = [greynames, cnames8]
    cnums = lindgen(n_elements(cnames8))

    ; Look up the rgb
    w=c2i_match_cnames(cnames8)
	rtable=cstruct[w].r
	gtable=cstruct[w].g
	btable=cstruct[w].b
    if dexist or (!d.name EQ 'PS') then begin
        tvlct, rtable, gtable, btable
    endif

    ; reset index8 and copy in values
    cstruct.index8 = -1
    cstruct[w].index8 = cnums



    ; also add alternative gray instead of grey
    greynames2 = 'gray'+string(strtrim(greys,2))
    w=c2i_match_cnames(greynames2)
    cstruct[w].index8 = cnums[0:ngreys-1]

end


function c2i_get_sample, struct=struct

    common c2i_block, cstruct, xsetup, dexist, bitsperpixel, rtable, gtable, btable
    ;; Can return a list of standard colors
    if !d.name eq 'PS' then begin 
        ; White background
        colornames =  ['Red','DarkGreen','Blue','Orange','DodgerBlue',$
            'Magenta',$
            'SkyBlue', 'DarkSlateGrey', $
            'SlateGrey','Firebrick','MidnightBlue',$
            'RoyalBlue','Cyan',$
            'DarkBlue','Turquoise',$
            'SeaGreen','ForestGreen','LightGreen',$
            'Sienna','Salmon',$
            'Yellow','OrangeRed','HotPink',$
            'DeepPink','Violet','DarkRed','Purple','Green']

    endif else begin 
        ; Dark background
        colornames =  ['Red','Green','Blue','Yellow','Cyan','Magenta',$
            'DarkGreen','SkyBlue', 'DarkSlateGrey', $
            'SlateGrey','Orange','MidnightBlue',$
            'RoyalBlue','DodgerBlue',$
            'DarkBlue','Turquoise',$
            'SeaGreen','ForestGreen','LightGreen',$
            'Sienna','Firebrick','Salmon',$
            'OrangeRed','HotPink',$
            'DeepPink','Violet','DarkRed','Purple']

    endelse 


    w=c2i_match_cnames(colornames)
    ind = c2i_extract_colorinfo(w)

    ; add the foreground color !p.color to the list
    oname = 'foreground'
    oind = !p.color
    ind = [oind, ind]
    names = [oname, colornames]

    if keyword_set(struct) then begin
        return, c2i_info2struct(names, ind)
    endif else begin
        return, ind
    endelse

end


pro c2i_getbits, bits=bits
    common c2i_block, cstruct, xsetup, dexist, bitsperpixel, rtable, gtable, btable
    if n_elements(bits) eq 0 then begin 
        if !d.n_colors le 256 then bitsperpixel=8 $
        else bitsperpixel=24
    endif else begin 
        bits=bits[0]
        if (bits eq 8) or (bits eq 24) then begin
            bitsperpixel=bits
        endif else begin
            message,'bits='+string(bits)+' is invalid. Must be 8 or 24'
        endelse
    endelse 
end

pro c2i_setup, bits=bits, rgbfile=rgbfile

    common c2i_block, cstruct, xsetup, dexist, bitsperpixel, rtable, gtable, btable

    c2i_getbits, bits=bits

    if n_elements(dexist) eq 0 then dexist = display_exists()

    if (!d.name eq 'X' $
                and !d.window eq -1 and n_elements(xsetup) eq 0) then begin
        if dexist then begin 
            window,/free,/pixmap,xs=4, ys=4
            wdelete, !d.window
            xsetup=1
        endif
    endif

    if n_elements(cstruct) eq 0 then cstruct = c2i_read_rgb(file=rgbfile)

    ; We need to create a color table for 8-bit devices
    if bitsperpixel eq 8 then begin
		; this will create the color table
		c2i_create8
	endif else begin
		; delete variables
		if n_elements(rtable) ne 0 then begin
			crap=temporary(rtable) 
			crap=temporary(gtable) 
			crap=temporary(btable) 
		endif
	endelse

end

pro c2i_showcolors
    common c2i_block, cstruct, xsetup, dexist, bitsperpixel, rtable, gtable, btable
    c2i_create8
    head='--- r -- g -- b -------------- name --- 8-bit'
    for i=0L, n_elements(cstruct)-1 do begin
        if cstruct[i].index8 ge 0 then avail='yes' else avail=' no'
        if (i mod 10) eq 0 then begin
            print,head
        endif
        print, $
            cstruct[i].r, cstruct[i].g, cstruct[i].b, $
            cstruct[i].name, avail, $
            format='(i5, i5, i5, A20, a10)'
    endfor
end

function c2i, cnames, $
        bits=bits, rgbfile=rgbfile, $
        struct=struct, all=all, sample=sample, $
        showcolors=showcolors, $
        rct=rct, gct=gct, bct=bct, $
        colorlist=colorlist

    common c2i_block, cstruct, xsetup, dexist, bitsperpixel, rtable, gtable, btable

    if ( (n_elements(cnames) eq 0) $
          and (not keyword_set(all)) $
          and (not keyword_set(sample)) $
          and (not keyword_set(showcolors)) ) then begin 
            print,'usage: cindex = c2i(colorname, bits=, /showcolors, '+$
                'colorlist=, rct=, gct=, bct=)'
            print
            message,'Halting'
    endif 


    ; set up dexist and such
    c2i_setup, bits=bits, rgbfile=rgbfile

    if keyword_set(showcolors) then begin
        c2i_showcolors
        return, -1
    endif 

    if keyword_set(sample) then return, c2i_get_sample(struct=struct)

    ; only convert strings.
    if size(cnames, /tname) ne 'STRING' then return, cnames

    w = c2i_match_cnames(cnames, all=all)

	if arg_present(rct) then rct=rtable
	if arg_present(gct) then gct=gtable
	if arg_present(bct) then bct=btable
    return, c2i_extract_colorinfo(w, struct=struct)

end 
