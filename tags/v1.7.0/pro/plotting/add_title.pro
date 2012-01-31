
;+
; NAME:
;  add_title
;
; PURPOSE:
;  Add a title to a plot. Designed to create a single title for an
;  axis even when multiple plot regions are in use (e.g. with !p.multi
;  settings).
;
; CATEGORY:
;  Plotting
;
; CALLING SEQUENCE:
;  add_title, type, text, offset=, _extra=
;
; INPUTS:
;  type: Title type.  Currently supported:
;          'title','xtitle','ytitle'
;
; KEYWORD PARAMETERS:
;   offset=: Offset of the text with respect to the default position. 
;   _extra=: Extra plotting keywords
;
; EXAMPLE:
;  !p.multi = [0,2,2]
;  .... bunch of plots....
;  add_title, 'ytitle', 'Rest Frame g-r'
;
; MODIFICATION HISTORY:
;  Created from code in MPLOT.pro 2007-01-04, Erin Sheldon, NYU
;
;-
;  Copyright (C) 2005  Erin Sheldon, NYU.  erin dot sheldon at gmail dot com
;
;    This program is free software; you can redistribute it and/or modify
;    it under the terms of version 2 of the GNU General Public License as 
;    published by the Free Software Foundation.
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



function add_title_getinfo, type, offset=offset, charsize=charsize

    if n_elements(offset) eq 0 then offset=0.0

    ; This doesn't seem to be important....
    ncols = !p.multi[1]
    nrows = !p.multi[2]
    if ncols le 0 then ncols = 1
    if nrows le 0 then nrows = 1
    nplots = ncols * nrows

    if n_elements(charsize) eq 0 then charsize=!p.charsize

    xCharSizeNorm = float(!d.x_ch_size) / float(!d.x_size);*!p.charsize/charsize
    yCharSizeNorm = float(!d.y_ch_size) / float(!d.y_size);*!p.charsize/charsize

    mxMargin = !x.margin
    myMargin = !y.margin

    dx = (1. - XCharSizeNorm * total(MXMargin)) / float(ncols)
    dy = (1. - YCharSizeNorm * total(MYMargin)) / float(nrows)


    case strlowcase(type) of
        'title':begin
            x = 0.5 * dx * ncols + xCharSizeNorm * mxMargin[0]
            y = 1. - (myMargin[1] - 0 - offset) * yCharSizeNorm 
            st={x:x,y:y,align:0.5,orientation:0,charsize:charsize*1.25}
        end
        'xtitle': begin
            x = 0.5 * dx * ncols + xCharSizeNorm * mxMargin[0]
            y = yCharSizeNorm * (myMargin[0] - 3. - offset)
 
            st={x:x,y:y,align:0.5,orientation:0,charsize:charsize}
        end
        'ytitle': begin
            x = xCharSizeNorm * (mxMargin[0] - 6. - offset)
            y = 0.5 * dy * nrows + yCharSizeNorm * myMargin[0]
 
            st={x:x,y:y,align:0.5,orientation:90,charsize:charsize}
        end
        else: message,'Unsupported title type: '+string(type)
    endcase

    return, st

end
pro add_title, type, text, charsize=charsize, $
    offset=offset, $
    _extra=_extra

    if n_elements(type) eq 0 or n_elements(text) eq 0 then begin
        print,'-Syntax: add_title, type, text, charsize=, offset=, _extra='
        on_error, 2
        message,'Halting'
    endif

    st = add_title_getinfo(type, offset=offset, charsize=charsize)

    xyouts, $
        st.x, $
        st.y, $
        text, $
        /normal, $
        align = st.align, $
        orientation = st.orientation, $
        charsize=st.charsize, $
        _extra=_extra

end 
