;+
; NAME:
;   setfont
;
; PURPOSE:
;   Set the font.  Currently only supports vector fonts, for which it is
;       more important since it is difficult to do properly on the fly.
;
; CALLING SEQUENCE:
;   setfont, font
;
; INPUTS:
;   font: The vector font number.
; OPTIONAL INPUTS:
;   tmpdir=:  The directory to hold the temporary file.  Default is /tmp
;
; SIDE EFFECTS:
;   The default font is set.
;
; RESTRICTIONS:
;   Must be able to create a temporary file in tmpdir.
;
; EXAMPLE:
;   ; Use the Complex Roman font
;   setfont, 6
;
; MODIFICATION HISTORY:
;   2007-08-25: Creation, Erin Sheldon, NYU
;
;-
;  Copyright (C) 2007  Erin Sheldon
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
pro setfont, font, tmpdir=tmpdir

    if n_elements(font) eq 0 then begin
        print,'-Syntax: setfont, font, tmpdir='
        on_error, 2
        message,'Halting'
    endif

    if n_elements(tmpdir) eq 0 then tmpdir='/tmp'

    ; Only vector fonts for now
    vf = strtrim( string(font[0]), 2)

    odevice = !d.name
    if odevice eq 'PS' then device, /close

    ofile=tmpfile(tmpdir=tmpdir, suffix='.ps')

    set_plot, 'PS'
    device, file=ofile

    plot,[0],title='!'+vf+'junk'
    device, /close
    set_plot, odevice

    file_delete, ofile, /quiet

end
