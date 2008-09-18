pro hist2d,x,y,hist,xrange,yrange,nxbins,nybins,$
           reverse_indices=reverse_indices, $
           xbins=xbins, ybins=ybins

;+
; NAME:
;    HIST2D
;
; PURPOSE:
;    like the IDL built in function hist_2d
;    but better since it can accept floats
;    output histogram is a longarray(nxbins,nybins)
;    uses only the relevent data in range
;
;  Dave Johnston
;   added return of reverse_indices April 1 2003
;       Erin Sheldon, UofChicago. Added calculation of mean in x and y direction
;             30-Jul-2003
;-
;
;
;
;  Copyright (C) 2006  Dave Johnston
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

if n_params() lt 7 then begin
	print,'-syntax hist2d,x,y,hist,xrange,yrange,nxbins,nybins,reverse_indices=reverse_indices, xbins=xbins, ybins=ybins'
	return
endif

xrange=float(xrange)
yrange=float(yrange)

xmin=xrange(0)
xmax=xrange(1)
ymin=yrange(0)
ymax=yrange(1)

w=where(x gt xmin and x lt xmax and y gt ymin and y lt ymax,count)
if count eq 0 then begin
	hist=-1
        reverse_indices=-1
	return
endif

xind=floor((x(w)-xmin)*(nxbins/(xmax-xmin)))
yind=floor((y(w)-ymin)*(nybins/(ymax-ymin)))

ind=xind+nxbins*yind
h=histogram(ind,min=0l,max=long(nxbins*nybins)-1,reverse_indices=reverse_indices)
nh = n_elements(h)

hist=reform(h,nxbins,nybins)

IF arg_present(xbins) OR arg_present(ybins) THEN BEGIN 

    xbins = dblarr(nxbins)
    ybins = dblarr(nybins)

    hx = histogram(x, min=xmin, max=xmax, nbins=nxbins, rev=xrev)
    hy = histogram(y, min=ymin, max=ymax, nbins=nybins, rev=yrev)

    FOR i=0L, nxbins-1 DO BEGIN 
        IF xrev[i] NE xrev[i+1] THEN BEGIN 
            w2=xrev[ xrev[i]:xrev[i+1]-1  ]
            nw2 = n_elements(w2)

            xbins[i] = total(x[w2])/nw2

        ENDIF 
    ENDFOR 

    FOR i=0L, nxbins-1 DO BEGIN 
        IF yrev[i] NE yrev[i+1] THEN BEGIN 
            w2=yrev[ yrev[i]:yrev[i+1]-1  ]
            nw2 = n_elements(w2)

            ybins[i] = total(y[w2])/nw2

        ENDIF 
    ENDFOR 

ENDIF 

return
end



