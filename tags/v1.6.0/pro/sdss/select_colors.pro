pro select_colors,cat,index,str,stars=stars,qso=qso,$
qfile=qfile,hist=hist,radrange=radrange,psf=psf,petro=petro,_extra=ex

;+
; NAME:
;	select_colors
;
;PURPOSE:
;	An interactive tool for selection sdss objects based on their
;	colors ,  magnitudes and size. It will make the following four plots:
;	u-g versus g-r
;	g-r versus r-i
;	r-i versus i-z
;	r versus petrorad(in r color) 
;	Then it prompts you to draw an arbitrary closed curve on all 
;	four plots. From these curves it selects the objects having
;	parameters contained in all four regions and outputs their index.
;		
;CALLING SEQUENCE:
;   -syntax select_colors,cat,index,str,stars=stars,qso=qso
;	qfile=qsofile,hist=hist,radrange=radrange,psf=psf,petro=petro
;
;INPUTS:
;	cat : the sdss photo-output structure
;	
;OUTPUTS:
;	index : the index of selected points
;	str : the structure that is made by search_color_space
;		and given to make_color_fLag
;		can be saved as a fits binary table for later use
;		(see those two programs for details)
;	
;KEYWORDS:
;	stars : if set will dispay only stars 
;	        (so one can see the locus better)
;	qso : if set will overplot known quasar colors (to aid 
;		selection of quasars)
;	qfile : fits binary file of known quasar colors
;                 if not given will try the default file		
;	hist : if set it will use 2D histograms rather than scatter plots.
;		Aids visualization when many objects are plotted and is faster
;		when there are many thousands to plot.
;	radrange : the range on petrorad , the default is [0,6]
;	psf : if set it will use psfcounts instead of fibercounts
;	petro  : if set it will use petrocounts instead of fibercounts
;	_extra=ex : extra keywords eventually handed to ploth or plot 
;
;EXTERNAL CALLS:
;	search_color_space
;	make_color_flag
;	
;METHOD:
;	uses IDL function defroi (Define Region Of Interest)
;	explained in search_color_space
;	and make_color_flag
;	
;EXAMPLE OF USES:
;	defining the stellar locus
;	selecting objects with quasar colors
;	star galaxy seperation
;	looking for interesting objects
;	make multiple selections like so:
;
;	IDL> select_colors,cat,stellar
;	IDL> select_colors,cat(stellar),qsocolor
;
;	IDL> qsoindex=stellar(qsocolor)
;   we have indexed the index so that cat(qsoindex) are indeed the
;   selected quasars 		
;	
;	
;NOTES
;
;HISTORY:  written by David Johnston -University of Chicago
;	Physics Department 1999
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

if n_params() eq 0 then begin
	print,'-syntax select_colors,cat,index,str,stars=stars,qso=qso,'
	print,'qfile=qfile,hist=hist,radrange=radrange,psf=psf,petro=petro'
	print,'_extra=ex'
	return
endif

if keyword_set(stars) then all=0 else all=1
;default is to use all objects

		;select away
	
search_color_space,cat,str,qso=qso,all=all,psf=psf,petro=petro,$
qfile=qfile,hist=hist,radrange=radrange,_extra=ex
		;made the structure containing you selection
		

make_color_flag,cat,str,flag,psf=psf,petro=petro
		;flag each objects if it was in selected areas

index=where(flag eq 15)
		
		;must be in ALL FOUR selected areas to quailfy
		;so all four bits are set,  15=2^0+2^1+2^2+2^4
return
end



