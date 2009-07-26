;+
; NAME:
;  BAYES_SET_COMMON
;
;
; PURPOSE:
;  Set the common block used by the bayesian s/g routines
;
;
; CATEGORY:
;  SDSS specific routine.
;
;
; CALLING SEQUENCE:
;  bayes_set_common
;
;
; MODIFICATION HISTORY:
;  ??-??-2004 Ryan Scranton
;
;     Current version: 1.5
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


PRO bayes_set_common

COMMON bayes_prob_block, PROB_PSF_DEFAULT, MAXMAG, MINMAG, MINSEE, MAXSEE, $
  PROBFLAG_NOITER_REQUESTED, PROBFLAG_NOITER, PROBFLAG_NOPROB, $
  PROBFLAG_NOMEAS, PROBFLAG_MAGBOUNDS, PROBFLAG_SEEINGBOUNDS, PROBFLAG_NEGCON,$
  SEE_A, SEE_OFF, MAG_A, MAG_OFF, N_SEEBINS, N_MAGBINS

print,'Setting common variable block:'

base = 2b
;; default for objc_prob_psf => default probgal = (1.-PROB_PSF_DEFAULT)
PROB_PSF_DEFAULT = -9999.

;; min/max mag and seeing for extrapolating the calibrations
MAXMAG = 22.5
MINMAG = 17.0
MINSEE = 0.7
MAXSEE = 1.8

N_SEEBINS = 12
N_MAGBINS = 15

SEE_A = 1.0*N_SEEBINS/(MAXSEE - MINSEE)
SEE_OFF = 0.5*(MAXSEE - MINSEE)/N_SEEBINS

MAG_A = 1.0*N_MAGBINS/(MAXMAG - MINMAG)
MAG_OFF = 0.5*(MAXMAG - MINMAG)/N_MAGBINS

print
print,'************************************************************'
print,'***       ',ntostr(N_MAGBINS),' magnitude bins from ',ntostr(MINMAG),$
      ' to ',ntostr(MAXMAG),'      ***'
print,'***  Index transform: ',ntostr(MAG_A),'(mag - ',ntostr(MINMAG),' - ',$
      ntostr(MAG_OFF),')  ***'
print,'***                                                      ***'
print,'***        ',ntostr(N_SEEBINS),' seeing bins from ',ntostr(MINSEE),$
      ' to ',ntostr(MAXSEE),'       ***'
print,'*** Index transform: ',ntostr(SEE_A),'(see - ',ntostr(MINSEE),' - ',$
      ntostr(SEE_OFF),') ***'
print,'************************************************************'
print


;; diagnostic flags

PROBFLAG_NOITER_REQUESTED = base^0 ;user requested no iteration
PROBFLAG_NOITER = base^1        ; No iteration performed because too 
                                ; few objects
PROBFLAG_NOPROB = base^2        ; Unable to measure probability
PROBFLAG_NOMEAS = base^3        ; Was not binned1 in g,r, or i or had only
                                ; bad measurements (bayes_combine_gri)
PROBFLAG_MAGBOUNDS = base^4     ; mag outside of extrapolation bounds: mag set
                                ; so > MINMAG < MAXMAG
PROBFLAG_SEEINGBOUNDS = base^5  ; seeing outside of extrapolation bounds: 
                                ; seeing set to > MINSEEING < MAXSEEING
PROBFLAG_NEGCON = base^6        ; galaxy probability set to 0.0 due to 
                                ; negative concentration; avoids problems with
                                ; tight stellar locus

RETURN

END
