;+
; NAME:
;   BAYES_PARS
;
;
; PURPOSE:
;  Util for bayes_prob.pro
;
; MODIFICATION HISTORY:
;    Creation:  ??-??-?? Dave Johnston, UofChicago
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

PRO bayes_pars,mag,see,probflags,musl,sigsl,musg,sigsg,lgr,mug,sigg

  IF n_params() EQ 0 THEN BEGIN 
      print,'-syntax bayes_pars,mag,see,mus,sigs,mug,sigg'
      print,'version 1.5'
      RETURN
  ENDIF

  COMMON bayes_prob_block, PROB_PSF_DEFAULT, MAXMAG, MINMAG, MINSEE, MAXSEE, $
    PROBFLAG_NOITER_REQUESTED, PROBFLAG_NOITER, PROBFLAG_NOPROB, $
    PROBFLAG_NOMEAS, $
    PROBFLAG_MAGBOUNDS, PROBFLAG_SEEINGBOUNDS, $
    PROBFLAG_NEGCON,$
    SEE_A, SEE_OFF, MAG_A, MAG_OFF, N_SEEBINS, N_MAGBINS

  IF n_elements(PROB_PSF_DEFAULT) EQ 0 THEN bayes_set_common

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; check that mag/seeing fall within allowed bounds
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  w=where(mag LT MINMAG OR mag GT MAXMAG, nw)

  IF nw NE 0 THEN BEGIN 
      probflags[w] = probflags[w] + PROBFLAG_MAGBOUNDS
  ENDIF 

  w=where(see LT MINSEE OR see GT MAXSEE, nw)

  IF nw NE 0 THEN BEGIN 
      probflags[w] = probflags[w] + PROBFLAG_SEEINGBOUNDS
  ENDIF 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; now bound the magnitude and seeing
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  mag = mag > MINMAG < MAXMAG
  see = see > MINSEE < MAXSEE

  ;; calibrated from the stripe 82 match data
  ;; 
  ;; Generate indices relative to our fitting grid

  x = MAG_A*(mag - MINMAG - MAG_OFF) 
  y = SEE_A*(see - MINSEE - SEE_OFF)

  ;; Now we select the centers (mu) and widths (sig) for the galaxies and stars 


  musl_k=[[ -0.00701443, 0.00321561, -0.000717819, 9.30847e-05, -3.51283e-06, -8.11192e-08],$
          [ -0.000415959, -0.00830821, 0.00760051, -0.00216570, 0.000235683, -8.67706e-06],$
          [ -0.000446884, 0.00691001, -0.00600037, 0.00167486, -0.000180941, 6.65161e-06],$
          [ 0.000223591, -0.00179379, 0.00148413, -0.000406473, 4.34630e-05, -1.58762e-06],$
          [ -3.01937e-05, 0.000181002, -0.000143413, 3.85588e-05, -4.07808e-06, 1.47890e-07],$
          [ 1.29261e-06, -6.19237e-06, 4.70830e-06, -1.24214e-06, 1.29920e-07, -4.67768e-09]]

  sigsl_k=[[ 0.0115711, -0.00191679, 0.000542482, -7.36299e-05, 5.54749e-06, -1.77542e-07],$
           [ -0.00301161, 0.00110109, 0.000944296, -0.000509976, 7.08973e-05, -3.00588e-06],$
           [ 0.00160368, 0.000103352, -0.00124127, 0.000498084, -6.38622e-05, 2.61235e-06],$
           [ -0.000293584, -0.000188796, 0.000374835, -0.000133650, 1.64131e-05, -6.56817e-07],$
           [ 2.02272e-05, 2.93447e-05, -3.99121e-05, 1.32626e-05, -1.58047e-06, 6.22144e-08],$
           [ -3.50253e-07, -1.25797e-06, 1.39030e-06, -4.37792e-07, 5.09228e-08, -1.97793e-09]]

  musg_k=[[ 0.0165002, -0.00988859, 0.00507875, -0.00112418, 0.000108384, -3.73552e-06],$
          [ -0.00973144, 0.0188166, -0.0127328, 0.00335537, -0.000357000, 1.30351e-05],$
          [ 0.00495905, -0.0106167, 0.00792622, -0.00216384, 0.000233113, -8.55420e-06],$
          [ -0.00108353, 0.00239653, -0.00184410, 0.000504871, -5.42795e-05, 1.98871e-06],$
          [ 0.000102075, -0.000227473, 0.000175070, -4.74812e-05, 5.06688e-06, -1.84796e-07],$
          [ -3.33234e-06, 7.58468e-06, -5.72784e-06, 1.52915e-06, -1.61603e-07, 5.86035e-09]]

  sigsg_k=[[ 0.0221831, -0.00589387, 0.00879247, -0.00268196, 0.000302536, -1.15699e-05],$
           [ 0.0531871, -0.0402070, 0.00580212, 0.000408672, -0.000128564, 6.36434e-06],$
           [ -0.0233682, 0.0173520, -0.00325623, 5.93038e-05, 2.99501e-05, -1.81142e-06],$
           [ 0.00371733, -0.00283866, 0.000580794, -2.24187e-05, -3.66965e-06, 2.54660e-07],$
           [ -0.000253712, 0.000201226, -4.27504e-05, 1.94774e-06, 2.26935e-07, -1.68937e-08],$
           [ 6.32477e-06, -5.12693e-06, 1.10515e-06, -5.30222e-08, -5.39747e-09, 4.13155e-10]]

  lgr_k=[[ 0.0556723, 0.138251, -0.116634, 0.0344861, -0.00404264, 0.000164449],$
         [ 0.161436, -0.0627720, 0.0621250, -0.0226185, 0.00301539, -0.000130933],$
         [ -0.0794398, -0.0473407, 0.0187951, -0.00126767, -0.000143467, 1.29932e-05],$
         [ 0.0126446, 0.0190192, -0.00867982, 0.00145415, -0.000101341, 2.38350e-06],$
         [ -0.000688121, -0.00201728, 0.000916631, -0.000167623, 1.35004e-05, -3.92421e-07],$
         [ 1.05189e-05, 6.57607e-05, -2.94783e-05, 5.55034e-06, -4.66961e-07, 1.42719e-08]]

  mug_k=[[ 1.44625, 0.446643, -0.408241, 0.0919633, -0.00837731, 0.000271996],$
         [ -0.786273, -0.323539, 0.304736, -0.0671251, 0.00594831, -0.000188131],$
         [ 0.245215, 0.0843883, -0.0836454, 0.0180417, -0.00155653, 4.79342e-05],$
         [ -0.0349729, -0.0101782, 0.0106136, -0.00224894, 0.000189394, -5.68675e-06],$
         [ 0.00228684, 0.000575115, -0.000631853, 0.000131938, -1.08779e-05, 3.19121e-07],$
         [ -5.61126e-05, -1.23309e-05, 1.42893e-05, -2.94810e-06, 2.38627e-07, -6.85547e-09]]

  sigg_k=[[ 0.298022, 0.0943127, -0.0716760, 0.0144152, -0.00120282, 3.63441e-05],$
          [ -0.136706, -0.0520431, 0.0314966, -0.00462883, 0.000248477, -3.62803e-06],$
          [ 0.0354637, 0.00960617, -0.00344128, -0.000198302, 8.85351e-05, -4.70424e-06],$
          [ -0.00440980, -0.000700205, -0.000128642, 0.000187575, -2.87246e-05, 1.23639e-06],$
          [ 0.000264400, 1.45722e-05, 3.70932e-05, -1.97234e-05, 2.63611e-06, -1.07546e-07],$
          [ -6.14087e-06, 2.21248e-07, -1.43532e-06, 6.20071e-07, -7.84570e-08, 3.12242e-09]]



  bayes_eval_poly2d,x,y,musl_k,musl
  bayes_eval_poly2d,x,y,musg_k,musg
  bayes_eval_poly2d,x,y,mug_k,mug
  bayes_eval_poly2d,x,y,lgr_k,lgr
  bayes_eval_poly2d,x,y,sigsl_k,sigsl
  bayes_eval_poly2d,x,y,sigsg_k,sigsg
  bayes_eval_poly2d,x,y,sigg_k,sigg

  musg = musg > 0.01

  RETURN

END 

