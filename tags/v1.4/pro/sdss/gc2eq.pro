
;+
;
; NAME:
;    GC2EQ
;       
; PURPOSE:
;    convert from great circle to equatorial coordinates
;
; CALLING SEQUENCE:
;    gc2eq, mu, nu, node, inc, ra, dec
;
; INPUTS: 
;    mu, nu: great circle coords.
;    node, inc: node and inclination of the stripe.
;       
; OUTPUTS: 
;   ra,dec: equatorial coords. 
;
; PROCEDURE: 
;    Taken from astrotools
;
; REVISION HISTORY:
;    14-NOV-2000  Erin Scott Sheldon UofMich Taken from astrotools
;       
;                                      
;-                                       
pro gc2eq, mu, nu, node, inc, ra, dec

    sdssidl_setup
    !sdss->gc2eq, mu, nu, node, inc, ra, dec

end 
