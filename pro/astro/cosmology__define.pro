
;+
; NAME:
;   cosmology (IDL Class file)
;
; PURPOSE:
;   Implements some common distance measurements in cosmology. Follows
;   conventions in Hogg astro-ph/9905116
;
; CALLING SEQUENCE:
;   c=obj_new('cosmology')
;
; METHODS:
;   DH(h=) 
;       Hubble distance
;   Ez_inverse(z, omega_m=, omega_l=, omega_k=, flat=) 
;       1.0/sqrt( omega_m*(1+z)^3 + omega_k*(1+z)^2 + omega_l )
;   Ezinv_integral(z1, z2, omega_m=, omega_l=, omega_k=, flat=, npts=)
;       Integral of 1/E(z) from z1 to z2
;   Dc(z1, z2, omega_m=, omega_l=, omega_k=, flat=, h=, npts=)
;       Comoving distance (line of sight) in Mpc/h
;   Dm(z1, z2, omega_m=, omega_l=, omega_k=, flat=, h=, npts=)
;       Comoving distance (transverse) between two events in Mpc/h
;   Da(z1, z2, omega_m=, omega_l=, omega_k=, flat=, h=, npts=, Dl=)
;       angular diameter distance. e.g. angular size = physical size/Da in Mpc/h
;   Dl(z1, z2, omega_m=, omega_l=, omega_k=, flat=, h=, npts=, Da=)
;       luminosity distance. 
;   distmod(z, omega_m=, omega_l=, omega_k=, flat=, h=, npts=)
;       distance modulus. mag = absmag + distmod
;   velement(z, omega_m=, omega_l=, omega_k=, flat=, h=, npts=, /comoving)
;       volume element in (Mpc/h)^3 *per steradian*
;   volume(zmin, zmax, omega_m=, omega_l=, omega_k=, flat=, h=, npts=, vnpts=, /comoving)
;       volume between zmin,zmax in (Mpc/h)^3 *per steradian*
;
;   genrandz(nrand, zmin, zmax, omega_m=, omega_l=, omega_k=, flat=, h=, npts=, /comoving)
;       Generate random redshifts uniformly in a volume specified by zmin, zmax
;
; EXAMPLE:
;   c=obj_new('cosmology')
;   z=0.3
;   omega_m=0.2
;   ; Calculate angular diameter distance from 0 to 0.3
;   da = c->Da(0.0, z, omega_m=omega_m)
;
; RESTRICTIONS:
;   Requires gauleg, a C extension for calculating gauss-legendre weights. This
;   is easy to compile, just do ./configure and then make in the sdssidl base
;   directory
;
; MODIFICATION HISTORY:
;   Created some time in 2006, Erin Sheldon, NYU
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

function cosmology::init

  self.c = 2.9979e5             ; km/s
  self.H0 = 100.0               ; h km/s/Mpc
  self.h = 1.0                  ; default hubble parameter
  self.flat = 1                 ; default is flat
  self.omega_m = 0.27           ; default omega_m
  self.omega_l = 1.0-self.omega_m   ; default omega_l
  self.omega_k = 0.0            ; default omega_m
  self.npts = 5                 ; default number of integration points
  self.vnpts = 100              ; default number of velement int. points
  return, 1

end 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General tools for calculating distances.  Only assumption is that
;; lambda is a constant
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Hubble distance in Mpc/h
function cosmology::DH, h=h

  IF n_elements(h) EQ 0 THEN h=self.h
  return, self.c/self.H0/h

end 

function cosmology::defaults
    st = {                      $
        flat:    self.flat,     $
        omega_k: self.omega_k,  $
        omega_m: self.omega_m,  $
        omega_l: self.omega_l   $
    }
    return, st
end

pro cosmology::print_defaults
    print,'Defaults: '
    print,'    h       = '+string(self.h, f='(f4.2)')
    print,'    flat    = '+string(self.flat, f='(i0)')+' 1:true 0:false'
    print,'    omega_m = '+string(self.omega_m, f='(f4.2)')
    print,'    omega_l = '+string(self.omega_l, f='(f4.2)')
    print,'    omega_k = '+string(self.omega_k, f='(f4.2)')
    print,'    npts    = '+string(self.npts, f='(i0)')+'   #integration points Ez good to 1.e-7'
    print,'    vnpts   = '+string(self.vnpts, f='(i0)')+' #volume int. points'
end

function cosmology::_extract_omegas, omega_m, omega_l, omega_k, flat

    st = self->defaults()

    nm=n_elements(omega_m) & nl=n_elements(omega_l) & nk=n_elements(omega_k)
    nf=n_elements(flat)

    if nf ne 0 then st.flat = flat

    if nk ne 0 then ok=omega_k
    if keyword_set(st.flat) then ok = 0.0

    if keyword_set(st.flat) or ok eq 0.0 then begin
        if nm ne 0 then begin
            if omega_m gt 1.0 then message,'omega_m must be <= 1 for flat universe'
            st.omega_m = omega_m
            st.omega_l = 1.0-omega_m
        endif else if nl ne 0 then begin
            if omega_l gt 1.0 then message,'omega_l must be <= 1 for flat universe'
            st.omega_l = omega_l
            st.omega_m = 1.0-omega_l
        endif
    endif else begin
        ;; not flat: anything goes
        if nk ne 0 then st.omega_k=omega_k
        if nm ne 0 then st.omega_m=omega_m
        if nl ne 0 then st.omega_l=omega_l
    endelse

    if st.omega_m lt 0.0 then message,'omega_m must be greater than 0'
    if st.omega_l lt 0.0 then message,'omega_l must be greater than 0'

    return, st

end



function cosmology::Ez_inverse, z, $
        omega_m=omega_m, omega_l=omega_l, omega_k=omega_k, flat=flat

    if n_elements(z) eq 0 then begin
        print,'-Syntax: ezi = c->Ez_inverse(z, omega_m=, omega_l=, omega_k=, flat=)'
        self->print_defaults
        on_error, 2
        message,'Halting'
    endif
    o=self->_extract_omegas(omega_m, omega_l, omega_k, flat)
    Ez_inverse = 1.0/sqrt( o.omega_m*(1.+z)^3 + o.omega_k*(1.+z)^2 + o.omega_l)
    return, Ez_inverse

end 

;; integral from 0 to z of 1/E(z)
;; Ez_inverse is a *very* slowly varying function of z.  Can use only 
;; npts=2 for integration and still do better than 0.05% to redshift 1!!
;; npts=5 is essentially exact
function cosmology::Ezinv_integral, z1, z2, $
    omega_m=omega_m, omega_l=omega_l, omega_k=omega_k, flat=flat, npts=npts

    COMMON Ezinv_integral_block, XXi, WWi, zmin_old, zmax_old, npts_old

    n1=n_elements(z1) & n2=n_elements(z2)
    if n1 eq 0 or n2 eq 0 then begin
        print,'-Syntax: int=c->Ezinv_integral(z1, z2, omega_m=, omega_l=k, omega_k=, flat=, npts=)'
        self->print_defaults
        on_error, 2
        message,'Halting'
    endif

    zmin = min([z1,z2], max=zmax)

    if n_elements(npts) eq 0 then npts=self.npts

    if n_elements(xxi) ne 0 then begin 
        if ( (zmin ne zmin_old) or $
            (zmax ne zmax_old) or $
            (npts ne npts_old) ) then begin 
            gauleg, zmin, zmax, npts, xxi, wwi
      endif 
    endif else begin 
        gauleg, zmin, zmax, npts, xxi, wwi
    endelse 
  
    ;; Keep for later.  Can save time.
    zmin_old = zmin
    zmax_old = zmax
    npts_old = npts

    Ez_inverse = self->Ez_inverse(XXi, $
        omega_m=omega_m, omega_l=omega_l, omega_k=omega_k, flat=flat)
    return, total(Ez_inverse*WWi)

end 



;; comoving distance (line of sight)
function cosmology::Dc, z1, z2, omega_m=omega_m, omega_l=omega_l, omega_k=omega_k, flat=flat, h=h, npts=npts

    n1=n_elements(z1) & n2=n_elements(z2)
    if n1 eq 0 or n2 eq 0 then begin
        print,'-Syntax: '
        print,'   Dc = cosmo->Dc(z1, z2, omega_m=, omega_l=, omega_k=, flat=, h=, npts=)'
        print,' Returns comoving line-of-sight between z1 to z2 in Mpc/h'
        self->print_defaults
        on_error, 2
        message,'Halting'
    endif 

    nz1 = n_elements(z1)
    nz2 = n_elements(z2)
    nz = max([nz1,nz2])

    ;; Deal with all the different permutations
    if nz1 eq nz2 then begin 
        usez1 = z1
        usez2 = z2
    endif else begin 

        if (nz1 ne 1) and (nz2 ne 1) and (nz1 ne nz2) then begin 
            message,$
                'z1 and z1 must be same size unless one or both are scalars',/inf
            return,-1
        endif 

        if nz1 gt nz2 then begin 
            usez1 = z1
            usez2 = replicate(z2[0], nz1)
        endif else begin 
            usez1 = replicate(z1[0], nz2)
            usez2 = z2
        endelse 

    endelse 

    if nz gt 1 then dc = fltarr(nz) ELSE Dc = 0.0
  

    DH = self->DH(h=h)
    for i=0l, nz-1 do begin 

        Dc[i] = DH*self->Ezinv_integral(usez1[i], usez2[i], $
            omega_m=omega_m, omega_l=omega_l, omega_k=omega_k, flat=flat, npts=npts)
    endfor 
    return,Dc

end 

;; comoving distance (transverse) between two events at the same
;; redshift



function cosmology::Dm, z1, z2, omega_m=omega_m, omega_l=omega_l, omega_k=omega_k, flat=flat, h=h, npts=npts

    n1=n_elements(z1) & n2=n_elements(z2)
    if n1 eq 0 or n2 eq 0 then begin
        print,'-Syntax: Dm = cosmo->Dm(z1, z2, omega_m=, omega_l=, omega_k=, flat=, '+$
            'h=, npts=)'
        print,' Returns comoving transverse distance between z1 and z2 in Mpc/h'
        self->print_defaults
        on_error, 2
        message,'Halting'
    endif 

    o=self->_extract_omegas(omega_m, omega_l, omega_k, flat)

    DH = self->DH(h=h)
    Dc = self->Dc(z1, z2, $
        omega_m=omega_m, omega_l=omega_l, omega_k=omega_k, flat=flat, h=h, npts=npts)

    if o.omega_k eq 0 then begin 
        return, Dc
    endif else if o.omega_k gt 0 then begin 
        return, DH*1./sqrt(o.omega_l)*sinh(sqrt(o.omega_k)*Dc/DH)
    endif else begin 
        return, DH*1./sqrt(o.omega_l)*sin(sqrt(o.omega_k)*Dc/DH)
    endelse 

end 




;; Angular diameter distance 
function cosmology::Da, z1, z2, omega_m=omega_m, omega_l=omega_l, omega_k=omega_k, flat=flat, h=h, npts=npts, Dl=Dl

    n1=n_elements(z1) & n2=n_elements(z2)
    if n1 eq 0 or n2 eq 0 then begin
        print,'-Syntax: Da=cosmo->Da(z1, z2, omega_m=, omega_l=, omega_k=, flat=, '+$
            'h=, npts=, Dl=)'
        print
        print,' Returns angular diameter distance between z1 and z2 in units of Mpc/h'
        self->print_defaults
        on_error, 2
        message,'Halting'
    endif 

    DM12 = self->DM(z1, z2, $
        omega_m=omega_m, omega_l=omega_l, omega_k=omega_k, flat=flat, h=h, npts=npts)
    na = n_elements(DM12)

    Da = replicate(DM12[0], na)
    if na eq 1 then Da = Da[0]

    Dl = Da

    w = where(z1 lt z2, nw, comp=wc, ncomp=nwc)
    if nw ne 0 then begin 
        Da[w] = DM12[w]/(1.+z2[w])
        Dl[w] = DA[w]*(1.+z2[w])^2
    endif 

    if nwc ne 0 then begin 
        Da[wc] = DM12[wc]/(1.+z1[wc])
        DL[wc] = Da[wc]*(1.+z1[wc])^2
    endif 

    return,Da
end 

;; luminosity distance
function cosmology::Dl, z1, z2, omega_m=omega_m, omega_l=omega_l, omega_k=omega_k, flat=flat, h=h, npts=npts, DA=DA

    n1=n_elements(z1) & n2=n_elements(z2)
    if n1 eq 0 or n2 eq 0 then begin
        print,'-Syntax: Dl=cosmo->Dl(z1, z2, omega_m=, omega_l=, omega_k=, flat=, '+$
            'h=, npts=, Da=)'
        print
        print,' Returns luminosity distance between z1 and z2 in units of Mpc/h'
        self->print_defaults
        on_error, 2
        message,'Halting'
    endif 

    DA = self->Da(z1, z2, Dl=Dl, $
        omega_m=omega_m, omega_l=omega_l, omega_k=omega_k, flat=flat, h=h, npts=npts)
    return, Dl
end


; distance modulus
function cosmology::distmod, z, omega_m=omega_m, omega_l=omega_l, omega_k=omega_k, flat=flat, h=h, npts=npts, Dl=Dl

    if n_elements(z) eq 0 then begin 
        print,'-Syntax: dmod=cosmo->distmod(z, omega_m=, omega_l=, omega_k=, flat=, h=1, npts=, Dl=)'
        self->print_defaults
        on_error, 2
        message,'Halting'
    endif 

    dmpc = self->Dl(0.0, z, $
        omega_m=omega_m, omega_l=omega_l, omega_k=omega_k, flat=flat, h=h)
    dpc = dmpc*1.e6
    dm = 5.0*alog10(dpc/10.0)
    return, dm
end


;  volume element in (Mpc/h)^3 *per steradian*
function cosmology::velement, $
                  z, omega_m=omega_m, omega_l=omega_l, omega_k=omega_k, flat=flat,$
                  h=h, npts=npts, $
                  comoving=comoving

    if n_elements(z) eq 0 then begin 
        print,'-Syntax: dV = cosmo->velement(z, omega_m=, omega_l=, omega_k=, flat=,'
        print,'                              h=, npts=, /comoving)'
        print,'Returns volume element in (Mpc/h)^2 *per steradian*'
        self->print_defaults
        on_error, 2
        message,'Halting'
    endif 


    DH = self->DH(h=h)
    DA = self->Da(0.0, z, $
        omega_m=omega_m, omega_l=omega_l, omega_k=omega_k, flat=flat, h=h, npts=npts)

    Ez = 1.0/self->Ez_inverse(z, $
        omega_m=omega_m, omega_l=omega_l, omega_k=omega_k, flat=flat)


  if not keyword_set(comoving) then begin 
      ;; Proper volume element
      dV = DH*DA^2/Ez/(1.+z)
  endif else begin 
      ;; comoving volume element
      dV = DH*DA^2/Ez*(1.+z)^2
  endelse 

  return,dV
end 

;; Total volume between zmin and zmax in (Mpc/h)^3 *per steradian*
function cosmology::volume, $
                  zmin, zmax, $
                  omega_m=omega_m, omega_l=omega_l, omega_k=omega_k, flat=flat, h=h, $
                  npts=npts,vnpts=vnpts, $
                  comoving=comoving

    n1=n_elements(zmin) & n2=n_elements(zmax)
    if n1 eq 0 or n2 eq 0 then begin 
        print,'-Syntax: v = cosmo->volume(z1, z2, omega_m=, omega_l=, omega_k=, flat=,'
        print,'                           h=, npts=, vnpts=, /comoving)'
        print,'Returns volume in (Mpc/h)^3 *per steradian*'
        self->print_defaults
        on_error, 2
        message,'Halting'
    endif 

    if n_elements(vnpts) eq 0 then vnpts = self.vnpts
    gauleg, zmin, zmax, vnpts, XXi, WWi

    velements = $
        self->velement(XXi, $
        omega_m=omega_m, omega_l=omega_l, omega_k=omega_k, flat=flat, $
        h=h, npts=npts, $
        comoving=comoving)
    return, total(velements*WWi)


end 

;; Generate random zs drawn uniformly from the proper volume between
;; redshift zmin and zmax
function cosmology::genrandz, nrand, zmin, zmax, $
    omega_m=omega_m, omega_l=omega_l, omega_k=omega_k, flat=flat, $
    comoving=comoving, $
    h=h, npts=npts, doplot=doplot, bin=bin

    nr=n_elements(nrand) & n1=n_elements(zmin) & n2=n_elements(zmax)
    if nrand eq 0 or n1 eq 0 or n2 eq 0 then begin 
        print,'-Syntax: rz = cosmo->genrandz(nrand, zmin, zmax, '+$
            'omega_m=, omega_l=, omega_k=, flat=, /comoving, h=, npts=, /doplot, bin='
        print
        self->print_defaults
        on_error, 2
        message,'Halting'
    endif 

    nz = 1000
    z = arrscl( findgen(nz), zmin,  zmax )

    velement = self->velement(z, $
		omega_m=omega_m, omega_l=omega_l, omega_k=omega_k, $
        flat=flat, $
        comoving=comoving, $
        h=h, npts=npts)

    velement = velement/max(velement)

    genrand, velement, z, nrand, zrand, plt=doplot, bin=bin

    return,zrand

end 


pro cosmology__define

  struct = { $
             cosmology, $
             c: 0.0,    $       ; speed of light
             H0: 0.0,   $       ; Hubble constant h=1
             h: 0.0,    $       ; default hubble parameter
             flat: 0,      $    ; default flat
             omega_m: 0.0, $    ; default omega_m
             omega_l: 0.0, $    ; default omega_l
             omega_k: 0.0, $    ; default omega_k
             npts: 0,   $       ; default number of integration points        
             vnpts: 0,   $      ; default number of velement int. points
             cgauleg: 0  $      ; is gauleg c function compiled? else use idl version.
           }

end 
