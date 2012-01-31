PRO z_stuff,list,mag=mag,kcorr=kcorr,model=model,outstr=outstr,absmagerr=absmagerr


  IF n_params() EQ 0 THEN begin
      print,'syntax- z_stuff, pstruct, mag=mag, kcorr=kcorr,model=model,outstr=outstr,absmagerr=absmagerr'
      return
  END

;  IF (n_elements(list.classification) EQ 5) THEN n=2 ELSE n=0  
  IF keyword_set(outstr) THEN out=0 ELSE BEGIN 
      outstr=list
      out=1
  ENDELSE 
;  dist=lumdist(list.z1d)
  t0=tag_exist(outstr,'ABSMAG')
  t1=tag_exist(outstr,'KCORR')
  t2=tag_exist(outstr,'LUM')
  t3=tag_exist(outstr,'ABSMAGERR')

  t=getztag(list,zerrtag=terr)
  z=list.(t)
  zerr=list.(terr)
  dist=angdist_lambda(z,dlum=ldist)
  disterr=angdist_lambda(z-zerr,z+zerr,dlum=ldisterr)

  IF keyword_set(model) THEN BEGIN
      mag =list.counts_model 
      magerr=list.counts_modelerr
  ENDIF ELSE BEGIN
      mag=list.petrocounts
      magerr=list.petrocountserr
  ENDELSE 


  FOR clr=0,4 DO BEGIN
;      kcorr_color,z,clr,mag[clr,*],mag[1,*]-mag[2,*],$
;        kcorr,absmag,lum
      wtheta_absmag_diffz,z,clr,mag[clr,*],$
        list.counts_model[1]-list.counts_model[2], $
        absmag,lum,kcorr=kcorr
   
      absmagerr=sqrt(magerr[clr]^2+(ldisterr/ldist/alog(10))^2)

      IF (t0) THEN outstr.absmag[clr]=absmag
      IF (t1) THEN outstr.kcorr[clr]=kcorr
      IF (t2) THEN outstr.lum[clr]=lum
      IF (t3) THEN outstr.absmagerr[clr]=absmagerr
  ENDFOR
  IF out THEN BEGIN 
      list=outstr 
      outstr=0
  ENDIF 

  return
END
