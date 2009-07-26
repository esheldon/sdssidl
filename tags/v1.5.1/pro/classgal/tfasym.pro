;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME:
;   TFASYM
;    
;       
; PURPOSE:  
;     Measures two-fold asymmetry of an object.
;     Uses variable centers and shapes and g images.  Used in CLASSIFY_GALAXIES3
;    
; CALLING SEQUENCE:  
;     tfasym,pstruct,asym,asymerr,silent=silent,random=random,output=output,
;            image=image
;    
; INPUTS: 
;     pstruct:  photo structure of galaxy with tags for corrected shapes
;   
; OUTPUTS:  
;     asym:  returns asym for each object
;
; KEYWORD PARAMETERS:
;     silent:  no printed output, if not set outputs images and values of 
;          subtractions
;     random:  goes through sample randomly and shows images of subtractions 
;          and outputs displays various information, used in testing
;     output:  prints numbers to show progress on large sample
;    
; CALLED ROUTINES:  
;     fetch_dir
;     get_atlas
;     asymerr
;     rdis     
;     
; PROCEDURE: 
;    Uses centers and dimensions from r image, r shapes (e1,e2), and g images.
;         Subtracts image contained within lower region surrounding center
;         over major axis.
;         Asym = abs(flux of subracted residuals)/flux of original image
;
; REVISION HISTORY:
;     Author:   Judith Racusin 3/00
;                   lots of changes 7/03/00
;                   finally decided to do image-1000
;                   distributions looks much better with (center-1)   8/07/00 
;                   split into sub-programs for mock-catalog 9/27/00
;                   version 5 with variable center  5/16/01
;                   fixed shape measurements 6/02
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO measure_asym,as,aserr,image,s,major_axis,minor_axis,theta,center,index,oldasym,npix,random=random,silent=silent,oimage,icenter=icenter,ias=ias,struct,amy=amy
  IF n_params() LT 7 THEN BEGIN 
      print,'Syntax - measure_asym,asym,asymerr,image,s,major_axis,minor_axis,theta,center,index,oldasym,npix,random=random,silent=silent,icenter=icenter,amy=amy'
      return 
  END
  as=0
  aserr=0
  IF (major_axis GT 1 AND minor_axis GT 1) THEN BEGIN
      tmpimage=intarr(max(s),max(s))
      tmpimage[*]=1000
      stmp=size(tmpimage,/dimensions)
      gray=stmp-s
      xstart=round(gray[0]/2)
      ystart=round(gray[1]/2)
      tmpimage[xstart:xstart+s[0]-1,ystart:ystart+s[1]-1]=image
      image=tmpimage
      center=center+[xstart,ystart]
      s=stmp
      
;;;;;finds pixel distance from major axis and then compare values across
      image=rot(image,theta,1,center[0],center[1], $
                /interp,/pivot,missing=1000)
      
      image[*]=image[*]-1000
      rimage=image
      image2=image
      image3=intarr(2*major_axis,minor_axis)
      image4=image3
      image5=image3
    
      FOR xx=0,2*major_axis-1 DO BEGIN
          x=center[0]-major_axis+xx
          IF (x LT s[0]-1 AND x GT 0) THEN BEGIN
              FOR yy=0,minor_axis-1 DO BEGIN
                  y=center[1]-minor_axis+yy
                  IF (2*center[1]-y LT s[1]-1 AND y GT 0) THEN BEGIN
                      IF (image2[x,y] NE 0 AND image(x,2*center[1]-y) NE 0)$
                        THEN BEGIN 
                          image2[x,y]=abs(image(x,y)-image(x,2*center[1]-y))
                          image4[xx,yy]=image2[x,y]
                          image3[xx,yy]=image[x,y]
                          image5[xx,yy]=image[x,2*center[1]-y]
                      ENDIF
                  ENDIF
              ENDFOR
          ENDIF
      ENDFOR
      as=total(image4)/total(image3)
      asymerr,image5,image3,sig_asym
      aserr=sig_asym
  ENDIF
  IF NOT keyword_set(silent) THEN BEGIN 
      viewout,image,index,as,aserr,theta,center,major_axis,minor_axis,image4,image3,oldasym,random=random,oimage,rimage,icenter=icenter,ias=ias,struct,amy=amy
  ENDIF 
  return
END

PRO viewout,image,index,as,aserr,theta,center,major_axis,minor_axis,image4,image3,oldasym,random=random,oimage,rimage,icenter=icenter,ias=ias,struct,amy=amy
  loadct,0
  wset,0
  !p.multi=[0,3,2]
  rdis,oimage,/silent
  rdis,rimage,/silent
  image[center[0],center[1]]=0
  rdis_setup,image,pls  
  print,'index = ',index
  print,'asym = ',as,' +/-',aserr
  print,'l.asymmetry = ',oldasym
  print,'initial asymmetry =      ',ntostr(ias[0])+' +/-    '+ntostr(ias[1])
  print,'initial center = ',icenter
  print,'center = ',center
  print,'theta = ',theta
  print,'major axis = ',major_axis
  print,'minor axis = ',minor_axis
  print,'subtracted image = ',total(image4)
  print,'before',total(image3) 
  rdis,image,/silent  
  tvellipse,major_axis,minor_axis,center[0],center[1],color=0,/data,thick=2
  rdis,image3,/silent
  rdis,image4,/silent
  wset,1
  view_galaxies,struct,amy=amy
  print,'  (n next object) (q to quit)'
  key=get_kbrd(10)
  IF (key EQ 'q') THEN random=2
  IF NOT keyword_set(random) THEN random=0
  return
END

PRO tfasym,gstruct,asym,asymerr,silent=silent,random=random,output=output,$
            image=image,amy=amy


  IF N_PARAMS() eq 0 THEN BEGIN
      
      PRINT,'Syntax: tfasym3, galaxy list, asymmetry,asymerr, silent=silent,'
      print,'        random=random,output=output,amy=amy'
      return
  END
  time=systime(1)
  IF keyword_set(silent) OR NOT keyword_set(random) THEN random=0 
  n=n_elements(gstruct)
  asym=fltarr(n)
  asymerr=asym
  tags=tag_names(gstruct)
  tgs=where(tags EQ 'ASYMMETRY')
  IF tgs[0] NE -1 THEN strasym=gstruct.asymmetry ELSE strasym=replicate(0,n)
  cindex=1
  as=0 & aserr=0
  FOR inde=0L,n-1 do BEGIN
      IF NOT keyword_set(random) THEN index=inde ELSE BEGIN 
          COMMON seed,seed
          index=fix((n-1)*randomu(seed))
      ENDELSE
      IF (gstruct[index].e1[cindex] NE 1.e10 AND gstruct[index].e1[cindex] NE 1.e10) THEN BEGIN
          image=0
          fetch_dir,gstruct[index].run,gstruct[index].camcol, $
            gstruct[index].rerun,dir,atldir
          atlas_name,gstruct[index].run,gstruct[index].camcol, $
            gstruct[index].field,name
          IF fexist(atldir+name) THEN BEGIN
              get_atlas,gstruct,index,clr=cindex,dir=atldir,col0=col0, $
                row0=row0,/nodisplay,/silent,/noprompt,maxsize=[800,800],$
                img=image
              is=size(image,/dimensions)
;              major_axis=((sqrt(gstruct[index].ixx[cindex]+$
;                                     gstruct[index].iyy[cindex])))
              findabtheta,gstruct[index].e1[cindex],gstruct[index].e2[cindex],ba,theta
              major_axis=sqrt((gstruct[index].ixx[cindex]+gstruct[index].iyy[cindex])/(1+ba^2))
;              theta=.5*atan(gstruct[index].e2[cindex],$ ;; should be , not /
;                            gstruct[index].e1[cindex])
              minor_axis=major_axis*ba
;              minor_axis=round(sqrt(gstruct[index].iyy[cindex])/cos(theta))
              
              npix=major_axis*minor_axis
              theta=((theta*!radeg))
;              IF (gstruct[index].ixx[cindex] LT $
;                  gstruct[index].iyy[cindex]) THEN theta=theta+90
              oimage=image
              major_axis=major_axis*3
              minor_axis=minor_axis*3

              ;;; find center of least asymmetry
              icenter=[gstruct[index].colc[cindex]-col0-1., $
                            gstruct[index].rowc[cindex]-row0-1.]
              min_center=icenter
              cpx=[0,-1,0,1,1,1,0,-1,-1]/5.
              cpy=[0,1,1,1,0,-1,-1,-1,0]/5.
              casym=fltarr(9)
              casymerr=fltarr(9)
              ccenter=fltarr(2,9)
              ccenter[*,0]=icenter
              cccenter=ccenter
              s=is
              image=oimage
              center=icenter
              measure_asym,as,aserr,image,s,major_axis,minor_axis,$
                theta,center,index,strasym[index],npix,random=random,/silent
              ias=[as,aserr]
              min_asym=as
              min_asymerr=aserr
              it=0
              WHILE (min_asym NE casym[0] AND $
                     abs(min_center[0]-icenter[0]) LE 1 AND $ 
                     abs(min_center[1]-icenter[1]) LE 1 AND $
                     it LT 100) DO BEGIN 
                  casym[0]=min_asym
                  casymerr[0]=min_asymerr
                  FOR cp=1,8 DO BEGIN 
                      image=oimage
                      s=is 
                      center=min_center+[cpx[cp],cpy[cp]]
                      cccenter[*,cp]=center
                      wcen=where(ccenter[0,*] EQ center[0] AND $
                                ccenter[1,*] EQ center[1] AND $
                                 center[0] NE 0,nwcen)
                      IF nwcen EQ 0 THEN BEGIN
                          measure_asym,as,aserr,image,s,major_axis,minor_axis,$
                            theta,center,index,strasym[index],npix,$
                            random=random,/silent
                      ENDIF ELSE BEGIN
                          as=casym[wcen]
                          aserr=casymerr[wcen]
                      ENDELSE
                      casym[cp]=as
                      casymerr[cp]=aserr
                  ENDFOR
                  ccenter=cccenter
                  min_asym=min(casym,wmin)
                  min_asymerr=casymerr[wmin]
                  min_center=min_center+[cpx[wmin],cpy[wmin]]
                  ccenter[0]=min_center
                  it=it+1
              ENDWHILE 
              center=min_center
              asym[index]=min_asym
              asymerr[index]=min_asymerr
              image=oimage
              s=is
              IF NOT keyword_set(silent) THEN BEGIN 
                  IF index EQ 0 THEN window,1
                  measure_asym,as,aserr,image,s,major_axis,minor_axis,$
                               theta,center,index,strasym[index],npix,oimage,$
                               random=random,icenter=icenter,ias=ias,gstruct[index],amy=amy
              ENDIF 
              IF random EQ 2 THEN BEGIN 
                  !p.multi=0
                  return
              END
          ENDIF
      ENDIF 

      IF (keyword_set(output) AND (inde MOD 100 EQ 0 AND inde NE 0)) THEN BEGIN
          print,'.',format='(a,$)'
          IF (inde MOD 2000 EQ 0 AND inde NE 0) THEN print,''
      ENDIF
  ENDFOR
  IF NOT keyword_set(silent) THEN BEGIN 
      print
      print,'last object'
      key=get_kbrd(1)
      !p.multi=0
      setprefs
  ENDIF
  IF keyword_set(output) THEN BEGIN 
      print,''
      ptime,systime(1)-time
  ENDIF 
  center=0 & theta=0 & minor_axis=0 & major_axis=0 & s=0 & n=0
  image2=0 & x=0 & tmpimage=0 & stmp=0 & gray=0 & image3=0
      
  return
END
