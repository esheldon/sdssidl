PRO asymerr,r1,r2,sig_asym

IF n_params() EQ 0 THEN BEGIN 
    print,'asymerr,r1,r2,sig_asym'
    return
END

;r1=image[center[0]-major_axis:center[0]+major_axis,$
;         center[1]:center[1]+minor_axis]
;r2=image[center[0]-major_axis:center[0]+major_axis,$
;         center[1]-minor_axis:center[1]]
n1=total(r1)
n2=total(r2)
sig_1=sqrt(n1)
sig_2=sqrt(n2)
sig_sub=sqrt(sig_1^2+sig_2^2)
sub=total(abs(r2-r1))
asym=sub/n2
sig_asym=sqrt((sig_sub/sub)^2+(1/sig_2)^2)*asym
return
END 
