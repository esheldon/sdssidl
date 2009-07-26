pro test

    ; generate lam eta
   
    n=100000
    rand_sinlam = arrscl( randomu(seed, n), sin(-!dpi/2), sin(!dpi/2), arrmin=0d, arrmax=1d)
    rlam = asin(rand_sinlam)*!r2d
    reta = arrscl(randomu(seed,  n), -90.0d, 90.0d)

    plot, rlam, reta, psym=3, color=(!d.n_colors-1)

    mf=expand_tilde('data/pixel_mask_dr4_basic')
    flags = sdsspix_mask(rlam,reta,mf)

    w=where(flags eq 0)
    pplot, rlam[w], reta[w], psym=3, /over, color=0

end
