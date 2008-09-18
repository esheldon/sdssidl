#!/bin/sh

#maskFile=/net/cheops1/data0/corrected/masks/pixel_mask_dr4_basic

#for nst in 1 2 3 4 5 6 7
#do
#    outFile=jackknife_samples_dr4_res256_nstripe$nst.dat
#    echo $outFile
#    echo ------------------------------------------------
#    echo
#    echo Running with nstripe = $nst
#    ./jackknife_stripe --n_stripe $nst $maskFile $outFile

#done


PrincetonMaskFile=/net/cheops1/data0/corrected/masks/pixel_mask_princeton_basic

for nst in 1 2 3 4 5 6 7
do
    outFile=jackknife_samples_princeton_res256_nstripe$nst.dat
    echo $outFile
    echo ------------------------------------------------
    echo
    echo Running with nstripe = $nst
    ./jackknife_stripe --n_stripe $nst $PrincetonMaskFile $outFile

done
exit

