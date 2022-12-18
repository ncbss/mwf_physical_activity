#!/bin/sh 
rois=rois.txt

sed 1d $rois | awk '{print $1, $2}' | while read roi r; do # prints roi ($1) and r values ($2)
fslmaths ROI_JLF_$roi -add $r -sub 1 -thr 0 ${roi}_r_temp # applies r values to each roi mask
done

sed 1d $rois | awk '{print $1, $3}' | while read roi p; do # prints roi name ($1) and p values ($3)
fslmaths ROI_JLF_$roi -add $p -sub 1 -thr 0 ${roi}_p_temp # applies r values to each roi mask
done

# merging rois
for stat in r p # separate for r and p values
do
fslmaths  Ant_CR_${stat}_temp -add GCC_${stat}_temp \
    -add Sag_Strat_${stat}_temp \
    PASE_results_b_$stat.nii.gz
done

# erasing temporary files
rm *temp.nii.gz