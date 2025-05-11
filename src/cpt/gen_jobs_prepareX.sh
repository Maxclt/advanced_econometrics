#/bin/bash
queue="low"
while getopts h option
do
    case $option
    in
	h) queue="high"
    esac
done

shift $((OPTIND-1))

params_filename=$1

while read Xdist ratio ninds seed rounds
do
    filename='../results/CPT_PrepareX_"'$Xdist'"_"'$ratio'"_"'$ninds'"_"'$seed'".sh'
    echo $filename
    Rout_filename='../results/CPT_PrepareX_"'$Xdist'"_"'$ratio'"_"'$ninds'"_"'$seed'".Rout'
    echo $Rout_filename
    touch $filename
    echo '#!/bin/bash' > $filename
    echo '' >> $filename
    echo 'export Xdist="'$Xdist'"' >> $filename
    echo 'export ratio="'$ratio'"' >> $filename
    echo 'export ninds="'$ninds'"' >> $filename    
    echo 'export seed="'$seed'"' >> $filename    
    echo "R CMD BATCH --no-save CPT_expr_prepareX.R "$Rout_filename >> $filename
    chmod 755 $filename
    if [ $queue = "high" ]
    then
	sbatch -p high $filename
    elif [ $queue = "low" ]
    then
	sbatch $filename
    fi
done < $params_filename
