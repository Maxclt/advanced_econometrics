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

while read Xdist algo popSize seed
do
    filename='../results/GA_SS_"'$Xdist'"_"'$algo'"_"'$popSize'"_"'$seed'".sh'
    echo $filename
    Rout_filename='../results/GA_SS_"'$Xdist'"_"'$algo'"_"'$popSize'"_"'$seed'".Rout'
    echo $Rout_filename
    touch $filename
    echo '#!/bin/bash' > $filename
    echo '' >> $filename
    echo 'export Xdist="'$Xdist'"' >> $filename
    echo 'export algo="'$algo'"' >> $filename
    echo 'export popSize="'$popSize'"' >> $filename    
    echo 'export seed="'$seed'"' >> $filename
    echo "R CMD BATCH --no-save CPT_GA_SS.R "$Rout_filename >> $filename
    chmod 755 $filename
    if [ $queue = "high" ]
    then
	sbatch -p high $filename
    elif [ $queue = "low" ]
    then
	sbatch $filename
    fi
done < $params_filename
