


################################################################################
####### run_migale_scenario_FM1 ##########
################################################################################

## (10 seeds) 11001:11010 #####
for seedMG in {11001..11010}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FM1.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done

## (40 seeds) 11011:11050 #####
for seedMG in {11011..11050}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FM1.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done


## (50 seeds) 11051:11100 #####
for seedMG in {11051..11100}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FM1.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done


## correction (if aborted)
### (aborted 2022-08-04 12:21)
for seedMG in 11044 11016 11017 11012 11011 11015 11014
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FM1.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -q long.q -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done

















################################################################################
####### run_migale_scenario_FM2 ##########
################################################################################
## (10 seeds) 12001:12010 #####
for seedMG in {12001..12010}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FM2.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done

## (40 seeds) 12011:12050 #####
for seedMG in {12011..12050}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FM2.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done


## (50 seeds) 12051:12100 #####
for seedMG in {12051..12100}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FM2.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done



































################################################################################
####### run_migale_scenario_FM3 ##########
################################################################################
## (10 seeds) 13001:13010 #####
for seedMG in {13001..13010}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FM3.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done


## (40 seeds) 13011:13050 #####
for seedMG in {13011..13050}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FM3.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done

## (50 seeds) 13051:13100 #####
for seedMG in {13051..13100}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FM3.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done









































################################################################################
####### run_migale_scenario_FM4 ##########
################################################################################
## (10 seeds) 14001:14010 #####
for seedMG in {14001..14010}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FM4.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done



## (40 seeds) 14011:14050 #####
for seedMG in {14011..14050}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FM4.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done


## (50 seeds) 14051:14100 #####
for seedMG in {14051..14100}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FM4.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done


















################################################################################
####### run_migale_scenario_FM1b ##########
################################################################################

## (10 seeds) 11501:11510 #####
for seedMG in {11501..11510}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FM1b.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done
























################################################################################
####### run_migale_scenario_FM2b ##########
################################################################################

## (10 seeds) 12501:12510 #####
for seedMG in {12501..12510}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FM2b.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done


















################################################################################
####### run_migale_scenario_FM3b ##########
################################################################################

## (10 seeds) 13501:13510 #####
for seedMG in {13501..13510}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FM3b.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done
















################################################################################
####### run_migale_scenario_FM4b ##########
################################################################################

## (10 seeds) 14501:14510 #####
for seedMG in {14501..14510}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FM4b.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done















