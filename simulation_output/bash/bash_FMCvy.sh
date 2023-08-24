################################################################################
####### scenario/run_migale_scenario_FMCvy10 ##########
################################################################################

## (10 seeds) 17001:17010 #####
for seedMG in {17001..17010}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FMCvy10.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done


## (40 seeds) 17011:17050 #####
for seedMG in {17011..17050}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FMCvy10.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done


## (50 seeds) 17051:17100 #####
for seedMG in {17051..17100}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FMCvy10.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done















################################################################################
####### scenario/run_migale_scenario_FMCvy12 ##########
################################################################################

## (10 seeds) 17101:17110 #####
for seedMG in {17101..17110}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FMCvy12.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done


## (40 seeds) 17111:17150 #####
for seedMG in {17111..17150}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FMCvy12.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done



## (50 seeds) 17151:17200 #####
for seedMG in {17151..17200}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_Cvy12.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done



















################################################################################
####### scenario/run_migale_scenario_FMCvy16 ##########
################################################################################

## (10 seeds) 17201:17210 #####
for seedMG in {17201..17210}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FMCvy16.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done


## (40 seeds) 17211:17250 #####
for seedMG in {17211..17250}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FMCvy16.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done


## (50 seeds) 17251:17300 #####
for seedMG in {17251..17300}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FMCvy16.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done









################################################################################
####### scenario/run_migale_scenario_FMCvy18 ##########
################################################################################

## (10 seeds) 17301:17310 #####
for seedMG in {17301..17310}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FMCvy18.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done


## (40 seeds) 17311:17350 #####
for seedMG in {17311..17350}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FMCvy18.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done


## (50 seeds) 17351:17400 #####
for seedMG in {17351..17400}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FMCvy18.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done

















################################################################################
####### scenario/run_migale_scenario_FMCvy20 ##########
################################################################################

## (10 seeds) 17401:17410 #####
for seedMG in {17401..17410}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FMCvy20.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done


## (90 seeds) 17411:17500 #####
for seedMG in {17411..17500}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FMCvy20.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done
























