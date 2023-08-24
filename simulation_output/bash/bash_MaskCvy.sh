################################################################################
####### scenario/run_migale_scenario_M0Cvy11 ##########
################################################################################

## (50 seeds) 30001:30050 #####
for seedMG in {30001..30050}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_M0Cvy11.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done

## (50 seeds) 30051:30100 #####
for seedMG in {30051..30100}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_M0Cvy11.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done

## correction (if aborted)
### (aborted Migalerte 2022-09-13)
for seedMG in 30033 30038
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_M0Cvy11.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done





################################################################################
####### scenario/run_migale_scenario_M0Cvy14 ##########
################################################################################

## (50 seeds) 30101:30150 #####
for seedMG in {30101..30150}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_M0Cvy14.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done

## (50 seeds) 30151:30200 #####
for seedMG in {30151..30200}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_M0Cvy14.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done

## correction (if aborted)
### (aborted Migalerte 2022-11-117)
for seedMG in 30154 30155 30156
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_M0Cvy14.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done







################################################################################
####### scenario/run_migale_scenario_M0Cvy17 ##########
################################################################################

## (50 seeds) 30201:30250 #####
for seedMG in {30201..30250}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_M0Cvy17.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done

## (50 seeds) 30251:30300 #####
for seedMG in {30251..30300}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_M0Cvy17.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done












################################################################################
####### scenario/run_migale_scenario_M0Cvy20 ##########
################################################################################

## (50 seeds) 30301:30350 #####
for seedMG in {30301..30350}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_M0Cvy20.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done

## (50 seeds) 30351:30400 #####
for seedMG in {30351..30400}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_M0Cvy20.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done

## correction (if aborted)
### (aborted Migalerte 2022-09-13 & 2022-11-17)
for seedMG in 30339 30380 30387
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_M0Cvy20.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done



#####################################################################################################################################################################################
#####################################################################################################################################################################################
#####################################################################################################################################################################################
#####################################################################################################################################################################################
#####################################################################################################################################################################################
#####################################################################################################################################################################################
#####################################################################################################################################################################################
#####################################################################################################################################################################################
#####################################################################################################################################################################################
#####################################################################################################################################################################################

################################################################################
####### scenario/run_migale_scenario_M25Cvy11 ##########
################################################################################

## (50 seeds) 30401:30450 #####
for seedMG in {30401..30450}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_M25Cvy11.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done

## (50 seeds) 30451:30500 #####
for seedMG in {30451..30500}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_M25Cvy11.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done





################################################################################
####### scenario/run_migale_scenario_M25Cvy14 ##########
################################################################################

## (50 seeds) 30501:30550 #####
for seedMG in {30501..30550}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_M25Cvy14.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done

## (50 seeds) 30551:30600 #####
for seedMG in {30551..30600}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_M25Cvy14.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done

## correction (if aborted)
### (aborted Migalerte 2022-09-14 & 2022-11-18)
for seedMG in 30502 30599 30600
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_M25Cvy14.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done



################################################################################
####### scenario/run_migale_scenario_M25Cvy17 ##########
################################################################################

## (50 seeds) 30601:30650 #####
for seedMG in {30601..30650}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_M25Cvy17.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done

## (50 seeds) 30651:30700 #####
for seedMG in {30651..30700}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_M25Cvy17.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done

## correction (if aborted)
### (aborted Migalerte 2022-09-14 & 2022-11-18)
for seedMG in 30625 30651 30682
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_M25Cvy17.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done





################################################################################
####### scenario/run_migale_scenario_M25Cvy20 ##########
################################################################################

## (50 seeds) 30701:30750 #####
for seedMG in {30701..30750}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_M25Cvy20.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done

## (50 seeds) 30751:30800 #####
for seedMG in {30751..30800}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_M25Cvy20.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done

## correction (if aborted)
### (aborted Migalerte 2022-09-14 & 2022-11-18)
for seedMG in 30731 30758
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_M25Cvy20.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done





#####################################################################################################################################################################################
#####################################################################################################################################################################################
#####################################################################################################################################################################################
#####################################################################################################################################################################################
#####################################################################################################################################################################################
#####################################################################################################################################################################################
#####################################################################################################################################################################################
#####################################################################################################################################################################################
#####################################################################################################################################################################################
#####################################################################################################################################################################################

################################################################################
####### scenario/run_migale_scenario_M50Cvy11 ##########
################################################################################

## (50 seeds) 30801:30850 #####
for seedMG in {30801..30850}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_M50Cvy11.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done

## (50 seeds) 30851:30900 #####
for seedMG in {30851..30900}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_M50Cvy11.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done





################################################################################
####### scenario/run_migale_scenario_M50Cvy14 ##########
################################################################################

## (50 seeds) 30901:30950 #####
for seedMG in {30901..30950}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_M50Cvy14.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done

## (50 seeds) 30951:31000 #####
for seedMG in {30951..31000}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_M50Cvy14.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done






################################################################################
####### scenario/run_migale_scenario_M50Cvy17 ##########
################################################################################

## (50 seeds) 31001:31050 #####
for seedMG in {31001..31050}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_M50Cvy17.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done

## (50 seeds) 31051:31100 #####
for seedMG in {31051..31100}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_M50Cvy17.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done

## correction (if aborted)
### (aborted Migalerte 2022-09-15)
for seedMG in 31036
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_M50Cvy17.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done




################################################################################
####### scenario/run_migale_scenario_M50Cvy20 ##########
################################################################################

## (50 seeds) 31101:31150 #####
for seedMG in {31101..31150}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_M50Cvy20.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done

## (50 seeds) 31151:31200 #####
for seedMG in {31151..31200}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_M50Cvy20.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done

## correction (if aborted)
### (aborted Migalerte 2022-09-15 & 2022-11-19)
for seedMG in 31118 31119 31123 31134 31153 31167 31176 31185 31196 31197
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_M50Cvy20.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done

### (killed Migalerte 2022-11-29)
for seedMG in 31166 31168 31159 31178 31151
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_M50Cvy20.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done















#####################################################################################################################################################################################
#####################################################################################################################################################################################
#####################################################################################################################################################################################
#####################################################################################################################################################################################
#####################################################################################################################################################################################
#####################################################################################################################################################################################
#####################################################################################################################################################################################
#####################################################################################################################################################################################
#####################################################################################################################################################################################
#####################################################################################################################################################################################

################################################################################
####### scenario/run_migale_scenario_M80Cvy11 ##########
################################################################################

## (50 seeds) 31201:31250 #####
for seedMG in {31201..31250}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_M80Cvy11.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done

## (50 seeds) 31251:31300 #####
for seedMG in {31251..31300}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_M80Cvy11.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done





################################################################################
####### scenario/run_migale_scenario_M80Cvy14 ##########
################################################################################

## (50 seeds) 31301:31350 #####
for seedMG in {31301..31350}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_M80Cvy14.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done

## (50 seeds) 31351:31400 #####
for seedMG in {31351..31400}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_M80Cvy14.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done






################################################################################
####### scenario/run_migale_scenario_M80Cvy17 ##########
################################################################################

## (50 seeds) 31401:31450 #####
for seedMG in {31401..31450}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_M80Cvy17.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done

## (50 seeds) 31451:31500 #####
for seedMG in {31451..31500}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_M80Cvy17.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done







################################################################################
####### scenario/run_migale_scenario_M80Cvy20 ##########
################################################################################

## (50 seeds) 31501:31550 #####
for seedMG in {31501..31550}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_M80Cvy20.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done

## (50 seeds) 31551:31600 #####
for seedMG in {31551..31600}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_M80Cvy20.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done

## correction (if aborted)
### (aborted Migalerte 2022-09-16 & 2022-11-19)
for seedMG in 31540 31569 31588
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_M80Cvy20.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done
























#####################################################################################################################################################################################
#####################################################################################################################################################################################
#####################################################################################################################################################################################
#####################################################################################################################################################################################
#####################################################################################################################################################################################
#####################################################################################################################################################################################
#####################################################################################################################################################################################
#####################################################################################################################################################################################
#####################################################################################################################################################################################
#####################################################################################################################################################################################

################################################################################
####### scenario/run_migale_scenario_M100Cvy11 ##########
################################################################################

## (50 seeds) 31601:31650 #####
for seedMG in {31601..31650}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_M100Cvy11.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done

## (50 seeds) 31651:31700 #####
for seedMG in {31651..31700}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_M100Cvy11.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done





################################################################################
####### scenario/run_migale_scenario_M100Cvy14 ##########
################################################################################

## (50 seeds) 31701:31750 #####
for seedMG in {31701..31750}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_M100Cvy14.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done

## (50 seeds) 31751:31800 #####
for seedMG in {31751..31800}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_M100Cvy14.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done






################################################################################
####### scenario/run_migale_scenario_M100Cvy17 ##########
################################################################################

## (50 seeds) 31801:31850 #####
for seedMG in {31801..31850}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_M100Cvy17.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done

## (50 seeds) 31851:31900 #####
for seedMG in {31851..31900}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_M100Cvy17.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done

## correction (if aborted)
### (aborted Migalerte 2022-09-16)
for seedMG in 31812 31813 31814 31826
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_M100Cvy17.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done



################################################################################
####### scenario/run_migale_scenario_M100Cvy20 ##########
################################################################################

## (50 seeds) 31901:31950 #####
for seedMG in {31901..31950}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_M100Cvy20.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done

## (50 seeds) 31951:32000 #####
for seedMG in {31951..32000}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_M100Cvy20.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done

## correction (if aborted)
### (aborted Migalerte 2022-09-16)
for seedMG in 31907 31912 31913 31914 31917 31918 31921 31922 31923 31931 31934 31943 31946
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_M100Cvy20.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done




