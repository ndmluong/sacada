################################################################################
####### run_migale_scenario_FMMask20 ##########
################################################################################

## (10 seeds) 15001:15010 #####
for seedMG in {15001..15010}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FMMask20.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done

## (40 seeds) 15011:15050 #####
for seedMG in {15011..15050}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FMMask20.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done

## (50 seeds) 15051:15100 #####
for seedMG in {15051..15100}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FMMask20.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done







################################################################################
####### run_migale_scenario_FMMask40 ##########
################################################################################

## (10 seeds) 15101:15110 #####
for seedMG in {15101..15110}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FMMask40.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done



## (40 seeds) 15111:15150 #####
for seedMG in {15111..15150}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FMMask40.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done


## (50 seeds) 15151:15200 #####
for seedMG in {15151..15200}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FMMask40.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done













################################################################################
####### scenario/run_migale_scenario_FMMask60 ##########
################################################################################

## (10 seeds) 15201:15210 #####
for seedMG in {15201..15210}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FMMask60.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done


## (40 seeds) 15211:15250 #####
for seedMG in {15211..15250}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FMMask60.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done


## (50 seeds) 15251:15300 #####
for seedMG in {15251..15300}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FMMask60.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done











################################################################################
####### scenario/run_migale_scenario_FMMask90 ##########
################################################################################

## (10 seeds) 15301:15310 #####
for seedMG in {15301..15310}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FMMask90.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done


## (40 seeds) 15311:15350 #####
for seedMG in {15311..15350}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FMMask90.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done


## (50 seeds) 15351:15400 #####
for seedMG in {15351..15400}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FMMask90.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done












################################################################################
####### scenario/run_migale_scenario_FMMask95 ##########
################################################################################

## (10 seeds) 15401:15410 #####
for seedMG in {15401..15410}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FMMask95.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done



## (40 seeds) 15411:15450 #####
for seedMG in {15411..15450}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FMMask95.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done



## (50 seeds) 15451:15500 #####
for seedMG in {15451..15500}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FMMask95.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done



















################################################################################
####### scenario/run_migale_scenario_FMMask100 ##########
################################################################################

## (10 seeds) 15501:15510 #####
for seedMG in {15501..15510}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FMMask100.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done


## (40 seeds) 15511:15550 #####
for seedMG in {15511..15550}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FMMask100.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done


## (50 seeds) 15551:15600 #####
for seedMG in {15551..15600}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FMMask100.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done






















