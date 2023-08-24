################################################################################
####### scenario/run_migale_scenario_FMAir600 ##########
################################################################################

## (10 seeds) 16001:16010 #####
for seedMG in {16001..16010}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FMAir600.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done


## (40 seeds) 16011:16050 #####
for seedMG in {16011..16050}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FMAir600.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done


## (50 seeds) 16051:16100 #####
for seedMG in {16051..16100}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FMAir600.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done














################################################################################
####### scenario/run_migale_scenario_FMAir1200 ##########
################################################################################

## (10 seeds) 16101:16110 #####
for seedMG in {16101..16110}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FMAir1200.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done


## (40 seeds) 16111:16150 #####
for seedMG in {16111..16150}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FMAir1200.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done


## (50 seeds) 16151:16200 #####
for seedMG in {16151..16200}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FMAir1200.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done












################################################################################
####### scenario/run_migale_scenario_FMAir1800 ##########
################################################################################

## (10 seeds) 16201:16210 #####
for seedMG in {16201..16210}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FMAir1800.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done


## (40 seeds) 16211:16250 #####
for seedMG in {16211..16250}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FMAir1800.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done


## (50 seeds) 16251:16300 #####
for seedMG in {16251..16300}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FMAir1800.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done




















################################################################################
####### scenario/run_migale_scenario_FMAir3000 ##########
################################################################################

## (10 seeds) 16301:16310 #####
for seedMG in {16301..16310}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FMAir3000.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done


## (40 seeds) 16311:16350 #####
for seedMG in {16311..16350}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FMAir3000.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done



## (50 seeds) 16351:16400 #####
for seedMG in {16351..16400}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FMAir3000.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done















################################################################################
####### scenario/run_migale_scenario_FMAir3600 ##########
################################################################################

## (10 seeds) 16401:16410 #####
for seedMG in {16401..16410}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FMAir3600.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done


## (40 seeds) 16411:16450 #####
for seedMG in {16411..16450}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FMAir3600.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done


## (50 seeds) 16451:16500 #####
for seedMG in {16451..16500}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FMAir3600.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done





















################################################################################
####### scenario/run_migale_scenario_FMAir4800 ##########
################################################################################

## (10 seeds) 16501:16510 #####
for seedMG in {16501..16510}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FMAir4800.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done


## (40 seeds) 16511:16550 #####
for seedMG in {16511..16550}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FMAir4800.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done


## (40 seeds) 16551:16600 #####
for seedMG in {16551..16600}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FMAir4800.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done











###################################################### AIR WITHOUT MASK ########









################################################################################
####### scenario/run_migale_scenario_FMAir1200bis ##########
################################################################################
## (10 seeds) 16601:16610 #####
for seedMG in {16601..16610}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FMAir1200bis.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done



## (40 seeds) 16611:16650 #####
for seedMG in {16611..16650}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FMAir1200bis.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done



## (50 seeds) 16651:16700 #####
for seedMG in {16651..16700}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FMAir1200bis.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done


















################################################################################
####### scenario/run_migale_scenario_FMAir3600bis ##########
################################################################################
## (10 seeds) 16701:16710 #####
for seedMG in {16701..16710}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FMAir3600bis.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done


## (40 seeds) 16711:16750 #####
for seedMG in {16711..16750}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FMAir3600bis.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done




## (50 seeds) 16751:16800 #####
for seedMG in {16751..16800}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FMAir3600bis.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done









################################################################################
####### scenario/run_migale_scenario_FMAir4800bis ##########
################################################################################
## (10 seeds) 16801:16810 #####
for seedMG in {16801..16810}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FMAir4800bis.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done



## (40 seeds) 16811:16850 #####
for seedMG in {16811..16850}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FMAir4800bis.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done



## (50 seeds) 16851:16900 #####
for seedMG in {16851..16900}
do dir="simulation_output/${seedMG}"
rm -rf $dir
mkdir -p $dir
echo "R CMD BATCH --no-save \"--args ${seedMG}\" scenario/run_migale_scenario_FMAir4800bis.R" $dir/RConsole.Rout | qsub -S /bin/bash -pe thread 5 -m a -o ${dir}/${seedMG}.out -e ${dir}/${seedMG}.err -cwd -v PATH -N S${seedMG} -M ngoc-du.luong@anses.fr
done
