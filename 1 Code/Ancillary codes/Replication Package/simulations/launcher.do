/* 	Run this file to reproduce simulations for
	Hitchhiker's Guide to Markup Estimation
	De Ridder, Grassi, Morzenti (2025)
*/ 



** preamble

* set working condition 
clear all 
global path "D:\Main\Dropbox\De Ridder Grassi Morzenti\ReplicationPackage_DGM2025\simulations"
global path_robustness "D:\Main\Dropbox\De Ridder Grassi Morzenti\ReplicationPackage_DGM2025\simulations"
global pathdata "$path\datagen\output_montecarlo"
cd "$path"
version 17 



** produce the panels 

* produce CSV files with the datasets 
shell matlab -noFigureWindows -r "try; run 'datagen\run_multi_file.m'; catch; end; quit" -wait //slow: 1 day runtime 
 
* produce stata files and segment data intro monte carlo repetitions 
python query //make sure that python is installed 
python script "$path\datagen\MonteCarlo_ProduceFileForStata.py"


** estimate the production function for all iterations - main results 

* name of the baseline dataset 
global dataset "results_dataReps_ForStata_baseline_sigma1d1_epsi10_alpha0d4_gamma0d8_eta1d1_saved_reps"
global robustness 0

* estimation routine - revenue (acf)
global depvar revenuefirm_obs
cd "$path"
do code\acf_estimator 

* estimation routine - quantity (dgm)
global depvar outputfirm_obs
do code\dgm_estimator 


** produce table 1 
do code\table1 

** produce table 2 
do code\table2 

** produce table 3 
do code\table3 

** produce table F1
do code\tableF1
 
** produce figure 1
do code\figure1 

** produce figure 2 
global robustness 1
do code\figure2 //slow (6 hours). Consider running the bottom part only. 

** produce figure 3 
global robustness 1
do code\figure3 //slow (22 hours). Consider running the bottom part only. 





