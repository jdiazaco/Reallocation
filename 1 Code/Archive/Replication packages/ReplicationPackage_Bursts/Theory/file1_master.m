%%% Replication Master for 'Creative Destruction through Innovation Bursts'
%%% by Giuseppe Berlingieri, Maarten De Ridder, Danial Lashkari, Davide
%%% Rigo. Econometrica, accepted 2026.  

%{ 
    % instructions: 
        1. In preamble, active execution flags. Full run recommended. 
        2. On line 32, insert stata exe file location. 
        3. Check top of stata_file1_master for Stata dependencies/versions. 
        4. Run this file. Runtime (8-2026, 8x 4.7 Ghz real cores) 6hrs
%} 

%% Preamble 

% Housekeeping
clc
clear
close all

% Execution flags
quantification      = 0;  % set to 1 to run model calibration
simulation          = 0;  % set to 1 to run firm simulations
calibrate_noise     = 1;  % set to 1 to calibrate noise SD (target: 26% continuing products share)
create_datasets     = 1;  % set to 1 to create CSV datasets for Stata
output_tables       = 1;  % set to 1 to generate output tables
run_stata_figures   = 1;  % set to 1 to run Stata figure scripts


%%%% BEFORE USE: CHANGE THE DIRECTORY OF THE STATA INSTALLATION %%%% 

% Stata directory (to produce plots) 
stata_exe = '"C:\Program Files\Stata18\StataMP-64.exe"';


%% Load parameters
disp('Loading exogenous parameters and targets..')
[s, s_quant, p, i, a] = file2_parameters_targets; %file2a_parameters_targets; %updated 4-3
disp('done!')


%% Quantify the model (optional)
if quantification == 1
    file3_quantification;
end

%% Load endogenous variables / parameters
disp('Load endogenous parameters and endogenous outcomes..')
load('estimation_results.mat');
disp('done!')

%% Simulation
if simulation == 1
    file4_simulation;
end

%% Monte Carlo count
nMC = 1;
if isfield(s_quant, 'nMC')
    nMC = s_quant.nMC;
end

%% Calibrate noise SD
if calibrate_noise == 1
    disp('Calibrating noise SD to match Shapley decomposition target...')
    target_share = 0.26; 
    % use first 10 datasets to calibrate shapley value 
    shock_sd = file5_calibrate_noise(target_share, 'montecarlo/simulationoutput', min(10,nMC), 'montecarlo/simulationoutput_raw_%02d');
    disp(['Calibrated noise SD: ', num2str(shock_sd)])
else
    % Load previously estimated noise SD
    load('estimated_noise_sd.mat', 'estimated_sd');
    shock_sd = estimated_sd;
    disp(['Loaded noise SD from file: ', num2str(shock_sd)])
end

%% Create datasets for Stata
if create_datasets == 1
    if nMC > 1
        for mc = 1:nMC
            suffix = sprintf('_%02d', mc);
            raw_main = sprintf('montecarlo/simulationoutput_raw_%02d', mc);
            raw_kk = sprintf('montecarlo/simulationoutput_kk_raw_%02d', mc);

            file6_create_datasets(raw_main, shock_sd, suffix, 'montecarlo/simulationoutput', 1);
            file6_create_datasets(raw_kk, shock_sd, suffix, 'montecarlo/simulationoutput_kk', 1);

            if exist(raw_main, 'dir') == 7
                rmdir(raw_main, 's');
            end
            if exist(raw_kk, 'dir') == 7
                rmdir(raw_kk, 's');
            end
        end
    else
        % Create datasets for main simulation output (uses shock_sd from calibration)
        file6_create_datasets('montecarlo/simulationoutput', shock_sd);

        % Create datasets for Klette-Kortum simulation output
        file6_create_datasets('montecarlo/simulationoutput_kk', shock_sd);
    end
end

%% Output: tables
if output_tables == 1
    file7_output_parameter_table;
    file8_output_targeted_moments;
end

%% Run Stata figures 
file9_plot_firmsizedistributions;
if run_stata_figures == 1
    disp('Running Stata figure scripts...')

    % Resolve the repo root from this script so the Stata wrapper is portable.
    script_path = mfilename('fullpath');
    if isempty(script_path)
        script_path = which('file1_master');
    end
    if isempty(script_path)
        repo_root = pwd;
    else
        repo_root = fileparts(script_path);
    end

    do_file = fullfile(repo_root, 'stata_file1_master.do');

    % Run Stata
    stata_cmd = sprintf('%s /e do "%s" "%s"', stata_exe, do_file, repo_root);
    [status, result] = system(stata_cmd);
    if status == 0
        disp('done!')
    else
        warning('Stata returned non-zero status. Check stata_run_figures_csv.log for details.')
        disp(result)
    end
end

%% Produce Figure 13
file10_plot_typeshare;

%% Produce Figure 14
file11_plot_growtcontribution;

% end
clc
disp('Replication is done!')
