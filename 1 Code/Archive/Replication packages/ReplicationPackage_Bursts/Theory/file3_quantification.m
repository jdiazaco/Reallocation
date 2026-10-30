%% FILE3_QUANTIFICATION - Model Calibration (Memory-Optimized)
%
%   This script performs model quantification by calibrating parameters
%   to match target moments. Called from file1_master.m
%
%   Requires: s, s_quant, p, i, a (from file2a_parameters_targets)
%   Produces: estimation_results.mat (contains endo_best)
%
% OPTIMIZATION STRATEGY:
%   Phase 1 (PARALLEL): Calibration and growth calculations
%     - These are computationally light and can run in parallel
%   Phase 2 (SEQUENTIAL): Firm simulations
%     - Each simulation uses ~600K firms with large matrices
%     - Running in parallel exhausts memory
%     - Run one at a time to stay within memory limits
%
% NUMERICAL PRESERVATION: All computations unchanged from baseline

disp('Starting model quantification')

% Use grid parameters from structure
grid_avgstep = a.grid_avgstep;
grid_zeta    = a.grid_zeta;
gp_avgstep   = a.gp_avgstep;
nZeta        = length(grid_zeta);

% Initialize output storage
storage_slope       = zeros(nZeta, 1);
storage_correlation = zeros(nZeta, 1);
storage_calibration = zeros(nZeta, 8);
storage_endo        = cell(nZeta, 1);

% Additional storage for Phase 2 inputs (needed for sequential simulations)
storage_z_local     = cell(nZeta, 1);
storage_t_local     = cell(nZeta, 1);
storage_p_local     = cell(nZeta, 1);

%% Find process innovation steps (done once, before parallel loop)
if ~exist('lambda', 'var') || ~exist('gamma', 'var')
    disp('Finding endogenous process innovation parameters..')
    [temp, ~, ~, ~] = lsqnonlin(@(x)file3a_stepsizesolver(x,p,i), ...
        [0.02, 0.8], ...
        ones(2,1)*10^(-10), ...
        [ones(2,1)], ...
        optimset('display','off','TolFun',2e-30,'MaxIter',2e30,'MaxFunEvals',2e30,'TolX',1e-12));
    lambda = temp(1);  % process step
    gamma  = temp(2);  % diminishing returns
    disp('done!')
else
    fprintf('Using pre-set lambda=%.6f, gamma=%.6f (skipping step size solver)\n', lambda, gamma);
end

%% Start parallel pool (if not already running)
pool = gcp('nocreate');
if isempty(pool)
    numWorkers = min(4, feature('numcores'));
    try
        parpool('local', numWorkers);
        fprintf('Started parallel pool with %d workers\n', numWorkers);
    catch ME
        warning('Could not start parallel pool: %s\nRunning sequentially.', ME.message);
    end
end

%% ========================================================================
%% PHASE 1: Calibration and Growth Calculations (PARALLEL)
%% ========================================================================
fprintf('\n=== PHASE 1: Parallel calibration over %d zeta values ===\n', nZeta);

parfor index = 1:nZeta

    tht = grid_zeta(index);

    % Create LOCAL copies of structures (required for parfor)
    p_local = p;
    a_local = a;
    s_local = s;
    i_local = i;

    %% Zeta initialization (local)
    p_local.zetapar = tht;
    z_local = struct();  % Initialize struct for parfor classification
    z_local.main   = zeta(p_local.zetapar);
    z_local.minone = zeta(p_local.zetapar-1);
    p_local.Hurwitz_tht   = hurwitzZeta_danial(p_local.zetapar, (1:120), 1000000);
    p_local.Hurwitz_tht_1 = hurwitzZeta_danial(p_local.zetapar-1, (1:120), 1000000);

    %% For a very fine grid of lambda_burst, find the relative lambda_burst and type shares
    fprintf('Worker %d: Finding types for zeta=%.4f\n', index, tht);

    calibration = zeros(gp_avgstep, 8);

    for j = 1:length(grid_avgstep)
        a_local.mean = grid_avgstep(j) + 1;

        [temp_cal, resnorm, ~] = lsqnonlin(@(x)file3b_typesolver(x,a_local), ...
            [0.01, 0.05, 0.3], ...
            [10^-30, 10^-30, 0], ...
            [10^10, 10^10, 1], ...
            optimset('display','off','TolFun',1e-10,'MaxIter',2e30,'MaxFunEvals',2e30,'TolX',1e-12));
        if resnorm*2 > 1e-10
            warning('Worker %d: Type solver did not converge for j=%d, tht=%.4f', index, j, tht);
        end
        calibration(j,1) = temp_cal(1);
        calibration(j,2) = temp_cal(2);
        calibration(j,3) = temp_cal(3);
    end

    %% Calculate growth for each calibration
    fprintf('Worker %d: Finding growth rates for zeta=%.4f\n', index, tht);

    for j = 1:length(grid_avgstep)
        t_loop = struct();  % Initialize struct for parfor classification
        t_loop.lambda_burst_type1 = calibration(j,1);
        t_loop.lambda_burst_type2 = calibration(j,2);
        t_loop.K_target           = [1-calibration(j,3), calibration(j,3)];
        if t_loop.lambda_burst_type2 >= 1/3, fprintf('Warning: j=%d, lb2=%.4f >= 1/3\n', j, t_loop.lambda_burst_type2); end

        [temp_xrel, resnorm] = lsqnonlin(@(x)file3c_xrelsolver(x,t_loop,p_local,i_local,z_local), ...
            [15, 1, 1], ...
            [0.01, 0.99, 0.99], ...
            [10^10, 1.01, 100], ...
            optimset('display','off','TolFun',1e-10,'MaxIter',2e30,'MaxFunEvals',2e30,'TolX',1e-12));
        if sqrt(resnorm*2) > 1e-8
            warning('Worker %d: xrel solver did not converge for j=%d, tht=%.4f', index, j, tht);
        end

        x_rel = temp_xrel(2:1+length(t_loop.K_target));
        temp_sum = 0;
        for type = 1:length(t_loop.K_target)
            temp_sum = temp_sum + t_loop.K_target(type)*x_rel(type);
        end
        x_abs_type1 = i_local.xrate_target*(z_local.main/z_local.minone)/(temp_sum);
 fprintf('  j=%d: lb1=%.4f, lb2=%.4f, x_abs=%.4f, x_rel=[%.4f,%.4f]\n', j, calibration(j,1), calibration(j,2), x_abs_type1, x_rel(1), x_rel(2));

        [x_opt, res] = fsolve(@(x)file3e_entranttypesolver(x,i_local,z_local,s_local,t_loop,p_local,x_abs_type1,x_rel), ...
            log(0.3), ...
            optimset('display','off','TolFun',1e-10,'TolX',1e-10,'MaxIter',2e30,'MaxFunEvals',2e30));
        if res > 1e-8
            warning('Worker %d: Entrant solver did not converge for j=%d, tht=%.4f', index, j, tht);
        end
        G = [exp(x_opt), 1 - exp(x_opt)];

        calibration(j,5) = file3g_growthcalculator(p_local,z_local,t_loop,i_local,x_abs_type1,x_rel,G,lambda,gamma);
        calibration(j,4) = G(2);
        calibration(j,6) = x_rel(1);
        calibration(j,7) = x_rel(2);
        calibration(j,8) = x_abs_type1;
        fprintf('  j=%d: lb1=%.4f, lb2=%.4f, x_abs=%.4f, x_rel=[%.4f,%.4f], growth=%.6f\n', j, calibration(j,1), calibration(j,2), calibration(j,8), calibration(j,6), calibration(j,7), calibration(j,5));                                                                          
    end

    %% Obtain best calibration for this zeta
    [~, idx] = min(abs(calibration(:,5) - i_local.target_growth));
    calibration_best = calibration(idx, :);

    % Create endo structure (initialize structs for parfor classification)
    endo_local = struct();
    t_local = struct();
    endo_local.lambda            = lambda;
    endo_local.gamma             = gamma;
    endo_local.lambdaburst_type1 = calibration_best(1);
    endo_local.lambdaburst_type2 = calibration_best(2);
    t_local.K_target             = [1-calibration_best(3), calibration_best(3)];
    endo_local.G                 = [1-calibration_best(4), calibration_best(4)];
    endo_local.x_rel             = [calibration_best(6), calibration_best(7)];
    endo_local.x_abs_type1       = calibration_best(8);
    endo_local.tht               = tht;
    endo_local.K_target          = t_local.K_target;
    endo_local.output            = 0;

    % Store calibration results (NO SIMULATION YET - deferred to Phase 2)
    storage_endo{index}        = endo_local;
    storage_calibration(index,:) = calibration_best;

    % Store inputs needed for Phase 2 simulations
    storage_z_local{index}     = z_local;
    storage_t_local{index}     = t_local;
    storage_p_local{index}     = p_local;

    fprintf('Worker %d: Completed calibration for zeta=%.4f\n', index, tht);
end

fprintf('\n=== Phase 1 complete: All calibrations done ===\n');

%% ========================================================================
%% PHASE 2: Firm Simulations (SEQUENTIAL - to avoid memory exhaustion)
%% ========================================================================
fprintf('\n=== PHASE 2: Sequential simulations for %d zeta values ===\n', nZeta);
fprintf('    (Running one at a time to stay within memory limits)\n\n');

for index = 1:nZeta

    % Retrieve stored inputs
    endo_local = storage_endo{index};
    z_local    = storage_z_local{index};
    t_local    = storage_t_local{index};
    p_local    = storage_p_local{index};
    endo_local.lite = 1;

    fprintf('Simulation %d/%d: zeta=%.4f ... ', index, nZeta, endo_local.tht);

    % Run simulation (memory-intensive)
    %[~, estimation_local] = file3h_firmsim_lite(z_local, i, t_local, p_local, s, endo_local);
    [estimation_local] = file3h_firmsim_lite(z_local, i, t_local, p_local, s, endo_local);

    % Store results
    storage_slope(index)       = estimation_local.beta_slope;

    fprintf('slope=%.4f', estimation_local.beta_slope);

    % Clear large variables to free memory before next iteration
    clear estimation_local
end

% Clear Phase 2 input storage (no longer needed)
clear storage_z_local storage_t_local storage_p_local

fprintf('\n=== Phase 2 complete: All simulations done ===\n');

%% ========================================================================
%% Save the "best" calibration
%% ========================================================================
[~, idx] = min(abs(storage_slope - i.target_slope));
endo_best = storage_endo{idx};
model_slope_best = storage_slope(idx);
model_growth_best = storage_calibration(idx,5);
if ~exist('results_filename', 'var'), results_filename = 'estimation_results.mat'; end
save(results_filename, 'endo_best', 'model_slope_best', 'model_growth_best');

fprintf('\nEstimation complete! Best zeta=%.4f (slope=%.4f vs target=%.4f)\n', ...
    endo_best.tht, storage_slope(idx), i.target_slope);
fprintf('Results saved in %s\n', results_filename);
