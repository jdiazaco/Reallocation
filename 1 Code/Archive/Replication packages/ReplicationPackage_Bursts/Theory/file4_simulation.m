%% FILE4_SIMULATION - Firm Dynamics Simulation
% MONTE CARLO UPDATE (2026-04-16): run one common burn and branch into nMC post-burn draws saved as raw iteration folders.
%
%   This script runs firm simulations for the quantified model and
%   the Klette-Kortum benchmark. Called from file1_master.m
%
%   Requires: endo_best (from estimation_results.mat), s_quant, p, i, t
%   Produces: Simulation outputs in montecarlo/simulationoutput/ and montecarlo/simulationoutput_kk/
%

%% Proposed change: rare but large process innovations as well

nMC = 1;
if isfield(s_quant,'nMC')
    nMC = s_quant.nMC;
end
s_burn = s_quant;
s_burn.nReal = 0;
s_burn.nBurn = 100;  
s_draw = s_quant;
s_draw.nBurn = 0;

%% Simulate the innovation bursts model
disp('Simulating common burn period for the quantified model..')
p.zetapar = endo_best.tht;
z.main   = zeta(p.zetapar);
z.minone = zeta(p.zetapar-1);
t.K_target = endo_best.K_target;
t.lambda_burst_type1 = endo_best.lambdaburst_type1;
t.lambda_burst_type2 = endo_best.lambdaburst_type2;

endo_main_burn = endo_best;
endo_main_burn.output = 0;
endo_main_burn.burn_only = 1;
[~, ~, state_main] = file4a_firmsim_ces(z,i,t,p,s_burn,endo_main_burn);
disp('done!')

disp('Running Monte Carlo post-burn draws for quantified model..')
for mc = 1:nMC
    if nMC == 1
        raw_folder = 'montecarlo/simulationoutput';
    else
        draw_suffix = sprintf('%02d', mc);
        raw_folder = ['montecarlo/simulationoutput_raw_', draw_suffix];
    end
    mkdir(raw_folder);

    p_draw = p;
    p_draw.seed = p.seed + mc;
    endo_draw = endo_best;
    endo_draw.output = 1;
    endo_draw.init_state = state_main;
    endo_draw.output_folder = raw_folder;

    fprintf('  Draw %d/%d -> %s\n', mc, nMC, raw_folder);
    [~, ~] = file4a_firmsim_ces(z,i,t,p_draw,s_draw,endo_draw);
end
disp('done!')

%% Simulate the Klette and Kortum model

% Redo the inner loop such that model is consistent with macro moments
p_kk = p;
p_kk.zetapar = 1000;
z_kk.main   = zeta(p_kk.zetapar);
z_kk.minone = zeta(p_kk.zetapar-1);
p_kk.Hurwitz_tht   = hurwitzZeta_danial(p_kk.zetapar, (1:120), 1000000);
p_kk.Hurwitz_tht_1 = hurwitzZeta_danial(p_kk.zetapar-1, (1:120), 1000000);

% Obtain relative innovation rates
[temp, resnorm] = lsqnonlin(@(x)file3c_xrelsolver(x,t,p_kk,i,z_kk), ...
    [15, 1, 1], ...
    [0.01, 0.99, 0.99], ...
    [10^10, 1.01, 100], ...
    optimset('display','off','TolFun',1e-10,'MaxIter',2e30,'MaxFunEvals',2e30,'TolX',1e-12));

endo_kk = endo_best;
endo_kk.tht = p_kk.zetapar;
endo_kk.x_abs_type1 = endo_kk.x_abs_type1 * z.minone / z.main;
endo_kk.x_rel = temp(2:1+length(t.K_target));

% Perform firm simulation
disp('Simulating common burn period for the Klette and Kortum model..')
endo_kk_burn = endo_kk;
endo_kk_burn.output = 0;
endo_kk_burn.burn_only = 1;
[~, ~, state_kk] = file4a_firmsim_ces(z_kk,i,t,p_kk,s_burn,endo_kk_burn);
disp('done!')

disp('Running Monte Carlo post-burn draws for Klette and Kortum model..')
for mc = 1:nMC
    if nMC == 1
        raw_folder = 'montecarlo/simulationoutput_kk';
    else
        draw_suffix = sprintf('%02d', mc);
        raw_folder = ['montecarlo/simulationoutput_kk_raw_', draw_suffix];
    end
    mkdir(raw_folder);

    p_kk_draw = p_kk;
    p_kk_draw.seed = p.seed + mc;
    endo_kk_draw = endo_kk;
    endo_kk_draw.output = 2;
    endo_kk_draw.init_state = state_kk;
    endo_kk_draw.output_folder = raw_folder;

    fprintf('  Draw %d/%d -> %s\n', mc, nMC, raw_folder);
    [~, ~] = file4a_firmsim_ces(z_kk,i,t,p_kk_draw,s_draw,endo_kk_draw);
end
disp('done! - use Stata to produce draft figures')
