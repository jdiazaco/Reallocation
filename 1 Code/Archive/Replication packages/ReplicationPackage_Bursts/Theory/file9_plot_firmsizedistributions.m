%% FILE9_PLOT_FIRMSIZEDISTRIBUTIONS
%
%   Generate the firm-size CSV inputs consumed by the main Stata wrapper.

clc
close all

[~, s_quant, p, i] = file2_parameters_targets;
load('estimation_results.mat', 'endo_best');

s_plot = s_quant;
s_plot.nReal = 0;
s_plot.nBurn = 2;
p.zetamax = 600;
s_plot.NNExt = 4000;
s_plot.NN = 4000;

plot_i = i;
p.zetapar = endo_best.tht;
z.main = zeta(p.zetapar);
z.minone = zeta(p.zetapar-1);
t.K_target = endo_best.K_target;
t.lambda_burst_type1 = endo_best.lambdaburst_type1;
t.lambda_burst_type2 = endo_best.lambdaburst_type2;

file9a_plot_firmsizedistributions(plot_i,p,s_plot,z,t,endo_best, ...
    struct('export_stata',1));

file9a_plot_firmsizedistributions(plot_i,p,s_plot,z,t,endo_best, ...
    struct('export_stata_hom',1));
