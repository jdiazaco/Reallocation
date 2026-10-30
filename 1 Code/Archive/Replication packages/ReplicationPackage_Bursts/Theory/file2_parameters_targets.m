function [s,s_quant,p,i,a] = file2_parameters_targets()

% FILE2_PARAMETERS_TARGETS  Define all exogenous parameters and targets
%
%   [s, s_quant, p, i, a] = file2_parameters_targets()
%
%   Returns:
%       s       - Simulation parameters for initial quantification
%       s_quant - Simulation parameters for quantified model output
%       p       - Model parameters
%       i       - Innovation rates and targets
%       a       - Analytical targets and grid parameter
%% Simulation parameters for initial quantification
s.delt   = 0.01     ; % continuous time period duration
s.nBurn  = 0.5      ; % number of years burnt (small as wasted for quantification)
s.nReal  = 1        ; % number of years taken for data
s.nFirm  = 15000000 ; % number of firms
s.NNExt  = 300      ; % maximum number of products (simulation) - must exceed zetamax
s.NN     = s.NNExt+6; % maximum number of products (innovation)

%% Simulation parameters for quantified model
s_quant.delt   = 0.01     ; % continuous time period duration
s_quant.nBurn  = 100       ; % number of years burnt
s_quant.nReal  = 10       ; % number of years taken for data
s_quant.nFirm  = 22000    ; % number of firms
s_quant.nMC    = 100        ; % number of Monte Carlo draws after common burn period
s_quant.NNExt  = 300      ; % maximum number of products (simulation)
s_quant.NN     = s_quant.NNExt+6; % maximum number of products (innovation)

%% Model parameters
p.zetamax   = 200    ; % maximum size of a burst
p.seed      = 1234   ; % seed
p.sigma     = 1.00   ; % product innovation scaling in size
p.epsilon   = 4.00   ; % elasticity of substitution
p.psi       = 2.00   ; % r&d convexity
p.rho       = 0.02   ; % discount rate

%% Innovation rates and targets
i.xrate_target  = 0.0696; % rate of product innovation - single product firm
i.z             = 1.00 ; % rate of process (internal) innovation
i.frac          = 0.345; % fraction of goods produced by single product firms  
i.e             = i.xrate_target*i.frac/(1-i.frac); % entry rate 
i.target_cd     = i.e+i.xrate_target; % target creative destruction rate
i.target_growth = 0.021; % target growth rate
i.target_int    = 0.005; % target internal innovation
i.target_decline= -0.035; % target decline
i.target_slope  = -2.357; % (2019) 

%% Analytical targets (for type calibration)
a.relvar     = 0.89   ; % function of relative variance of new vs old products
a.autoregbeta= 0.3161 ; % autoregressive coefficient

%% Quantification grid parameters
% Grid for mean steps of product creation
a.min_avgstep  = 0.01;
a.max_avgstep  = 0.04;
a.gp_avgstep   = 20;
a.grid_avgstep = a.min_avgstep:(a.max_avgstep-a.min_avgstep)/(a.gp_avgstep-1):a.max_avgstep;

% Grid for zeta distribution (Pareto tail parameter)
a.min_zeta = 2.96;
a.max_zeta = 3.14;
a.gp_zeta  = 10; %multiples of 4 for speed 
a.grid_zeta = a.min_zeta:(a.max_zeta-a.min_zeta)/(a.gp_zeta-1):a.max_zeta;

%% Application/policy experiment parameters
a.nbars = 1:10;  % firm size thresholds for tax experiments
a.zetas_tax   = [2.6, 2.9 ,3.1, 3.6, 4.6, 1000];  % zeta values for tax experiments
a.zetas_notax = [2.6, 2.9, 3.1, 3.6, 4.6, 1000];     % zeta values for no-tax benchmark
a.tax_rate = 0.01;  % tax rate for policy experiments
a.hurwitz_Nmax = 10000000;  % Maximum N for Hurwitz zeta calculation

end
