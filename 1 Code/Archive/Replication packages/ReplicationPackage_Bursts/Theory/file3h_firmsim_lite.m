function estimation = file3h_firmsim_lite(z,i,t,p,algS,endo)
% FILE3H_FIRMSIM_LITE  Ultra-fast simulation for quantification
%
%   estimation = file3h_firmsim_lite(z,i,t,p,algS,endo)
%
%   ONLY computes beta_slope (log-rank regression coefficient).
%   Designed for quantification phase with 1.5M firms.
%
%   WHAT'S TRACKED:
%   - nProd: product count per firm (needed for nProd_lag)
%   - exit: whether firm exited during period
%   - numProdAdded: products added per firm-period
%
%   WHAT'S SKIPPED (massive speedup):
%   - Quality tracking (3D array: ~9GB savings)
%   - Step tracking (3D array)
%   - Prodindex tracking (3D array)
%   - Internal innovation (doesn't affect nProd/exit/numProdAdded)
%   - All quality matrix operations
%   - Product data generation for Stata
%
%   NUMERICAL NOTE:
%   - RNG sequence differs from original due to skipped random calls
%   - beta_slope is statistically equivalent (same distribution)

%% Extract parameters
tht = endo.tht;
zetamax = p.zetamax;
G1 = endo.G(1);

% Pre-compute zeta sampling constants (Devroye algorithm)
zeta_b = 2^(tht-1);
zeta_am1 = tht - 1;
zeta_bm1 = zeta_b - 1;

% Set seed
rng(p.seed, 'combRecursive');

%% Simulation parameters
nSubPer = round(1/algS.delt);
nBurn = round(algS.nBurn/algS.delt);
nTKeep = round(algS.nReal/algS.delt);
nTSim = nBurn + nTKeep;
nFirm = algS.nFirm;
NNExt = algS.NNExt;
nPeriods = algS.nReal + 1;

%% Preallocate ONLY what's needed for regression
nProd = ones(nFirm, nPeriods);
exit_arr = zeros(nFirm, nPeriods);
numProdAdded = zeros(nFirm, nPeriods);

%% Initialize firm size distribution
innovationrates = endo.x_rel .* endo.x_abs_type1;
M = file3f_firmsizedistribution(innovationrates, i.target_cd, endo.G, i, algS, z, t, p);
M_typsum = sum(M, 1);
M_share = M ./ M_typsum;

% Initialize nProdD (product count per firm)
nProdD = zeros(nFirm, 1);
cumFirms = [0; cumsum(ceil(M_typsum(:) * nFirm / sum(M_typsum)))];
for s = 1:NNExt
    if cumFirms(s) + 1 <= nFirm
        nProdD(cumFirms(s)+1:end) = s;
    end
end

% Initialize firmtypeD (needed for state determination)
firmtypeD = zeros(nFirm, 1);
in = 1;
for s = 1:algS.NN
    for type = 1:2
        firmtypeD(in:end) = type;
        in = in + ceil(M_typsum(s) * nFirm * M_share(type,s) / sum(M_typsum));
    end
end

%% Innovation rates - vectorized lookup
eq_xn = innovationrates .* ones(algS.NN, 1);
xDelt = [eq_xn; eq_xn(end) * ones(NNExt - algS.NN, 1)] * algS.delt;
tauDelt = i.target_cd * algS.delt;
xDelt_vec = xDelt(:);

%% Current state variables
exitD = zeros(nFirm, 1);
numProdAddedD = zeros(nFirm, 1);
firmIndex = (1:nFirm)';

subPer = 1;
period = 1;

%% Main simulation loop
for j = 1:nTSim

    %% State determination (fully vectorized)
    draw = rand(nFirm, 1);
    stateUpMajor = xDelt_vec(nProdD) .* nProdD;
    stateDown = stateUpMajor + tauDelt * nProdD;

    % Innovation outcomes (skip internal - doesn't affect our outputs)
    upMajor = draw < stateUpMajor;
    down = ~upMajor & (draw < stateDown);

    %% Zeta draws (inlined for speed)
    nUpMajor = sum(upMajor);
    upcount = zeros(nFirm, 1);

    if nUpMajor > 0
        % Inlined rejection sampling for zeta distribution
        nSample = ceil(nUpMajor * 1.3);
        accepted = zeros(nUpMajor, 1);
        nAccepted = 0;

        while nAccepted < nUpMajor
            u1 = rand(nSample, 1);
            proposals = floor(u1.^(-1/zeta_am1));
            u2 = rand(nSample, 1);
            tt = (1 + 1./proposals).^zeta_am1;
            acceptMask = u2.*proposals.*(tt-1)/zeta_bm1 <= tt/zeta_b;
            newAccepted = proposals(acceptMask);
            nNew = min(length(newAccepted), nUpMajor - nAccepted);
            accepted(nAccepted+1:nAccepted+nNew) = newAccepted(1:nNew);
            nAccepted = nAccepted + nNew;
            nSample = ceil((nUpMajor - nAccepted) * 1.3) + 10;
        end

        zeta_draws = min(accepted, zetamax);
        upcount(upMajor) = zeta_draws;
    end

    %% Update product count
    nProdD = min(nProdD + upcount - down, NNExt - 1);

    %% Handle firm exit
    ifExit = nProdD < 1;
    nExit = sum(ifExit);
    nProdD(ifExit) = 1;  % Entrant starts with 1 product

    % Type assignment for entrants (needed for state determination)
    if nExit > 0
        draw2 = rand(nExit, 1);
        exitIsType1 = draw2 <= G1;
        firmtypeD(ifExit) = 2 - exitIsType1;
    end

    % SKIP: All quality matrix operations
    % SKIP: Internal innovation
    % SKIP: Quality normalization

    %% Store results
    if j == nBurn
        nProd(:, period) = nProdD;
        period = period + 1;
    end

    if j > nBurn
        exitD = exitD + ifExit;
        numProdAddedD = numProdAddedD + upcount;

        if subPer == nSubPer
            nProd(:, period) = nProdD;
            exit_arr(:, period) = exitD > 0;
            numProdAdded(:, period) = numProdAddedD;
            period = period + 1;
            subPer = 1;

            % Reset counters
            exitD = zeros(nFirm, 1);
            numProdAddedD = zeros(nFirm, 1);
        else
            subPer = subPer + 1;
        end
    end
end

%% Compute beta_slope (exactly as in original)

% Lagged product count
nProd_lag = zeros(size(nProd));
nProd_lag(:, 2:end) = nProd(:, 1:end-1);

% Sample: single-product firms that didn't exit and added products
sample = numProdAdded(exit_arr == 0 & nProd_lag == 1);
sample = sample(sample > 0 & sample < max(sample));

% Log-rank regression
sample_log = log(sort(sample));
n = length(sample_log);
rank = 1 - (1:n)' / n;

% Handle ties (same rank for same value)
for idx = 2:n
    if sample_log(idx) == sample_log(idx-1)
        rank(idx) = rank(idx-1);
    end
end

rank = log(rank);
flag = rank > min(rank);
rank = rank(flag);
sample_log = sample_log(flag);

% OLS regression
vec1 = ones(length(sample_log), 1);
warn_state = warning('off', 'stats:regress:RankDefDesignMat');
beta = regress(rank, [vec1, sample_log]);
warning(warn_state);
estimation.beta_slope = beta(2);

% Set other fields to NaN (not computed in lite mode)
estimation.corr_logrevpp_nprod_pooled = NaN;

end
