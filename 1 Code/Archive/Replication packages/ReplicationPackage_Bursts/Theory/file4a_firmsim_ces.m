function [save,estimation,state_out] = file4a_firmsim_ces(z,i,t,p,algS,endo) 
% MONTE CARLO UPDATE (2026-04-16): optional shared-burn state I/O, burn-only mode, and configurable output folder.

if isfield(endo,'burn_only')
    burn_only = endo.burn_only;
else
    burn_only = 0;
end

if isfield(endo,'init_state')
    init_state = endo.init_state;
else
    init_state = [];
end

if isfield(endo,'output_folder') && ~isempty(endo.output_folder)
    output_folder = endo.output_folder;
elseif endo.output == 1
    output_folder = 'montecarlo/simulationoutput';
elseif endo.output == 2
    output_folder = 'montecarlo/simulationoutput_kk';
else
    output_folder = '';
end

state_out = struct();

% define mean step sizes
lambda  = endo.lambda;
lambdaburst_vec = [endo.lambdaburst_type1,endo.lambdaburst_type2];
p.gamma = endo.gamma;

% define innovation step distribution
sim.H_int         = makedist('GeneralizedPareto',lambda/(lambda  +1),lambda/(lambda  +1),1);
for type = 1:length(lambdaburst_vec)

    % step size
    lambdaburst = lambdaburst_vec(type);

    % Create a name for the sim.H_ext field
    fieldName = ['H_ext', num2str(type)];

    % Create the Generalized Pareto distribution
    k = 1 - 1 / (lambdaburst + 1);
    sigma = lambdaburst * (1 - k);
    H_ext = makedist('GeneralizedPareto', 'k', k, 'sigma', sigma, 'theta', 1); % (reverted from theta=0)

    % Dynamically assign the created distribution to the structure field
    sim.(fieldName) = H_ext;
end
clear H_ext

% set seed
rng(p.seed,'combRecursive');

% sim parameters
nSubPer = 1/algS.delt;
nBurn   = algS.nBurn/algS.delt;
nTKeep  = algS.nReal/algS.delt;                      % number of sim period kept for statistics
nTSim   = nBurn + nTKeep;                            % number of sim period (total)

% Preallocation for reporting
nProd           = ones (algS.nFirm,algS.nReal + 1);
firmtype        = ones (algS.nFirm,algS.nReal + 1);
age             = zeros(algS.nFirm,algS.nReal + 1);
exit            = zeros(algS.nFirm,algS.nReal + 1);
quality         = zeros(algS.nFirm,algS.NNExt,algS.nReal + 1);
stepdraw        = zeros(algS.nFirm,algS.NNExt,algS.nReal + 1);
prodindex       = zeros(algS.nFirm,algS.NNExt,algS.nReal + 1);
step            = zeros(algS.nFirm,algS.NNExt,algS.nReal + 1);
numBursts       = zeros(algS.nFirm,algS.nReal + 1);   % number of innovation bursts
numInternal     = zeros(algS.nFirm,algS.nReal + 1);   % number of internal innovation in a year
numInnovation   = zeros(algS.nFirm,algS.nReal + 1);   % number of all innovations in a year
numProdAdded    = zeros(algS.nFirm,algS.nReal + 1);   % number of innovation bursts x size of busts
numLost        = zeros(algS.nFirm,algS.nReal + 1);

% load in the firm size distribution
innovationrates = endo.x_rel.* endo.x_abs_type1;

if isempty(init_state)
    M = file3f_firmsizedistribution(innovationrates,i.target_cd,endo.G,i,algS,z,t,p);
    M_typsum = sum(M,1); %add up mass of N-sized firms across types. Will return M for single type simulation.
    M_share  = M./M_typsum;

    %initialize the firm-size distribution and firm type
    nProdD    = zeros(algS.nFirm,1);
    in  = 1;
    for s = 1:algS.NNExt
        nProdD(in:end,:) = s;
        in = in + ceil(M_typsum(s)*algS.nFirm/sum(M_typsum));
    end

    % initialize the type distribution
    firmtypeD    = zeros(algS.nFirm,1);
    in  = 1;
    for s = 1:algS.NN
        for type = 1:length(lambdaburst_vec)
            firmtypeD(in:end,:) = type;
            in = in + ceil(M_typsum(s)*algS.nFirm*M_share(type,s)/sum(M_typsum));
        end
    end

    qualityD    = zeros(algS.nFirm,algS.NNExt);
    stepdrawD   = zeros(algS.nFirm,algS.NNExt);
    sequence    = 1:(algS.nFirm*algS.NNExt);
    prodindexD  = reshape(sequence,[algS.NNExt, algS.nFirm]) ;
    prodindexD  = prodindexD';
    qualityD(:,1)  = 1;                             % All firms start with relative quality equal to 1
    stepD     = 0*ones(algS.nFirm,algS.NNExt);     % -1 means the quality entry is unoccupied.
    stepD(:,1)= 1;
    for s = 1:algS.nFirm
        qualityD(s,1:nProdD(s)) = 1;
        stepD(s,1:nProdD(s))= 1;
    end
    prodindexD(qualityD==0) = 0 ; %productindex = 0 for columns with number of products larger than firms' product count
    qualityD_old      = qualityD;

    % Initial Quality Dist
    qual_Step = [qualityD(:) stepD(:)];
    qual_Step = qual_Step((qual_Step(:,2)>0),:);

    ageD      = zeros(algS.nFirm,1);
else
    nProdD       = init_state.nProdD;
    firmtypeD    = init_state.firmtypeD;
    ageD         = init_state.ageD;
    qualityD     = init_state.qualityD;
    stepdrawD    = init_state.stepdrawD;
    stepD        = init_state.stepD;
    prodindexD   = init_state.prodindexD;
    qualityD_old = qualityD;
    qual_Step    = [qualityD(:) stepD(:)];
    qual_Step    = qual_Step((qual_Step(:,2)>0),:);
end

exitD     = zeros(algS.nFirm,1);

firmIndex = (1:algS.nFirm)';
numBurstsD    = zeros(algS.nFirm,1);    % number of major innovation in a year
numInternalD   = zeros(algS.nFirm,1);   % number of internal innovation in a year
numInnovationD = zeros(algS.nFirm,1);   % number of all innovations in a year
numProdAddedD = zeros(algS.nFirm,1);    % number of all innovations in a year
numLostD     = zeros(algS.nFirm,1);
numInnoqD    = zeros(algS.nFirm,4);

%innovation rates
eq.xn     =  innovationrates.*ones(algS.NN,1);
xDelt     = [eq.xn;eq.xn(end)*ones(algS.NNExt - algS.NN,1)]*algS.delt;   %  extending xn a little bit with constant innovation intensity
zDelt     = i.z*algS.delt;
tauDelt   = i.target_cd*algS.delt;

subPer    = 1;
period    = 1;


storage = 0;
storage2= 0;

%% perform sim

if nBurn == 0
    nProd(:,period)      = nProdD;
    firmtype(:,period)   = firmtypeD;
    age(:,period)        = ageD;
    quality(:,:,period)  = qualityD;
    step(:,:,period)     = stepD;
    prodindex(:,:,period)= prodindexD;
    period               = period + 1;
end

for j = 1:nTSim
%j

    %% State determination
    draw            = rand(algS.nFirm,1);
    stateUpMajor    = zeros(algS.nFirm,1);
        for type = 1:length(lambdaburst_vec)
            for prodcount = 1:max(nProdD)
                stateUpMajor(firmtypeD == type & nProdD == prodcount )    = xDelt(prodcount,type); %note: could remove dependence on size
            end
        end
    stateUpMajor = stateUpMajor .*nProdD;
    %stateUpMajor    = xDelt(nProdD).*nProdD;
    stateDown       = stateUpMajor + tauDelt.*nProdD;
    stateInternal   = stateDown    + zDelt.*nProdD;
    if max(stateDown)>=1
        warning('Probability is greater than 1')
    end

    % discrete innovation outcomes
    upMajor         = (draw < stateUpMajor);
    down            = logical((1 - upMajor ).*(draw < stateDown));
    internal        = logical((1 - upMajor  - down).*(draw < stateInternal));

    % introduce zeta  draws
    zeta_draws          = randraw('zeta', endo.tht,sum(upMajor) )                                      ;       % create zeta realizations
    zeta_draws          = min(zeta_draws,p.zetamax)                                                     ;       % large outliers creating some issue
    upcount             = zeros(algS.nFirm,1);
    upcount(upMajor>0)  = zeta_draws;

    % update firms' product count
    nProdD          = min(nProdD + upcount  - down,algS.NNExt - 1);                         % update number of product line
    ageD            = ageD + 1;                                   % update age

    % Quality matrix modification due to down
    drawDown  = rand(sum(down),1);       % we randomly drop the quality
    nProdDOld = nProdD(down) + 1;
    dropQuals = ceil(drawDown.*nProdDOld);
    firmIndDown = firmIndex(down);

    for s = 1:sum(down)
        qualityD(firmIndDown(s)     ,dropQuals(s):end-1)    = qualityD(firmIndDown(s)   ,dropQuals(s)+1:end);  % shifting such that one randomly quality drops (this is kind of problematic if all quality matrix fills with product lines)
        stepdrawD(firmIndDown(s)    ,dropQuals(s):end-1)    = stepdrawD(firmIndDown(s)   ,dropQuals(s)+1:end);
        stepD(firmIndDown(s)        ,dropQuals(s):end-1)    = stepD(firmIndDown(s)      ,dropQuals(s)+1:end);
        prodindexD(firmIndDown(s)   ,dropQuals(s):end-1)    = prodindexD(firmIndDown(s) ,dropQuals(s)+1:end);

         % //added 29-11-25 but might be redundant for all firms not at the
         % max. number of products
         qualityD( firmIndDown(s), end)   = 0;
        stepdrawD(firmIndDown(s), end)   = 0;
        stepD(     firmIndDown(s), end)  = 0;
        prodindexD(firmIndDown(s), end)  = 0;
    end

    % obtain mean quality
    meanqual =  mean(mean(qualityD(qualityD>0)));

    % Firms that exit/replaced by entrant
    ifExit           = (nProdD<1);
    nProdD(ifExit)   = 1;                                       % enters again with one product
    draw2            = rand(algS.nFirm,1);
    %firmtypeD(in:end,:) = type;
    in = 0;
    for type = 1:length(lambdaburst_vec)
        firmtypeD(ifExit==1&draw2>in) = type;
        in = in + endo.G(type);
    end
    firmIndExit      = firmIndex(ifExit);


    for type = 1:length(lambdaburst_vec)
        % obtain type's pareto distribution
        fieldName = ['H_ext', num2str(type)];
        H_ext = sim.(fieldName);

        % Generate random draws and assign the new quality levels
        lambda_draws = random(H_ext, round(sum(ifExit&firmtypeD==type)), 1);
        %newQual_Step     = datasample(qual_Step(qual_Step>0),sum(ifExit&firmtypeD==type));
        newQual_Step = datasample(qual_Step(qual_Step(:,1)>0,1), sum(ifExit&firmtypeD==type)); %bug fixed 17/4/26
        qualityD(ifExit==1&firmtypeD==type,1) =  newQual_Step(:,1)  .*(lambda_draws) ; % (reverted from additive)
        %qualityD(ifExit==1&firmtypeD==type,1) =  newQual_Step(:,1)  + lambdaburst_vec(type); %.*(lambda_draws) ;
        stepdrawD(ifExit==1&firmtypeD==type,1) =  (lambda_draws) ;
    end
    stepD(firmIndExit,1)    = 1;
    ageD(ifExit)            = 0;


    %% Update qualities and product index

    % Quality matrix for product creation by surviving firms
    for type = 1:length(lambdaburst_vec)

        % Obtain type's pareto distribution for step sizes
        fieldName = ['H_ext', num2str(type)];
        H_ext = sim.(fieldName);

        % Generate random draws and assign the new quality levels
        typedummy    = firmtypeD == type;
        lambda_draws = random(H_ext,round(sum(upcount.*typedummy)),1);
        newQual_Step = datasample(qual_Step(qual_Step(:,1)>0,1), sum(upcount.*typedummy)); % bug fixed 17-4-2026 
        %newQual_Step = datasample(qual_Step(qual_Step    >0  ),sum(upcount.*typedummy));

        % Identify products to be added
        IndMajor           = (nProdD(upMajor==1&firmtypeD==type) - upcount(upMajor==1&firmtypeD==type))*algS.nFirm + firmIndex(upMajor==1&firmtypeD==type);
        if max(zeta_draws) > 1 %keeps adding products to a firm until 'element' exceeds the bursts' size.
            for element = 2:1:max(zeta_draws)
                    upMajor_zeta       = (upcount.*typedummy)>=element ; %only include firms of the current iteration's type
                    IndMajor           = [IndMajor;(nProdD(upMajor_zeta) - upcount(upMajor_zeta) +element-1)*algS.nFirm + firmIndex(upMajor_zeta)];
            end
        end

        % Update quality and step matrices
        qualityD(IndMajor)  = newQual_Step(:,1) .* lambda_draws ; % (reverted from additive)
        stepdrawD(IndMajor) =  (lambda_draws) ;
        stepD(IndMajor)     = 1;

    end



    % Quality matrix for own-product innovation
    drawInternal            = rand(sum(internal),1);
    nProdDInternal          = nProdD(internal);
    internalQuals           = ceil(drawInternal.*nProdDInternal);
    IndInternal             = (internalQuals - 1)*algS.nFirm + firmIndex(internal);
    %stepD(IndInternal)      = newQual_Step(:,2)  + 1; %ADDED MDR
    lambda_draws            = random(sim.H_int,round(sum(internal)),1);
    %p.gamma                 = 0.4;
    qualityD(IndInternal)   = qualityD(IndInternal)  .*(1+(lambda_draws-1).*(p.gamma.^(stepD(IndInternal)-1)));
    %qualityD(IndInternal)   = qualityD(IndInternal)  .*(1+(endo.steps.lambda).*(p.gamma.^(stepD(IndInternal)-1)));
    stepD(IndInternal)      = stepD(IndInternal) + 1;

    % Finalize new quality matrix - normalize by CES aggregate (not arithmetic mean) 
    Q_CES             = mean(qualityD(qualityD>0).^(p.epsilon-1))^(1/(p.epsilon-1));
    qualityD          = qualityD / Q_CES ; 
    %var(qualityD(qualityD>0))

    % update the firm-product codes
    prodindexD(qualityD==0) = 0; %remove product code for products that firm does not produce
    newcode                 = 1:length( prodindexD(qualityD_old==0&qualityD~=0));
    prodindexD(qualityD_old==0&qualityD~=0) = max(max(prodindexD)) + newcode'  ; %add product code to products that firm has started producing

    % add new product code for entrant's product (their quality is never set to zero so previous step doesn't suffice)
    temp                    = prodindexD(:,1);
    newcode                 = 1:sum(ifExit) ;
    temp(ifExit)            = max(max(prodindexD)) + newcode';
    prodindexD(:,1)         = temp ; %replace exiter's products with new code

    % store old qualities
    qualityD_old      = qualityD;

    % New quality distributioon
    qual_Step = [qualityD(:) ]; %fixed 14 oct 24

    if j == nBurn   % this is initial distribution
        nProd(:,period)     = nProdD;
        firmtype(:,period)  = firmtypeD;
        age(:,period)       = ageD;
        quality(:,:,period) = qualityD;
        step(:,:,period)    = stepD;
        prodindex(:,:,period)= prodindexD;
        period              = period + 1;
    end

    if j>nBurn
        storage = [storage;qualityD(IndMajor)];
        storage2 = [storage2;qualityD(qualityD ~= 0)];
        exitD = exitD + ifExit;    % track whether the firms exits within period after burning period.
        numBurstsD(upMajor)   = numBurstsD(upMajor) + 1;
        numInternalD          = numInternalD + internal;
        numInnovationD        = numInnovationD + upMajor + internal ;
        numProdAddedD         = numProdAddedD + upcount ;
        numLostD              = numLostD + down ;

        if subPer == nSubPer

            % Reporting the end-year values
            nProd(:,period)         = nProdD;
            firmtype(:,period)      = firmtypeD;
            age(:,period)           = ageD;
            quality(:,:,period)     = qualityD;
            stepdraw(:,:,period)    = stepdrawD;
            step(:,:,period)        = stepD;
            exit(:,period)          = (exitD>0);
            numBursts(:,period)     = numBurstsD;
            numInternal(:,period)   = numInternalD;
            numInnovation(:,period) = numInnovationD;
            numProdAdded(:,period)  = numProdAddedD;
            prodindex(:,:,period)   = prodindexD;
            numLost(:,period)       = numLostD;
            period = period + 1;
            subPer = 1;

            % reset
            numBurstsD            = zeros(algS.nFirm,1);
            numInternalD          = zeros(algS.nFirm,1);
            numInnovationD        = zeros(algS.nFirm,1);
            numProdAddedD         = zeros(algS.nFirm,1);
            numLostD              = zeros(algS.nFirm,1);
            exitD                 = zeros(algS.nFirm,1);
        else
            subPer = subPer + 1;
        end
    end
%

end



%% simulation saved
save.nProd         = nProd;
save.firmtype      = firmtype;
save.age           = age;
save.quality        = quality;
save.step           = step;
save.exit          = exit;
save.numBursts     = numBursts;
save.numInternal   = numInternal;
save.numInnovation = numInnovation;
save.numProdAdded  = numProdAdded;
save.prodindex      = prodindex;
save.numLost       = numLost;

state_out.nProdD     = nProdD;
state_out.firmtypeD  = firmtypeD;
state_out.ageD       = ageD;
state_out.qualityD   = qualityD;
state_out.stepdrawD  = stepdrawD;
state_out.stepD      = stepD;
state_out.prodindexD = prodindexD;

if burn_only == 1
    estimation = struct();
    return
end



%% produce tail estimates

% save number of products added for subset of firms with single product at start if period that didn't exit
nProd_lag = zeros(length(nProd(:,1)),length(nProd(1,:)));
nProd_lag(:,2:end) =  nProd(:,1:end-1);
sample_numProdAdded = save.numProdAdded(save.exit==0 &nProd_lag==1);
sample_numProdAdded = sample_numProdAdded(sample_numProdAdded>0&sample_numProdAdded<max(sample_numProdAdded));


% calculate rank of number of products added
sample_numProdAdded = log(sort(sample_numProdAdded)) ;
rank = 1-[1:length(sample_numProdAdded)]'/length(sample_numProdAdded);
for index = 2:length(sample_numProdAdded)
    if sample_numProdAdded(index) == sample_numProdAdded(index-1)
        rank(index) = rank(index-1);
    end
end
rank = log(rank);
flag = rank>min(rank);
rank = rank(flag);
sample_numProdAdded = sample_numProdAdded(flag);

% run the log rank, log-size regression, save result
vec1                = zeros(length(sample_numProdAdded),1)+1;
warn_state = warning('off', 'stats:regress:RankDefDesignMat');
beta = regress(rank,[vec1,sample_numProdAdded]);
warning(warn_state);
estimation.beta_slope = beta(2);




%% Pooled correlation: number of products vs log(revenue per product)

nF = size(quality,1);
nT = size(quality,3);

revpp_log = NaN(nF,nT);   % firm-year log revenue per product

for tt = 1:nT
    Qtt = quality(:,:,tt);
    Qtt(Qtt<=0) = NaN;

    % revenue per product (levels)
    Rtt = Qtt.^(p.epsilon - 1);

    % average revenue per product (firm-year)
    avgR = nanmean(Rtt,2);

    % log of the average
    revpp_log(:,tt) = log(avgR);
end

% valid obs
valid = ~isnan(revpp_log) & ~isnan(nProd);

% pooled correlation across all firm-years
x = revpp_log(valid);
y = nProd(valid);

if numel(x) >= 3
    C = corrcoef(x,y);
    estimation.corr_logrevpp_nprod_pooled = C(1,2);
else
    estimation.corr_logrevpp_n
end





%% write raw simulation output (MAT only)
if endo.output == 1 || endo.output == 2
    if isempty(output_folder)
        if endo.output == 1
            output_folder = 'montecarlo/simulationoutput';
        else
            output_folder = 'montecarlo/simulationoutput_kk';
        end
    end
    if exist(output_folder,'dir') ~= 7
        mkdir(output_folder);
    end

    parameters = [algS.nFirm,algS.NNExt,(algS.nReal+1), p.epsilon,algS.delt , 0.016];
    builtin('save', fullfile(output_folder,'raw_simulation.mat'), ...
        'age','exit','nProd','firmtype','numBursts','numInnovation','numInternal', ...
        'numProdAdded','numLost','quality','step','stepdraw','prodindex', ...
        'parameters','-v7.3')
end
disp('done!')







