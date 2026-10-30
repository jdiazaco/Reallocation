%% FILE5_CALIBRATE_NOISE - Calibrate noise SD using file5a_decomposition_shapley
%
%   Finds shock_sd such that Shapley decomposition assigns target share
%   to continuing products on 1-year horizon.
%
%   Inputs:
%       target_share        - target Shapley share for continuing products
%       input_folder        - folder for unsuffixed calibration output
%       nMC                 - number of Monte Carlo draws
%       draw_folder_pattern - sprintf pattern for raw draw folders
%
%   Outputs:
%       estimated_sd        - calibrated noise standard deviation

function estimated_sd = file5_calibrate_noise(target_share, input_folder, nMC, draw_folder_pattern)

if nargin < 1
    target_share = 0.26;
end
if nargin < 2
    input_folder = 'montecarlo/simulationoutput';
end
if nargin < 3
    nMC = 1;
end
if nargin < 4
    draw_folder_pattern = 'montecarlo/simulationoutput_raw_%02d';
end

fprintf('=== Calibration using file5a_decomposition_shapley ===\n');
fprintf('Target continuing products share: %.3f\n\n', target_share);

if nMC > 1
    draw_folders = cell(nMC,1);
    for mc = 1:nMC
        draw_folders{mc} = sprintf(draw_folder_pattern, mc);
    end
else
    draw_folders = {input_folder};
end

%% Step 1: Load all draw data from MAT raw files
fprintf('Step 1: Loading simulation data...', length(draw_folders));
tic;
base_data_all = cell(length(draw_folders),1);
for d = 1:length(draw_folders)
    [base_data_all{d}, params] = load_and_prepare_data(draw_folders{d});
end
fprintf('  Loaded in %.1f seconds\n', toc);

%% Step 2: Bisection search
sd_low = 0.01;
sd_high = 0.5;
tolerance = 0.002;
max_iter = 20;

%fprintf('\nStep 2: Bisection search (bounds: [%.3f, %.3f])\n', sd_low, sd_high);
%fprintf('---------------------------------------------------\n');

tic;
share_low = evaluate_noise(sd_low, base_data_all, params);
%fprintf('  SD = %.4f -> Continuing share = %.4f (%.1fs)\n', sd_low, share_low, toc);

tic;
share_high = evaluate_noise(sd_high, base_data_all, params);
%fprintf('  SD = %.4f -> Continuing share = %.4f (%.1fs)\n', sd_high, share_high, toc);

if (share_low - target_share) * (share_high - target_share) > 0
    warning('Target may not be bracketed. Proceeding anyway.');
end

%fprintf('\n--- Bisection iterations ---\n');

for iter = 1:max_iter
    sd_mid = (sd_low + sd_high) / 2;

    tic;
    share_mid = evaluate_noise(sd_mid, base_data_all, params);
    elapsed = toc;

    error_val = share_mid - target_share;
    %fprintf('Iter %2d: SD = %.4f -> Share = %.4f (err = %+.4f) [%.1fs]\n', iter, sd_mid, share_mid, error_val, elapsed);

    if abs(error_val) < tolerance
        fprintf('\n*** Converged! ***\n');
        break;
    end

    if share_mid > target_share
        sd_high = sd_mid;
    else
        sd_low = sd_mid;
    end
end

estimated_sd = sd_mid;

%% Step 3: Save result and write unsuffixed calibrated product dataset
fprintf('\n=== FINAL RESULT ===\n');
fprintf('Estimated noise SD: %.6f\n', estimated_sd);
fprintf('Achieved share:     %.4f (target: %.4f)\n', share_mid, target_share);

save('estimated_noise_sd.mat', 'estimated_sd', 'target_share', 'share_mid');
fprintf('\nResults saved to estimated_noise_sd.mat\n');

if exist(input_folder,'dir') ~= 7
    mkdir(input_folder);
end
product_output = create_product_table(estimated_sd, base_data_all{1}, params);
output_file = fullfile(input_folder, 'simulation_productlevel.csv');
writetable(product_output, output_file);
fprintf('Done! Dataset saved to: %s\n', output_file);

end


%% ========================================================================
%% HELPER FUNCTIONS
%% ========================================================================

function [base_data, params] = load_and_prepare_data(input_folder)

raw = load(fullfile(input_folder, 'raw_simulation.mat'));

p = raw.parameters;
params.firmcount = p(1);
params.prodmax = p(2);
params.periods = p(3);
params.epsilon = p(4);
params.delt = p(5);
params.growth = p(6);

firmcount = params.firmcount;
prodmax = params.prodmax;
periods = params.periods;

age = reshape(raw.age, [], 1);
exit_var = reshape(raw.exit, [], 1);
nProd = reshape(raw.nProd, [], 1);
numProdAdded = reshape(raw.numProdAdded, [], 1);

firmid = repmat((1:firmcount)', periods, 1);
time = repelem((1:periods)', firmcount);

firm_data = table((1:length(age))', firmid, time, age * params.delt, exit_var, nProd, numProdAdded, ...
    'VariableNames', {'idx', 'firmId', 'year', 'age', 'exit', 'nProd', 'numProdAdded'});
firm_data = sortrows(firm_data, {'firmId', 'year'});

unique_firms = unique(firm_data.firmId);
firm_data.exit_tracker = zeros(height(firm_data), 1);
for f = 1:length(unique_firms)
    mask = firm_data.firmId == unique_firms(f);
    idx = find(mask);
    exits = firm_data.exit(idx);
    if length(idx) > 1
        tracker = [0; cumsum(exits(2:end))];
    else
        tracker = 0;
    end
    firm_data.exit_tracker(idx) = tracker;
end
[~, ~, firm_data.firmId2] = unique([firm_data.firmId, firm_data.exit_tracker], 'rows');

prodindex_1d = reshape(permute(raw.prodindex, [1, 3, 2]), [], 1);
quality_1d   = reshape(permute(raw.quality, [1, 3, 2]), [], 1);
step_1d      = reshape(permute(raw.step, [1, 3, 2]), [], 1);
stepdraw_1d  = reshape(permute(raw.stepdraw, [1, 3, 2]), [], 1);

year_base = repelem((1:periods)', firmcount);
firm_base = repmat((1:firmcount)', periods, 1);
year = repmat(year_base, prodmax, 1);
firm = repmat(firm_base, prodmax, 1);

product = table(prodindex_1d, quality_1d, step_1d, stepdraw_1d, year, firm, ...
    'VariableNames', {'prodindex', 'quality', 'steps', 'stepdraws', 'year', 'firmId'});
product = product(product.prodindex ~= 0, :);

firm_lookup = firm_data(:, {'firmId', 'year', 'age', 'exit', 'nProd', 'numProdAdded', 'firmId2'});
product = innerjoin(product, firm_lookup, 'Keys', {'firmId', 'year'});

[~, ~, year_idx] = unique(product.year);
mean_quality = accumarray(year_idx, product.quality, [], @mean);
product.meanquality = mean_quality(year_idx);

product = sortrows(product, {'prodindex', 'year'});
product.newproduct = zeros(height(product), 1);
unique_prods = unique(product.prodindex);
for p_idx = 1:length(unique_prods)
    prod_mask = product.prodindex == unique_prods(p_idx);
    first_idx = find(prod_mask, 1, 'first');
    product.newproduct(first_idx) = 1;
end

product = product(product.year > 1, :);

base_data.firm = firm_data;
base_data.product = product;

end


function share = evaluate_noise(shock_sd, base_data_all, params)

share_draw = zeros(length(base_data_all),1);

for d = 1:length(base_data_all)
    product_output = create_product_table(shock_sd, base_data_all{d}, params);
    evalc('results = file5a_decomposition_shapley(product_output);');
    share_draw(d) = results.shapley_percent(3);
end

share = mean(share_draw);

end


function product_output = create_product_table(shock_sd, base_data, params)

product = base_data.product;
epsilon = params.epsilon;

rng(4444);
n_prods = height(product);
lognormal_shock = exp(shock_sd * randn(n_prods, 1));

product.revenue = (product.quality ./ product.meanquality).^(epsilon-1) ...
                  .* lognormal_shock .* product.meanquality;

[~, ~, group_idx] = unique([product.firmId2, product.year], 'rows');
revenue_firm = accumarray(group_idx, product.revenue, [], @sum);
product.revenue_firm = revenue_firm(group_idx);

product_output = product(:, {'prodindex', 'quality', 'steps', 'stepdraws', ...
    'year', 'firmId2', 'age', 'exit', 'nProd', 'numProdAdded', 'meanquality', ...
    'newproduct', 'revenue', 'revenue_firm'});

end
