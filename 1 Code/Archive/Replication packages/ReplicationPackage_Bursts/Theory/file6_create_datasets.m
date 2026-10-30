%% FILE6_CREATE_DATASETS - Create datasets from simulation output
% MAT PIPELINE UPDATE (2026-04-16): raw input is MAT-only (raw_simulation.mat).
%
%   This script replicates stata_file2_createdatasets.do functionality
%   Imports simulation raw MAT file and creates merged datasets
%   Outputs CSV files for use in Stata scripts
%
%   Called from file1_master.m when create_datasets flag is set to 1
%
%   Input (from input_folder):
%       - raw_simulation.mat
%
%   Outputs (to output_folder):
%       - simulation_firmlevel.csv (or suffixed variant)
%       - simulation_productlevel.csv (or suffixed variant)
%
%   Parameters:
%       input_folder - path to simulation output folder
%       shock_sd     - standard deviation for lognormal revenue shocks

function file6_create_datasets(input_folder, shock_sd, suffix, output_folder, delete_raw)

%% Default parameters
if nargin < 1
    input_folder = 'montecarlo/simulationoutput';
end
if nargin < 2
    shock_sd = 0.2742;  % Default from Stata code
end
if nargin < 3
    suffix = '';
end
if nargin < 4 || isempty(output_folder)
    output_folder = input_folder;
end
if nargin < 5
    delete_raw = 0;
end
if ~isempty(suffix) && suffix(1) ~= '_'
    suffix = ['_', suffix];
end
if exist(output_folder,'dir') ~= 7
    mkdir(output_folder);
end

fprintf('Creating datasets from: %s\n', input_folder);

%% Load raw simulation MAT
raw = load(fullfile(input_folder, 'raw_simulation.mat'));

params = raw.parameters;
firmcount = params(1);
prodmax   = params(2);
periods   = params(3);
epsilon   = params(4);
delt      = params(5);

age           = raw.age;
exit_mat      = raw.exit;
nProd_mat     = raw.nProd;
firmtype_mat  = raw.firmtype;
numBursts_mat = raw.numBursts;
numInnovation_mat = raw.numInnovation;
numInternal_mat   = raw.numInternal;
numProdAdded_mat  = raw.numProdAdded;
numLost_mat       = raw.numLost;

quality_mat   = raw.quality;
step_mat      = raw.step;
stepdraw_mat  = raw.stepdraw;
prodindex_mat = raw.prodindex;

%% ========================================================================
%% PART 1: Create firm-level dataset
%% ========================================================================

% Flatten matrices in the same order as legacy CSV path
age           = reshape(age, [], 1);
exit_var      = reshape(exit_mat, [], 1);
nProd         = reshape(nProd_mat, [], 1);
firmtype      = reshape(firmtype_mat, [], 1);
numBursts     = reshape(numBursts_mat, [], 1);
numInnovation = reshape(numInnovation_mat, [], 1);
numInternal   = reshape(numInternal_mat, [], 1);
numProdAdded  = reshape(numProdAdded_mat, [], 1);
numLost       = reshape(numLost_mat, [], 1);

firmid = repmat((1:firmcount)', periods, 1);
time   = repelem((1:periods)', firmcount);

n_obs = length(age);
index = (1:n_obs)';

firm_data = table(index, age, exit_var, nProd, firmtype, numBursts, numInnovation, ...
                  numInternal, numProdAdded, numLost, firmid, time);

firm_data.Properties.VariableNames{'exit_var'} = 'exit';
firm_data.Properties.VariableNames{'time'} = 'year';
firm_data.Properties.VariableNames{'firmid'} = 'firmId';

firm_data.age = firm_data.age * delt;
firm_data = sortrows(firm_data, {'firmId', 'year'});

unique_firms = unique(firm_data.firmId);
firm_data.exit_tracker = zeros(height(firm_data), 1);

for f = 1:length(unique_firms)
    firm_mask = firm_data.firmId == unique_firms(f);
    firm_indices = find(firm_mask);
    n_periods = length(firm_indices);

    firm_exit = firm_data.exit(firm_indices);
    if n_periods > 1
        exit_tracker_firm = [0; cumsum(firm_exit(2:end))];
    else
        exit_tracker_firm = 0;
    end
    firm_data.exit_tracker(firm_indices) = exit_tracker_firm;
end

[~, ~, firm_data.firmId2] = unique([firm_data.firmId, firm_data.exit_tracker], 'rows');

firm_output = firm_data(:, {'firmId', 'year', 'age', 'exit', 'nProd', 'firmtype', ...
    'numBursts', 'numInnovation', 'numInternal', 'numProdAdded', 'numLost', ...
    'exit_tracker', 'firmId2'});

output_file = fullfile(output_folder, ['simulation_firmlevel', suffix, '.csv']);
writetable(firm_output, output_file);
fprintf('Saved: %s\n', output_file);

%% ========================================================================
%% PART 2: Create product-level dataset
%% ========================================================================

fprintf('Creating product-level dataset from: %s\n', fullfile(input_folder, 'raw_simulation.mat'));

prodindex_1d = reshape(permute(prodindex_mat, [1, 3, 2]), [], 1);
quality_1d   = reshape(permute(quality_mat, [1, 3, 2]), [], 1);
step_1d      = reshape(permute(step_mat, [1, 3, 2]), [], 1);
stepdraw_1d  = reshape(permute(stepdraw_mat, [1, 3, 2]), [], 1);

year_base = repelem((1:periods)', firmcount);
firm_base = repmat((1:firmcount)', periods, 1);
year = repmat(year_base, prodmax, 1);
firm = repmat(firm_base, prodmax, 1);

product = table(prodindex_1d, quality_1d, step_1d, stepdraw_1d, year, firm, ...
    'VariableNames', {'prodindex', 'quality', 'steps', 'stepdraws', 'year', 'firmId'});

product = product(product.prodindex ~= 0, :);

firm_lookup = firm_output(:, {'firmId', 'year', 'age', 'exit', 'nProd', ...
    'numProdAdded', 'exit_tracker', 'firmId2'});

product = innerjoin(product, firm_lookup, 'Keys', {'firmId', 'year'});

[~, ~, year_idx] = unique(product.year);
mean_quality_by_year = accumarray(year_idx, product.quality, [], @mean);
product.meanquality = mean_quality_by_year(year_idx);

product = sortrows(product, {'prodindex', 'year'});
product.newproduct = zeros(height(product), 1);
unique_prods = unique(product.prodindex);

for p = 1:length(unique_prods)
    prod_mask = product.prodindex == unique_prods(p);
    first_idx = find(prod_mask, 1, 'first');
    product.newproduct(first_idx) = 1;
end

product = product(product.year > 1, :);

rng(4444);
n_prods = height(product);
lognormal_shock = exp(shock_sd * randn(n_prods, 1));

product.revenue = (product.quality ./ product.meanquality).^(epsilon-1) ...
                  .* lognormal_shock .* product.meanquality;

rng(4444);
uniform_var = rand(n_prods, 1);
clear uniform_var

[~, ~, group_idx] = unique([product.firmId2, product.year], 'rows');
revenue_firm = accumarray(group_idx, product.revenue, [], @sum);
product.revenue_firm = revenue_firm(group_idx);

product_output = product(:, {'prodindex', 'quality', 'steps', 'stepdraws', ...
    'year', 'firmId2', 'age', 'exit', 'nProd', 'numProdAdded', 'meanquality', ...
    'newproduct', 'revenue', 'revenue_firm'});

output_file = fullfile(output_folder, ['simulation_productlevel', suffix, '.csv']);
writetable(product_output, output_file);
fprintf('Saved: %s\n', output_file);

if delete_raw == 1
    raw_path = fullfile(input_folder, 'raw_simulation.mat');
    if exist(raw_path,'file') == 2
        delete(raw_path);
    end
end

disp('done!')

end
