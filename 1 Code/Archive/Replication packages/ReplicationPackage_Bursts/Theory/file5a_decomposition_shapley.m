%% FILE5A_DECOMPOSITION_SHAPLEY - Shapley R² Decomposition of Revenue Growth Variance
% MATLAB implementation matching stata_file3d_decomposition_table_csv.do
% Implements Haltiwanger decomposition and Shapley R² decomposition
% Author: Claude (from Stata code by mdr)

function results = file5a_decomposition_shapley(input_data)
    %% Parameters
    year_end = 41;
    laghorizon = 1;  % 1-year horizon only

    %% Load data
    fprintf('Loading simulation_productlevel.csv...\n');
    if nargin < 1 || isempty(input_data)
        datapath = fullfile(fileparts(mfilename('fullpath')), 'montecarlo', 'simulationoutput', 'simulation_productlevel.csv');
        data = readtable(datapath);
    elseif istable(input_data)
        data = input_data;
    else
        data = readtable(input_data);
    end

    % Rename columns to match Stata conventions
    data.siren = data.firmId2;
    data.id = data.prodindex;

    fprintf('Loaded %d observations\n', height(data));

    %% Step 1: Identify firm's first and last year with positive revenue
    fprintf('\nStep 1: Identifying firm tenure...\n');

    % Calculate total revenue per firm-year
    [G, firmyear_siren, firmyear_year] = findgroups(data.siren, data.year);
    yearrev = splitapply(@(x) sum(x, 'omitnan'), data.revenue, G);

    % Create lookup table for firm-year revenue
    firmyear_tbl = table(firmyear_siren, firmyear_year, yearrev, ...
        'VariableNames', {'siren', 'year', 'yearrev'});

    % Merge back to main data
    data = outerjoin(data, firmyear_tbl, 'Keys', {'siren', 'year'}, ...
        'MergeKeys', true, 'Type', 'left');

    % Find min/max year per firm (where yearrev > 0)
    data.firmyear = data.year;
    data.firmyear(isnan(data.yearrev) | data.yearrev <= 0) = NaN;

    [G_firm, firm_ids] = findgroups(data.siren);
    minyear = splitapply(@(x) min(x, [], 'omitnan'), data.firmyear, G_firm);
    maxyear = splitapply(@(x) max(x, [], 'omitnan'), data.firmyear, G_firm);

    firm_tenure = table(firm_ids, minyear, maxyear, 'VariableNames', {'siren', 'minyear', 'maxyear'});

    % Merge tenure info back
    data = outerjoin(data, firm_tenure, 'Keys', 'siren', 'MergeKeys', true, 'Type', 'left');

    %% Step 2: Create lead/lag revenue for product classification (vectorized)
    fprintf('Step 2: Creating lead/lag variables for product classification...\n');

    % Create lookup tables for lead/lag joins
    % Lead: for each (product, year), find revenue at (product, year + laghorizon)
    lead_lookup = table(data.id, data.year - laghorizon, data.revenue, ...
        'VariableNames', {'id', 'year', 'lead_revenue'});
    lead_lookup = lead_lookup(~isnan(lead_lookup.lead_revenue), :);

    % Lag: for each (product, year), find revenue at (product, year - laghorizon)
    lag_lookup = table(data.id, data.year + laghorizon, data.revenue, ...
        'VariableNames', {'id', 'year', 'lag_revenue'});
    lag_lookup = lag_lookup(~isnan(lag_lookup.lag_revenue), :);

    % Merge lead revenue
    data = outerjoin(data, lead_lookup, 'Keys', {'id', 'year'}, ...
        'MergeKeys', true, 'Type', 'left');

    % Merge lag revenue
    data = outerjoin(data, lag_lookup, 'Keys', {'id', 'year'}, ...
        'MergeKeys', true, 'Type', 'left');

    %% Step 3: Classify products (jplus, jminus, lcurl, ncurl)
    fprintf('Step 3: Classifying products...\n');

    % Initialize as NaN (double arrays to allow NaN)
    n_rows = height(data);
    data.jplus = NaN(n_rows, 1);
    data.jminus = NaN(n_rows, 1);
    data.lcurl = NaN(n_rows, 1);
    data.ncurl = NaN(n_rows, 1);

    % Valid observations: not at boundary years and has revenue
    valid_forward = data.year ~= data.maxyear & ~isnan(data.revenue);
    valid_backward = data.year ~= data.minyear & ~isnan(data.revenue);

    % jplus: products that firm will continue to produce (has revenue now and in future)
    data.jplus(valid_forward) = double(~isnan(data.revenue(valid_forward)) & ~isnan(data.lead_revenue(valid_forward)));

    % jminus: products firm continued from last period (has revenue now and in past)
    data.jminus(valid_backward) = double(~isnan(data.revenue(valid_backward)) & ~isnan(data.lag_revenue(valid_backward)));

    % lcurl: products that firm is going to lose (has revenue now but not in future)
    data.lcurl(valid_forward) = double(~isnan(data.revenue(valid_forward)) & isnan(data.lead_revenue(valid_forward)));

    % ncurl: products that are new to the firm (has revenue now but not in past)
    data.ncurl(valid_backward) = double(~isnan(data.revenue(valid_backward)) & isnan(data.lag_revenue(valid_backward)));

    %% Step 4: Calculate revenue per category at firm-year level
    fprintf('Step 4: Calculating revenue by category...\n');

    categories = {'jplus', 'jminus', 'lcurl', 'ncurl'};

    % Create firm-year aggregates
    [G_fy, fy_siren, fy_year] = findgroups(data.siren, data.year);

    % Total revenue (EAP = "Enterprise, All Products")
    revenue_EAP = splitapply(@(x) sum(x, 'omitnan'), data.revenue, G_fy);

    % Revenue by category
    rev_jplus = splitapply(@(r, flag) sum(r .* flag, 'omitnan'), data.revenue, data.jplus, G_fy);
    rev_jminus = splitapply(@(r, flag) sum(r .* flag, 'omitnan'), data.revenue, data.jminus, G_fy);
    rev_lcurl = splitapply(@(r, flag) sum(r .* flag, 'omitnan'), data.revenue, data.lcurl, G_fy);
    rev_ncurl = splitapply(@(r, flag) sum(r .* flag, 'omitnan'), data.revenue, data.ncurl, G_fy);

    % Get age and exit info (take first non-NaN value per group)
    age_fy = splitapply(@(x) x(find(~isnan(x), 1, 'first')), data.age, G_fy);

    % Create firm-year level dataset
    firmyear_data = table(fy_siren, fy_year, revenue_EAP, rev_jplus, rev_jminus, rev_lcurl, rev_ncurl, age_fy, ...
        'VariableNames', {'siren', 'year', 'revenue_EAP', 'rev_jplus', 'rev_jminus', 'rev_lcurl', 'rev_ncurl', 'age'});

    % Add firm tenure info
    firmyear_data = outerjoin(firmyear_data, firm_tenure, 'Keys', 'siren', 'MergeKeys', true, 'Type', 'left');

    % Calculate exit year and birth year BEFORE panel balancing
    firmyear_data.exityyear_used = firmyear_data.maxyear;
    firmyear_data.exityyear_used(firmyear_data.maxyear == year_end) = NaN;
    firmyear_data.birthyear_used = round(firmyear_data.year - firmyear_data.age);

    %% Step 4b: Balance panel (like Stata: expand to 10 years per firm, then merge)
    fprintf('Step 4b: Balancing panel (10 years per firm)...\n');

    % Save original data
    original_firmyear = firmyear_data;

    % Get unique firms
    unique_firms = unique(firmyear_data.siren);
    n_firms = length(unique_firms);

    % Create balanced panel: each firm gets years 1-10
    % Stata: expand 10; bysort id: gen year = _n
    balanced_siren = repelem(unique_firms, 10);
    balanced_year = repmat((1:10)', n_firms, 1);
    balanced_panel = table(balanced_siren, balanced_year, 'VariableNames', {'siren', 'year'});

    % Merge: keep ALL from both sides (Stata default merge behavior)
    % This gives: years 1-10 for all firms PLUS any additional years from original
    firmyear_data = outerjoin(balanced_panel, original_firmyear, 'Keys', {'siren', 'year'}, ...
        'MergeKeys', true, 'Type', 'full');

    % Fill in exityyear_used, birthyear_used, minyear, maxyear for all years of each firm
    % (Stata does: bysort id: egen temp = mean(exityyear_used))
    for i = 1:n_firms
        firm_idx = firmyear_data.siren == unique_firms(i);

        % Exit year
        exit_vals = firmyear_data.exityyear_used(firm_idx);
        if any(~isnan(exit_vals))
            firmyear_data.exityyear_used(firm_idx) = nanmean(exit_vals);
        end

        % Birth year
        birth_vals = firmyear_data.birthyear_used(firm_idx);
        if any(~isnan(birth_vals))
            firmyear_data.birthyear_used(firm_idx) = nanmean(birth_vals);
        end

        % Min/max year
        min_vals = firmyear_data.minyear(firm_idx);
        max_vals = firmyear_data.maxyear(firm_idx);
        if any(~isnan(min_vals))
            firmyear_data.minyear(firm_idx) = nanmean(min_vals);
        end
        if any(~isnan(max_vals))
            firmyear_data.maxyear(firm_idx) = nanmean(max_vals);
        end
    end

    fprintf('  Balanced panel: %d observations\n', height(firmyear_data));

    %% Step 5: Create lagged revenue variables (on balanced panel)
    fprintf('Step 5: Creating lagged revenue at firm level...\n');

    firmyear_data = sortrows(firmyear_data, {'siren', 'year'});

    % Create lagged variables using vectorized lookup on balanced panel
    % Since panel is balanced (years 1-10 for each firm), we can use join
    lag_lookup_EAP = table(firmyear_data.siren, firmyear_data.year + laghorizon, firmyear_data.revenue_EAP, ...
        'VariableNames', {'siren', 'year', 'lag_revenue_EAP'});
    lag_lookup_jplus = table(firmyear_data.siren, firmyear_data.year + laghorizon, firmyear_data.rev_jplus, ...
        'VariableNames', {'siren', 'year', 'lag_rev_jplus'});
    lag_lookup_lcurl = table(firmyear_data.siren, firmyear_data.year + laghorizon, firmyear_data.rev_lcurl, ...
        'VariableNames', {'siren', 'year', 'lag_rev_lcurl'});

    % Remove rows where lagged value is NaN (to avoid duplicate key issues)
    lag_lookup_EAP = lag_lookup_EAP(~isnan(lag_lookup_EAP.lag_revenue_EAP), :);
    lag_lookup_jplus = lag_lookup_jplus(~isnan(lag_lookup_jplus.lag_rev_jplus), :);
    lag_lookup_lcurl = lag_lookup_lcurl(~isnan(lag_lookup_lcurl.lag_rev_lcurl), :);

    % Merge lagged variables
    firmyear_data = outerjoin(firmyear_data, lag_lookup_EAP, 'Keys', {'siren', 'year'}, ...
        'MergeKeys', true, 'Type', 'left');
    firmyear_data = outerjoin(firmyear_data, lag_lookup_jplus, 'Keys', {'siren', 'year'}, ...
        'MergeKeys', true, 'Type', 'left');
    firmyear_data = outerjoin(firmyear_data, lag_lookup_lcurl, 'Keys', {'siren', 'year'}, ...
        'MergeKeys', true, 'Type', 'left');

    %% Step 6: Calculate Haltiwanger decomposition variables
    fprintf('Step 6: Computing Haltiwanger decomposition...\n');

    % Denominator: average of current and lagged revenue
    denom = 0.5 * (firmyear_data.revenue_EAP + firmyear_data.lag_revenue_EAP);

    % Haltiwanger growth components
    firmyear_data.halt_revgrowth = (firmyear_data.revenue_EAP - firmyear_data.lag_revenue_EAP) ./ denom;
    firmyear_data.halt_revgrowth_loss = firmyear_data.lag_rev_lcurl ./ denom;  % Products lost (lagged)
    firmyear_data.halt_revgrowth_cont = (firmyear_data.rev_jminus - firmyear_data.lag_rev_jplus) ./ denom;  % Continuing
    firmyear_data.halt_revgrowth_plus = firmyear_data.rev_ncurl ./ denom;  % New products

    %% Step 7: Handle entry and exit
    fprintf('Step 7: Handling firm entry and exit...\n');

    % exityyear_used and birthyear_used were calculated and filled in Step 4b

    % Identify exited and entered firms
    % exited: firm exited within the lag horizon (exityyear - year is between -laghorizon and -1)
    firmyear_data.exited = (firmyear_data.exityyear_used - firmyear_data.year < 0) & ...
                           (firmyear_data.exityyear_used - firmyear_data.year >= -laghorizon);

    % entered: firm entered within the lag horizon (birthyear - year is between -laghorizon+1 and 0)
    firmyear_data.entered = (firmyear_data.birthyear_used - firmyear_data.year <= 0) & ...
                            (firmyear_data.birthyear_used - firmyear_data.year > -laghorizon);

    % Fix Haltiwanger variables for exited firms (current revenue = 0)
    exit_idx = firmyear_data.exited == 1;
    denom_exit = 0.5 * (0 + firmyear_data.lag_revenue_EAP(exit_idx));
    firmyear_data.halt_revgrowth(exit_idx) = (0 - firmyear_data.lag_revenue_EAP(exit_idx)) ./ denom_exit;
    firmyear_data.halt_revgrowth_loss(exit_idx) = firmyear_data.lag_revenue_EAP(exit_idx) ./ denom_exit;
    firmyear_data.halt_revgrowth_cont(exit_idx) = 0;
    firmyear_data.halt_revgrowth_plus(exit_idx) = 0;

    % Fix Haltiwanger variables for entered firms (lagged revenue = 0)
    enter_idx = firmyear_data.entered == 1;
    denom_enter = 0.5 * (firmyear_data.revenue_EAP(enter_idx) + 0);
    firmyear_data.halt_revgrowth(enter_idx) = (firmyear_data.revenue_EAP(enter_idx) - 0) ./ denom_enter;
    firmyear_data.halt_revgrowth_loss(enter_idx) = 0;
    firmyear_data.halt_revgrowth_cont(enter_idx) = 0;
    firmyear_data.halt_revgrowth_plus(enter_idx) = firmyear_data.revenue_EAP(enter_idx) ./ denom_enter;

    %% Step 8: Run regression
    fprintf('Step 8: Running regression...\n');

    % Remove rows with NaN in regression variables
    reg_vars = {'halt_revgrowth', 'halt_revgrowth_loss', 'halt_revgrowth_plus', 'halt_revgrowth_cont'};
    valid_idx = all(~isnan(firmyear_data{:, reg_vars}) & ~isinf(firmyear_data{:, reg_vars}), 2);

    reg_data = firmyear_data(valid_idx, :);
    fprintf('Observations for regression: %d\n', height(reg_data));

    % Dependent variable
    Y = reg_data.halt_revgrowth;

    % Independent variables (order matters for Shapley output)
    X = [reg_data.halt_revgrowth_loss, reg_data.halt_revgrowth_plus, reg_data.halt_revgrowth_cont];
    var_names = {'loss', 'plus', 'continuing'};

    % Run OLS regression
    [b, ~, ~, ~, stats] = regress(Y, [ones(size(Y)), X]);
    R2_full = stats(1);

    fprintf('\nRegression Results:\n');
    fprintf('  R-squared: %.6f\n', R2_full);
    fprintf('  Coefficients:\n');
    fprintf('    Constant: %.6f\n', b(1));
    for i = 1:length(var_names)
        fprintf('    %s: %.6f\n', var_names{i}, b(i+1));
    end

    %% Step 9: Shapley R² Decomposition
    fprintf('\nStep 9: Computing Shapley R² decomposition...\n');

    shapley_values = shapley_r2_decomposition(Y, X, var_names);

    %% Display results
    fprintf('\n========================================\n');
    fprintf('Shapley R² Decomposition Results (1-Year Horizon)\n');
    fprintf('========================================\n');
    fprintf('\nComponent      | Shapley Value |  Percent\n');
    fprintf('---------------|---------------|----------\n');
    for i = 1:length(var_names)
        fprintf('%-14s | %13.6f | %7.2f%%\n', var_names{i}, shapley_values(i), 100*shapley_values(i)/R2_full);
    end
    fprintf('---------------|---------------|----------\n');
    fprintf('%-14s | %13.6f | %7.2f%%\n', 'TOTAL', R2_full, 100);
    fprintf('========================================\n');

    %% Save results
    results = struct();
    results.shapley_values = shapley_values;
    results.shapley_percent = shapley_values / R2_full;
    results.R2_full = R2_full;
    results.var_names = var_names;
    results.coefficients = b;
    results.n_obs = height(reg_data);

end

function shapley_values = shapley_r2_decomposition(Y, X, var_names)
    % SHAPLEY_R2_DECOMPOSITION Compute Shapley values for R² decomposition
    %
    % This implements the Shapley value decomposition of R² as in the
    % shapley2 Stata command by F. Chavez Juarez.
    %
    % The Shapley value for variable j is the weighted average of its
    % marginal contribution to R² across all possible subsets of other variables.
    %
    % Weight for subset S: |S|! * (K - |S| - 1)! / K!
    % where K is the total number of variables

    K = size(X, 2);  % Number of independent variables
    n = size(Y, 1);  % Number of observations

    % Generate all 2^K combinations of variables
    n_combos = 2^K;

    % Store R² for each combination
    R2_combos = zeros(n_combos, 1);
    combo_matrix = zeros(n_combos, K);

    for c = 0:(n_combos - 1)
        % Convert c to binary to get variable inclusion
        combo = bitget(c, 1:K);
        combo_matrix(c+1, :) = combo;

        if sum(combo) == 0
            % Empty model: R² = 0
            R2_combos(c+1) = 0;
        else
            % Get included variables
            X_subset = X(:, combo == 1);

            % Run regression and get R²
            [~, ~, ~, ~, stats] = regress(Y, [ones(n, 1), X_subset]);
            R2_combos(c+1) = stats(1);
        end
    end

    % Compute Shapley values
    shapley_values = zeros(K, 1);

    for j = 1:K
        % For variable j, compute weighted average of marginal contributions
        marginal_sum = 0;
        weight_sum = 0;

        for c = 0:(n_combos - 1)
            combo = bitget(c, 1:K);

            % Only consider subsets that DON'T include variable j
            if combo(j) == 0
                % Subset S (without j)
                S_size = sum(combo);

                % Subset S ∪ {j} (with j)
                combo_with_j = combo;
                combo_with_j(j) = 1;
                c_with_j = sum(combo_with_j .* (2.^(0:(K-1))));

                % Marginal contribution: R²(S ∪ {j}) - R²(S)
                marginal = R2_combos(c_with_j + 1) - R2_combos(c + 1);

                % Shapley weight: |S|! * (K - |S| - 1)!
                weight = factorial(S_size) * factorial(K - S_size - 1);

                marginal_sum = marginal_sum + weight * marginal;
                weight_sum = weight_sum + weight;
            end
        end

        % Shapley value = weighted average (divide by K! which equals weight_sum)
        shapley_values(j) = marginal_sum / factorial(K);
    end

    fprintf('  Variable contributions:\n');
    for j = 1:K
        fprintf('    %s: %.6f\n', var_names{j}, shapley_values(j));
    end
    fprintf('  Sum of Shapley values: %.6f\n', sum(shapley_values));
end
