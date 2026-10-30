%% FILE8_OUTPUT_TARGETED_MOMENTS - Generate LaTeX Table for Targeted Moments
%
%   This script generates a LaTeX table showing model performance on
%   targeted moments (comparing targets to model outcomes).
%   Called from file1_master.m when output_tables flag is set to 1.
%
%   Requires: endo_best (from estimation_results.mat), p, i, a, t, z
%   Produces: plots/table_targeted_moments.tex

disp('Generating targeted moments table...')

%% Setup zeta parameters
p.zetapar = endo_best.tht;
z.main   = zeta(p.zetapar);
z.minone = zeta(p.zetapar-1);
t.K_target = endo_best.K_target;

%% Extract target values (from file2a_parameters_targets)
% Product innovation targets
target_slope    = i.target_slope;      % Gabaix-Ibragimov regression slope
target_sigma    = p.sigma;             % Scaling of bursts in firm size
target_xrate    = i.xrate_target;      % Arrival rate of innovation bursts
target_entry    = i.e;                 % Entry rate

% Own products targets
target_int      = i.target_int;        % Revenue growth on continuing products
target_decline  = i.target_decline;    % Change in revenue growth in tenure
target_shapley  = 0.26;               % Shapley-Owen contribution (28.8%)

% Firm types targets (jointly determined)
target_autoregbeta = a.autoregbeta;    % Persistence of revenue per new product
target_relvar      = a.relvar;         % Ratio of variance of log revenue
target_growth      = i.target_growth;  % Growth rate of manufacturing TFP

% Other
target_markup   = p.epsilon / (p.epsilon - 1);  % Markup from elasticity

%% Compute model values from endo_best
% Product innovation (model outcomes)
model_slope     = model_slope_best;
model_sigma     = target_sigma;        % Directly set
model_xrate     = target_xrate;        % Calibrated to match
model_entry     = target_entry;        % Calibrated to match

% Own products (implied model outcomes)
lambda_proc = endo_best.lambda;
gamma_proc  = endo_best.gamma;

sStep = i.target_cd/(i.z + i.target_cd) ...
      * gamma_proc ...
      / (1 - gamma_proc*i.z/(i.z + i.target_cd)) ...
      * lambda_proc;

model_int = (p.epsilon - 1) ...
          * (sStep*i.z - i.target_growth) ...
          + i.target_growth;

model_decline = (p.epsilon - 1) ...
              * lambda_proc ...
              * gamma_proc ...
              * log(gamma_proc);

noise_results = load('estimated_noise_sd.mat', 'share_mid');
model_shapley = round(noise_results.share_mid,2);

% Firm types (compute from endo_best using formulas from file3b_typesolver)
pi1 = endo_best.K_target(1);
lambda1 = 1 + endo_best.lambdaburst_type1;
lambda2 = 1 + endo_best.lambdaburst_type2;

% Autoregbeta (persistence)
model_autoregbeta = (pi1*(1-pi1)*(1/lambda1 - 1/lambda2)^2) / ...
                    (pi1*(1-pi1)*(1/lambda1 - 1/lambda2)^2 ...
                    + pi1*((lambda1-1)/lambda1)^2 ...
                    + (1-pi1)*((lambda2-1)/lambda2)^2);

% Relvar (ratio of variance)
W1 = log(lambda1);
W2 = log(lambda2);
EW = pi1*W1 + (1-pi1)*W2;
VarLog = pi1*(W1 - EW)^2 + (1-pi1)*(W2 - EW)^2;
model_relvar = VarLog / (EW^2);

% Growth (mean quality improvement)
model_growth    = model_growth_best;

% Other
model_markup    = target_markup;       % Directly implied by epsilon

%% Generate LaTeX table
fid = fopen('plots/Table_6.tex', 'w');

% Table header
fprintf(fid, '\\begin{table}[t!]\n');
fprintf(fid, '\\centering\n');
fprintf(fid, '\\caption{Performance on Targeted Moments}\n');
fprintf(fid, '\\vspace*{0.5mm}\n');
fprintf(fid, '\\label{tab:parameters_targetedmoments}\n');
fprintf(fid, '\\footnotesize{\n');
fprintf(fid, '\\begin{tabular}{llrr} \\hline\n');
fprintf(fid, 'Parameter & Moment & Target & Model \\\\ \\hline\n');

% Section: Product Innovation
fprintf(fid, '\\textit{Product Innov.} \\\\\n');
fprintf(fid, '\n');
fprintf(fid, '$\\theta$ & Gabaix-Ibragimov regression of new products (log) on rank (log) if $N_{t-1}$=1 & %s & %s \\\\\n', fmtnum(target_slope), fmtnum(model_slope));
fprintf(fid, '$\\sigma$ & Scaling of number of innovation bursts in firm size & %s & %s \\\\\n', fmtnum(target_sigma), fmtnum(model_sigma));
fprintf(fid, '$\\eta_b$ & Arrival rate of innovation bursts (per existing product) & %s & %s \\\\\n', fmtnum(target_xrate), fmtnum(model_xrate));
fprintf(fid, '$\\eta_e$ & Entry rate & %s & %s \\\\\n', fmtnum(target_entry), fmtnum(model_entry));
fprintf(fid, '& \\\\\n');

% Section: Own Products
fprintf(fid, '\\textit{Own Products} \\\\\n');
fprintf(fid, '\n');
fprintf(fid, '$x_o$, $\\lambda$ & Revenue growth on continuing products & %s & %s \\\\\n', fmtnum(target_int), fmtnum(model_int));
fprintf(fid, '$\\beta$ & Change in revenue growth in product tenure & %s & %s \\\\\n', fmtnum(target_decline), fmtnum(model_decline));
fprintf(fid, '$\\Xi$ & Shapley-Owen contrib.\\ of own products to revenue growth & %s\\%% & %s\\%% \\\\\n', fmtnum(target_shapley*100), fmtnum(model_shapley*100));
fprintf(fid, '& \\\\\n');

% Section: Firm Types (jointly determined by 3 moments)
fprintf(fid, '\\textit{Firm Types} \\\\\n');
fprintf(fid, '\n');
fprintf(fid, '$\\lambda_{b,L}/\\lambda_{b,H}$ & Persistence of revenue per new product within firms & %s & %s \\\\\n', fmtnum(target_autoregbeta), fmtnum(model_autoregbeta));
fprintf(fid, '$G(h)$ & Ratio of the variance of log revenue on new products over squared average & %s & %s \\\\\n', fmtnum(target_relvar), fmtnum(model_relvar));
fprintf(fid, '$\\lambda_{b}$ & Growth rate of manufacturing total factor productivity & %s & %s \\\\\n', fmtnum(target_growth), fmtnum(model_growth));
fprintf(fid, '& \\\\\n');

% Section: Other
fprintf(fid, '\\textit{Other} \\\\\n');
fprintf(fid, '\n');
fprintf(fid, '$\\epsilon$ & Markup & %s & %s \\\\ \\hline\n', fmtnum(target_markup), fmtnum(model_markup));

% Table footer
fprintf(fid, '\\end{tabular}}\n');
fprintf(fid, '\\end{table}\n');

fclose(fid);

%% Display confirmation
disp('Targeted moments table saved to: plots/Table_6.tex')
disp(' ')
disp('Targeted moments:')
fprintf('  Slope (theta)         : Target = %.4f, Model = %.4f\n', target_slope, model_slope);
fprintf('  Sigma                 : Target = %.4f, Model = %.4f\n', target_sigma, model_sigma);
fprintf('  Arrival rate (eta_b)  : Target = %.4f, Model = %.4f\n', target_xrate, model_xrate);
fprintf('  Entry rate (eta_e)    : Target = %.4f, Model = %.4f\n', target_entry, model_entry);
fprintf('  Internal innov.       : Target = %.4f, Model = %.4f\n', target_int, model_int);
fprintf('  Decline rate (beta)   : Target = %.4f, Model = %.4f\n', target_decline, model_decline);
fprintf('  Shapley-Owen          : Target = %.4f, Model = %.4f\n', target_shapley, model_shapley);
fprintf('  Autoregbeta           : Target = %.4f, Model = %.4f\n', target_autoregbeta, model_autoregbeta);
fprintf('  Relvar (G(h))         : Target = %.4f, Model = %.4f\n', target_relvar, model_relvar);
fprintf('  TFP growth (lambda_b) : Target = %.4f, Model = %.4f\n', target_growth, model_growth);
fprintf('  Markup (epsilon)      : Target = %.4f, Model = %.4f\n', target_markup, model_markup);
disp(' ')
disp('done!')

%% Helper function for number formatting
function s = fmtnum(x)
    % Format number: 3 decimals without leading zero if |x| < 1, else 2 decimals
    if abs(x) < 1
        s = sprintf('%.3f', x);
        % Remove leading zero (handle negative numbers)
        if x >= 0
            s = s(2:end);  % Remove '0' from '0.xxx'
        else
            s = ['-', s(3:end)];  % Remove '0' from '-0.xxx'
        end
    else
        s = sprintf('%.2f', x);
    end
end
