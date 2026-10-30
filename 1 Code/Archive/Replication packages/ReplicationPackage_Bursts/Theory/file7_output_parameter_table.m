%% FILE7_OUTPUT_PARAMETER_TABLE - Generate LaTeX Parameter Summary Table
%
%   This script generates a LaTeX table summarizing all model parameters.
%   Called from file1_master.m when output_tables flag is set to 1.
%
%   Requires: endo_best (from estimation_results.mat), p, i, t, z
%   Produces: plots/table_parameters.tex
%
% NOTE: eta_b is divided by avg burst size (zeta(theta-1)/zeta(theta))
%       for comparability with entry cost (cost per innovation)

disp('Generating parameter summary table...')

%% Setup zeta parameters (needed for R&D cost computation)
p.zetapar = endo_best.tht;
z.main   = zeta(p.zetapar);
z.minone = zeta(p.zetapar-1);
t.K_target = endo_best.K_target;

%% Compute R&D cost parameters
[eta_b_raw, eta_e, ~, ~] = file7a_policy_findbaselineRDcost(endo_best, i, z, p, t);
% Divide eta_b by average burst size for comparability (cost per innovation)
avg_burst_size = z.minone / z.main;  % = zeta(theta-1)/zeta(theta)
eta_b = eta_b_raw / avg_burst_size;

%% Extract parameter values
% Exogenous parameters
theta   = endo_best.tht;              % Tail parameter of zeta distribution
sigma   = p.sigma;                    % Degree of returns to product counts
psi     = p.psi;                      % Convexity of R&D costs
rho     = p.rho;                      % Discount rate
epsilon = p.epsilon;                  % Elasticity of substitution
x_o     = i.z;                        % Poisson rate of process innovations

% Endogenous parameters (from calibration)
beta_decline = endo_best.gamma;       % Decline rate of follow-up process innovation
lambda_proc  = endo_best.lambda;      % Process innovation step size
G            = endo_best.G;           % Entrant share of type L and H
% Lambda_burst values: Note that the "quality improvement" is 1 + lambdaburst
lambda_bL    = 1 + endo_best.lambdaburst_type1;  % Avg quality improvement type L
lambda_bH    = 1 + endo_best.lambdaburst_type2;  % Avg quality improvement type H

%% Generate LaTeX table
fid = fopen('plots/Table_5.tex', 'w');

% Table header
fprintf(fid, '\\begin{table}[t!]\n');
fprintf(fid, '\\centering\n');
fprintf(fid, '\\caption{Summary of Parameter Values}\n');
fprintf(fid, '\\vspace*{0.5mm}\n');
fprintf(fid, '\\label{tab:parameters}\n');
fprintf(fid, '\\footnotesize{\n');
fprintf(fid, '\\begin{tabular}{llrl} \\hline\n');
fprintf(fid, 'Parameter & Description & Value & Method \\\\ \\hline\n');
fprintf(fid, '\n');

% Parameters - following order from user's example table
fprintf(fid, '$\\theta$ & Tail parameter of the zeta distribution & %s & Grid search \\\\\n', fmtnum(theta));
fprintf(fid, '$\\sigma$ & Degree of returns to product counts in product innovation & %s & Direct \\\\\n', fmtnum(sigma));
fprintf(fid, '$\\eta_e$ & Product creation R\\&D cost scalar for entrants & %.1f & Direct \\\\\n', eta_e);
fprintf(fid, '$\\eta_b$ & Product creation R\\&D cost scalar & %.1f & Direct \\\\\n', eta_b_raw);
fprintf(fid, '$\\psi$ & Convexity of R\\&D costs in innovation rate & %s & External \\\\\n', fmtnum(psi));
fprintf(fid, '$\\rho$ & Discount rate & %s & External \\\\\n', fmtnum(rho));
fprintf(fid, '$\\beta$ & Decline rate of follow-up process innovation size & %s & Direct \\\\\n', fmtnum(beta_decline));
fprintf(fid, '$x_o$ & Poisson rate of process innovations & %s & Direct \\\\\n', fmtnum(x_o));
fprintf(fid, '$\\lambda$ & Process innovation step size & %.3f & Direct \\\\\n', lambda_proc);
fprintf(fid, '$\\epsilon$ & Elasticity of substitution & %s & External \\\\\n', fmtnum(epsilon));
fprintf(fid, '$\\Xi$ & Standard deviation of idiosyncratic quality shocks & %.3f & Grid search \\\\\n', floor(shock_sd * 1000) / 1000);
fprintf(fid, '$G$ & Entrant share of type \\textit{L} and \\textit{H} & [%s,%s] & Grid search \\\\\n', fmtnum(G(1)), fmtnum(G(2)));
fprintf(fid, '$\\lambda_{b,h}$ & Avg.\\ quality improv.\\ size of product innovation \\textit{L} and \\textit{H} & [%.3f,%.3f] & Grid search \\\\ \\hline\n', lambda_bL, lambda_bH);

% Table footer
fprintf(fid, '\\end{tabular}}\n');
fprintf(fid, '\\vspace{-0.2cm}\n');
fprintf(fid, '\\end{table}\n');

fclose(fid);

%% Display confirmation
disp('Parameter table saved to: plots/Table_5.tex')
disp(' ')
disp('Parameter values:')
fprintf('  theta (zeta tail)     = %.4f\n', theta);
fprintf('  sigma                 = %.4f\n', sigma);
fprintf('  eta_e (entrant R&D)   = %.4f\n', eta_e);
fprintf('  eta_b (per innov.)    = %.4f (raw: %.4f, avg burst: %.4f)\n', eta_b, eta_b_raw, avg_burst_size);
fprintf('  psi                   = %.4f\n', psi);
fprintf('  rho                   = %.4f\n', rho);
fprintf('  beta (decline)        = %.4f\n', beta_decline);
fprintf('  x_o (process rate)    = %.4f\n', x_o);
fprintf('  lambda (process step) = %.4f\n', lambda_proc);
fprintf('  epsilon               = %.4f\n', epsilon);
fprintf('  G                     = [%.4f, %.4f]\n', G(1), G(2));
fprintf('  lambda_b (L,H)        = [%.4f, %.4f]\n', lambda_bL, lambda_bH);
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
