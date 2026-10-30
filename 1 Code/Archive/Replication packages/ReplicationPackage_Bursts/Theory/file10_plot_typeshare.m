%% FILE10_PLOT_TYPESHARE_N_THETA_INF_LOG
%
%   Produce Figure 13 with a log x-axis
%   (ticks at 1, 2, 4, 8, ..., 1024).
%   The empirical histogram remains an exact-size product-share histogram:
%   each bar height equals the fraction of products produced by firms with
%   exactly n products.

%% Configuration
theta_vec  = [2.6, 3.6, 4.5, 3.06];
n_max_plot = 1024;

output_pdf = fullfile('plots', 'Figure_13.pdf');
if ~exist('plots', 'dir')
    mkdir('plots');
end

if isempty(theta_vec)
    error('theta_vec must contain at least one theta value.')
end
if any(~isfinite(theta_vec)) || any(theta_vec <= 1)
    error('All theta values in theta_vec must be finite and strictly greater than 1.')
end

%% X-axis layout
log2_max = floor(log2(n_max_plot));   % 10 for n_max_plot = 1024

%% Load baseline model objects
[~, s_quant, p, i] = file2_parameters_targets;
load('estimation_results.mat', 'endo_best');

s_plot = s_quant;
s_plot.nReal  = 0;
s_plot.nBurn  = 2;
p.zetamax     = 600;
s_plot.NNExt  = 4000;
s_plot.NN     = 4000;

plot_i = i;
p.zetapar       = endo_best.tht;
z_base.main     = zeta(p.zetapar);
z_base.minone   = zeta(p.zetapar - 1);
t.K_target           = endo_best.K_target;
t.lambda_burst_type1 = endo_best.lambdaburst_type1;
t.lambda_burst_type2 = endo_best.lambdaburst_type2;

N            = (1:s_plot.NNExt)';
n_plot_final = min(n_max_plot, numel(N));
if n_plot_final < 1
    error('n_max_plot must be at least 1.')
end
N_plot = N(1:n_plot_final);

%% Sweep over theta
theta_vec = theta_vec(:)';
n_theta   = numel(theta_vec);

type2_frac_mat = NaN(numel(N), n_theta);
theta_labels   = cell(1, n_theta);

disp('Calculating type-2 firm share per n across theta values..')
for idx = 1:n_theta
    theta = theta_vec(idx);

    p_theta          = p;
    p_theta.zetapar  = theta;

    z_theta.main    = zeta(theta);
    z_theta.minone  = zeta(theta - 1);

    endo_theta              = endo_best;
    endo_theta.tht          = theta;
    endo_theta.x_abs_type1  = endo_theta.x_abs_type1 * ...
        (z_theta.main / z_theta.minone) * (z_base.minone / z_base.main);

    innovationrates_theta = endo_theta.x_rel .* endo_theta.x_abs_type1;

    type2_frac_mat(:, idx) = local_compute_type2_frac( ...
        innovationrates_theta, plot_i, s_plot, z_theta, t, p_theta, endo_theta.G);

    theta_labels{idx} = sprintf('\\theta = %s', num2str(theta, '%.4g'));
end

%% KK benchmark
p_kk          = p;
p_kk.zetapar  = 1e10;
z_kk.main     = zeta(p_kk.zetapar);
z_kk.minone   = zeta(p_kk.zetapar - 1);

endo_kk              = endo_best;
endo_kk.tht          = p_kk.zetapar;
endo_kk.x_abs_type1  = endo_kk.x_abs_type1 * (z_base.minone / z_base.main);

innovationrates_kk = endo_kk.x_rel .* endo_kk.x_abs_type1;
type2_frac_kk = local_compute_type2_frac( ...
    innovationrates_kk, plot_i, s_plot, z_kk, t, p_kk, endo_kk.G);

%% Empirical firm-size histogram (exact-size product share on log x-axis)
prodcount_data = readtable(fullfile('plot_coordinates', 'prodcount.csv'));
emp_size = round(prodcount_data.N(:));
emp_prod_share = 100 * prodcount_data.Var4(:);

valid = isfinite(emp_size) & isfinite(emp_prod_share) & emp_size >= 1;
emp_size = emp_size(valid);
emp_prod_share = emp_prod_share(valid);

pmf_pct = zeros(size(N_plot));
in_support = emp_size <= N_plot(end);
pmf_pct(emp_size(in_support)) = emp_prod_share(in_support);

%% Plot
myplot = figure('Color', 'w');
set(myplot, 'Position', [100, 100, 1400, 560]);
ax = axes(myplot);
set(ax, 'Position', [0.08, 0.16, 0.80, 0.76]);

% --- Right axis: exact-size empirical histogram
yyaxis(ax, 'right')
hold(ax, 'on')
h_bar = gobjects(0);
for k = 1:n_plot_final
    x_left  = max(N_plot(k) - 0.5, 0.5);
    x_right = N_plot(k) + 0.5;
    xv = [x_left, x_right, x_right, x_left];
    yv = [0, 0, pmf_pct(k), pmf_pct(k)];
    if k == 1
        h_bar = patch(ax, xv, yv, [0.10 0.55 0.15], ...
            'FaceAlpha', 0.25, 'EdgeColor', 'none', 'LineStyle', 'none', ...
            'Marker', 'none', ...
            'DisplayName', 'Empirical product share (right-hand axis)');
    else
        patch(ax, xv, yv, [0.10 0.55 0.15], ...
            'FaceAlpha', 0.25, 'EdgeColor', 'none', 'LineStyle', 'none', ...
            'Marker', 'none', ...
            'HandleVisibility', 'off');
    end
end
set(ax, 'YColor', 'k')
ylim(ax, [0, 40])
yticks(ax, [10 20 30 40])
ylabel(ax, 'Empirical product share (%)')

% --- Left axis: model lines
yyaxis(ax, 'left')
hold(ax, 'on')

stata_blue = [0, 0, 1];
stata_red  = [0.780, 0.208, 0.204];

other_colors = [
    0.514, 0.176, 0.608;   % purple  — theta=2.6
    0.039, 0.569, 0.518;   % teal    — theta=3.6
    0.859, 0.384, 0.055;   % orange  — theta=4.5
];
other_styles = {'--', '-.', ':'};

[~, calib_idx] = min(abs(theta_vec - endo_best.tht));

h_theta   = gobjects(n_theta, 1);
other_idx = 0;
for idx = 1:n_theta
    if idx == calib_idx
        clr = stata_blue;
        ls  = '-';
        lw  = 4.0;
        lbl = [theta_labels{idx}, ' (bursts calibration)'];
    else
        other_idx = other_idx + 1;
        clr = other_colors(min(other_idx, size(other_colors, 1)), :);
        ls  = other_styles{min(other_idx, numel(other_styles))};
        lw  = 2.6;
        lbl = theta_labels{idx};
    end

    h_theta(idx) = plot(ax, N_plot, type2_frac_mat(N_plot, idx), ...
        'LineStyle', ls, 'LineWidth', lw, 'Color', clr, ...
        'Marker', 'none', 'DisplayName', lbl);
end

h_kk = plot(ax, N_plot, type2_frac_kk(N_plot), ...
    'LineStyle', '--', 'LineWidth', 3.0, ...
    'Color', stata_red, ...
    'Marker', 'none', 'DisplayName', 'Poisson Innovation');

finite_kk_idx = find(isfinite(type2_frac_kk(N_plot)), 1, 'last');
if ~isempty(finite_kk_idx) && N_plot(finite_kk_idx) < n_max_plot
    plot(ax, [N_plot(finite_kk_idx), n_max_plot], [100, 100], ...
        'LineStyle', '--', 'LineWidth', 3.0, ...
        'Color', stata_red, ...
        'Marker', 'none', 'HandleVisibility', 'off');
end

% --- Shared axis formatting
set(ax, 'XScale', 'log')
xlim(ax, [0.85, n_max_plot * 1.08])

xtick_vals = 2 .^ (0:log2_max);
set(ax, 'XTick', xtick_vals, 'XTickLabel', arrayfun(@num2str, xtick_vals, 'UniformOutput', false))

grid(ax, 'on')
set(ax, 'XMinorGrid', 'off', 'YMinorGrid', 'off')
box(ax, 'on')
ylim(ax, [0, 102])
set(ax, 'YColor', 'k')

xlabel(ax, 'Number of Products')
ylabel(ax, 'Share of Firms with High Innovation Type (%)')

% Legend ordered: Poisson, theta=4.5, 3.6, 3.06, 2.6, empirical
[~, sort_idx] = sort(theta_vec, 'descend');
h_lines = [h_kk; h_theta(sort_idx); h_bar];
set(ax, 'FontSize', 18)
set(get(ax, 'XLabel'), 'FontSize', 20)
set(get(ax, 'YLabel'), 'FontSize', 20)
lgd = legend(h_lines, 'Location', 'southeast');
set(lgd, 'FontSize', 14)
set(lgd, 'Box', 'on')
set(lgd, 'Color', 'w')

set(myplot, 'PaperOrientation', 'landscape');
set(myplot, 'PaperUnits',       'inches');
set(myplot, 'PaperPosition',    [0 0 14.0 5.6]);
set(myplot, 'PaperSize',        [14.0 5.6]);
print(myplot, '-dpdf', output_pdf);

disp('done!')
fprintf('Saved Figure 13: %s\n', output_pdf)

%% Local functions

function type2_frac = local_compute_type2_frac(innovationrates, plot_i, s_plot, z_use, t_use, p_use, G_use)
    M_full = file3f_firmsizedistribution( ...
        innovationrates, plot_i.target_cd, G_use, plot_i, s_plot, z_use, t_use, p_use);

    M_prodcount = M_full(:, 1:s_plot.NNExt);
    if size(M_full, 2) > s_plot.NNExt
        M_prodcount(:, end) = M_prodcount(:, end) + sum(M_full(:, s_plot.NNExt+1:end), 2);
    end

    total_at_n = sum(M_prodcount, 1)';
    type2_at_n = M_prodcount(2, :)';

    type2_frac                   = 100 * type2_at_n ./ total_at_n;
    type2_frac(total_at_n < eps) = NaN;
end
