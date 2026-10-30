%% FILE11_PLOT_GROWTH_CONTRIB_THETA_LOG
%
%   Produce Figure 14: cumulative growth-contribution curves on a
%   log-scaled product-count axis, through a firm-size threshold of 256.

%% Configuration
theta_vec = [2.60, 3.6, 4.5, 3.06];
theta_style_target = 3.06;
n_max_plot = 256;
n_max_tick = 256;
output_pdf = fullfile('plots', 'Figure_14.pdf');

if ~exist('plots', 'dir')
    mkdir('plots');
end

%% Load baseline model objects
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
z_base.main = zeta(p.zetapar);
z_base.minone = zeta(p.zetapar - 1);
t.K_target = endo_best.K_target;
t.lambda_burst_type1 = endo_best.lambdaburst_type1;
t.lambda_burst_type2 = endo_best.lambdaburst_type2;

N = (1:s_plot.NNExt)';
lambda_bar_h = [1 + endo_best.lambdaburst_type1; 1 + endo_best.lambdaburst_type2];
g_weight_h = (lambda_bar_h.^(p.epsilon - 1) - 1) / (p.epsilon - 1);

theta_vec = theta_vec(:)';
n_theta = numel(theta_vec);
C_g_cum_pct_mat = zeros(numel(N), n_theta);
theta_labels = cell(1, n_theta);

disp('Calculating Figure 14 growth-contribution curves...')
for idx = 1:n_theta
    theta = theta_vec(idx);

    p_theta = p;
    p_theta.zetapar = theta;
    z_theta.main = zeta(theta);
    z_theta.minone = zeta(theta - 1);

    endo_theta = endo_best;
    endo_theta.tht = theta;
    endo_theta.x_abs_type1 = endo_theta.x_abs_type1 * ...
        (z_theta.main / z_theta.minone) * (z_base.minone / z_base.main);

    innovationrates_theta = endo_theta.x_rel .* endo_theta.x_abs_type1;
    C_g_cum_pct_mat(:, idx) = local_compute_cg_cum_pct( ...
        innovationrates_theta, plot_i, s_plot, z_theta, t, p_theta, endo_theta.G, g_weight_h);

    if abs(theta - theta_style_target) < 1e-10
        theta_labels{idx} = sprintf('\\theta = %s (bursts calibration)', num2str(theta, '%.4g'));
    else
        theta_labels{idx} = sprintf('\\theta = %s', num2str(theta, '%.4g'));
    end
end

%% Compute the Poisson-innovation comparison
p_kk = p;
p_kk.zetapar = 1e10;
z_kk.main = zeta(p_kk.zetapar);
z_kk.minone = zeta(p_kk.zetapar - 1);

endo_kk = endo_best;
endo_kk.tht = p_kk.zetapar;
endo_kk.x_abs_type1 = endo_kk.x_abs_type1 * (z_base.minone / z_base.main);

innovationrates_kk = endo_kk.x_rel .* endo_kk.x_abs_type1;
C_g_cum_pct_kk = local_compute_cg_cum_pct( ...
    innovationrates_kk, plot_i, s_plot, z_kk, t, p_kk, endo_kk.G, g_weight_h);

%% Produce only the 256-product MATLAB figure
local_plot_matlab_log( ...
    N, C_g_cum_pct_mat, C_g_cum_pct_kk, theta_vec, theta_labels, ...
    theta_style_target, n_max_plot, n_max_tick, output_pdf);

fprintf('Saved Figure 14: %s\n', output_pdf)

function local_plot_matlab_log(N, C_g_cum_pct_mat, C_g_cum_pct_kk, theta_vec, theta_labels, theta_style_target, n_max_plot, n_max_tick, output_pdf)
    N_plot = N(N <= n_max_plot);
    N_plot_log2 = log2(N_plot);
    max_tick_pow = round(log2(n_max_tick));
    tick_vals = 2 .^ (0:max_tick_pow);
    tick_pos = log2(tick_vals);

    myplot = figure('Color', 'w');
    set(myplot, 'Position', [100, 100, 1350, 560]);

    ax = axes(myplot, 'Color', 'w', 'Box', 'on', ...
        'FontName', 'Helvetica', 'FontSize', 18, 'LineWidth', 1.0);
    hold(ax, 'on')

    n_theta = numel(theta_vec);
    line_width_main = 2.5;
    line_width_emphasis = 1.5 * line_width_main;
    h_theta = gobjects(n_theta, 1);
    for idx = 1:n_theta
        theta = theta_vec(idx);
        plot_width = line_width_main;

        if abs(theta - 2.6) < 1e-10
            plot_color = [0.4940 0.1840 0.5560];
            plot_style = '--';
        elseif abs(theta - 3.6) < 1e-10
            plot_color = [0.0000 0.5000 0.0000];
            plot_style = '-.';
        elseif abs(theta - theta_style_target) < 1e-10
            plot_color = [0.0000 0.0000 1.0000];
            plot_style = '-';
            plot_width = line_width_emphasis;
        elseif abs(theta - 4.5) < 1e-10
            plot_color = [0.8500 0.3250 0.0980];
            plot_style = ':';
        else
            plot_color = [0.2 0.2 0.2];
            plot_style = '-';
        end

        h_theta(idx) = plot(ax, N_plot_log2, C_g_cum_pct_mat(N_plot, idx), ...
            'LineStyle', plot_style, ...
            'LineWidth', plot_width, ...
            'Color', plot_color, ...
            'DisplayName', theta_labels{idx});
    end

    h_kk = plot(ax, N_plot_log2, C_g_cum_pct_kk(N_plot), ...
        '--', ...
        'Color', [1.0000 0.0000 0.0000], ...
        'LineWidth', line_width_emphasis, ...
        'DisplayName', 'Poisson Innovation');

    grid(ax, 'on')
    set(ax, 'GridColor', [0.85 0.85 0.85], 'GridAlpha', 0.32, ...
        'XMinorGrid', 'off', 'YMinorGrid', 'off')
    box(ax, 'on')
    xlim(ax, [0, log2(n_max_tick)])
    ylim(ax, [0, 100])
    xticks(ax, tick_pos)
    xticklabels(ax, string(tick_vals))
    xlabel(ax, 'Firm size threshold \itn\rm', ...
        'Interpreter', 'tex', 'FontName', 'Helvetica', 'FontSize', 20)
    ylabel(ax, '% of growth from firms of size \geq \itn\rm', ...
        'Interpreter', 'tex', 'FontName', 'Helvetica', 'FontSize', 20)

    legend_order = [];
    target_theta_order = [2.6, theta_style_target, 3.6, 4.5];
    for theta_target = target_theta_order
        match_idx = find(abs(theta_vec - theta_target) < 1e-10, 1, 'first');
        if ~isempty(match_idx)
            legend_order(end+1) = h_theta(match_idx); %#ok<AGROW>
        end
    end
    legend_order(end+1) = h_kk;
    lgd = legend(ax, legend_order, 'Location', 'northeast');
    set(lgd, ...
        'FontName', 'Helvetica', ...
        'FontSize', 18, ...
        'Box', 'on', ...
        'EdgeColor', [0 0 0], ...
        'TextColor', [0 0 0], ...
        'Color', [1 1 1])

    exportgraphics(myplot, output_pdf, ...
        'ContentType', 'vector', 'BackgroundColor', 'white');
end

function C_g_cum_pct = local_compute_cg_cum_pct(innovationrates, plot_i, s_plot, z_use, t_use, p_use, G_use, g_weight_h)
    M_full = file3f_firmsizedistribution( ...
        innovationrates, plot_i.target_cd, G_use, plot_i, s_plot, z_use, t_use, p_use);
    M_prodcount = M_full(:, 1:s_plot.NNExt);
    if size(M_full, 2) > s_plot.NNExt
        M_prodcount(:, end) = M_prodcount(:, end) + ...
            sum(M_full(:, s_plot.NNExt+1:end), 2);
    end

    N_local = (1:s_plot.NNExt)';
    weighted_g_by_n = N_local .* ...
        sum(M_prodcount .* (innovationrates(:) .* g_weight_h), 1)';
    C_g = weighted_g_by_n / sum(weighted_g_by_n);
    C_g_cum_pct = 100 * flipud(cumsum(flipud(C_g)));
    C_g_cum_pct = min(max(C_g_cum_pct, 0), 100);
end
