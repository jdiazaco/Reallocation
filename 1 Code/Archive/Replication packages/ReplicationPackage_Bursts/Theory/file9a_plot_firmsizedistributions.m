function avg_products = file9a_plot_firmsizedistributions(i,p,s,z,t,endo,varargin)
% FILE9A_PLOT_FIRMSIZEDISTRIBUTIONS  Plot stationary size-rank curves by type and combined.
%
%   avg_products = file9a_plot_firmsizedistributions(i,p,s,z,t,endo)
%
%   One panel per series (type 1, type 2, combined), with both:
%   - Innovation Bursts calibration (endo, zeta = endo.tht)
%   - Klette-Kortum limit (zeta -> infinity)

opts = local_default_options();
if ~isempty(varargin)
    opts_in = varargin{1};
    opt_fields = fieldnames(opts_in);
    for k = 1:numel(opt_fields)
        opts.(opt_fields{k}) = opts_in.(opt_fields{k});
    end
end

[M_main_plot, M_main_full] = local_make_Mplot(i,p,s,z,t,endo);

p_kk = p;
p_kk.zetapar = 1e10;
z_kk.main = zeta(p_kk.zetapar);
z_kk.minone = zeta(p_kk.zetapar-1);
endo_kk = endo;
endo_kk.tht = p_kk.zetapar;
endo_kk.x_abs_type1 = endo_kk.x_abs_type1 * z.minone / z.main;
[M_kk_plot, M_kk_full] = local_make_Mplot(i,p_kk,s,z_kk,t,endo_kk);

M_kk_equal_plot = [];
M_kk_equal_full = [];
need_kkequal = opts.include_kkequal == 1 | opts.export_stata == 1;
if need_kkequal == 1
    p_kk_equal = p;
    p_kk_equal.zetapar = 1e10;
    z_kk_equal.main = zeta(p_kk_equal.zetapar);
    z_kk_equal.minone = zeta(p_kk_equal.zetapar-1);
    endo_kk_equal = endo;
    endo_kk_equal.tht = p_kk_equal.zetapar;
    endo_kk_equal.x_rel = ones(size(endo.x_rel));
    endo_kk_equal.x_abs_type1 = i.xrate_target;
    [M_kk_equal_plot, M_kk_equal_full] = local_make_Mplot(i,p_kk_equal,s,z_kk_equal,t,endo_kk_equal);
end

[avg_main_combined, avg_main_by_type] = local_average_products(M_main_full);
[avg_kk_combined, avg_kk_by_type] = local_average_products(M_kk_full);
if need_kkequal == 1
    [avg_kk_equal_combined, avg_kk_equal_by_type] = local_average_products(M_kk_equal_full);
else
    avg_kk_equal_combined = [];
    avg_kk_equal_by_type = [];
end

main_summary = local_summary_stats(M_main_full);
kk_summary = local_summary_stats(M_kk_full);
if need_kkequal == 1
    kk_equal_summary = local_summary_stats(M_kk_equal_full);
else
    kk_equal_summary = struct('mean', [], 'median', [], 'p90', [], 'p95', [], 'p99', []);
end

summary_table = table( ...
    [main_summary.mean;   kk_summary.mean], ...
    [main_summary.median; kk_summary.median], ...
    [main_summary.p90;    kk_summary.p90], ...
    [main_summary.p95;    kk_summary.p95], ...
    [main_summary.p99;    kk_summary.p99], ...
    'VariableNames', {'Mean','Median','P90','P95','P99'}, ...
    'RowNames', {'InnovationBursts','KletteKortum'});

moments_table = summary_table(:, {'Mean','Median','P90','P95'});

avg_products = struct( ...
    'main_combined', avg_main_combined, ...
    'kk_combined', avg_kk_combined, ...
    'main_by_type', avg_main_by_type, ...
    'kk_by_type', avg_kk_by_type, ...
    'kk_equal_combined', avg_kk_equal_combined, ...
    'kk_equal_by_type', avg_kk_equal_by_type, ...
    'main_summary', main_summary, ...
    'kk_summary', kk_summary, ...
    'kk_equal_summary', kk_equal_summary, ...
    'summary_table', summary_table, ...
    'moments_table', moments_table, ...
    'entry_rate', i.e, ...
    'creative_destruction_rate', i.target_cd);

nTypes = size(M_main_plot,1);
nCols = nTypes + 1;
x_cap = 256;

series_main = local_build_series(M_main_plot, x_cap);
series_kk = local_build_series(M_kk_plot, x_cap);
if need_kkequal == 1
    series_kk_equal = local_build_series(M_kk_equal_plot, x_cap);
else
    series_kk_equal = [];
end

y_all = [series_main.y_all; series_kk.y_all];
if need_kkequal == 1
    y_all = [y_all; series_kk_equal.y_all];
end
data_xy = local_load_data_points(x_cap);
if ~isempty(data_xy)
    y_all = [y_all; data_xy(:,2)];
end
if isempty(y_all)
    y_lim = [log(1e-13) 0];
else
    y_pad = 0.05 * range(y_all);
    if y_pad == 0
        y_pad = 0.5;
    end
    y_lim = [log(1e-13), max(y_all)+y_pad];
end

if opts.export_stata == 1
    local_export_stata_table(series_main, series_kk, series_kk_equal, data_xy, x_cap);
    return;
end

if opts.export_stata_hom == 1
    local_export_stata_homogeneous_table(i,p,s,z,t,endo,data_xy,x_cap);
    return;
end

if opts.export_stata_full_unscaled == 1
    local_export_stata_full_unscaled_table(i,p,s,z,t,endo,data_xy,x_cap);
    return;
end

disp('Firm-size moments (number of products):')
disp(moments_table)

if opts.export_separate == 1
    local_export_separate_panels(series_main, series_kk, series_kk_equal, data_xy, x_cap, y_lim, opts);
    return;
end

fig = figure('Name','Stationary Firm-Size Distribution (Log-Rank)', ...
    'Color', 'w', 'Units', 'pixels', 'Position', [100 100 1600 680]);
set(fig, 'PaperOrientation', 'landscape', 'PaperPositionMode', 'auto');
tl = tiledlayout(1, nCols, 'Padding', 'loose', 'TileSpacing', 'compact');
tl.Position = [0.07 0.12 0.91 0.82];

all_axes = gobjects(0);

for col = 1:nCols
    ax = nexttile(col);
    if col == nCols
        data_this = data_xy;
    else
        data_this = [];
    end
    local_plot_one(ax, ...
        series_main.x{col}, series_main.y{col}, ...
        series_kk.x{col}, series_kk.y{col}, ...
        [], [], ...
        data_this, series_main.names{col}, x_cap, y_lim, col == nCols, col == 1, opts);
    all_axes(end+1) = ax; %#ok<AGROW>
end

for k = 1:numel(all_axes)
    yt = get(all_axes(k), 'YTick');
    set(all_axes(k), ...
        'YTickLabel', arrayfun(@(v) sprintf('10^{%d}', round(v/log(10))), yt, 'UniformOutput', false), ...
        'TickLabelInterpreter', 'tex');
end

if exist('plots','dir') ~= 7
    mkdir('plots');
end
pdf_out = fullfile('plots','theory_firmsize_N.pdf');
png_out = fullfile('plots','theory_firmsize_N.png');
emf_out = fullfile('plots','theory_firmsize_N.emf');

try
    exportgraphics(fig, pdf_out, 'ContentType', 'vector');
    exportgraphics(fig, png_out, 'Resolution', 300);
catch
    if isgraphics(fig)
        print(fig, pdf_out, '-dpdf', '-painters');
        print(fig, png_out, '-dpng', '-r300');
    end
end

if isgraphics(fig)
    try
        set(fig, 'PaperPositionMode', 'auto');
        print(fig, emf_out, '-dmeta', '-painters');
    catch
        if isgraphics(fig)
            saveas(fig, emf_out, 'meta');
        end
    end
end

end

function [M_plot, M_full] = local_make_Mplot(i,p,s,z,t,endo)
innovationrates = endo.x_rel .* endo.x_abs_type1;
M_full = file3f_firmsizedistribution(innovationrates, i.target_cd, endo.G, i, s, z, t, p);
M_plot = M_full(:,1:s.NNExt);
if size(M_full,2) > s.NNExt
    M_plot(:,end) = M_plot(:,end) + sum(M_full(:,s.NNExt+1:end),2);
end
end

function [avg_combined, avg_by_type] = local_average_products(M_full)
n = 1:size(M_full,2);
mass_by_type = sum(M_full,2);
weighted_n_by_type = M_full * n';
avg_by_type = weighted_n_by_type ./ max(mass_by_type, eps);
avg_combined = sum(weighted_n_by_type) / max(sum(mass_by_type), eps);
end

function stats = local_summary_stats(M_full)
mass_n = sum(M_full,1);
n = 1:numel(mass_n);
valid = isfinite(mass_n) & (mass_n > 0);

if ~any(valid)
    stats = struct('mean', NaN, 'median', NaN, 'p90', NaN, 'p95', NaN, 'p99', NaN);
    return;
end

mass_n = mass_n(valid);
n = n(valid);
pmf = mass_n / sum(mass_n);

stats = struct();
stats.mean = sum(n .* pmf);
stats.median = local_weighted_percentile(n, pmf, 0.50);
stats.p90 = local_weighted_percentile(n, pmf, 0.90);
stats.p95 = local_weighted_percentile(n, pmf, 0.95);
stats.p99 = local_weighted_percentile(n, pmf, 0.99);
end

function pval = local_weighted_percentile(x, w, q)
cdf = cumsum(w);
idx = find(cdf >= q, 1, 'first');
if isempty(idx)
    pval = x(end);
else
    pval = x(idx);
end
end

function series = local_build_series(M_plot, x_cap)
nTypes = size(M_plot,1);
nCols = nTypes + 1;
series.x = cell(1, nCols);
series.y = cell(1, nCols);
series.names = cell(1, nCols);
series.y_all = [];

for typeIdx = 1:nTypes
    [x_log, y_log] = local_rank_series(M_plot(typeIdx,:), x_cap);
    series.x{typeIdx} = x_log;
    series.y{typeIdx} = y_log;
    series.names{typeIdx} = sprintf('Type %d', typeIdx);
    series.y_all = [series.y_all; y_log]; %#ok<AGROW>
end

[x_log, y_log] = local_rank_series(sum(M_plot,1), x_cap);
series.x{nCols} = x_log;
series.y{nCols} = y_log;
series.names{nCols} = 'Full Distribution';
series.y_all = [series.y_all; y_log];
end

function [x_log, y_log] = local_rank_series(ms_row, x_cap)
ms = ms_row(:);
n = (1:numel(ms))';
x_max = min(x_cap, max(n));

if sum(ms) <= 0
    x_log = [];
    y_log = [];
    return;
end

tot_mass = sum(ms);
cum_before = [0; cumsum(ms(1:end-1))];

rk = 1 - (cum_before + 0.5*ms) / tot_mass;

f = (rk > 0) & isfinite(rk) & (n <= x_max);
x_log = log(n(f));
y_log = log(rk(f));
end

function local_plot_one(ax, x_main, y_main, x_kk, y_kk, x_kk_equal, y_kk_equal, data_xy, panel_title, x_cap, y_lim, show_legend, force_last_kk_marker, opts)
axes(ax); %#ok<LAXES>
font_scale = 1.5;
if isempty(x_main) && isempty(x_kk) && isempty(x_kk_equal) && isempty(data_xy)
    if opts.show_titles == 1
        th = title(panel_title);
        th.FontSize = font_scale * th.FontSize;
    end
    grid on;
    local_apply_stata_xaxis(ax, x_cap);
    return;
end

hold on;
mark_n = [1 2 4 8 16 32 64 128 256];
if ~isempty(x_main)
    h_main = plot(x_main, y_main, '-', 'Color', 'b', 'LineWidth', 3.12);
else
    h_main = [];
end
if ~isempty(x_kk)
    n_kk = round(exp(x_kk));
    mark_kk = ismember(n_kk, mark_n);
    if force_last_kk_marker && ~isempty(mark_kk)
        mark_kk(end) = true;
    end
    h_kk = plot(x_kk, y_kk, ':', 'Color', 'r', 'LineWidth', 3.12);
    plot(x_kk(mark_kk), y_kk(mark_kk), 's', ...
        'Color', 'r', 'MarkerFaceColor', 'r', 'LineStyle', 'none', 'MarkerSize', 0.5, 'LineWidth', 1.2);
else
    h_kk = [];
end
if ~isempty(x_kk_equal)
    h_kk_equal = plot(x_kk_equal, y_kk_equal, '-.', 'Color', [0.35 0.35 0.35], 'LineWidth', 3.12);
else
    h_kk_equal = [];
end
if ~isempty(data_xy)
    h_data = plot(data_xy(:,1), data_xy(:,2), 'd', ...
        'Color', [0.2 0.6 0.2], 'MarkerFaceColor', 'none', ...
        'LineStyle', 'none', 'MarkerSize', 12.5, 'LineWidth', 1.2);
    if size(data_xy,1) >= 2
        b = polyfit(data_xy(:,1), data_xy(:,2), 1);
        x_fit = linspace(0, log(x_cap), 200);
        y_fit = polyval(b, x_fit);
        plot(x_fit, y_fit, '--', 'Color', [0.6 0.6 0.6], 'LineWidth', 4.2);
    end
else
    h_data = [];
end

xh = xlabel('Number of Products');
yh = ylabel('Rank');
xh.FontSize = (0.75 * font_scale) * xh.FontSize;
yh.FontSize = font_scale * yh.FontSize;
if opts.show_titles == 1
    th = title(panel_title);
    th.FontSize = font_scale * th.FontSize;
end
xh.Clipping = 'off';
grid on;
if show_legend
    hs = gobjects(0);
    legend_labels = {};
    if isgraphics(h_main)
        hs(end+1) = h_main; %#ok<AGROW>
        legend_labels{end+1} = 'Innovation Bursts'; %#ok<AGROW>
    end
    if isgraphics(h_kk)
        hs(end+1) = h_kk; %#ok<AGROW>
        legend_labels{end+1} = 'Poisson Innovation'; %#ok<AGROW>
    end
    if isgraphics(h_kk_equal)
        hs(end+1) = h_kk_equal; %#ok<AGROW>
        legend_labels{end+1} = 'Klette and Kortum (2024) types'; %#ok<AGROW>
    end
    if isgraphics(h_data)
        hs(end+1) = h_data; %#ok<AGROW>
        legend_labels{end+1} = 'Data'; %#ok<AGROW>
    end
    legend(hs, legend_labels, 'Location', 'southwest');
end

local_apply_stata_xaxis(ax, x_cap);
ylim(ax, y_lim);
end

function local_apply_stata_xaxis(ax, x_cap)
xt_vals = [1 2 4 8 16 32 64 128 256 512];
xt_vals = xt_vals(xt_vals <= x_cap);
if isempty(xt_vals)
    xt_vals = 1;
end
set(ax, ...
    'XTick', log(xt_vals), ...
    'XTickLabel', string(xt_vals), ...
    'FontSize', 18);
xlim(ax, [0, log(max(xt_vals))]);
end

function local_export_stata_table(series_main, series_kk, series_kk_equal, data_xy, x_cap)
panel_stub = ["type1","type2","full"];
tables = cell(0,1);
nCols = numel(series_main.x);

for col = 1:nCols
    panel = panel_stub(col);
    tables{end+1,1} = local_make_export_table(panel, "baseline", "bursts", series_main.x{col}, series_main.y{col}); %#ok<AGROW>
    tables{end+1,1} = local_make_export_table(panel, "baseline", "poisson", series_kk.x{col}, series_kk.y{col}); %#ok<AGROW>
    if col == nCols && ~isempty(data_xy)
        tables{end+1,1} = local_make_export_table(panel, "baseline", "data", data_xy(:,1), data_xy(:,2)); %#ok<AGROW>
        tables{end+1,1} = local_make_fit_export_table(panel, "baseline", data_xy, x_cap); %#ok<AGROW>
    end

    tables{end+1,1} = local_make_export_table(panel, "kkequal", "bursts", series_main.x{col}, series_main.y{col}); %#ok<AGROW>
    tables{end+1,1} = local_make_export_table(panel, "kkequal", "poisson", series_kk.x{col}, series_kk.y{col}); %#ok<AGROW>
    if ~isempty(series_kk_equal)
        tables{end+1,1} = local_make_export_table(panel, "kkequal", "kkequal", series_kk_equal.x{col}, series_kk_equal.y{col}); %#ok<AGROW>
    end
    if col == nCols && ~isempty(data_xy)
        tables{end+1,1} = local_make_export_table(panel, "kkequal", "data", data_xy(:,1), data_xy(:,2)); %#ok<AGROW>
        tables{end+1,1} = local_make_fit_export_table(panel, "kkequal", data_xy, x_cap); %#ok<AGROW>
    end
end

out = vertcat(tables{:});
out = sortrows(out, {'panel','variant','series','x'});
if exist('plots','dir') ~= 7
    mkdir('plots');
end
writetable(out, fullfile('plots','theory_firmsize_stata.csv'));
end

function out = local_make_export_table(panel, variant, series, x, y)
n = numel(x);
out = table(repmat(string(panel),n,1), repmat(string(variant),n,1), repmat(string(series),n,1), x(:), y(:), ...
    'VariableNames', {'panel','variant','series','x','y'});
end

function out = local_make_fit_export_table(panel, variant, data_xy, x_cap)
b = polyfit(data_xy(:,1), data_xy(:,2), 1);
x_fit = linspace(0, log(x_cap), 200)';
y_fit = polyval(b, x_fit);
out = local_make_export_table(panel, variant, "fit", x_fit, y_fit);
end

function local_export_stata_homogeneous_table(i,p,s,z,t,endo,data_xy,x_cap)
avg_burst_size = z.minone / z.main;

endo_bursts_hom = endo;
endo_bursts_hom.x_rel = ones(size(endo.x_rel));
endo_bursts_hom.x_abs_type1 = i.xrate_target / avg_burst_size;
[M_bursts_hom_plot, ~] = local_make_Mplot(i,p,s,z,t,endo_bursts_hom);
[x_bursts_hom, y_bursts_hom] = local_rank_series(sum(M_bursts_hom_plot,1), x_cap);

p_poisson_hom = p;
p_poisson_hom.zetapar = 1e10;
z_poisson_hom.main = zeta(p_poisson_hom.zetapar);
z_poisson_hom.minone = zeta(p_poisson_hom.zetapar-1);
endo_poisson_hom = endo;
endo_poisson_hom.tht = p_poisson_hom.zetapar;
endo_poisson_hom.x_rel = ones(size(endo.x_rel));
endo_poisson_hom.x_abs_type1 = i.xrate_target;
[M_poisson_hom_plot, ~] = local_make_Mplot(i,p_poisson_hom,s,z_poisson_hom,t,endo_poisson_hom);
[x_poisson_hom, y_poisson_hom] = local_rank_series(sum(M_poisson_hom_plot,1), x_cap);

tables = {
    local_make_hom_export_table("bursts_hom", x_bursts_hom, y_bursts_hom)
    local_make_hom_export_table("poisson_hom", x_poisson_hom, y_poisson_hom)
    };

if ~isempty(data_xy)
    tables{end+1,1} = local_make_hom_export_table("data", data_xy(:,1), data_xy(:,2));
    b = polyfit(data_xy(:,1), data_xy(:,2), 1);
    x_fit = linspace(0, log(x_cap), 200)';
    y_fit = polyval(b, x_fit);
    tables{end+1,1} = local_make_hom_export_table("fit", x_fit, y_fit);
end

out = vertcat(tables{:});
out = sortrows(out, {'series','x'});
if exist('plots','dir') ~= 7
    mkdir('plots');
end
writetable(out, fullfile('plots','theory_firmsize_homogeneous_stata.csv'));
end

function local_export_stata_full_unscaled_table(i,p,s,z,t,endo,data_xy,x_cap)
[M_bursts_plot, ~] = local_make_Mplot(i,p,s,z,t,endo);
[x_bursts, y_bursts] = local_rank_series(sum(M_bursts_plot,1), x_cap);

p_poisson = p;
p_poisson.zetapar = 1e10;
z_poisson.main = zeta(p_poisson.zetapar);
z_poisson.minone = zeta(p_poisson.zetapar-1);

endo_poisson = endo;
endo_poisson.tht = p_poisson.zetapar;
endo_poisson.x_abs_type1 = endo_poisson.x_abs_type1 * z.minone / z.main;
[M_poisson_plot, ~] = local_make_Mplot(i,p_poisson,s,z_poisson,t,endo_poisson);
[x_poisson, y_poisson] = local_rank_series(sum(M_poisson_plot,1), x_cap);

endo_poisson_unscaled = endo;
endo_poisson_unscaled.tht = p_poisson.zetapar;
[M_poisson_unscaled_plot, ~] = local_make_Mplot(i,p_poisson,s,z_poisson,t,endo_poisson_unscaled);
[x_poisson_unscaled, y_poisson_unscaled] = local_rank_series(sum(M_poisson_unscaled_plot,1), x_cap);

tables = {
    local_make_hom_export_table("bursts", x_bursts, y_bursts)
    local_make_hom_export_table("poisson", x_poisson, y_poisson)
    local_make_hom_export_table("poisson_unscaled", x_poisson_unscaled, y_poisson_unscaled)
    };

if ~isempty(data_xy)
    tables{end+1,1} = local_make_hom_export_table("data", data_xy(:,1), data_xy(:,2));
    b = polyfit(data_xy(:,1), data_xy(:,2), 1);
    x_fit = linspace(0, log(x_cap), 200)';
    y_fit = polyval(b, x_fit);
    tables{end+1,1} = local_make_hom_export_table("fit", x_fit, y_fit);
end

out = vertcat(tables{:});
out = sortrows(out, {'series','x'});
if exist('plots','dir') ~= 7
    mkdir('plots');
end
writetable(out, fullfile('plots','theory_firmsize_full_unscaled_stata.csv'));
end

function out = local_make_hom_export_table(series, x, y)
n = numel(x);
out = table(repmat(string(series),n,1), x(:), y(:), ...
    'VariableNames', {'series','x','y'});
end

function data_xy = local_load_data_points(x_cap)
data_xy = [];
csv_path = fullfile('plot_coordinates','firmsize_user.csv');
if exist(csv_path, 'file') ~= 2
    return;
end

T = readtable(csv_path);
if ~all(ismember({'share','level'}, T.Properties.VariableNames))
    return;
end

share = T.share;
level = T.level;
valid = isfinite(share) & isfinite(level) & share > 0 & level > 0 & level <= x_cap;
if ~any(valid)
    return;
end

data_xy = [log(level(valid)), log(share(valid))];
end

function local_export_separate_panels(series_main, series_kk, series_kk_equal, data_xy, x_cap, y_lim, opts)
nCols = numel(series_main.x);
panel_stub = {'type1','type2','full'};
fig_width = round(1600/nCols);
fig_height = 680;

if exist('plots','dir') ~= 7
    mkdir('plots');
end

for col = 1:nCols
    fig = figure('Color', 'w', 'Units', 'pixels', 'Position', [100 100 fig_width fig_height]);
    set(fig, 'PaperPositionMode', 'auto');
    ax = axes(fig, 'Position', [0.16 0.09 0.78 0.85]);
    if col == nCols
        data_this = data_xy;
        show_legend = 1;
    else
        data_this = [];
        show_legend = 0;
    end
    if opts.include_kkequal == 1
        x_kk_equal = series_kk_equal.x{col};
        y_kk_equal = series_kk_equal.y{col};
        suffix = '_kkequal';
    else
        x_kk_equal = [];
        y_kk_equal = [];
        suffix = '';
    end

    local_plot_one(ax, ...
        series_main.x{col}, series_main.y{col}, ...
        series_kk.x{col}, series_kk.y{col}, ...
        x_kk_equal, y_kk_equal, ...
        data_this, series_main.names{col}, x_cap, y_lim, show_legend, col == 1, opts);

    yt = get(ax, 'YTick');
    set(ax, ...
        'YTickLabel', arrayfun(@(v) sprintf('10^{%d}', round(v/log(10))), yt, 'UniformOutput', false), ...
        'TickLabelInterpreter', 'tex');

    pdf_out = fullfile('plots', ['theory_firmsize_', panel_stub{col}, suffix, '.pdf']);
    print(fig, pdf_out, '-dpdf', '-painters');
    close(fig);
end
end

function opts = local_default_options()
opts = struct();
opts.export_separate = 0;
opts.export_stata = 0;
opts.export_stata_hom = 0;
opts.export_stata_full_unscaled = 0;
opts.include_kkequal = 0;
opts.show_titles = 1;
end
